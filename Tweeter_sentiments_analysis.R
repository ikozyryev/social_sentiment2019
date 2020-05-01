# This program analyzes tweets with #COVID19 hashtags to see what parameters of the tweet effect the number of retweets.
# Specifically, I considered the following tweet variables: i). followers_count ii). friends_count iii). number of hashtags 
# iv). number of user mentions v). sentiment of the text (how positive or negative the wording of the tweeter was).
# My initial hypothesis was that the more negative the tweet was the more retweets it will get. Let's see below if that applies to 
# tweets related to covid19...

# first load all the necessary libraries for the processing
source('Load_libraries.R')
# search_tweets() # to connect to twitter and authorize browser pop-up on the first try
# options(max.print = 100)

# Let's see which topics are trending now ---------------------------------

# get all the trending topics now
US_trends <- get_trends("United States")
head(US_trends)

US_trends_sorted <- US_trends %>%
  group_by(trend) %>%
  summarize(tweet_vol = mean(tweet_volume)) %>%
  arrange(desc(tweet_vol))

# let's look at the top 10 trending topics now
US_trends_sorted$trend[1:10]

# Read in tweets and process the text -------------------------------------

# you can pick a different hashtag that is trending now to analyze. However, I was interested in seeing how tweets with #COVID19 hashtags
# you can read in 18,000 tweets every 15 minutes. So if you want to study more than 18k tweets together, enable "retryonratelimit" option below
covid19_st <- search_tweets("#COVID19 -filter:retweets -filter:quote -filter:replies",n = 1000, include_rts = FALSE, lang = "en") # retryonratelimit = TRUE
head(covid19_st)
# check out what information is available in the tweeter data we obtained
colnames(covid19_st)
# determine which tweets have been retweeted at least ones if you need to study just those
retwt_inds <- which(covid19_st$retweet_count>0)
# by adding retwt_inds to the rows below you can select only tweets that have been retweeted before 
# at different stages of the project I was interested in analizing only tweets which have been retweeted
# keep the status_id in order to account for the case if one user tweeted multiple times as a unique identifier 
covid_twts <- covid19_st[,c("status_id","screen_name","text","retweet_count","followers_count","friends_count","mentions_user_id","hashtags")]

twt_txt <- covid_twts$text # get the tweet text
# start to clean the tweets data
# let's clean text for tweets before starting to analyze
twt_txt_no_url <- rm_twitter_url(twt_txt) # first remove the URL 
head(twt_txt_no_url)
twt_chrs <- gsub("[^A-Za-z]"," ", twt_txt_no_url) # only keep the upper and lower case characters; replace everything else with a space
head(twt_chrs)

# lets count the number of mentions and hashtags
twts_tidy <- covid_twts %>%
  mutate(text = twt_chrs) %>%
  mutate(mentions = lengths(mentions_user_id)) %>%
  mutate(hashtags = lengths(hashtags))
# notice that most of the tweets don't have any mentions with "NA" in place so we need to properly account for that below
twts_inds1 <- which(is.na(covid_twts$mentions_user_id)) # find which mentions are non-existent
twts_tidy$mentions[twts_inds1] <- 0 # correct the number of mentions entries for those indeces
# also note that each tweet has at least one hashtag since we specifically searched for #COVID19 tweets to begin with!

# change the column names for convinience
twts_tidy <- twts_tidy %>% 
  dplyr::rename(id = status_id, retweets = retweet_count, friends = friends_count, followers = followers_count)
colnames(twts_tidy)
# remove the columns that we no longer need
twts_tidy <- twts_tidy %>% 
  select(-c(screen_name,mentions_user_id))

# unnest tokens and remove stop words
twts_clean <- twts_tidy %>%
  unnest_tokens(word,text) %>%
  anti_join(get_stopwords())

# let's look at the most common words used
common_words <- twts_clean %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(n=25, wt = n)

common_words$word
  
# remove unique stop words that don't add any meaning
# also notice that we removed "covid" since all the tweets have it because we specifically searched for #COVID19
# otherwise the wordcloud is overwhelmed by the "covid" word
uniq_sw <- data.frame(word = c("s","covid","amp","t", "via", "m", "re", "don", "ve", "q", "gt", "o", "pm"))
# 
twts_clean <- twts_clean %>% 
   anti_join(uniq_sw, by = "word")


# Let's visualize the most common words -----------------------------------


# let's see which words are the most popular in tweets
pal <- brewer.pal(8,"Dark2")

twts_clean %>%
  count(word) %>%
  with(wordcloud(word,n,random.order = FALSE, max.words = 100, colors = pal))


# Perform tweet sentiment analysis with afinn lexicon ---------------------


# now lets assign a quantitative score to tweets to determine whether they are positive or negative
# we will use the afinn lexicon here

twts_afinn <- twts_clean %>%
  inner_join(get_sentiments("afinn"))

# now lets assign the sentiment score to each unique tweet

afinn_sentiment <- twts_afinn %>%
  group_by(id,retweets,followers,friends,hashtags,mentions) %>%
  summarize(score = sum(value)) %>%
  arrange(desc(score))

# let's look at some of the most positive tweets to get inspiration for your day!
covid_twts$text[which(twts_tidy$id == afinn_sentiment$id[1])]
covid_twts$text[which(twts_tidy$id == afinn_sentiment$id[2])]
covid_twts$text[which(twts_tidy$id == afinn_sentiment$id[3])]
# those are indeed very positive tweets 


# Apply logistic regression  --------
# to see what factors determine whether the tweet is retweeted at all

# change the retweets into binary form: "N"(no retweets)/"A"(any retweets)
afinn_binary <- afinn_sentiment %>%
  mutate(retwtBi = ifelse(retweets > 0, "A", "N"))
# convert the retweet indicator into a ranked factor: N < A
afinn_binary <- afinn_binary %>%
  mutate(retwtBi = factor(retwtBi, levels = c("N", "A")))

afinn_binary %>%
  ggplot(aes(retwtBi)) +
  geom_bar() +
  xlab("Retweet category") +
  ggtitle("Distribution of the #COVID19 tweets into retweet categories")

# let's see what fraction of the tweets have at least one retweet
afinn_binary %>%
  group_by(retwtBi) %>%
  summarize(n = n())

# for this given run, looks like 39% of 10,000 tweets I am analyzing have been retweeted

# now we have the data set ready for running the logistic regression on it
tr <- sample(nrow(afinn_binary),round(nrow(afinn_binary)*0.6)) # split into training and test subsets
train <- afinn_binary[tr,]
test <- afinn_binary[-tr,]

model1LR <- glm(retwtBi ~ followers + friends + score + mentions + hashtags, data = train, family = "binomial")
summary(model1)

p <- predict(model1,test, type = "response")

# summary(p)

# let's use the same variables but fit the decision tree instead of logit regression 
model1DT <- rpart(retwtBi ~ followers + friends + score + mentions + hashtags, data = train, method = "class")
rpart.plot(model1DT)
pDT <- predict(model1DT,test, type = "class")

# let's try random forest


model1RF <- train(retwtBi ~ followers + friends + score + mentions + hashtags, data = train, method = "ranger",tuneLength = 5)
pRF <- predict(model1RF,test, type = "prob")

# let's plot the ROC curve
caTools::colAUC(cbind(p,pDT,pRF), test$retwtBi,plotROC = T)

# area under the ROC curve can be used to compare various models in order to pick the optimal one

cl <- ifelse(p > 0.5, "A", "N")
# table(cl, test$retwtBi)

confusionMatrix(factor(cl, levels = c("N","A")),test$retwtBi)

# from the summary table of model1 we can see that only follower, mentions and hashtags are statistically significant in determining 
# whether the tweet will be retweeted at least once or not
# let's run the second model now only with those parameters
model2 <- glm(retwtTF ~ followers + mentions + hashtags, data = train, family = "binomial")
summary(model2)
p <- predict(model2,test, type = "response")
summary(p)
cl <- ifelse(p > 0.5, TRUE, FALSE)
table(cl, test$retwtTF)

# one of the advantages of the ligistic regression is that it's coefficient lend to interpretation 
exp(coef(model2)) # we can see that mentions of other users have the largest effect on log-odds. This is something that
# the author of the tweet can control thus potentially increasing the odd of the tweet being retweeted

# remember that we are looking at the coefficients for log-odds here

# Cross-validation --------------------------------------------------------
# use cross-validation from the caret library
train <- train %>%
  mutate(retwtBi = factor(retwtTF))

test <- test %>%
  mutate(retwtBi = factor(retwtTF))

set.seed(40)

cv_model1 <- train(
  retwtBi ~ followers,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

cv_model2 <- train(
  retwtBi ~ mentions,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

cv_model3 <- train(
  retwtBi ~ hashtags,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

cv_model4 <- train(
  retwtBi ~ followers + mentions + hashtags,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# let's comapre the performance of different models now

summary(
  resamples(
    list(
      cv1 = cv_model1,
      cv2 = cv_model2,
      cv3 = cv_model3,
      cv4 = cv_model4
    )
  )
)$statistics$Accuracy

# notice that the accuracy is about the same for all the tested models indicating that adding additional variables that we have does not lead to better predicting power 
# We can try adding more information about the tweet to begin with (remember there are 90 different variables associated with a single tweet we can get)
# however, personally I am interested in seeing which tweets influence many people: i.e. what causes tweet to become "viral" and be retweeted
# thousands of times and potentially have a significant influence on the perception of many people about COVID19. Thus, let's actually divide
# tweets which have been retweet into multiple sub-categories. 

# Multivariate_Logit ------------------------------------------------------

# change the retweets into 5 CATegories that define how "viral the tweet" is: "N (none)":0; "S (small)":1-10; "M (medium)":11-100; "L (large)":101-1,000; "V (viral)":>1,000
# afinn_five <- afinn_sentiment %>%
#   mutate(retwtCAT = ifelse(retweets == 0, "N",ifelse(retweets %in% 1:10, "S", ifelse(retweets %in% 11:100, "M",ifelse(retweets %in% 101:1000, "L","V")))))

# In order to do the random forest regression we cannot have any empty categories; so delete the Viral category since it is often empty
afinn_four <- afinn_sentiment %>%
  mutate(retwtCAT = ifelse(retweets == 0, "N",ifelse(retweets %in% 1:10, "S", ifelse(retweets %in% 11:100, "M", "L"))))

# lets convert the categories for retweets into fanked factors since there is an inherent order to them
# this order will be used with the multivariate logistic regression
# afinn_fiveRK <- afinn_five %>%
#   mutate(retwtCAT = factor(retwtCAT,levels = c("N","S","M","L","V")))

afinn_RK <- afinn_four %>%
  mutate(retwtCAT = factor(retwtCAT,levels = c("N","S","M","L")))

afinn_RK %>%
  ggplot(aes(retwtCAT)) +
  geom_bar() +
  xlab("Retweet category") +
  ggtitle("Distribution of the #COVID19 tweets into retweet categories")

# let's display the sentiment distribution for each category

afinn_RK %>%
  ggplot(aes(retwtCAT,score)) +
  geom_boxplot() +
  xlab("Retweet category") +
  ylab("Sentiment score") +
  ggtitle("Sentiment distribution of #COVID19 tweets for each retweet category")

# # first let's set the baseline outcome
# afinn_fiveRK %>% 
#   mutate(retwtCAT2 = relevel(retwtCAT, ref = "N"))
## because there is an inherent order in the retweeting category: V > L > M > S > N
# we will use ordinal Logistic regression model to fit the data
# specifically we will use "polr()" function from MASS package which 
# runs proportional odds logistic regression
# model2 <- polr(retwtCAT ~ followers + friends + score + mentions + hashtags, data = afinn_fiveRK)
# summary(model2)

#### let's now do the multinomial logistic regression here

model3 <- multinom(retwtCAT ~ followers + friends + score + mentions + hashtags, data = afinn_RK)
summary(model3)

z <- summary(model3)$coefficients/summary(model3)$standard.errors

z 

# let's calculate the p-values for Wald's test to estimate the statistical significance for various 
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# let's extract the coefficients now

exp(coef(model3))

# these coefficient allow us to estimate the relative risk ratios


# Decision trees to determine retweet categories --------------------------

# # in order to run 
# afinn_fiveRK <- afinn_fiveRK %>%
#   mutate(isN = ifelse(retwtCAT == "N", T, F)) %>%
#   mutate(isS = ifelse(retwtCAT == "S", T, F)) %>%
#   mutate(isM = ifelse(retwtCAT == "M", T, F)) %>%
#   mutate(isL = ifelse(retwtCAT == "L", T, F)) %>%
#   mutate(isV = ifelse(retwtCAT == "V", T, F))

# first split the retweet categorical data into train and test subsets
tr2 <- sample(nrow(afinn_fiveRK),round(nrow(afinn_RK)*0.6)) # split into training and test subsets
trainRK <- afinn_RK[tr2,]
testRK <- afinn_RK[-tr2,]

modelRK_DT <- rpart(retwtCAT ~ followers + friends + score + mentions + hashtags, data = trainRK, method = "class")# ,xval = 5)

predRK_DT <- predict(modelRK_DT,testRK, type = "class")

confusionMatrix(predRK_DT, testRK$retwtCAT)

# looks like a simple decision tree performs very poorly here

# let's try with random forest instead

modelRK_RF <- randomForest(retwtCAT ~ followers + friends + score + mentions + hashtags, data = trainRK, method = "class")

predRK_RF <- predict(modelRK_RF,testRK, type = "class")

confusionMatrix(predRK_RF, testRK$retwtCAT)


# plot to see if there is any relationship
retwts_sentiment %>%
  ggplot(aes(score,retweets,size=followers)) +
  geom_point()# +
ylim(0,350)


# Use Bing lexicon for sentiment analysis ---------------------------------

# While "bing" lexicon only specifies whether a word is positive or negative it still can be useful for sentiment analysis

### now lets try using "bing" lexicon
twts_bing <- twts_clean %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(golden = followers/friends) %>%

twts_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","blue"),max.words =100)

# ggsave("COVID19_wordcloud.pdf", width = 5, height = 5)

dev.copy(pdf,file = "COVID19_wordcloud.pdf",useDingbats=F)
dev.off()

twts_bing %>%
  ggplot(aes(retweets)) +
  geom_bar() +
  xlim(0,50)
  scale_x_log10()

retwts_bing_sent <- twts_bing %>%
  group_by(id,retweets,followers,golden) %>%
  # filter(retweets > 5) %>%
  count(id,retweets,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(word_cnt = positive + negative)

retwts_bing_sent %>%
  ggplot(aes(sentiment,retweets)) +
  geom_bar(stat = "identity") 

twts_bing %>%
  group_by(sentiment) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")

bing_word_counts <- twts_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

retwts_bing_sent %>%
  ggplot(aes(word_cnt,retweets,size = golden)) +
  geom_point() 
+
  geom_bar(stat = "identity") 
+
  geom_smooth(model = lm) +
  ylim(0,500)

retwts_bing_sent %>%
  ggplot(aes(followers,retweets)) +
  geom_point() +
  geom_smooth() +
  ylim(0,500) +
  scale_x_log10()

  summarize(score = sum(value)) %>%
  #mutate(score = sum(value)) %>%
  arrange(desc(score))# %>%
#ungroup()

twts_clean %>%
  count(word,sort = TRUE)

# convert the text to a vector source > corpus
twt_corpus <- twt_txt_chrs %>%
  VectorSource() %>%
  Corpus()
twt_corpus[[2]]$content

twt_corpus_lwr <- tm_map(twt_corpus, tolower) # convert all the letters to lower case
twt_corpus_lwr[[2]]$content # check to see if it worked

# remove stopwords next
twt_corpus_no_stpwd <- tm_map(twt_corpus_lwr, removeWords, stopwords("english"))
twt_corpus_no_stpwd[[2]]$content

# next remove additional white spaces
twt_corpus_final <- tm_map(twt_corpus_no_stpwd, stripWhitespace)
twt_corpus_final[[2]]$content

custom_stop <- c("Covid")

#### let's start processing the tweeter data now

# visualize populat terms from tweets

term_count <- freq_terms(twt_corpus_final, 60)

s_plot(covid19_st, by = "minutes", color = "red")

stream_tweets("covid_19",parse = FALSE,file_name = '')
# get the time series data
camry_st <- ts_data(camry_st, by='hours')
head(camry_st)
names(camry_st) <- c("time","camry_n")
head(camry_st)

merged_df <- merge(tesla_ts, camry_st, by = 'time', all = TRUE)
head(merged_df)

# stack the tweet frwquency columns using melt() function
# prepare the data frame for plotting with ggplot
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")
head(melt_df)

# plot the results now
ggplot(data = melt_df, 
       aes(x = time, y = value, col = variable)) + 
      geom_line(lwd = 0.8)

#### let's look at the CDCgov twits 

CDC <- get_timelines("@katyperry", n = 3) # max number of tweets per user is 3,200


  