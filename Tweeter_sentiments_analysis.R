# first load all the necessary libraries for the processing
source('Load_libraries.R')
# search_tweets() # to connect to twitter and authorize browser pop-up on the first try
# options(max.print = 100)

# get all the trending topics now
US_trends <- get_trends("United States")
head(US_trends)
# get
# tweet_volume
# trends_available() # extract geographic locations where trends are available
# head(US_trends$trend)

trend_df <- US_trends %>% 
  group_by(trend) %>%
  summarize(tweet_vol = mean(tweet_volume))

# reorder the tweets by popularity
trends_df_sort <- arrange(trend_df, desc(tweet_vol))
# which(trends_df_sort[,1]=="#NFLDraft")
trends_df_sort$trend # to see all 40 trending topics
# lets perform the timeseries analysis next
# apply the necessary filter to obtain a manageable file
# #SaturdayThoughts
covid19_st <- search_tweets("#COVID19 -filter:retweets -filter:quote -filter:replies",n = 10000, include_rts = FALSE, lang = "en") # retryonratelimit = TRUE
head(covid19_st)
# check oout what information is available in the tweeter data we obtained
colnames(covid19_st)
# determine which tweets have been retweeted at least ones if you need to study just those
retwt_inds <- which(covid19_st$retweet_count>0)
# select only tweets which have been retweeted
# keep the status_id in order to account for the case if one use tweeted multiple times as a unique identifier 
covid_twts <- covid19_st[,c("status_id","screen_name","text","retweet_count","followers_count","friends_count","mentions_user_id","hashtags")]
# create a time series plot
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
  arrange(desc(n))

common_words$word
  
# remove unique stop words that don't add any meaning
uniq_sw <- data.frame(word = c("s","amp","t", "via", "m", "re", "don", "ve", "q", "gt", "o", "pm"))
# 
twts_clean <- twts_clean %>% 
   anti_join(uniq_sw, by = "word")

# let's see which words are the most popular in tweets
pal <- brewer.pal(8,"Dark2")

twts_clean %>%
  count(word) %>%
  with(wordcloud(word,n,random.order = FALSE, max.words = 100, colors = pal))

# now lets assign a quantitative score to tweets to determine whether they are positive or negative
# we will use the afinn lexicon here

twts_afinn <- twts_clean %>%
  inner_join(get_sentiments("afinn"))

# now lets assign the sentiment score to each unique tweet

# twts_sentiment <- twts_afinn %>%
#   group_by(id) %>%
#   #summarize(score = sum(value)) %>%
#   mutate(score = sum(value))
#   #arrange(desc(score)) %>%
#   ungroup()

afinn_sentiment <- twts_afinn %>%
  group_by(id,retweets,followers,friends,hashtags,mentions) %>%
  summarize(score = sum(value)) %>%
  arrange(desc(score))

# let's look at some of the most positive tweets to get inspiration for your day!
covid_twts$text[which(twts_tidy$id == afinn_sentiment$id[1])]
covid_twts$text[which(twts_tidy$id == afinn_sentiment$id[2])]
covid_twts$text[which(twts_tidy$id == afinn_sentiment$id[3])]
# those are indeed very positive tweets 

# change the retweets into binary form: 0/1
afinn_binary <- afinn_sentiment %>%
  mutate(retwtTF = ifelse(retweets > 0, TRUE, FALSE))

# change the retweets into 5 CATegories that define how "viral the tweet" is: "N (none)":0; "S (small)":1-10; "M (medium)":11-100; "L (large)":101-1,000; "V (viral)":>1,000
afinn_five <- afinn_sentiment %>%
  mutate(retwtCAT = ifelse(retweets == 0, "N",ifelse(retweets %in% 1:10, "S", ifelse(retweets %in% 11:100, "M",ifelse(retweets %in% 101:1000, "L","V")))))

# lets convert the categories for retweets into fanked factors since there is an inherent order to them
afinn_fiveRK <- afinn_five %>%
  mutate(retwtCAT = factor(retwtCAT,levels = c("N","S","M","L","V")))

afinn_fiveRK %>%
  ggplot(aes(retwtCAT)) +
  geom_bar() +
  xlab("Retweet category") +
  ggtitle("Distribution of the #COVID19 tweets into retweet categories")

# now we have the data set ready for running the logistic regression on it
tr <- sample(nrow(afinn_binary),round(nrow(afinn_binary)*0.6)) # split into training and test subsets
train <- afinn_binary[tr,]
test <- afinn_binary[-tr,]

model1 <- glm(retwtTF ~ followers + friends + score + mentions + hashtags, data = train, family = "binomial")
summary(model1)
p <- predict(model1,test, type = "response")
summary(p)
cl <- ifelse(p > 0.5, TRUE, FALSE)
table(cl, test$retwtTF)

confusionMatrix(factor(cl),factor(test$retwtTF))


# plot to see if there is any relationship
retwts_sentiment %>%
  ggplot(aes(score,retweets,size=followers)) +
  geom_point()# +
ylim(0,350)

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


  