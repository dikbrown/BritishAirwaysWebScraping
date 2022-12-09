library(tidytext)



### Get one word per row
tidy_reviews <- review_df %>%
  unnest_tokens(word, review)


### Get rid of stop words with an anti-join
data(stop_words)
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words)

### Get a count of common words
tidy_reviews %>%
  count(word, sort= TRUE) %>%
  head(20)



# ### All reviews are from 2022, so add month column
# tidy_reviews <- tidy_reviews %>%
#   mutate(month = month(date))

# ##################################################################
# ### Determines total count of positive & negative words for the month
# ##################################################################
# bing_sentiment <- tidy_reviews %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(month, sentiment) %>%
#   pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
#   mutate(sentiment = positive - negative, numReviews = review_counts$n)
# 
# 
# ##########
# ### plotting
# ############
# 
# library(ggplot2)
# ggplot(bing_sentiment, aes(month, sentiment, group = 1)) +
#   geom_line()


##################################################################
### Determines average sentiment score for each month, based on individual review scores
###     Bing lexicon
##################################################################
bing_sentiment_avg <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(reviewNumber, month, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  group_by(month) %>%
  summarize(average = mean(sentiment), sd = sd(sentiment)) %>%
  mutate(numReviews = review_counts$n)

bing_sentiment_avg

##########
### plotting
############
ggplot(bing_sentiment_avg, aes(month, average, group = 1)) +
  geom_line()+ 
  ggtitle("bing analysis") +
  geom_errorbar(aes(ymin = average - sd, ymax = average + sd))

##################################################################
### Determines average sentiment score for each month, based on individual review scores
###     Afinn lexicon
##################################################################
library(textdata)
afinn_sentiment_avg <- tidy_reviews %>%
  inner_join(get_sentiments("afinn")) %>%
  count(reviewNumber, month, value) %>%
  group_by(month, reviewNumber) %>%
  summarize(score = sum(value)) %>%
  group_by(month) %>%
  summarize(average = mean(score), sd = sd(score)) %>%
  mutate(numReviews = review_counts$n)

afinn_sentiment_avg

##########
### plotting
############
ggplot(afinn_sentiment_avg, aes(month, average, group = 1)) +
  geom_line()+ 
  ggtitle("afinn analysis") +
  geom_text(aes(label = paste(numReviews, "reviews")), nudge_y = .2)+
  geom_errorbar(aes(ymin = average - sd, ymax = average + sd))


##################################################################
### Determines average sentiment score for each month, based on individual review scores
###     nrc lexicon
##################################################################

nrc_sentiment_avg <- tidy_reviews %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))
             ) %>%
  count(reviewNumber, month, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  group_by(month) %>%
  summarize(average = mean(sentiment), sd = sd(sentiment))

nrc_sentiment_avg

##########
### plotting
############
ggplot(nrc_sentiment_avg, aes(month, average, group = 1)) +
  geom_line() + 
  ggtitle("nrc analysis") +
  geom_errorbar(aes(ymin = average - sd, ymax = average + sd))


###### Combine
ggplot() +
  geom_line(data = nrc_sentiment_avg, aes(month, average, group = 1)) +
  geom_line(data = bing_sentiment_avg, aes(month, average, group = 1)) +
  geom_line(data = afinn_sentiment_avg, aes(month, average, group = 1))


###Play with combining individual sentiments into one score
sentiment_avg <- data.frame(month = nrc_sentiment_avg$month, 
                       nrc = nrc_sentiment_avg$average,
                       afinn = afinn_sentiment_avg$average,
                       bing = bing_sentiment_avg$average) %>%
  mutate(avg = (nrc + afinn + bing) / 3)

ggplot(sentiment_avg, aes(month, avg, group = 1)) +
  geom_line()
