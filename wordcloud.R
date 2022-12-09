
############################
###   Create word cloud  ###
############################

library(wordcloud)
library(wordcloud2)
library(tm)
library(RColorBrewer)

docs <- Corpus(VectorSource(txt))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
words <- sort(rowSums(mat), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

set.seed(42)

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
