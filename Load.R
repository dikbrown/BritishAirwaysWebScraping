library(rvest)
library(tidyverse)
library(stringr)
library(purrr)

url <- 'https://www.airlinequality.com/airline-reviews/british-airways/?sortby=post_date%3ADesc&pagesize=100'
url2 <- 'https://www.airlinequality.com/airline-reviews/british-airways/page/2/?sortby=post_date%3ADesc&pagesize=100'

p1 <- read_html(url)
p2 <- read_html(url2)

reviews <- c(p1 %>% 
                  html_elements(".text_content ") %>%
                  html_text() %>%
                  strsplit('\\|') %>%
                  sapply('[[', 2),
                 p2 %>% 
                   html_elements(".text_content ") %>%
                   html_text() %>%
                   strsplit('\\|') %>%
                   sapply('[[', 2))

# 
# txt <- strsplit(reviews, '\\|') %>%  # split out "verified tag" from text
#   sapply('[[', 2) %>%            # get 2nd element from split
#   paste(collapse = '')

### Get all text from first 100 reviews and put it in a single string
txt <- paste(reviews, collapse = '')
txt <- gsub('\\"', '', txt)    # Remove \" from text string

############
## Get date for each review
############
library(lubridate)

dates <- c(p1 %>%
                html_elements("time") %>%
                html_text() %>%
                dmy(),
           p2 %>%
                 html_elements("time") %>%
                 html_text() %>%
                 dmy())

###############
###  Combine into df
###############

dated_reviews <- data.frame(date = dates, review = reviews)

###
### Keep only reviews for current year
###
dated_reviews <- dated_reviews[year(dated_reviews$date) >= year(date("2022-01-01")),]

###
### Only one December review so far, so remove it.
###
dated_reviews <- dated_reviews[-1,]



review_df <- dated_reviews %>%
  mutate(reviewNumber = row_number(),
         month = month(date, label = TRUE))


review_counts <- review_df[,-(1:2)] %>%
  group_by(month) %>%
  summarize(n = n())
