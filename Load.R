library(rvest)
library(tidyverse)
library(stringr)
library(purrr)
library(lubridate)

# url <- 'https://www.airlinequality.com/airline-reviews/british-airways/?sortby=post_date%3ADesc&pagesize=100'
# url2 <- 'https://www.airlinequality.com/airline-reviews/british-airways/page/2/?sortby=post_date%3ADesc&pagesize=100'
# 
# p1 <- read_html(url)
# p2 <- read_html(url2)

#################
### for loop  ###
#################
reviews <- c()
dates <- dmy(c())
for (i in 1:35) {
  # splitreview <- function(x) {
  #   if (grepl('|', x)) {
  #     x %>%
  #       strsplit('\\|') %>%
  #       sapply('[[', 2)}
  # }

  url <- paste0('https://www.airlinequality.com/airline-reviews/british-airways/page/',i,'/?sortby=post_date%3ADesc&pagesize=100')
  p <- read_html(url)
  cat(url, '\n')
  review <- p %>%
                   html_elements(".text_content ") %>%
                   html_text()
  review <- gsub('.*\\|', '', review)
  
  reviews <- c(reviews, review)
  # reviews <- c(reviews,
  #              p %>%
  #                html_elements(".text_content ") %>%
  #                html_text() %>%
  #                strsplit('\\|') %>%
  #                sapply('[[', 2))
  dates <- c(dates, 
             p %>%
               html_elements("time") %>%
               html_text() %>%
               dmy())
}

########################
### end of for loop  ###
########################


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



# #######################
# ### Testing section ###
# #######################
# url10 <- 'https://www.airlinequality.com/airline-reviews/british-airways/page/10/?sortby=post_date%3ADesc&pagesize=100'
# p10 <- read_html(url10)
# r10 <- p10 %>%
#   html_elements(".text_content") %>%
#   html_text() 
# r10 <- gsub('.*\\|', '', r10) 
# 
# url11 <- 'https://www.airlinequality.com/airline-reviews/british-airways/page/11/?sortby=post_date%3ADesc&pagesize=100'
# p11 <- read_html(url11)
# 
# r11 <- p11 %>%
#   html_elements(".text_content") %>%
#   html_text() 
# 
# reviews <- c(p1 %>%
#                   html_elements(".text_content ") %>%
#                   html_text() %>%
#                   strsplit('\\|') %>%
#                   sapply('[[', 2),
#                  p2 %>%
#                    html_elements(".text_content ") %>%
#                    html_text() %>%
#                    strsplit('\\|') %>%
#                    sapply('[[', 2))
# 
# # 
# # txt <- strsplit(reviews, '\\|') %>%  # split out "verified tag" from text
# #   sapply('[[', 2) %>%            # get 2nd element from split
# #   paste(collapse = '')
# 
# ### Get all text from first 100 reviews and put it in a single string
# txt <- paste(reviews, collapse = '')
# txt <- gsub('\\"', '', txt)    # Remove \" from text string
# 
# 
# ###############################
# ### End of testing section  ###
# ###############################

# ############
# ## Get date for each review   - Moved into for-loop above
# ############
# library(lubridate)
# 
# dates <- c()
# dates <- c(dates,
#            p1 %>%
#                 html_elements("time") %>%
#                 html_text() %>%
#                 dmy())
# dates <- c(dates,
#            p2 %>%
#                  html_elements("time") %>%
#                  html_text() %>%
#                  dmy())
