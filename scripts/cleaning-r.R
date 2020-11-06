library(tidyverse)
source("R/scrape.R")

Name_reviews_processed <- read.csv("data/Name_reviews_processed.csv",
                                   stringsAsFactors = FALSE)

#Romain: I think we should split the dataset at the beginning into pro and con corpus
reviews.corpus.pro <- corpus(x = Name_reviews_processed,
                             text_field = "employeer_pros")

#Romain: no need for word1 argument, as we are not concerned by symbols or whatever.
reviews.tokens.pro <- tokens(reviews.corpus.pro, 
                        remove_punct = TRUE, 
                        remove_symbols = TRUE, 
                        what="word")

reviews.tokens.pro <- tokens_tolower(reviews.tokens.pro) %>% 
  tokens_wordstem() %>%
  tokens_remove(stopwords("english"))


reviews.pro.dfm <- dfm(reviews.tokens.pro)
dim(reviews.pro.dfm)
#Romain: only 5503 differents tokens for the corpus=> reviews are short, straight, make sense

