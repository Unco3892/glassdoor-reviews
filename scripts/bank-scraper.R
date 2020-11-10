library(tidyverse)
source("scraper/scrape.R")
#> Loading libraries...
#> Sourcing functions...


# A vector with the URL of the desired companies
companies <- c(
  "https://www.glassdoor.com/Reviews/J-P-Morgan-Reviews-E145",
  "https://www.glassdoor.com/Reviews/TD-Reviews-E3767",
  "https://www.glassdoor.com/Reviews/UBS-Reviews-E3419",
  "https://www.glassdoor.com/Reviews/HSBC-Holdings-Reviews-E3482",
  "https://www.glassdoor.com/Reviews/Deutsche-Bank-Reviews-E3150"
)

# define the minimum number of required reviews
review_per_company <- 5010

# loop through n pages given that there are 10 reviews per page
pages <- 1:(ceiling(review_per_company/10))

# create sleep generator with a mean 5 and standard deviation of 2
rnorm_sleep_generator <-
  abs(rnorm(review_per_company, mean = 5, sd = 2))

# here we use a loop to go over the list of companies one by one and save the
# results in the all_review tibble
all_reviews <- tibble()

for (i in seq_along(companies)) {
  # define the function to extract URLs
  out <- lapply(pages, function(page) {
    Sys.sleep(rnorm_sleep_generator[pages])
    try_scrape_reviews(companies[i], page)
  })
  # filter for stuff we successfully extracted
  single_reviews <-
    bind_rows(Filter(Negate(is.null), out), .id = "page")
  all_reviews <- bind_rows(all_reviews, single_reviews)
}

# remove any duplicates, parse the review time
all_reviews %<>%
  distinct() %>%
  mutate(
    review_time = clean_review_datetime(review_time_raw),
    page = as.numeric(page)
  ) %>% 
  select(-review_time_raw)

# Writing the finished part
write_csv(all_reviews, here::here("data/Bank_reviews_processed.csv"))
