library(tidyverse)
source("R/scrape.R")
#> Loading libraries...
#> Sourcing functions...

# example urls, we'll go with Google
tesla_url <- "https://www.glassdoor.com/Reviews/Tesla-Reviews-E43129"
# apple_url <- "https://www.glassdoor.com/Reviews/Apple-Reviews-E1138"
google_url <- "https://www.glassdoor.com/Reviews/Google-Reviews-E9079"

# loop through n pages
pages <- 1:5
out <- lapply(pages, function(page) {
  Sys.sleep(1)
  try_scrape_reviews(google_url, page)
})

# with random time

# filter for stuff we successfully extracted
reviews <- bind_rows(Filter(Negate(is.null), out), .id = "page")

# remove any duplicates, parse the review time
reviews %>%
  distinct() %>%
  mutate(
    review_time = clean_review_datetime(review_time_raw),
    page = as.numeric(page)
  ) %>% 
  select(
    page,
    review_id,
    review_time_raw,
    review_time,
    review_title,
    employee_role,
    employee_history,
    employeer_pros,
    employeer_cons,
    employeer_rating,
    work_life_balance,
    culture_values,
    career_opportunities,
    compensation_and_benefits,
    senior_management
  ) %>% 
  glimpse()



x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()