# To turn it into a pivot and make it easier to explore
turn_review_cols <-
bank_reviews %>% pivot_longer(
  cols= c(employer_pros , employer_cons),
  names_to = "reviews",
  names_pattern = "employer_(\\D+)",
  values_to = "type_review"
)