# Why joining two sides of the reviews and then using valence shifters seperately
# does not work

# ---
# Joining the two kind of reviews
joined_data <- bank_reviews %>% 
  unite(reviews_combined,c(employer_pros,employer_cons), 
                       sep = ". ", remove = FALSE)

# Getting the sentiments by company and then the review
shifters <- joined_data %>%
  dplyr::mutate(pros = get_sentences(employer_pros)) %$%
  sentiment_by(pros, list(company, review_id)) # this method does not work as
# well as seperate sentiment analysis for pros and cons and then adding them


# Getting the sentiments by company
shifters <- joined_data %>%
  dplyr::mutate(pros = get_sentences(employer_pros)) %$%
  sentiment_by(pros, list(company)) # We can see that this method 
# also does not catch HSBC and UBS correctly but has sd's which is good.

# Computing the average score 
average_score <- shifters %>% group_by(company) %>%
  summarize(ave_sentiment=mean(ave_sentiment))

actual_score <- 
  aggregate(employer_rating~company, data=bank_reviews, FUN=mean) %>% 
  arrange(desc(employer_rating))

# Comparing the score of valence shifter vs the basic numerical sentiment
actual_score %>%
  left_join(average_score,by = "company") %>% 
  left_join(valence_score,by = "company") %>% 
  left_join(scores_compared,by = "company") %>%
  rename(actual_employer_rating = employer_rating) %>% 
  arrange(desc(2)) %>% kable_maker()

# The default averaging algorithm is average_downweighted_zero but there are also
# other options are average_weighted_mixed_sentiment and average_mean which did not
# bring improvements in our case

# ---
# The approach we take is that we their sentiments separately and then add them
# to have a third column and use that third column in our final analysis.
pro_sentiment <-
  bank_reviews %>%
  dplyr::mutate(pros = get_sentences(employer_pros)) %$%
  sentimentr::sentiment_by(pros, list(company, review_id)) %>% 
  dplyr::rename(pro_score = ave_sentiment) %>% 
  dplyr::select(company, review_id, pro_score)

con_sentiment <-
  bank_reviews %>%
  dplyr::mutate(cons = get_sentences(employer_cons)) %$%
  sentimentr::sentiment_by(cons, list(company, review_id)) %>%
  dplyr::rename(con_score = ave_sentiment) %>%
  dplyr::select(company, review_id, con_score)

overall_score <-pro_sentiment %>% 
  left_join(con_sentiment,by = c("company", "review_id")) %>% 
  mutate(total_score = pro_score+con_score) %>% 
  tibble()

joined_review <- 
  bank_reviews %>% 
  left_join(overall_score, by= c("company","review_id"))

# Counting and taking the log of the number of words of each review
counter <- function(a_word_column) {
  sapply(strsplit(
    as.character(a_word_column), "[[:space:]]"
  ), length)
}

logs_added <- joined_review %>%
  mutate(
    across(c(employer_pros, employer_cons),
           c("counted" = counter)),
    total_counts = (employer_pros_counted + employer_cons_counted),
    log_total_counts = log10(total_counts) #could be with log or inervse as well
  )









# plotting the logs
ggplot(logs_added, aes(x=total_counts)) +
  geom_histogram(binwidth=10, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(total_counts, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# plotting the logs
ggplot(logs_added, aes(x=log_total_counts)) +
  geom_histogram(binwidth=0.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(log_total_counts, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# A nice distribution density option
library(ggpubr)
library(moments)

ggdensity(logs_added, x = "total_counts", fill = "lightgray", title = "total_counts") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(logs_added, x = "log_total_counts", fill = "lightgray", title = "log_total_counts") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# We still don't have a normal distribution and this has to be treated

# ALSO TAKE CARE OF DOTS AS IT'S DOING SOME OF THE CALCULATIONS WRONG.
# Consider using the coxplot

library(MASS)

Box = boxcox(logs_added$total_counts ~ 1,              # Transform Turbidity as a single vector
             lambda = seq(-6,6,0.1)      # Try values -6 to 6 by 0.1
)


# We need to do a square tranfomration and below is why
# "dependent variable values decrease more rapidly with increasing independent 
# variable values – you can first consider a square transformation."
# ==> This means that if people are writing more it may be because they are going
# to write something negative


# Taking whether they were former vs current employer


# May want to use validate_sentiment later for assessing how good we have how
# accurately we mapped one sentiment against the other