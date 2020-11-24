# deperecated code from Ilia and Romain's EDA

# ---
# Ilia
# ---
# To turn it into a pivot and make it easier to explore
turn_review_cols <-
bank_reviews %>% pivot_longer(
  cols= c(employer_pros , employer_cons),
  names_to = "reviews",
  names_pattern = "employer_(\\D+)",
  values_to = "type_review"
)

# deprecated code for trying the result of sentiment analysis against other 
# benchmarks
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/sentimentr", "trinker/stansent", "sfeuerriegel/SentimentAnalysis", "wrathematics/meanr")
pacman::p_load(syuzhet, qdap, microbenchmark, RSentiment)
install.packages("qdap")

bank.trial <- bank_reviews[sample(nrow(bank_reviews), 200),]
ase <- bank.trial$employer_pros %>% get_sentences()

bank.trial

syuzhet <- setNames(as.data.frame(lapply(c("syuzhet", "bing", "afinn", "nrc"),
                                         function(x) get_sentiment(ase, method=x))), c("jockers", "bing", "afinn", "nrc"))

SentimentAnalysis <- apply(analyzeSentiment(ase)[c('SentimentGI', 'SentimentLM', 'SentimentQDAP') ], 2, round, 2)
colnames(SentimentAnalysis) <- gsub('^Sentiment', "SA_", colnames(SentimentAnalysis))

left_j(data.frame(
  stanford = sentiment_stanford(ase)[["sentiment"]],
  sentimentr_jockers_rinker = round(sentiment(ase, question.weight = 0)[["sentiment"]], 2),
  sentimentr_jockers = round(sentiment(ase, lexicon::hash_sentiment_jockers, question.weight = 0)[["sentiment"]], 2),    
  sentimentr_huliu = round(sentiment(ase, lexicon::hash_sentiment_huliu, question.weight = 0)[["sentiment"]], 2),    
  sentimentr_sentiword = round(sentiment(ase, lexicon::hash_sentiment_sentiword, question.weight = 0)[["sentiment"]], 2),    
  RSentiment = calculate_score(ase), 
  SentimentAnalysis,
  meanr = score(ase)[['score']],
  syuzhet,
  sentences = ase,
  stringsAsFactors = FALSE
), "sentences")

# ---
# Romain
# ---
