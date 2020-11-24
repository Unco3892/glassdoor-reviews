library(quanteda)
library(text2vec)
library(ranger)
library(caret)

# Not that this is a classification approach, we will also do a regression one

# mode <- lm(employer_rating~ total_counts ,
#            data=logs_added)

# do a cox-box distribution to take what to take for the word count
mode <- lm(employer_rating~ total_count+total_score,
           data=logs_added)

layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(mode$resid~logs_added$employer_rating[order(logs_added$employer_rating)],
     main="Rating x Residuals\nfor Simple Regression",
     xlab="Employer Rating", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(mode$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(mode$resid)
qqline(mode$resid)

# REMEMBER THAT SOME OF TEXT COLUMNS WERE NOT COUNTED PROPERLY!

# Using the corpus and word embedding method
reviews_tokenized <-
  logs_added %>%
  unite(
    reviews_combined,
    c(employer_pros, employer_cons),
    sep = ". ",
    remove = FALSE
  ) %>%
  corpus(text_field = "reviews_combined") %>% 
  quanteda::tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE,
    what = "word1"
  ) %>%  
  tokens_tolower(.) %>% ## option for removing hastags and tags symbols 
  # (@, #, etc.)
  tokens_remove(stopwords("english")) %>%
  # removes tweets with 2 or fewer tokens
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) # at least 5 words


fcm <- fcm(
  reviews_tokenized,
  context = "window",
  count = "weighted",
  window = 5,
  weights = 1 / (1:5),
  tri = FALSE
)

glove <- GlobalVectors$new(rank = 100, x_max = 1)

word_vectors_main <- glove$fit_transform(fcm, n_iter = 100)

# The rest of the word embedding
word_vectors_context <- glove$components
reviews_glove <- word_vectors_main + t(word_vectors_context)

ndoc <- length(reviews_tokenized) # number of documents
centers <-
  matrix(nr = ndoc, nc = 100) # document embedding matrix (1 document per row)
for (i in 1:ndoc) {
  words_in_i <- reviews_glove[reviews_tokenized[[i]], , drop = FALSE]
  centers[i,] <- apply(words_in_i, 2, mean)
}
row.names(centers) <- names(reviews_tokenized)

y <- factor(docvars(reviews_tokenized, "employer_rating"))

df <- data.frame(Class=y, X=centers)

index.tr <- sample(size=round(0.8*length(y)), x=c(1:length(y)), replace=FALSE)

set.seed(1235)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]
reviews_fit <- ranger(Class ~ ., 
                     data = df.tr)

pred.te <- predict(reviews_fit, df.te)

confusionMatrix(data=pred.te$predictions, reference = df.te$Class)

# ---
# More methods tried below
# ---
# By themselves the words are weak predictors
# Using the tf_IDF and adding features
reviews_tfidf <- dfm(reviews_tokenized)
reviews_lsa <-  textmodel_lsa(review_tfidf, nd=100)

df <- data.frame(Class=y, X=reviews_lsa$docs)
df <- cbind(df, 
            val_shift_score = docvars(reviews_tokenized, c("total_score")),
            log_total_counts=docvars(reviews_tokenized, c("log_total_counts")))

df.tr <- df[index.tr,]
df.te <- df[-index.tr,]
review_fit_supp <- ranger(Class ~ ., 
                     data = df.tr, 
                     importance = "impurity")
pred.te <- predict(review_fit_supp, df.te)
confusionMatrix(data=pred.te$predictions, reference = df.te$Class)

# Another idea is to see how long the review title was

# Trying out other learners
reviews_lsa <- textmodel_lsa(review_tfidf, nd=100)
df <- data.frame(Class=y, X=reviews_lsa$docs, 
                 val_shift_score = docvars(reviews_tokenized, c("total_score")),
                 log_total_counts=docvars(reviews_tokenized, c("log_total_counts")))
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]

# trying svm --> Best result so far with nd = 100
library(e1071)
svm.model <- svm(Class ~ ., data = df.tr, kernel="radial")
svm.pred  <- predict(svm.model, df.te)
confusionMatrix(data=svm.pred, reference = df.te$Class)


# trying glmnet
library(glmnet)
glmnet.model <- cv.glmnet(x = as.matrix(df.tr[,-1]), y = df.tr[,1], family = "multinomial", 
                          alpha=0.5)
glmnet.pred <- predict(glmnet.model, newx = as.matrix(df.te[,-1]), s = "lambda.min", type="class")
confusionMatrix(data=factor(glmnet.pred), reference = df.te$Class)


