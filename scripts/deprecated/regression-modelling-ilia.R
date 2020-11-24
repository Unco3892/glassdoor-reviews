# Please note that you need to run the modelling-t
# ---
# Using a regression approach
# ---
# without supplementary features
# WHAT MORE COULD BE DONE? NN AND pre-trained Glove. Ask to professor Boldi if
# you can use LSA with embedding at the same time. Also ask him how we cancan use
# the lables
# Did not normalize which may be needed
# Check slide 3 and the idea-for-explorations-ilia for possible modelling
# We can also scale (already done)
# LDA would also be useful
# May need more cleaning for good great and so on


y <- docvars(reviews_tokenized, "employer_rating")

reviews_tfidf <- dfm_tfidf(dfm(reviews_tokenized))
reviews_lsa <- textmodel_lsa(reviews_tfidf, nd=50)
set.seed(1235)
index.tr <- sample(size=round(0.8*length(y)), x=1:length(y), replace=FALSE)
df <- data.frame(Score=y, X=reviews_lsa$docs)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]

reviews_rf <- ranger(Score ~ ., 
                     data = df.tr)
pred.te <- predict(reviews_rf, df.te)$predictions
rmse <- sqrt(mean((pred.te - df.te$Score)^2))
# 0.998
plot(pred.te ~ df.te$Score, ylab="Predictions", xlab="Observed", pch=20)

# ---
# with more topics
reviews_tfidf <- dfm_tfidf(dfm(reviews_tokenized))
reviews_lsa <- textmodel_lsa(reviews_tfidf, nd=100)
set.seed(1235)
index.tr <- sample(size=round(0.8*length(y)), x=1:length(y), replace=FALSE)
df <- data.frame(Score=y, X=reviews_lsa$docs)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]

reviews_rf <- ranger(Score ~ ., 
                     data = df.tr)
pred.te <- predict(reviews_rf, df.te)$predictions
rmse <- sqrt(mean((pred.te - df.te$Score)^2))
# 1.02
plot(pred.te ~ df.te$Score, ylab="Predictions", xlab="Observed", pch=20)


# ---
# with less topics
reviews_tfidf <- dfm_tfidf(dfm(reviews_tokenized))
reviews_lsa <- textmodel_lsa(reviews_tfidf, nd=10)
set.seed(1235)
index.tr <- sample(size=round(0.8*length(y)), x=1:length(y), replace=FALSE)
df <- data.frame(Score=y, X=reviews_lsa$docs)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]

reviews_rf <- ranger(Score ~ ., 
                     data = df.tr)
pred.te <- predict(reviews_rf, df.te)$predictions
rmse <- sqrt(mean((pred.te - df.te$Score)^2))
# 1.04
plot(pred.te ~ df.te$Score, ylab="Predictions", xlab="Observed", pch=20)

# ---
# with supplementary features
df <- data.frame(Score=y, X=reviews_lsa$docs)
df_supp <- cbind(df, 
            val_shift_score = docvars(reviews_tokenized, c("total_score")),
            log_total_counts=docvars(reviews_tokenized, c("log_total_counts")))
df.tr_sup <- df_supp[index.tr,]
df.te_sup <- df_supp[-index.tr,]

reviews_rf <- ranger(Score ~ ., 
                     data = df.tr_sup)
pred.te <- predict(reviews_rf, df.te_sup)$predictions
rmse <- sqrt(mean((pred.te - df.te_sup$Score)^2))
# 1
plot(pred.te ~ df.te_sup$Score, ylab="Predictions", xlab="Observed", pch=20)

# ---
# linear regression
# ---
# For without, we should change the df to what it was again
df <- data.frame(Score=y, X=reviews_lsa$docs)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]

reviews_lm <- lm(Score ~ ., 
                 data = df.tr)
pred.te <- predict(reviews_lm, df.te)
rmse <- sqrt(mean((pred.te - df.te$Score)^2))
# 1.02
plot(pred.te ~ df.te$Score, ylab="Predictions", xlab="Observed", pch=20)

# with supplementary features
reviews_lm <- lm(Score ~ ., 
                 data = df.tr_sup)
pred.te <- predict(reviews_lm, df.te_sup)
rmse <- sqrt(mean((pred.te - df.te_sup$Score)^2))
# 0.994
plot(pred.te ~ df.te_sup$Score, ylab="Predictions", xlab="Observed", pch=20)

# with supplementary features including the centers (embedding)
df <- data.frame(Score=y, X=centers)
df_supp <- cbind(df, 
                 val_shift_score = docvars(reviews_tokenized, c("total_score")),
                 log_total_counts=docvars(reviews_tokenized, c("log_total_counts")))
df.tr_sup <- df_supp[index.tr,]
df.te_sup <- df_supp[-index.tr,]

reviews_lm <- lm(Score ~ ., 
                 data = df.tr_sup)
pred.te <- predict(reviews_lm, df.te_sup)
rmse <- sqrt(mean((pred.te - df.te_sup$Score)^2))
# 0.991 however it does not look as good as the LSA one
plot(pred.te ~ df.te_sup$Score, ylab="Predictions", xlab="Observed", pch=20)


# We can also scale
# reviews_lsa <- textmodel_lsa(reviews_tfidf, nd=100)
# df <- data.frame(Score=y, X=reviews_lsa$docs)
# df <- data.frame(Score=y, X=centers)
df_supp <- cbind(df, 
                 val_shift_score = docvars(reviews_tokenized, c("total_score")),
                 log_total_counts=docvars(reviews_tokenized, c("log_total_counts")))
df.tr_sup <- df_supp[index.tr,]
df.te_sup <- df_supp[-index.tr,]

reviews_lm <- train(Score ~ ., data = df.tr_sup, method = "lm",
                preProcess = c("center","scale"))

pred.te <- predict(reviews_lm, newdata = df.te_sup)

rmse <- sqrt(mean((pred.te - df.te_sup$Score)^2))
# 0.991 however it does not look as good as the LSA one
# 0.987 with lsa instead of embedding, howwever here we make negative predictions
# which should not be possible
# 0.975 with lsa of 100 topics, maybe also try LDA or do a diagram of their ideal
# number

plot(pred.te ~ df.te_sup$Score, ylab="Predictions", xlab="Observed", pch=20)

