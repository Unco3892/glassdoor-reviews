# What could be used in the project work
textstat_frequency(speech.dfm, groups = "Party", n=5)  %>%
  ggplot(aes(x = reorder(feature,frequency), y = frequency, fill=group)) +
  geom_bar(stat="identity") + 
  xlab("Feature") +
  coord_flip() 


# To know the position of a specific word (maybe less relevant given the short
textplot_xray(kwic(crude.cp, pattern = "oil"),
              kwic(crude.cp, pattern = "price"), scale="absolute")


sum(dfm_select(crude.dfm, pattern="opec")[-2,])

crude.dfm %>% dfm_select("opec") %>% sum()


opec.tab <- as.table(matrix(c(13,248,18,1189), nc=2, byrow=TRUE))
rownames(opec.tab) <- c("target", "reference")
colnames(opec.tab) <- c("opec","other")
opec.tab


# Look what the word "share" means in the context of this analysis, maybe
# consider adding "share prices" to your analysis, minute 1:27:30 of the video
# lecture of Sentiment Analysis course.


# Build your own dictionary using the words that have been used to determine the
# score, see slide 2 of the sentiment analysis lecture.

# dictionary on slide 3 of the same chapter (sentiment) may be relevant for us as
# it is about review 
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon

# filter only for scores that are 5 and see what words come up and use them to 
# build a dictionary for the pros and then do the same for cons