install.packages("ggrepel")
install.packages("gridExtra")
install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.textplots")
install.packages("irlba")
install.packages("caret")
install.packages("randomForest")
install.packages("gbm")
install.packages("xgboost")
install.packages("e1071")


library(ggplot2)
library(ggrepel)
library(gridExtra)
library(stringi)
library(stringr)
library(dplyr)
library(data.table)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(RColorBrewer)
library(irlba)
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(e1071)

#loading testing and training data

training <- read.csv("train.csv")
test <- read.csv("test.csv")

#creating a data table for easier processing of text

training <- data.table(training)
test <- data.table(test)

# Combining the traning and testing data into one single table to process the
# text faster

comb <- data.table(rbind(training, mutate(test, target = NA)))
testing_ID <- test$id

dim(training)

#gathering number of disaster and non-disaster tweets to visualize the training set

target <- c(nrow(filter(comb[!is.na(target), ], target == 0)), 
        nrow(filter(comb[!is.na(target), ], target == 1)))

print(target)

#ploting the number of disaster and non-disaster tweets

ggplot(data = comb[!is.na(target), ], aes(x = factor(target), fill = target)) + 
  geom_bar(stat = "Count") + theme(legend.position = "none") +
  labs(x = "Target, 1 is true and 0 is false", y = "Count")+
  ggtitle("Distribution of the Training Set") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  coord_cartesian(ylim = c(0, 5000))

# adding a column to count the number of character of tweets

comb[, Text_Length := nchar(text), by = seq_len(nrow(comb))]

# plotting the average of characters for disaster and non-disaster tweets

ggplot(comb[!is.na(target), ], aes(x = factor(target), y = Text_Length, fill = target)) + geom_boxplot()


# Finding the features in a tweet.

# The following features were found for the purpose of our model:
# number of capital letter, number of links, number of punctuations like "!" and "?"
# number of hashtags, number of mentions "@"

comb[, ncaps := str_count(text, "[A-Z]"), by = seq_len(nrow(comb))]
comb[, linkcount := str_count(text, "http"), by = seq_len(nrow(comb))]
comb[, punc_count := str_count(text, "[\\!\\?\\:\\.]"), by = seq_len(nrow(comb))]
comb[, hashtag_count := str_count(text, "#"), by = seq_len(nrow(comb))]
comb[, mention_count := str_count(text, "@"), by = seq_len(nrow(comb))]

#created a corpus from the text column to further process the tweets

corpus_comb <- corpus(comb$text)

# renoving all punctuation

corpus_comb <- stri_replace_all_regex(corpus_comb, "[\\p{p}\\p{S}]", "")

# removing all hyperlinks

corpus_comb <- stri_replace_all_regex(corpus_comb, "http.*", "")

# Tokenizing the text and removing numbers, punctuations, symbols, hyphens, URLs

token_comb <- tokens(corpus_comb, what = "word", remove_numbers = T,
                     remove_punct = T, remove_symbols = T, split_hyphens = T,
                     remove_url = T, remove_twitter = T)

# tokens are set to lower case, removing stop words, making tokens root words

token_comb <- tokens_tolower(token_comb)
token_comb <- tokens_select(token_comb, stopwords(), selection = "remove")
token_comb <- tokens_wordstem(token_comb)

#creating a data frame matrix and selecting the tokens that appear 5 times or more

dfm_comb <- dfm(token_comb)
dfm_comb <- dfm_trim(dfm_comb, min_termfreq = 5)

dim(dfm_comb)

#creating a disaster and non disaster variable to be later added to the data frame matrix

disaster <- comb$target == 1 ; disaster[is.na(disaster)] <- F
nodisaster <- comb$target == 0 ; nodisaster[is.na(nodisaster)] <- F

# Finding the most common singular or unigram term frequency 

disaster_unidfm <- dfm_comb[disaster, ]
disaster_unicount <- colSums(disaster_unidfm)
disaster_unicount <- disaster_unicount[disaster_unicount > 80]
disaster_freq1gs <- names(disaster_unicount)

# Plotting the most frequently used words in a wordcloud

textplot_wordcloud(disaster_unidfm, min_freq = 80, color = brewer.pal(10, "BrBG"))  

# Finding the most commong 2 word terms 

nodisaster_unidfm <- dfm_comb[nodisaster, ]
nodisaster_unicount <- colSums(nodisaster_unidfm)
nodisaster_unicount <- nodisaster_unicount[nodisaster_unicount > 80]
nodisaster_freq1gs <- names(nodisaster_unicount)

# Plotiing the most frequently used 2-term words in a wordcloud

textplot_wordcloud(nodisaster_unidfm, min_freq = 80, color = brewer.pal(10, "BrBG"))  

# Creating a dfm for bigrams

comb_bigrams <- tokens_ngrams(token_comb, n = 2)
comb_dfm2g <- dfm(comb_bigrams)

# Organizing disaster bigrams to be able to create a wordcloud 

dis_dfm2g <- comb_dfm2g[disaster, ]
dis_count2g <- colSums(dis_dfm2g)
dis_count2g <- dis_count2g[dis_count2g > 25]

#plotting the bigram wordcloud

textplot_wordcloud(dis_dfm2g, min.freq = 25, color = brewer.pal(10, "PuBuGn")) 

# Catergorizing bigrams from non-disaster tweets

nodis_dfm2g <- comb_dfm2g[nodisaster, ]
nodis_count2g <- colSums(nodis_dfm2g)
nodis_count2g <- nodis_count2g[nodis_count2g > 15]

#Plotting non-disaster bigrams in a wordcloud

textplot_wordcloud(nodis_dfm2g, min.freq = 15, color = brewer.pal(10, "Set1")) 

# creating a list of the bigrams

dis_freq2gs <- names(dis_count2g)
nodis_freq2gs <- names(nodis_count2g)

red_dis2g <- which(dis_freq2gs %in% nodis_freq2gs)
red_nodis2g <- which(nodis_freq2gs %in% dis_freq2gs)

dis_freq2gs <- dis_freq2gs[-red_dis2g]
nodis_freq2gs <- nodis_freq2gs[-red_nodis2g]

# This section is to pre-process to make bigram words joined by underscore to make them all terms match

preprocessed_tokens <- list()

for (i in 1:length(token_comb)){
  rslt <- paste(token_comb[i], collapse = "_")
  preprocessed_tokens <- append(preprocessed_tokens, rslt)
}

# creating function to match bigrams

bigram_match <- function(textinput, bigram_list){
  num_output <- sum(str_count(textinput, bigram_list))
  num_output
}

nr_disaster2gs <- sapply(preprocessed_tokens, bigram_match, dis_freq2gs)
nr_noDisaster2gs <- sapply(preprocessed_tokens, bigram_match, nodis_freq2gs)

# adding disaster and non-disaster bigrams into the comb table

comb$nr_disaster2gs <- nr_disaster2gs
comb$nr_noDisaster2gs <- nr_noDisaster2gs

# Due to the huge size of the dfm, we want to get the features that have the most variability 
# We are going to analyze using singular value decomposition using the irlba function

comb_svd1g <- irlba(t(dfm_comb), nv = 100, maxit = 1000)
comb_topfeatures1g <- comb_svd1g$v

comb_svd2g <- irlba(t(comb_dfm2g), nv = 100, maxit = 1000)
comb_topfeatures2g <- comb_svd2g$v

comb_topfeatures1g <- data.table(comb_topfeatures1g)
names(comb_topfeatures1g) <- paste("U", as.character(1:100), sep = "")

comb_topfeatures2g <- data.table(comb_topfeatures2g)
names(comb_topfeatures2g) <- paste("B", as.character(1:100), sep = "")

# Encoding keywords
# any keyword that has a blank cell we want to add a value or string to make the process more efficient

comb[keyword == ""]$keyword <- "None"

# Making the encoding of keywords and encoding categorical variables into dummy variables

kw <- data.table(comb$keyword) ; names(kw) <- "keyword_"
ohe <- dummyVars(~ ., data = kw)
ohecat_kw <- predict(ohe, kw)
ohecat_kw <- data.table(ohecat_kw)

# Encoding creates lots of variables due to dummy variables, We want to make the data not overfit our model
# We will be removing dummy variables that appear less than 15

low_occurence <- names(which(ohecat_kw[, lapply(.SD, sum) < 15]))
ohecat_kw <- select(ohecat_kw, -low_occurence)

# Creating the complete final table with all the features that we will use for our model

comb_complete <- data.table(target = comb$target, 
                            ncaps = comb$ncaps,
                            text_length = comb$text_length,
                            linkcount = comb$linkcount,
                            hashtag_count = comb$hashtag_count,
                            mention_count = comb$mention_count,
                            punc_count = comb$mention_count,
                            nr_disasterWords = comb$nr_disasterWords,
                            nr_noDisasterWords = comb$nr_noDisasterWords,
                            nr_disaster2gs = comb$nr_disaster2gs,
                            nr_noDisaster2gs = comb$nr_noDisaster2gs,
                            ohecat_kw,
                            comb_topfeatures1g,
                            comb_topfeatures2g)


# Separating the training data set from the testing data set.
#  (Note: the original testing and training data was combined previously at the begining to reduce steps)

training_range <- 1:nrow(training)
testing_range <- (nrow(training)+1):nrow(comb)
model_train <- comb_complete[training_range]
preproc_test <- comb_complete[testing_range]
preproc_test <- select(preproc_test, -target)

#Modeling using Support Vector Machines (SVM)

modfit_svm <- svm(target ~ ., data = model_train, 
                  type = "C-classification",
                  kernel = "radial",
                  cost = 2,
                  coef0 = 0.8)

# making the final prediction

answer_svm <- predict(modfit_svm, preproc_test)

# making a data frame for our solution

solution_svm <- data.frame(Id = testing_ID, target = answer_svm)

#Writing a csv file with the predictions

write.csv(solution_svm, "disaster_tweet_solution.csv", row.names = F)

#Plotting our solution

solution <- read.csv("disaster_tweet_solution.csv")

ggplot(data = solution[!is.na(target), ], aes(x = factor(target), fill = target)) + 
  geom_bar(stat = "Count") + theme(legend.position = "none") +
  labs(x = "Target, 1 is true and 0 is false", y = "Count")+
  ggtitle("Distribution of the Solution") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
  coord_cartesian(ylim = c(0, 5000))
