rm(list = ls())

library(caret)
library(rpart)
library(rpart.plot)
library(gmodels)

setwd("~/R/Data Programming/assignment 5")

Auctions.df <- read.csv("eBayAuctions.csv")

Auctions.df <- subset(Auctions.df, select=-c(Category, Currency, EndDay))


#Making the 60/40 split for the training and validation sets
set.seed(1)  
train.index <- sample(c(1:dim(Auctions.df)[1]), dim(Auctions.df)[1]*0.6)  
train.df <- Auctions.df[train.index, ]
valid.df <- Auctions.df[-train.index, ]


#Creating the Decision Tree
Auctions.ct <- rpart(Competitive ~ ., data = train.df, 
                           control = rpart.control(maxdepth = 6), 
                           method = "class")

summary(Auctions.ct)

#drawing the Decision Tree
prp(Auctions.ct, type = 4, extra = 101, box.palette = "GnYlRd", 
    fallen.leaves = TRUE, branch = .3, split.font = 1, varlen = -10, 
    under=TRUE)

?rpart.rules

#Looking up the rules of the Decision Tree
rpart.rules(Auctions.ct, extra = 5, cover = TRUE)


#Making the prediction for the training data set
Auctions.pred.train <- predict(Auctions.ct,train.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(as.factor(Auctions.pred.train), as.factor(train.df$Competitive))

#repeat the code for the validation data
Auctions.pred.valid <- predict(Auctions.ct,valid.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(as.factor(Auctions.pred.valid), as.factor(valid.df$Competitive))



#Removing the ClosePrice predictor
Auctions.NoClosingPrice.df <- subset(Auctions.df, select=-c(ClosePrice))

#Creating the 60/40 split with the data that does not have the ClosePrice predictor
set.seed(1)  
NoClosingPrice.train.df <- Auctions.NoClosingPrice.df[train.index, ]
NoClosingPrice.valid.df <- Auctions.NoClosingPrice.df[-train.index, ]

#creating the decision tree 
Auctions.NoClosingPrice.ct <- rpart(Competitive ~ ., data = NoClosingPrice.train.df, 
                     control = rpart.control(maxdepth = 6), 
                     method = "class")

summary(Auctions.NoClosingPrice.ct)

#drawing the decision tree with no ClosePrice Predictor
prp(Auctions.NoClosingPrice.ct, type = 4, extra = 101, box.palette = "GnYlRd", 
    fallen.leaves = TRUE, branch = .3, split.font = 1, varlen = -10, 
    under=TRUE)


#Retrieving the decision tree rules
rpart.rules(Auctions.NoClosingPrice.ct, extra = 5, cover = TRUE)

# preciting the values on the training data set
NoClosingPrice.pred.train <- predict(Auctions.NoClosingPrice.ct,train.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(as.factor(NoClosingPrice.pred.train), as.factor(train.df$Competitive))
#repeat the code for the validation data
NoClosingPrice.pred.valid <- predict(Auctions.NoClosingPrice.ct,valid.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(as.factor(NoClosingPrice.pred.valid), as.factor(valid.df$Competitive))
