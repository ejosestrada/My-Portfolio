rm(list = ls())
options(repos = c(CRAN = "http://cran.rstudio.com"))

# install.packages("rpart") # Installation is required for the first of use
library(rpart)
# install.packages("rpart.plot")    # Installation is required for the first of use
library(rpart.plot)


#===================================== classification tree =========================== 



#setwd("Set your working directory/path in here")

mower.df <- read.csv("CIS4930_CSV_RidingMowers.csv")


### 1. use rpart() to run a classification tree 

# define rpart.control() in rpart() to determine the depth of the tree.
# method = "class" indicates that the target variable is category

class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(maxdepth = 5), 
                    method = "class")
#Expected loss from loss function, the lower the better
summary(class.tree)


### 2. plot classification tree 

# 2-1. use prp() to plot the tree. 
# You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(class.tree, type = 4, extra = 101, box.palette = "GnYlRd", 
    fallen.leaves = TRUE, branch = .3, split.font = 1, varlen = -10, 
    under=TRUE)  

# 2-2. plot a fancy tree
# install.packages("rattle")  # Installation is required for the first of use
library(rattle)
fancyRpartPlot(class.tree)


### 3. look at decision rules 

rpart.rules(class.tree, extra = 4, cover = TRUE)


### 4. evaluate the model 

# install.packages("gmodels")  # Installation is required for the first of use
library(gmodels)
pred.mower <-predict(class.tree, mower.df,type ="class")
library(caret)
confusionMatrix(as.factor(pred.mower), 
                as.factor(mower.df$Ownership))


###   Bank Loan Example ############

bank.df <- read.csv("CIS4930_CSV_Loan.csv")
bank.df <- subset(bank.df, select=-c(ID, ZIP.Code)) # Drop ID and zip code columns.


### 0. partition the dataset 

set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]


### 1. use rpart() to run a classification tree 

default.ct <- rpart(Personal.Loan ~ ., data = train.df, 
                    method = "class")

#CP: Complexity Parameter 
#xerror: cross validation error
#We select CP with xerror
summary(default.ct)



### 2. plot classification tree 

prp(default.ct, type = 3, extra = 101,  clip.right.lab = FALSE, box.palette = "GnYlRd", 
    branch = .3, varlen = -10, cex.main=3, space=0)  
# Or, 
fancyRpartPlot(default.ct)


### 3. look at decision rules 

rpart.rules(default.ct, extra = 4, cover = TRUE)


### 4. evaluate the model 

# 4-1. prediction for training data
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(as.factor(default.ct.point.pred.train), as.factor(train.df$Personal.Loan))

# 4-2. repeat the code for the validation data
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(as.factor(default.ct.point.pred.valid), as.factor(valid.df$Personal.Loan))


### 5. let's compare a full tree vs. pruned tree 

# 1. Grow a full tree
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
                   cp = 0, minsplit = 1)
printcp(deeper.ct)
# count number of leaves

length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  
fancyRpartPlot(deeper.ct)

# 2. Grow a pruned tree
# Use Complexity Parameter (CP) to determine optimal stopping of growth, Table 9.4
# argument xval is number K of folds in a K-fold cross-validation.
# argument cp sets the smallest value for the complexity parameter.
# minsplit is the minimum number of observations in a node for a split to be attempted. 
# xval is number of cross-validations.
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, 
               method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)

# use printcp() to print the table. 
printcp(cv.ct)

# prune tree by lower cp, 
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
#,or
pruned.ct <- prune(cv.ct, cp = 0.0048485)
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  
prp(pruned.ct, type = 3, extra = 101,  clip.right.lab = FALSE, box.palette = "GnYlRd", 
    branch = .3, varlen = -10, cex.main=3, space=0)  
fancyRpartPlot(pruned.ct)


#===================================== random forest ===========================
# install.packages("randomForest")
library(randomForest)
## random forest
rf <- randomForest(as.factor(Personal.Loan) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)
?randomForest
summary(rf)
rf$votes

## Prediction & Evaluation
rf.pred <- predict(rf, valid.df)
library(caret)
confusionMatrix(as.factor(rf.pred), as.factor(valid.df$Personal.Loan))


#===================================== k-Nearest Neighbors ===========================
#install.packages('class')
library(class)
train.loan <- as.factor(train.df$Personal.Loan)
valid.loan <- as.factor(valid.df$Personal.Loan)

nn3 <- knn(train.df, valid.df, cl=train.loan, k=5)
confusionMatrix(as.factor(nn3), as.factor(valid.loan))

# Find optimal K (from 1 to 14)
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = 0)
View(accuracy.df)
for(i in 1:14) {
    knn.pred <- knn(train.df, valid.df, cl=train.loan, k=i)
    accuracy.df[i, 'accuracy'] <- confusionMatrix(knn.pred, valid.loan)$overall[1] 
}
accuracy.df
