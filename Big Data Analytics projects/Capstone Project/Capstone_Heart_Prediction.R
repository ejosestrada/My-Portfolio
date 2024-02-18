rm(list = ls())

#setwd('Set working diretory')

install.packages('fastDummies')
install.packages('dplyr')
install.packages('corrplot')
install.packages("car")

library('fastDummies')
library(corrplot) #for visualization of correlation
library(lattice) #for visualization
library(ggplot2) #for visualization
library(caTools) #for splitting data into testing and training data
library(dplyr) #manipulating data frame
library(plotly)
library(rpart)
library(rpart.plot)
library(rattle)
library(forecast)
library(caret)
library(rpart.plot)
library(rpart)
library(gmodels)
library(car)

#loading the data set
heart.df <- read.csv('heart.csv')

#initial survey of the dataset
str(heart.df)
summary(heart.df)
head(heart.df)

#checking out for any missing values
numberOfNA <- length(which(is.na(heart.df)==T))
numberOfNA

#creating dummy variables
heartdummies.df <- dummy_cols(heart.df, select_columns = c('Sex', 'ChestPainType', 'RestingECG','ExerciseAngina', 'ST_Slope'),
                              remove_selected_columns = TRUE)
summary(heartdummies.df)

#creating a correlation plot
corrplot(cor(heartdummies.df))


#droping values with high correlation to avoid duplicate data 
drop.vars <- c('Sex_M', 'ExerciseAngina_N', 'ST_Slope_Flat', 'RestingECG_ST', 'ChestPainType_TA')

#selecting the variables that will be used
heartData <- heartdummies.df[,!colnames(heartdummies.df) %in% drop.vars]
corrplot(cor(heartData))


# creating seed and creating a 70/30 split for training and validation sets
set.seed(1)
train.index <- sample(c(1:dim(heartData)[1]), dim(heartData)[1]*0.7)  
train.df <- heartData[train.index, ]
valid.df <- heartData[-train.index, ]

#Logistic Regression 

logit.reg <- glm(HeartDisease ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

#logistic prediction
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")


#generating the confusion matrix for 0.5, 0.4, 0.6 cuttoff values
confusionMatrix(as.factor(valid.df$HeartDisease), as.factor(ifelse(logit.reg.pred> 0.5,1,0)))

confusionMatrix(as.factor(valid.df$HeartDisease), as.factor(ifelse(logit.reg.pred> 0.4,1,0)))

confusionMatrix(as.factor(valid.df$HeartDisease), as.factor(ifelse(logit.reg.pred> 0.6,1,0)))


#Decision Tree

#creating a full decision tree
deeper.ct <- rpart(HeartDisease ~ ., data = train.df, method = "class", 
                   cp = 0, minsplit = 1)
printcp(deeper.ct)
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

cv.ct <- rpart(HeartDisease ~ ., data = train.df, 
               method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)

#creating a pruned decision tree
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

#drawing a pruned decision tree
prp(pruned.ct, type = 4, extra = 101, box.palette = "GnYlRd", 
    fallen.leaves = TRUE, branch = .3, split.font = 1, varlen = -10, 
    under=TRUE)  

#making prediction of the full tree

pred.full.ct <- predict(cv.ct, valid.df, type ="class")

#making prediction of pruned tree
pred.ct <-predict(pruned.ct, valid.df, type ="class")

#generating confusion matrix for the pruned tree
confusionMatrix(as.factor(pred.ct), 
                as.factor(valid.df$HeartDisease))

#generating confusion matrix for the full tree
confusionMatrix(as.factor(pred.full.ct), 
                as.factor(valid.df$HeartDisease))

#Neural Network
library(neuralnet)
library(NeuralNetTools)
library(nnet)

#creating the neural net wtih 2 layers with 4 nodes each
nn <- neuralnet(HeartDisease ~ .,data = train.df, hidden=c(4,4),learningrate=0.1,stepmax=1e6)

#creating a neural net with 1 hidden layer
nn2 <- neuralnet(HeartDisease ~ .,data = train.df, hidden=5,learningrate=0.1,stepmax=1e6)

# neural network results
plotnet(nn)
neuralweights(nn)

plotnet(nn2)

# neural network performance
nn.train.pred <- compute(nn, train.df)
nn.train.class <- ifelse(nn.train.pred$net.result > 0.5, 1, 0)

#computing predictions
nn2.train.pred <- compute(nn2, train.df)
nn2.train.class <- ifelse(nn2.train.pred$net.result > 0.5, 1, 0)

nn.pred <- compute(nn, valid.df)
nn.class <- ifelse(nn.pred$net.result > 0.5, 1, 0)
nn.class

nn2.pred <- compute(nn2, valid.df)
nn2.class <- ifelse(nn2.pred$net.result > 0.5, 1, 0)

#results for neural network with 2 hidden layers
confusionMatrix(as.factor(nn.class), as.factor(valid.df$HeartDisease))

#results for neural network with 1 hidden layer
confusionMatrix(as.factor(nn2.class), as.factor(valid.df$HeartDisease))
