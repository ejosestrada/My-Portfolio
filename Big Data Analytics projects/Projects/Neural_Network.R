rm(list = ls())

install.packages("neuralnet")
library(neuralnet)
library(NeuralNetTools)
library(nnet)
library(class)
library(caret)



Tayko.df <- read.csv('CIS4930_CSV_Tayko.csv')

#removing the id column
Tayko.df <- subset(Tayko.df, select=-c(id))

#splitting the data set into a 60:40 split
set.sead(1)
train.index <- sample(c(1:dim(Tayko.df)[1]), dim(Tayko.df)[1]*0.6)
train.df <- Tayko.df[train.index, ]
valid.df <- Tayko.df[-train.index, ]

#making the logistic regression model
logit.reg <- glm(Purchase ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

#predicting the values on the training set
logit.reg.pred <- predict(logit.reg, train.df, type = "response")
logit.reg.pred.class<-ifelse(logit.reg.pred > 0.5, 1, 0)

#predicting the values on the testing set
logit.reg.pred2 <- predict(logit.reg, valid.df, type ="response")
logit.reg.pred.class2 <- ifelse(logit.reg.pred2 >0.5, 1, 0)

#generating the confusion matrix on the training set
confusionMatrix(as.factor(logit.reg.pred.class), as.factor(train.df$Purchase))

#generating the confusion matrix on the testing set
confusionMatrix(as.factor(logit.reg.pred.class2), as.factor(valid.df$Purchase))

#creating the neural network with 1 hidden layer and 5 nodes within the hidden layer
nn <- neuralnet(Purchase ~., data=train.df, hidden=5, learningrate=0.1,stepmax=1e6)
plotnet(nn)

#generaing the prediction on the validation set
nn.pred <- compute(nn, valid.df)
nn.class <- ifelse(nn.pred$net.result > 0.5, 1, 0)
nn.class

#generating the confusion matrix on the validation set
confusionMatrix(as.factor(nn.class), as.factor(valid.df$Purchase))
