library(caret)

#setwd("Set your working directory/set your path here") 

rm(list = ls())
#loading the file
administrator.df <- read.csv("SystemAdministrators.csv")

#Creating dummy variable for the categorical variable Completed
administrator.df$isCompleted <- ifelse(administrator.df$Completed=='Yes', 1, 0)

#deselecting the completed column 
train.df <- subset (administrator.df, select = -Completed)

#Creating the logistic model
logit.reg <- glm(isCompleted ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

#making the prediction
logit.reg.pred <- predict(logit.reg, administrator.df, type = "response")

table(ifelse(administrator.df$isCompleted==1,"Completed", "Not Completed"))
table(ifelse(logit.reg.pred > 0.5, "Completed", "Not Completed"))


#creating the confusion matrix
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, "Completed", "not Completed")), 
               as.factor(ifelse(administrator.df$isCompleted==1,"Completed", "not Completed")))


#making a prediction with a cutoff value of 0.7
logit.reg.pred2 <- predict(logit.reg, administrator.df, type = "response")
table(ifelse(administrator.df$isCompleted==1,"Completed", "Not Completed"))
table(ifelse(logit.reg.pred2 > 0.7, "Completed", "Not Completed"))

#creating the confusion matrix for the cutoff value of 0.7
confusionMatrix(as.factor(ifelse(logit.reg.pred2 > 0.7, "Completed", "not Completed")), 
                as.factor(ifelse(administrator.df$isCompleted==1,"Completed", "not Completed")))


#Making a prediction with a cutoff value of 0.3
logit.reg.pred3 <- predict(logit.reg, administrator.df, type = "response")
table(ifelse(administrator.df$isCompleted==1,"Completed", "Not Completed"))
table(ifelse(logit.reg.pred3 > 0.3, "Completed", "Not Completed"))

#creating the confusion matrix for the cutoff value of 0.3
confusionMatrix(as.factor(ifelse(logit.reg.pred3 > 0.3, "Completed", "not Completed")), 
                as.factor(ifelse(administrator.df$isCompleted==1,"Completed", "not Completed")))
