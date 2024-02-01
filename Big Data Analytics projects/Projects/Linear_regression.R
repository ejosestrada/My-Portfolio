install.packages("forecast")
library(forecast)
library(stringr)



rm(list = ls())
airfare.df <- read.csv("Airfares.csv")

selected.var <- c("COUPON","NEW","VACATION_YES","SW_YES","HI","S_INCOME","E_INCOME","S_POP","E_POP","SLOT_CTRL","GATE_CONS","DISTANCE","PAX","FARE")
airfareSelected.df <- airfare.df[selected.var]

#The FARE data was not a numeric value, this line transforms the character type to numeric and also deletes the "$" that was in all of the data for FARE 
airfareSelected.df$FARE <- as.numeric(gsub("[$,]|[[:alpha:]]+.*", "",airfareSelected.df$FARE))

# Select rows randomly
row.number    <- sample(1:nrow(airfareSelected.df), size=0.4*nrow(airfareSelected.df))

# testing Data
airfare_test  <-  airfareSelected.df[row.number,]

# training Data
airfare_train <- airfareSelected.df[-row.number,]

#checking for dimmensions 
dim(airfare_test)
dim(airfare_train)


#Modeling the linear regression
airfareModel.lm <- lm(FARE~., data = airfare_train)
options(scipen=999) # remove scientific notation
sum_linear <- summary(airfareModel.lm)

#calling sum_linear to check the results of the linear regression 
sum_linear

#modeling the backwards steps
airfareModel.step <- step(airfareModel.lm, direction = "backward")
sum_back <- summary(airfareModel.step)

#calling sum_back to check the results of the backwards step
sum_back

#Calculating the accuracy of the linear model
airfareModel.lm.pred <- predict(airfareModel.lm, airfare_test)
options(scipen=999, digits = 3)
some.residuals <- airfare_test$FARE[1:20] - airfareModel.lm.pred[1:20]
data.frame("Predicted" = airfareModel.lm.pred[1:20], "Actual" = airfare_test$FARE[1:20],
           "Residual" = some.residuals)
options(scipen=999, digits = 3)
accuracy(airfareModel.lm.pred, airfare_test$FARE)

#calculating the accuracy of the backwards step model
airfareModel.step.pred <- predict(airfareModel.step, airfare_test)
options(scipen=999, digits = 3)
some.residuals <- airfare_test$FARE[1:20] - airfareModel.step.pred[1:20]
data.frame("Predicted" = airfareModel.step.pred[1:20], "Actual" = airfare_test$FARE[1:20],
           "Residual" = some.residuals)
options(scipen=999, digits = 3)
accuracy(airfareModel.step.pred, airfare_test$FARE)