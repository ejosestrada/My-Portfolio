rm(list = ls())
setwd("~/Big Data/R Files/assingment 3")

library(forecast)

SouvenirSales.data <- read.csv("SouvenirSales.csv")

#Creating a time series

sales.ts <- ts(SouvenirSales.data$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12)

sales.ts

plot(sales.ts, xlab = "Time", ylab = "Sales in Australian Dollars")

length(sales.ts)

nValid <- 72
nTrain <- length(sales.ts) - nValid

#Making a training and testing split

train.ts <- window(sales.ts, start = c(1995, 1), 
                   end = c(2000, nTrain))

valid.ts <- window(sales.ts, start = c(2000, nTrain + 1), end = c(2001, nTrain + nValid))

train.ts
valid.ts

#Naive forcast

snaive.pred <- snaive(train.ts, h = nValid)

summary(snaive.pred)

par(mfrow = c(1, 1))

#Plotting the forecast and overlaying with the actual data

plot(train.ts,  ylab = "Sales", 
     xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1995,2001.25), main = "")
axis(1, at = seq(1995, 2002, 1), labels = format(seq(1995, 2002, 1)))
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(2001, 2001), c(0, 85000)) 

#checking for accuracy
accuracy(snaive.pred, valid.ts)

#adding seasonality into the model

train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season <- tslm(train.ts ~ season,lambda = 0)
summary(train.lm.season)

#adding trend and seasonality into the model

train.level_and_season <- tslm(train.ts ~ trend + season)
summary(train.level_and_season)


lm.pred <- forecast(train.level_and_season, h=nValid, level=0)
accuracy(lm.pred, valid.ts)

# Setting a training time series that uses the first 6 years
sales.72.ts <- window(train.ts, start = c(1995, 1), 
                          end = c(1995, 72))
Acf(sales.72.ts, lag.max = 12, plot= TRUE, main = "")

#Seting Holt-Winter's exponential smoothing
hwin <- ets(train.ts, model = "MAA")

hwin.pred <- forecast(hwin, h = nValid, level = 0)

#Plotting the forcast using exponential smoothing and overlaying on top of the actual values

plot(hwin.pred,  ylab = "Sales", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1995,2002.25), main = "", flty = 2)
axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(hwin.pred, valid.ts)
accuracy(lm.pred, valid.ts)

hwin
summary(hwin.pred)
