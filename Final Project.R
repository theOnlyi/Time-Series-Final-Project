#######################################
##################################
##	Final Project
## 	MATH2250 Time Series Analysis
## 	Li, Yihao
##################################

##################################
### Package
library(xts)
library(forecast)
library(fpp2)
library(TSA)

##################################
### Enviroment set up and data cleaning
setwd("/Users/yihao/Desktop/Yihao/Wentworth/Mathmatics /MATH2250 TIme Series/Project/Final Project")
sh_plate<-read.csv("shanghai_plate_price.csv")
sh_plate<-sh_plate[,c(-2,-3,-5)]
#View(sh_plate)
#plot(sh_plate$avg.price,type = "l")
avgprice<-sh_plate$avg.price
head(avg_price)
### Set the monthly date for my data and combine them into a datafarme 
d <- as.Date("2001-12-1") + seq(0,6100,30)
next.month <- function(d) as.Date(as.yearmon(d) + 1/12) + 
  as.numeric(d - as.Date(as.yearmon(d))) 
next.month(d)
avg_price<-data.frame(date = next.month(d)+0:203,avgprice) #match the lenth of data set
avg_price<-xts(avg_price$avgprice,as.Date(avg_price$date))
plot(avg_price,main="Average Price of Shanghai Car's Plate")
a<-plot(diff(log(avg_price)),main = "Shanghai Vehicals Plates Price") # transport the data to a stationary data
a
a<-ts(avg_price,start = c(2002,1),frequency = 12) # convert to time series
plot(decompose(a))
head(avg_price)
View(avg_price)
?as.Date.yearmon
?decompose

##################################
### linear regression
monthdata <- season(a)
modellinear <- lm(a~monthdata-1)
summary(modellinear)
plot(modellinear)
plot(modellinear$residuals)
hist(modellinear$residuals, prob=TRUE)
lines(density(modellinear$residuals))
sqrt(sum((modellinear$residuals)^2))
AIC(modellinear)
BIC(modellinear)

##################################
### ARIMA model
acf(a)
pacf(a)
monavg_price<-auto.arima(a)
plot(forecast(monavg_price,24))
summary(monavg_price)
plot(monavg_price$residuals)
hist(monavg_price$residuals)

##################################
### Holt's linear model
holtavg_price<-holt(a,h=50)
plot(holtavg_price)
points(holtavg_price$fitted,col='pink',type = 'l')
autoplot(a)+
  autolayer(holtavg_price,series = "Linear",PI=FALSE)+
  ggtitle("Holt's Linear")+
  guides(colour=guide_legend(title="Forecast"))
summary(holtavg_price)
plot(holtavg_price$residuals)
hist(holtavg_price$residuals)

#holtavg_price2<-holt(a,damped=TRUE,h=50)
#plot(holtavg_price2)
#accuracy(holtavg_price2)

###################################
### Holt's Winter Method
holtWavg_price<-HoltWinters(a,seasonal = "multiplicative")
summary(holtWavg_price)
plot(forecast(holtWavg_price,12))
fore<-forecast(holtWavg_price,12)
accuracy(fore)
plot(fore$residuals)
hist(fore$residuals)
#since the residuals of Holt's winter is much bigger than the previou two. So it would not be considered in this case.

###################################
### Best model of fit: ARIMA
traingroup <- head(a, 173)
testgroup <- tail(a, 31)
finalmodel <- auto.arima(traingroup)
summary(finalmodel)
plot(forecast(finalmodel, 31))
p15 <- forecast(finalmodel, 31)
err <- testgroup-p15$mean
MSE <- mean(err*err)
MSE
plot(a)
lines(p15$mean, col='red')

###################################
### Hypothesis & Testing
Box.test(finalmodel$residuals, lag = 12, type = "Ljung-Box")
#The p value is 0.9926, and so the model does not exhibit the lack of fit.
adf.test(a, alternative="stationary")
#The p value is 0.5492, and so there is no stationary
acf(finalmodel$residuals)







