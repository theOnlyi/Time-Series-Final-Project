#######################################
##################################
##	Final Project
## 	MATH2250 Time Series Analysis
## 	Li, Yihao
##################################

##################################
### Package we used
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
### ARIMA model
acf(a)
pacf(a)
monavg_price<-auto.arima(a)
plot(forecast(monavg_price,24))
summary(monavg_price)

holtavg_price<-holt(a,h=50)
plot(holtavg_price)
points(holtavg_price$fitted,col='pink',type = 'l')
holtavg_price2<-holt(a,damped=TRUE,h=50)
plot(holtavg_price2)
autoplot(a)+
  autolayer(holtavg_price,series = "Linear",PI=FALSE)+
  autolayer(holtavg_price2,series = "Damped",PI=FALSE)+
  ggtitle("Holt's Linear and Dampled")+
  guides(colour=guide_legend(title="Forecast"))

###################################
### Holt's Winter Method
holtWavg_price<-HoltWinters(a,seasonal = "multiplicative")
summary(holtWavg_price)
plot(forecast(holtWavg_price,12))
fore<-forecast(holtWavg_price,12)
accuracy(fore)
?accuracy
adf.test(monavg_price)
?fitted









