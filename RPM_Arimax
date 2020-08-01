Arimax Code

#Installing library package
library(tseries)
library("TTR")
library("TSA")
library("forecast")
library("lubridate")
library(tsoutliers)
library(expsmooth)
library(fma)


#getting required data (base data of RPM for wednesday)
arima_data_Wednesday <- read.csv("D:/Projects/Arimax_All_Days_Model/RPM_all_days/Wednesday_RPM.csv", stringsAsFactors=FALSE)

#filtering for the required data based on timeperiod
arima_data_Wednesday <- arima_data_Wednesday[18:226,]

#Assigning column names 
names(arima_data_Wednesday) <- c("Date", "RPM")

#Obtaining RPM Column 
arima_RPM=arima_data_Wednesday[,2]

#converting into time series
arima_data_timeseries <- ts(arima_RPM)

names(arima_data_timeseries) <- c("RPM")

#Identifying Additive Outlier and Temporaray Change 
outlier.arima_data <- tsoutliers::tso(arima_data_timeseries,types = c("AO","TC"))
outlier.arima_data
plot(outlier.arima_data)

par(mfrow=c(2,1))
plot(outlier.arima_data)


#plotting time series, ACF and PACF Plots
par(mfrow=c(2,2))
plot.ts(arima_data_timeseries, main = "Time Series plot of Wednesday RPM")
#plot(arima_data_wednesday, main = "RPM of Wednesday trend", type = "l")
acf(arima_data_timeseries)
pacf(arima_data_timeseries)

#running Dickey-Fuller test of stationarity
adf.test(arima_data_timeseries, alternative="stationary", k=0)
pp.test(arima_data_timeseries)
kpss.test(arima_data_timeseries, "Trend")

#Regressor Dataset
seasonal_reg <- read.csv("D:/Projects/Arimax_All_Days_Model/Regressor_all_days/Wednesday_Regressor.csv", stringsAsFactors=FALSE)

#Filtering Regressor dataset for Training Timeperiod
xreg <- seasonal_reg[44:252,-c(1)]

#running arima model on dataset
dataarima_2 <- arima(arima_data_timeseries, order=c(5,2,2),xreg = xreg)

#Filtering Regressor dataset for Model Validation Time period 
newxreg <- seasonal_reg[253:267,]

#Predicting RPM for the next 15 wednesday's
dataforecasts <- predict(dataarima_2, n.ahead=15,newxreg = newxreg[,-c(1)])
dataforecasts
#writing data onto CSV file
write.table(dataforecasts, file = "D:\\Projects\\ARIMAX\\Trial.csv", sep = ",")

write.table(dataarima_2$residuals, file = "D:\\Projects\\ARIMAX\\Residuals.csv", sep = ",")

#plots
par(mfrow=c(2,1))
acf(x=dataarima_2$residuals)
pacf(x=dataarima_2$residuals,drop.lag.0 = FALSE)

auto.arima(dataarima_2)
