#----------------------------------PROBLEM 1----------------------------------------#

# Setting the working directory and importing libraries
setwd('C:\\Users\\raona\\OneDrive\\Documents\\Time Series')
install.packages('tseries')
install.packages('forecast')
install.packages('urca')
library('forecast')
library('tseries')
library('urca')

# Reading the csv file
cet_temp <- read.csv('cet_temp.csv')

# Temperature column as time series data
z_1 <- ts(cet_temp$avg_annual_temp_C,start=1900,frequency=1)

# Plotting temperature Vs time
ts.plot(cet_temp$avg_annual_temp_C,ylab= "Annual Temperature", xlab='Years', main="Annual mean temperature in °C \n for the years 1900 to 2021")
abline(reg=lm(data=cet_temp,avg_annual_temp_C~time(avg_annual_temp_C)),col='red')

# Time series is not stationary need to take difference
y_1 <- diff(cet_temp$avg_annual_temp_C,differences = 1) 
ts.plot(y_1,ylab= "Annual Temperature", xlab='Years', main='Annual mean temperature in °C \n for the years 1900 to 2021')
abline(reg=lm(data=cet_temp,y_1~time(y_1)),col='red')

# Visualization after detrending
hist(y_1,breaks =20,main = "Histogram after removing trend",xlab = "")
boxplot(y_1,horizontal= TRUE,main = "Box plot after removing trend",xlab = "")
qqnorm(y_1,main = "Normality Plot after removing trend")
qqline(y_1,col = "red")

# Augmented Dickey-Fuller Test to check whether time series is Stationary
adf.test(y_1)

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
summary(ur.kpss(y_1))

# Theoretical ACF and PACF
t_acf<-ARMAacf(ar=c(),ma=c(-0.65),lag.max=121)
t_pacf<-ARMAacf(ar=c(),ma=c(-0.65),lag.max=121,pacf=TRUE)

# Plot for sample ACF and PACF 
par(mfrow=c(1,2))
acf(y_1, main = 'Series Annual Temperature')
lines(c(0:121),t_acf,col="red")
pacf(y_1, main=' Series Annual Temperature')
lines(c(1:121),t_pacf,col="red")

# Fitting an ARMA(0,1,1)
model<-arima(cet_temp$avg_annual_temp_C,order=c(0,1,1),method="ML")

# Residual plot for ARIMA(0,1,1)
res<- residuals(model)
checkresiduals(model)

# Performing Ljung Box test on residuals
Box.test(res,type=c("Ljung-Box"))










