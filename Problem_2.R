#----------------------------------PROBLEM 2----------------------------------------#

# Setting the working directory and importing libraries
setwd('C:\\Users\\raona\\OneDrive\\Documents\\Time Series')
install.packages('tseries')
install.packages('forecast')
install.packages('urca')
install.packages('seastests')
library('forecast')
library('tseries')
library('ggplot2')
library('urca')
library('seastests')

# Reading the csv file
em_house_prices <- read.csv('em_house_prices.csv')

# Housing price column as time series data
z<-ts(em_house_prices$average_price_gbp,start=2010,frequency=12)

#Augmented Dicky Fuller test for stationarity
adf.test(z)

#Krsukall Wallis test for Seasonality
isSeasonal(z, test = "kw", freq = 12)
kw(z, freq = 12, diff = T, residuals = F, autoarima = T)

# Plotting Housing Price Vs time
ts.plot(em_house_prices$average_price_gbp,ylab= "Mean House prices(in £GBP)", xlab='Months', main="Sale prices in East Midlands (in £GBP)")
abline(reg=lm(data=em_house_prices,average_price_gbp~time(average_price_gbp)),col='red')

# Time series is not stationary need to take difference of order 2
y <- diff(em_house_prices$average_price_gbp,differences=2) 
ts.plot(y,ylab= "Mean House prices(in £GBP)", xlab='Months', main="Sale prices in East Midlands (in £GBP)")
abline(reg=lm(data=em_house_prices,y~time(y)),col='red')

# Visualization after detrending
hist(y,breaks =20,main = "Histogram after removing trend",xlab = "")
boxplot(y,horizontal= TRUE,main = "Box plot after removing trend",xlab = "")
qqnorm(y,main = "Normality Plot after removing trend")
qqline(y,col = "red")

# Seasonal Plots for checking Seasonality
ggseasonplot(z, year.labels=TRUE, year.labels.left=TRUE,main="Sale prices in East Midlands (in £GBP)",ylab= "Mean House prices(in £GBP)") 
ggseasonplot(z, polar=TRUE,main="Sale prices in East Midlands (in £GBP)",ylab= "Mean House prices(in £GBP)")
ggsubseriesplot(z)
gglagplot(z)

# Augmented Dickey-Fuller Test to check whether time series is Stationary
adf.test(y)

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test after differencing
summary(ur.kpss(y))

# Theoretical ACF and PACF
t_acf<-ARMAacf(ar=c(-0.6,-0.3),ma=c(-0.35),lag.max=120)
t_pacf<-ARMAacf(ar=c(-0.6,-0.3),ma=c(-0.35),lag.max=120,pacf=TRUE)

# Plot for sample ACF and PACF 
par(mfrow=c(1,2))
acf(y, main = 'Series Price of Houses')
lines(c(0:120),t_acf,col="red")
pacf(y, main=' Series Price of Houses')
lines(c(1:120),t_pacf,col="red")

# Fitting an ARIMA(2,2,1)(0,1,1)[12]
model1<-arima(em_house_prices$average_price_gbp,order=c(3,2,2), seasonal =list(order=c(0,1,2),period=12),method="ML")
summary(model1)

# Residual Plots for the above model
res<- residuals(model1)
checkresiduals(model1)

#Forecast for next six  months
model1 %>% forecast(h=6) %>% autoplot(include=80,ylab= "Mean House prices(in £GBP)",main="Forecast for Sale prices in East Midlands (in £GBP)",transform.pars=TRUE)
model1 %>% forecast(h=6)

#Ljung Box test on residuals
Box.test(res,type = "Ljung-Box") 




