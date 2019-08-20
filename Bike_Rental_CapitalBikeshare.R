########## Time-Series analysis and Forecasting with ARIMA of Bike Rental from Capital Bikeshare system, Washington D.C. in R ###################

library('ggplot2')
library('forecast')
library('tseries')


# 1) loading data
# 2) visualization of the data using ggplot and removal of outliers
# 3) smoothing the series with moving average (MA)
# 4) Decomposition into trend, seasonality, and cyclicity and checking stationary/non-stationary problem
# 5) Differencing to get stationarity in the data
# 6) Fitting ARIMA model
# 7) Evaluation of the model

########## 1) loading data  ##############

bike <- read.csv('day.csv', header = TRUE, stringsAsFactors = FALSE)
View(bike)
sum(is.na(bike)) # data lacks NA values


###############  2) Visualization ################

bike$Date <- as.Date(bike$dteday)
ggplot(bike, aes(Date, cnt)) + geom_line() + xlab("month") + ylab("Bike Checkouts per day")
## plot of the daily checkout shows the variation in number from less than 100 to more than 7500. This variation might produce the outliers, for which tsclean() will be used.

count <- ts(bike[, c('cnt')])  
bike$cnt <- tsclean(count)
ggplot() + geom_line(data=bike, aes(x=Date, y=cnt)) + ylab ("Bicycle count without outliers") # plot of daily checkout without outliers
  

####### 3) smoothing the series with moving average (MA) ############

bike$ma_cnt <- ma(bike$cnt, order = 7) # weekly moving average
bike$ma_cnt30 <- ma(bike$cnt, order = 30) # monthly moving average

ggplot() +
  geom_line(data = bike, aes(x=Date, y=cnt, colour = "bike counts")) +
  geom_line(data=bike, aes(x=Date, y=ma_cnt, colour = "Weekly Moving Average counts")) + 
  geom_line(data=bike, aes(x=Date, y=ma_cnt30, colour = "Monthly Moving Average counts")) +
  ylab('Bicycle count per day')

## modeling daily variation in bike count will require a complex statistics of the variation in seasonality associated with 
## day of week, week of year, month of year, and holidays, which will be computationally complex. So, lets deal with weekly data.

###################### 4) Decomposing the data treated with the smoothed series of weekly moving average. ##########################################
## lets use stl() for decomposing and forecasting the series. stl() includes additive model by default.
## For multiplicative, allow.multiplicative.trend=TRUE should be used. This project includes additive model.

ma_count <- ts(na.omit(bike$ma_cnt), frequency = 30)
decomp <- stl(ma_count, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

## Our plot suggests the non-stationary nature of the data. lets check the non-stationary through statistically.
adf.test(ma_count, alternative = "stationary")
# p-value is 0.99, suggesting not to reject the null hypothesis. This indicates our data is non-stationary.



############ 5) Differencing to get stationarity in the data #########################

## lets plot ACF and PACF plots in order to determine order of differencing: order of the MA (q), and order of AR(p).
Acf(ma_count, main='')
Pacf(ma_count, main='')

## ACF plot shows clear autocorrelations, with several lags, which can also be due to the carry-over correlation
## from the first or early lags due to only two spikes at lags 1 and 7 in PACF plot.

## lets start with 1 differencing and check if stationarity is attained.

diff1 <- diff(deseasonal_cnt, differences = 1)
plot(diff1)
adf.test(diff1, alternative = "stationary")

## the adf (Augmented Dickey-Fuller Test) shows p-value of 0.01, indicates the series is stationary with 1 difference.

# Lets plot ACF and PACF
Acf(diff1, main='ACF for Differenced Series')
Pacf(diff1, main='PACF for Differenced Series')
## ACF plot shows autocorrelations at lags 1, 2, 3 and others. PACF show significant spike at lags 1 & 7.


############ 6) Fit an ARIMA model #####################

auto.arima(deseasonal_cnt, seasonal = FALSE)
# auto.arima () is used to know the maximum order for (p,d,q). Our results show ARIMA(1,1,1).
# This suggests 1 difference of degree, autoregressive term of first lag, and moving average of 1 order.

################ 7)  evaluation of the model ############################
arima1 <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(arima1), lag.max = 45, main = '(1,1,1) Model Residuals')

## ACF/PACF residual plots show repeating clear pattern at lag 7. which suggests to use p=7 or q=7.

arima2 <- arima(deseasonal_cnt, order=c(1,1,7))
arima2
tsdisplay(residuals(arima2), lag.max = 15, main = 'Seasonal Model Residuals')
## now residuals are white noise.

##### lets do forecast

fcast <- forecast(arima2, h=30)
plot(fcast)

## lets check the performance of our model by using the "hold-out" process, where some data is put 
## under the "hold-out" set, model is fitted, and compared the forecast with the actual observed values:

arima_hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:725)]), order = c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout, h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

## Above plot shows the forecast going to be a straight line, which is different than the pattern of past data. Our model is assuming a series with 
## no seasonality, and is differencing the originial non-stationary data. The predictions assume no seasonal fluctuations in the data, and presence of 
## constant mean and variance in the change in number of bicycles checkout from one day to another. The model looks like a naive model.

#### lets put back the seasonal component extracted earlier. 

fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality


seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
tsdisplay(residuals(seas_fcast), lag.max = 15, main = 'Seasonal Model Residuals')

## The forecast plotted show confidence interval of 80% and 95%. the confidence limit widens with increase in time, or longer term forecast have more uncertainty.

