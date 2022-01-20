# Time-Series Analysis and Forecasting with ARIMA of Bike Rental from Capital Bikeshare system, Washington D.C. using RStudio.

#### Upload Libraries
library('ggplot2')

library('forecast')

library('tseries')

#### Steps Followed
#### 1) loading data
#### 2) visualization of the data using ggplot and removal of outliers
#### 3) smoothing the series with moving average (MA)
#### 4) Decomposition into trend, seasonality, and cyclicity and checking stationary/non-stationary problem
#### 5) Differencing to get stationarity in the data
#### 6) Fitting ARIMA model
#### 7) Evaluation of the model

#### 1) loading data
bike <- read.csv('day.csv', header = TRUE, stringsAsFactors = FALSE)

View(bike)

sum(is.na(bike)) # data lacks NA values

#### 2) visualization
bike$Date <- as.Date(bike$dteday)

ggplot(bike, aes(Date, cnt)) + geom_line() + xlab("month") + ylab("Bike Checkouts per day")
![image](https://user-images.githubusercontent.com/48388697/150259076-f748f0e2-a365-44a0-902c-ecef6b189ddb.png)

#### plot of the daily checkout shows the variation in number from less than 100 to more than 7500. This variation might produce the outliers, for which tsclean() will be used.

count <- ts(bike[, c('cnt')]) 

bike$cnt <- tsclean(count)

ggplot() + geom_line(data=bike, aes(x=Date, y=cnt)) + ylab ("Bicycle count without outliers") # plot of daily checkout without outliers

![image](https://user-images.githubusercontent.com/48388697/150259820-360d2cd3-dade-42e7-a322-61aa42d97c9e.png)

#### 3) smoothing the series with moving average (MA)

bike$ma_cnt <- ma(bike$cnt, order = 7) # weekly moving average

bike$ma_cnt30 <- ma(bike$cnt, order = 30) # monthly moving average

ggplot() +
  geom_line(data = bike, aes(x=Date, y=cnt, colour = "bike counts")) +
  geom_line(data=bike, aes(x=Date, y=ma_cnt, colour = "Weekly Moving Average counts")) + 
  geom_line(data=bike, aes(x=Date, y=ma_cnt30, colour = "Monthly Moving Average counts")) +
  ylab('Bicycle count per day')

![image](https://user-images.githubusercontent.com/48388697/150260526-9c03ddc8-ee5c-48a1-9257-5185024ea613.png)

#### Modeling daily variation in bike count will require a complex statistics of the variation in seasonality associated with the day of week, week of year, month of year, and holidays, which will be computationally complex. So, lets deal with weekly data.

##### 4) Decomposing the data treated with the smoothed series of weekly moving average.

#### Lets use stl() for decomposing and forecasting the series. stl() includes additive model by default. For multiplicative, allow.multiplicative.trend=TRUE should be used. This project includes additive model.
ma_count <- ts(na.omit(bike$ma_cnt), frequency = 30)

decomp <- stl(ma_count, s.window = "periodic")

deseasonal_cnt <- seasadj(decomp)

plot(decomp)

![image](https://user-images.githubusercontent.com/48388697/150260881-e42b1aa4-f005-46be-a70b-3029fcdd7404.png)

#### Our plot suggests the non-stationary nature of the data. lets check the non-stationary through statistically.
adf.test(ma_count, alternative = "stationary")
![image](https://user-images.githubusercontent.com/48388697/150260943-6dbbaf86-28f2-4756-8a61-82f81dcf38c0.png)

#### p-value is 0.99, suggesting not to reject the null hypothesis. This indicates our data is non-stationary.

#### 5) Differencing to get stationarity in the data

#### Lets plot ACF and PACF plots in order to determine order of differencing: order of the MA (q), and order of AR(p).
Acf(ma_count, main='')
![image](https://user-images.githubusercontent.com/48388697/150261053-0e8919b6-7de4-47f5-be44-d10bcb2219e7.png)
Pacf(ma_count, main='')
![image](https://user-images.githubusercontent.com/48388697/150261111-178fc7cd-a314-45b8-814f-37a32871a53a.png)

#### ACF plot shows clear autocorrelations, with several lags, which can also be due to the carry-over correlation from the first or early lags due to only two spikes at lags 1 and 7 in PACF plot.

#### Lets start with 1 differencing and check if stationarity is attained.
diff1 <- diff(deseasonal_cnt, differences = 1)

plot(diff1)
![image](https://user-images.githubusercontent.com/48388697/150261209-738c4507-a8e1-40f4-b034-1b772ad9ef80.png)

adf.test(diff1, alternative = "stationary")
![image](https://user-images.githubusercontent.com/48388697/150261259-fb6784f4-571d-44fc-8959-df87727c9034.png)

#### The adf (Augmented Dickey-Fuller Test) shows p-value of 0.01, indicates the series is stationary with 1 difference.

#### Lets plot ACF and PACF
Acf(diff1, main='ACF for Differenced Series')
![image](https://user-images.githubusercontent.com/48388697/150261322-4b35b910-c207-4296-a014-cfba27a4f8a7.png)

Pacf(diff1, main='PACF for Differenced Series')
![image](https://user-images.githubusercontent.com/48388697/150261360-d6f598e6-463a-41b4-a576-0c22021764f0.png)

#### ACF plot shows autocorrelations at lags 1, 2, 3 and others. PACF show significant spike at lags 1 & 7.

#### 6) Fit an ARIMA model

auto.arima(deseasonal_cnt, seasonal = FALSE)
![image](https://user-images.githubusercontent.com/48388697/150261482-d47bcb44-b58d-411c-903c-d7901b70397b.png)

#### auto.arima () is used to know the maximum order for (p,d,q). Our results show ARIMA(1,1,1). This suggests 1 difference of degree, autoregressive term of first lag, and moving average of 1 order.

#### 7)  evaluation of the model
arima1 <- auto.arima(deseasonal_cnt, seasonal = FALSE)

tsdisplay(residuals(arima1), lag.max = 45, main = '(1,1,1) Model Residuals')

![image](https://user-images.githubusercontent.com/48388697/150261595-b83c32a1-3ce5-4179-84b7-37f0f6084dbe.png)

#### ACF/PACF residual plots show repeating clear pattern at lag 7. which suggests to use p=7 or q=7.

arima2 <- arima(deseasonal_cnt, order=c(1,1,7))

arima2

![image](https://user-images.githubusercontent.com/48388697/150261660-6f6dbc76-72de-43ea-8843-fdc3c470bd61.png)

tsdisplay(residuals(arima2), lag.max = 15, main = 'Seasonal Model Residuals')

![image](https://user-images.githubusercontent.com/48388697/150261700-25797a77-8a6e-45e4-8049-81bf001942bc.png)

#### now residuals are white noise.

##### lets do forecast

fcast <- forecast(arima2, h=30)

plot(fcast)

![image](https://user-images.githubusercontent.com/48388697/150261755-50205d7b-9c60-43ac-89f2-5fc616d43b72.png)

#### Lets check the performance of our model by using the "hold-out" process, where some data is put under the "hold-out" set, model is fitted, and compared the forecast with the actual observed values:

arima_hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:725)]), order = c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout, h=25)

plot(fcast_no_holdout, main=" ")

lines(ts(deseasonal_cnt))

![image](https://user-images.githubusercontent.com/48388697/150261823-d9a53a13-c6ce-4916-b32b-a9d447185269.png)


#### Above plot shows the forecast going to be a straight line, which is different than the pattern of past data. Our model is assuming a series with no seasonality, and is differencing the originial non-stationary data. The predictions assume no seasonal fluctuations in the data, and presence of constant mean and variance in the change in number of bicycles checkout from one day to another. The model looks like a naive model.

#### lets put back the seasonal component extracted earlier. 

fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal=TRUE)

fit_w_seasonality

![image](https://user-images.githubusercontent.com/48388697/150261902-079aadcd-9306-46d6-af26-03fdf7b85133.png)

seas_fcast <- forecast(fit_w_seasonality, h=30)

plot(seas_fcast)

![image](https://user-images.githubusercontent.com/48388697/150261944-672bb602-e10b-42bd-addc-045544bc66b3.png)


tsdisplay(residuals(seas_fcast), lag.max = 15, main = 'Seasonal Model Residuals')

![image](https://user-images.githubusercontent.com/48388697/150261983-8e5c1de7-ad47-4bfb-8622-9811b48ebe45.png)

#### The forecast plotted show confidence interval of 80% and 95%. the confidence limit widens with increase in time, or longer term forecast have more uncertainty.















