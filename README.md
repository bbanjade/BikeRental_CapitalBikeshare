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
