# import the following library

library("ggplot2")
library('forecast')
library('tseries')

# import the data 

daily_data <- read.csv("day.csv", header = TRUE, stringsAsFactors = FALSE)

# Examine My Data
daily_data$Date <- as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date("month") + 
  ylab("Daily Bike Checkouts") + xlab("")

# There's definitely some volatile data in this data and thus we can clean them using
# tsclean.

count_ts <- ts(daily_data[, c("cnt")])

daily_data$clean_cnt <- tsclean(count_ts)

ggplot(daily_data, aes(Date, clean_cnt)) + geom_line() + scale_x_date("month") + 
  ylab("Daily Bike Checkouts") + xlab("")

# The data however is still pretty volatile for us to use, how can i create a 
# singe line for us to understand the data easily?

# We can create a regression line or we can call it a moving average line 

# using the clean count with no outliers
daily_data$cnt_ma <- ma(daily_data$clean_cnt, order = 7) 

# using the clean count with 30 days moving average
daily_data$cnt_ma30 <- ma(daily_data$clean_cnt, order = 30)

# Plotting of Moving Average and the main data. 
ggplot() + 
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, color="Counts")) + 
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma, color = "Weekly Moving Average")) + 
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, color = "Monthly Moving Average"))

# Step 3: Decomposing Of Data

# Building blocks of a time series analysis are seasonality, trend, and cycle

# Seasonal component refers to fluctuations in the data related to calendar cycles
# Trend component is the overall pattern of the series
# Cycle component consists of decreasing or increasing patterns that are not seasonal.

# Finally, part of the series that can't be attributed to seasonal, cycle, or 
# trend components is referred to as residual or error

# Let's begin by finding the seasonal component
# We can do this simply by stl()

count_ma <- ts(na.omit(daily_data$cnt_ma), frequency = 30)
decomp <- stl(count_ma, s.window = "periodic")

# removing the seasonality can be accomplished by simply subtracting the seasonal component 
# from the original series
deseasonal_cnt <- seasadj(decomp)

plot(decomp)

# Stationarity 

# To fit an ARIMA model,  it requires the series to be stationary
# augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity. 
# The null hypothesis assumes that the series is non-stationary

# Our bicycle data however is non-stationary

adf.test(count_ma, alternative = "stationary")

# non-stationary series can be corrected by a simple transformation such as differencing. 

# Autocorrelation plots (also known as ACF or the auto correlation function) 
# are a useful visual tool in determining whether a series is stationary

Acf(count_ma, main='')
Pacf(count_ma, main='')

# From the chart, we need to look at the blue dotted lines and from there we can see 
# where the spikes are since R plots 95% of the plot to be the area of significance.

# Start with the order of d = 1 and re-evaluate whether further differencing is needed

count_d1 <- diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# Plotting the differenced series, we see an oscillating pattern around 0 
# with no visible strong trend

# Next, spikes at particular lags of the differenced series can help inform the 
# choice of p or q for our model:

Acf(count_d1, main='ACF for Differenced Series')
# significant auto correlations at lag 1 and 2 and beyond

Pacf(count_d1, main="PACF For Differenced Series")
# Partial correlation plots show a significant spike at lag 1 and 7

# A spike at lag 7 might suggest that there is a seasonal pattern present, 
# perhaps as day of the week.

# Step 6 Fitting an ARIMA Model
auto.arima(deseasonal_cnt, seasonal = FALSE)

# Step 7 Evaluate and Iterate 
fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

# FORECAST! 
fcast <- forecast(fit2, h=30)
plot(fcast)

# We wanted to get a sense of how well our model will perform in the future 
# One method will be reserving a portion of our data as a "hold-out" set, fit the model
# and then compare the forecasted value to the actual observed value

hold <- window(ts(deseasonal_cnt), start=700)
  
fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:725)]), order = c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

# ROFL its not accurate at all!
# This forecast may be a naive model, but it illustrates the process of choosing 
# an ARIMA model and could also serve as a benchmark to grade against as more 
# complex models are built.

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)

# Material here can be used as a general guideline to examining your series, 
# using ACF and PACF plots to choose model order, and fitting the model in R.