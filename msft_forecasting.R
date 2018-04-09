# import the following library

library("ggplot2")
library("tseries")
library("forecast")

# import the data 
msft_stock <- read.csv("MSFT.csv", header = TRUE, stringsAsFactors = FALSE)

# EDA 
str(msft_stock)

# Date is not yet a Date format
msft_stock$Date <- as.Date(msft_stock$Date)

ggplot(msft_stock, aes(Date, Adj.Close)) + geom_line() + scale_x_date("month") +
  ylab("Price") + xlab("")

# Let's plot out the moving average of the data so that we can see whether the price
# is overvalued or undervalued

# Weekly Moving Average 
msft_stock$weekly_ma <- ma(msft_stock$Adj.Close, order = 7)

# Monthly Moving Average
msft_stock$monthly_ma <- ma(msft_stock$Adj.Close, order = 30)

ggplot() + 
  geom_line(data = msft_stock, aes(x = Date, y = Adj.Close, color="Close Price")) +
  geom_line(data = msft_stock, aes(x = Date, y = weekly_ma, color="Weekly Moving Average")) +
  geom_line(data = msft_stock, aes(x = Date, y = monthly_ma, color="Monthly Moving Average"))

# Maybe I took too much data! >_< 

# Step 3: Decompose the data
price_ma <- ts(na.omit(msft_stock$weekly_ma), frequency = 30)
decomp <- stl(price_ma, s.window = "periodic")
deseasonal_price <- seasadj(decomp)
plot(decomp)

# Stationarity
adf.test(price_ma, alternative = "stationary")

Acf(price_ma, main='')
Pacf(price_ma, main='')

# Based on the Pacf, it look like 2

price_d1 <- diff(deseasonal_price, differences = 2)
plot(price_d1)
adf.test(price_d1, alternative = "stationary")

Acf(price_d1, main='ACF for Differenced Series')
Pacf(price_d1, main="PACF For Differenced Series")

# Fitting an ARIMA Model
fit <- auto.arima(deseasonal_price, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

fit2 = arima(deseasonal_price, order=c(1,1,7))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

# Forecast the Model
fcast <- forecast(fit2, h=30)
plot(fcast)


# Using previous data and observe performance
hold <- window(ts(deseasonal_price), start=2400)

fit_no_holdout <- arima(ts(deseasonal_price[-c(2400:2518)]), order = c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_price))

# Fit with seasonality

fit_w_seasonality = auto.arima(deseasonal_price, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
