# Load necessary libraries
library(forecast)
library(tseries)
#Q-1))
# 1 (a) Simulate 100 observations from AR(1) process
set.seed(42)
n <- 100
phi <- 0.5
ar1_process <- arima.sim(model = list(ar = phi), n = n)



# 1 (b) Plot the time series
plot.ts(ar1_process, main = "Simulated AR(1) Process", ylab = "X_t", col = "blue")


# 1 (c) Estimate AR(1) parameter using arima function
ar1_fit <- arima(ar1_process, order = c(1, 0, 0))
phi_estimated <- ar1_fit$coef[1]
phi_estimated  # Print estimated phi


# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ar1_process, main = "ACF of AR(1) Process")
pacf(ar1_process, main = "PACF of AR(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid


# 3 (i) Fit AR(1) or AR(2) model to the data
ar_fit <- arima(ar1_process, order = c(1, 0, 0))  # AR(1)
ar_fit_2 <- arima(ar1_process, order = c(2, 0, 0))  # AR(2)

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ar_fit, h = 10)
plot(forecasted_values, main = "Forecast of AR(1) Process", col = "red")



#Q-2))

# Load necessary libraries
library(forecast)
library(tseries)

# 1 (a) Simulate 100 observations from MA(1) process
set.seed(42)
n <- 100
theta <- 0.5
ma1_process <- arima.sim(model = list(ma = theta), n = n)

# 1 (b) Plot the time series
plot.ts(ma1_process, main = "Simulated MA(1) Process", ylab = "X_t", col = "blue")

# 1 (c) Estimate MA(1) parameter using arima function
ma1_fit <- arima(ma1_process, order = c(0, 0, 1))
theta_estimated <- ma1_fit$coef[1]
theta_estimated  # Print estimated theta

# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ma1_process, main = "ACF of MA(1) Process")
pacf(ma1_process, main = "PACF of MA(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid

# 3 (i) Fit MA(1) or MA(2) model to the data
ma_fit <- arima(ma1_process, order = c(0, 0, 1))  # MA(1) model
ma_fit_2 <- arima(ma1_process, order = c(0, 0, 2))  # MA(2) model

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ma_fit, h = 10)
plot(forecasted_values, main = "Forecast of MA(1) Process", col = "red")
