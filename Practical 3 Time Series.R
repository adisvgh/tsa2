### PRACTICAL 3 ####

### Q1

# Load necessary libraries
library(forecast)
library(tseries)

# Step 1: Set up the Data
data <- data.frame(
  Time = 1:15,
  Values = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240)
)

# Convert the data to a time series object
ts_data <- ts(data$Values)

# Step 2: Perform Stationarity Test (ADF test)
adf_test <- adf.test(ts_data)
print(adf_test)

# Step 3: Fit an ARMA(2,2) Model to the original data
model_arma <- arima(ts_data, order = c(2, 0, 2))
summary(model_arma)

### COMMENT ###

# this data is non stationary thats why it will not fit in arima(2,2). To make this work first convert this data into stationary and then proceed

# Step 4: Check the Residual Diagnostics
checkresiduals(model_arma)

# Step 5: Forecast the Next 10 Values
forecasted_values <- forecast(model_arma, h = 10)

# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecasted_values, main = "Original and Forecasted Values", 
     xlab = "Time", ylab = "Values", ylim = c(100, 300))
lines(ts_data, col = "blue", lwd = 2)  # Original values
lines(fitted(model_arma), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")



#Q2
# Load necessary libraries
library(forecast)
library(tseries)

# Step 1: Set up the Data
data <- data.frame(
  Quarter = 1:12,
  Sales = c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
)

# Convert the sales data to a time series object
ts_sales <- ts(data$Sales, start = 1, frequency = 1)

# Step 2: Plot ACF and PACF
par(mfrow = c(1, 2))  # Set up the plotting area
acf(ts_sales, main = "ACF of Sales Data")
pacf(ts_sales, main = "PACF of Sales Data")

# Step 3: Fit an ARMA(1,1) Model
model_arma <- arima(ts_sales, order = c(1, 0, 1))
summary(model_arma)

# Step 4: Check the Residual Diagnostics
checkresiduals(model_arma)

# Step 5: Forecast the Next 12 Values
forecasted_values <- forecast(model_arma, h = 12)

# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecasted_values, main = "Original and Forecasted Sales Values", 
     xlab = "Quarter", ylab = "Sales", ylim = c(480, 800))
lines(ts_sales, col = "blue", lwd = 2)  # Original values
lines(fitted(model_arma), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")

