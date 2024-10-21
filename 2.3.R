





















##2.3  -> Using the above time series data, perform the following tasks
#1. Plot the ACF and PACF of the original time series.
#2. Fit an ARMA(2,2) model to the data.
#3. Check the residual diagnostics of the fitted model.
#4. Forecast the next 10 values using the fitted model.
#5. Plot the original time series, the fitted values, and the forecasted values.

# Load necessary library
library(forecast)

# Create the time series data
time_series <- ts(c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240))

# Plot the ACF and PACF
par(mfrow = c(1, 2)) # Set up the plotting area
acf(time_series, main = "ACF of Original Time Series")
pacf(time_series, main = "PACF of Original Time Series")
# Standardize the time series data
time_series_scaled <- scale(time_series)
#################
# Automatically select the best ARMA model
auto_arma_model <- auto.arima(time_series)

# Display the summary of the model
summary(auto_arma_model)

############
adf_test <- adf.test(time_series)

# Display the result
print(adf_test)

# Apply log transformation to stabilize the variance
log_series <- log(time_series)

# Perform first-order differencing on the log-transformed series
log_series_diff <- diff(log_series, differences = 1)
# Fit an ARIMA(2,1,2) model on the log-transformed and differenced series
arma_model <- arima(log_series, order = c(2, 1, 2))

# Display the summary of the model
summary(arma_model)
# Plot the residual diagnostics
tsdiag(arma_model)

# Perform the Ljung-Box test
Box.test(residuals(arma_model), lag = 10, type = "Ljung-Box")
# Forecast the next 10 time points
forecast_values <- forecast::forecast(arma_model, h = 10)

# Display the forecasted values
print(forecast_values)

# Plot the original series and the forecast
#plot(forecast_values, main = "Original Series with Forecasts")
#lines(time_series, col = "black", lwd = 2)
##################################################
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
# Plot the original series and the forecast with labels
plot(forecast_values, 
     main = "Original Series with Forecasts", 
     xlab = "Time", 
     ylab = "Sales", 
     col = "blue")  # Change color for better visibility
lines(time_series, col = "black", lwd = 2)  # Original series in black
legend("topleft", legend = c("Forecast", "Original Series"), col = c("blue", "black"), lty = 1, lwd = 2)


#Q2Q.2 
#Using the following time series data, perform the following tasks:
 # 1. Plot the ACF and PACF of the original time series.
#2. Fit an ARMA(1,1) model to the data.
#3. Check the residual diagnostics of the fitted model.
#4. Forecast the next 12 values using the fitted model.
#5. Plot the original time series, the fitted values, and the forecasted values.

#####################################

library(ggplot2)
# Create the time series data
sales_data <- c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
time_series <- ts(sales_data, frequency = 4, start = c(1, 1))  # quarterly data

# Open a new plotting window with a larger size
par(mfrow = c(1, 2)) # Set up the plotting area
acf(time_series, main = "ACF of Sales Data")
pacf(time_series, main = "PACF of Sales Data")


# Fit an ARMA(1,1) model
arma_model <- arima(time_series, order = c(1, 0, 1))
summary(arma_model)

# Plot residual diagnostics
tsdiag(arma_model)

# Perform the Ljung-Box test
ljung_box_test <- Box.test(residuals(arma_model), lag = 10, type = "Ljung-Box")
print(ljung_box_test)

# Forecast the next 12 time points
forecast_values <- forecast(arma_model, h = 12)
print(forecast_values)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
# Plot the original series and the forecast
plot(forecast_values, main = "Sales Data with ARMA(1,1) Forecast", xlab = "Quarter", ylab = "Sales", ylim = c(490, 740))
lines(time_series, col = "black", lwd = 2)
legend("topleft", legend = c("Original", "Forecast"), col = c("black", "blue"), lty = 1)


