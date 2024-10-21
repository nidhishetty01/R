


























#2.1 Exponential and Moving Average smoothing – Holt winters smoothing
###############
#Q1 
#############
#Analyze given time series data according to simple exponential series by considering alpha α=0.3
#Use holts exponential smoothing method with α=0.3 and β=0.2 smooth the given data.
#Conclude your results by comparing both the plots with original series

# Load necessary libraries
library(forecast)

# Create the time series data
years <- 1996:2019
observations <- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7,
                  153.6, 153.5, 154.4, 154.9, 155.7, 156.3, 156.6,
                  156.7, 157.0, 157.3, 157.8, 158.3, 158.6, 158.6, 
                  159.1, 159.3)

time_series <- ts(observations, start = 1996, frequency = 1)

# Simple Exponential Smoothing
alpha <- 0.3
ses_model <- HoltWinters(time_series, alpha = alpha, beta = FALSE, gamma = FALSE)

# Holt's Exponential Smoothing
beta <- 0.2
holt_model <- HoltWinters(time_series, alpha = alpha, beta = beta, gamma = FALSE)

# Plot the original series and the smoothing results
plot(time_series, main = "Time Series Data with Smoothing", xlab = "Year", ylab = "Observations", col = "black", lwd = 2)

# Add Simple Exponential Smoothing line
lines(ses_model$fitted[,1], col = "blue", lwd = 2, lty = 2)  # Fitted values for SES

# Add Holt's Smoothing line
lines(holt_model$fitted[,1], col = "red", lwd = 2, lty = 2)  # Fitted values for Holt's

# Add legend
legend("topleft", legend = c("Original Series", "Simple Exponential Smoothing", "Holt's Smoothing"),
       col = c("black", "blue", "red"), lty = c(1, 2, 2), lwd = 2)

###################

#Q2)Given table denotes Atmospheric CO2 Concentration at Mauna Lao from year 1991-2003.

####################
#a) Make time series plot of the given data.
# Load necessary libraries
library(ggplot2)

# Create the time series data
years <- 1991:2003
co2_concentration <- c(355.62, 356.36, 357.1, 358.86, 360.9,
                       362.58, 363.84, 366.58, 368.3,
                       369.47, 371.03, 373.61, 357.61)

# Create a time series object
co2_ts <- ts(co2_concentration, start = 1991, frequency = 1)

# a) Make time series plot of the given data
par(mfrow = c(1,1))
plot(co2_ts, main = "Atmospheric CO2 Concentration at Mauna Loa (1991-2003)",
     xlab = "Year", ylab = "Average CO2 Concentration (ppm)", 
     col = "blue", lwd = 2, type = "o", pch = 16)

# b) Forecast 2004 value by 3 yearly moving average smoothing method
# Calculate 3-year moving average with padding for the last values
moving_average <- filter(co2_ts, rep(1/3, 3), sides = 2)

# The last valid moving average value will forecast 2004
forecast_2004 <- tail(moving_average, n = 1)

# If forecast_2004 is NA, use the last 3 values manually
if (is.na(forecast_2004)) {
  forecast_2004 <- mean(tail(co2_concentration, 3))  # Average of the last 3 years
}

# Print the forecast value for 2004
cat("Forecasted CO2 Concentration for 2004 using 3-year moving average:", forecast_2004, "ppm\n")


##################
#Q3 Q.3 Apply the Holt-Winters method to AirPassengers data and forecast next 12 months data.(Use multiplicative model)
 
###################

# Load necessary libraries
library(forecast)

# Load the AirPassengers data
data("AirPassengers")

# Display the first few rows of the dataset
head(AirPassengers)

# Fit the Holt-Winters model with a multiplicative model
holt_winters_model <- HoltWinters(AirPassengers, seasonal = "multiplicative")

# Display the model summary
summary(holt_winters_model)

# Forecast the next 12 months
forecast_values <- forecast(holt_winters_model, h = 12)

# Print the forecasted values
print(forecast_values)

# Plot the original data and the forecast
plot(forecast_values, main = "Holt-Winters Forecast for AirPassengers Data",
     xlab = "Year", ylab = "Number of Passengers", col = "red", lwd = 2)

