



















#2.5
# Install the necessary packages if you haven't already
install.packages("forecast")
install.packages("ggplot2")
install.packages("tseries")

# Load libraries
library(forecast)
library(ggplot2)
library(tseries)
library(dplyr)
# Create the sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330, 
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350, 
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370, 
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390, 
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Convert to time series object
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)
sales_ts
# a) Plot the sales data
plot(sales_ts, main = "Monthly Sales Data", xlab = "Time", ylab = "Sales", col = "blue", lwd = 2)

# Observations: Check for seasonality and trend by inspecting the plot

#b]
# Differencing to remove trend
diff_sales_ts <- diff(sales_ts, differences = 1)
diff_sales_ts
plot(diff_sales_ts, main = "Differenced Sales Data", xlab = "Time", ylab = "Differenced Sales")

#a) After plotting the time series, 
#we may observe a long-term increasing trend with a recurring seasonal pattern. 
#The series might also show random fluctuations.
#b) To achieve stationarity, the data may require differencing 
#(to remove the trend) and possibly seasonal differencing (to remove the seasonal component). 
#Log transformation can also stabilize the variance.
################
#Q2
#Question 2: Model Identification
#Given the same dataset:
# a) Explain how you would identify the order of differencing required for the series (d in ARIMA).
#b) How would you determine the seasonal differencing term (D) for the SARIMA model?
#c) Identify the potential AR and MA orders (p, q, P, Q) using the ACF and PACF plots.

#A)
# Perform Augmented Dickey-Fuller test for stationarity
library(tseries)
adf_test <- adf.test(sales_ts)
print(adf_test)
# If p-value > 0.05, differencing is needed (non-stationary).

#B)
# Seasonal differencing can be checked by examining the ACF plot
Acf(sales_ts)
# Look for seasonal lags in the ACF plot (e.g., significant peaks at 12 months).

#c)
# ACF and PACF plots
Acf(diff_sales_ts, main="ACF of Differenced Sales Data")
Pacf(diff_sales_ts, main="PACF of Differenced Sales Data")


#a) The order of differencing (d) can be determined by checking the stationarity of 
#the series using methods like the Augmented Dickey-Fuller (ADF) test. 
#If the p-value is above a threshold (e.g., 0.05), differencing is needed.
#b) The seasonal differencing term (D) is usually determined by checking for 
#seasonality at the seasonal lag (e.g., 12 months for yearly seasonality). 
#This can be confirmed through ACF plots showing peaks at seasonal lags.
#c) The ACF and PACF plots are used to identify the AR (p) and MA (q) terms, 
#and the seasonal AR (P) and MA (Q) terms by looking at significant lags.



#Question 3: SARIMA Model Fitting
#After identifying the appropriate (p, d, q) and (P, D, Q, m) orders for the seasonal data:
#a) Fit a SARIMA model to the data.
#b) Interpret the model output and residual diagnostics.
#c) If the residuals show autocorrelation, what steps would you take to improve the model?

#a) Fit a SARIMA Model to the Data
# Load necessary libraries
library(forecast)

# Assuming you have already created the time series object `sales_ts`

# Fit a SARIMA model, e.g., SARIMA(1, 1, 1)(1, 1, 1)[12]
# Replace (1, 1, 1)(1, 1, 1)[12] with your chosen orders
sarima_model <- Arima(sales_ts, order = c(1, 1, 1), seasonal = c(1, 1, 1), lambda = 0)

# Print the summary of the model
summary(sarima_model)

#b) Interpret the Model Output and Residual Diagnostics
# Residual diagnostics
checkresiduals(sarima_model)  # This function gives ACF plot of residuals and performs the Ljung-Box test

# You can also plot the residuals
plot(sarima_model$residuals, main = "Residuals of SARIMA Model", ylab = "Residuals", xlab = "Time")

#c) 
#a) Use SARIMA(p, d, q)(P, D, Q, s) to fit the model using statistical software like  R.
#b) Interpret the coefficients and check residual diagnostics, including ACF plots 
#and tests for white noise (Ljung-Box test).
#c) If autocorrelation remains, consider refining the AR and MA terms, 
#adding additional seasonal terms, or applying further differencing.



#Q4 ) 
#Question 4: Forecasting with SARIMA
#Using the SARIMA model from the previous question:
#a) Forecast the sales for the next 12 months.
#b) Plot the forecasted values along with the original time series data. 
#How well does the model capture the seasonality?
# Load necessary libraries
library(forecast)
library(tseries)

# Step 1: Create the monthly sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280,
                310, 330, 220, 230, 250, 240, 260, 280, 320, 300,
                290, 310, 330, 350, 240, 250, 270, 260, 280, 300,
                340, 320, 310, 330, 350, 370, 260, 270, 290, 280,
                300, 320, 360, 340, 330, 350, 370, 390, 280, 290,
                310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Create a time series object
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)

# Step 2: Check for NA or infinite values
if (any(is.na(sales_ts)) || any(!is.finite(sales_ts))) {
  stop("Data contains NA or infinite values.")
}

# Visualize the time series
plot(sales_ts, main="Sales Data", ylab="Sales", xlab="Time")

# Step 3: Check for stationarity
adf_test <- adf.test(sales_ts)
print(adf_test)

# If needed, apply differencing
# diff_sales_ts <- diff(sales_ts)

# Step 4: Fit a simpler SARIMA model
sarima_model_simple <- Arima(sales_ts, order = c(1, 1, 0), seasonal = c(0, 1, 0))

# Step 5: Check residuals
checkresiduals(sarima_model_simple)

# Step 6: Automatic model selection
sarima_model_auto <- auto.arima(sales_ts)

# Print the automatic model summary
summary(sarima_model_auto)

# Step 7: Forecast the sales for the next 12 months
forecasted_values <- forecast(sarima_model_auto, h = 12)

# Step 8: Plot the forecasted values along with the original time series data
plot(forecasted_values, main = "Sales Forecast for the Next 12 Months", 
     ylab = "Sales", xlab = "Time", col = "blue", flty = 2)

# Add the original time series data for comparison
lines(sales_ts, col = "black", lwd = 2)

# Add a legend to the plot
legend("topleft", legend = c("Original Series", "Forecasted Values"),
       col = c("black", "blue"), lty = 1, lwd = 2)

#a) Use the fitted SARIMA model to generate 12-month forecasts 
#using the predict or forecast function.
#b) Plot the original and forecasted values. 
#The SARIMA model should effectively capture the seasonality and trend, 
#but model performance can be evaluated using metrics like RMSE or MAPE

###############

  
  #Question 5: Model Comparison
  #You decide to compare the SARIMA model with a simple ARIMA model:
  #a) Fit an ARIMA model without seasonality.
  #b) Compare the performance of the SARIMA and 
  #ARIMA models using appropriate evaluation metrics (AIC, BIC, RMSE).
  #c) Based on your findings, which model would you recommend and why?
  
  #a)
  # Load necessary library
library(forecast)

# Load necessary library
library(forecast)

# Assuming sales_ts is your time series object

# Step 1: Fit an ARIMA model without seasonality
arima_model <- Arima(sales_ts, order = c(1, 1, 1))  # Adjust order as necessary
arima_model
# Fit the SARIMA model

sarima_model <- Arima(sales_ts, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
sarima_model

# Step 2: Compare performance metrics
arima_aic <- AIC(arima_model)
sarima_aic <- AIC(sarima_model)

arima_bic <- BIC(arima_model)
sarima_bic <- BIC(sarima_model)

arima_rmse <- sqrt(mean(residuals(arima_model)^2))
sarima_rmse <- sqrt(mean(residuals(sarima_model)^2))

# Create a comparison table
comparison <- data.frame(
  Model = c("ARIMA", "SARIMA"),
  AIC = c(arima_aic, sarima_aic),
  BIC = c(arima_bic, sarima_bic),
  RMSE = c(arima_rmse, sarima_rmse)
)

print(comparison)

# Step 3: Recommendation based on the comparison
if (sarima_aic < arima_aic && sarima_bic < arima_bic && sarima_rmse < arima_rmse) {
  cat("Recommendation: The SARIMA model is preferred due to lower AIC, BIC, and RMSE.\n")
} else {
  cat("Recommendation: The ARIMA model is preferred due to lower AIC, BIC, and RMSE.\n")
}

#a) Fit an ARIMA model without seasonal terms using the same dataset.
#b) Compare the models using metrics like AIC 
#(Akaike Information Criterion), BIC (Bayesian Information Criterion), 
#and RMSE (Root Mean Squared Error). SARIMA typically performs better with seasonal data.
#c) SARIMA is usually recommended for datasets with strong seasonal components, 
#as it captures the seasonality more effectively than ARIMA.


#############
#Question 6: Holiday Adjustment in SARIMA
#Suppose the retail store experiences a surge in sales during certain holiday months, 
#which is not captured by the original SARIMA model.
#a) How would you modify the SARIMA model to account for the holiday effect?
#b) Refit the modified SARIMA model and compare the results with the previous model.

#a)
# Suppose you have a holiday variable (1 for holiday, 0 for non-holiday)
# Add holiday variable as external regressor
holiday_effect <- c(rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1)

# Fit SARIMA with external regressors
sarima_with_holiday <- auto.arima(sales_ts, xreg = holiday_effect, seasonal = TRUE)
summary(sarima_with_holiday)


#b)
# Compare the SARIMA with and without holiday adjustments using AIC, BIC, and RMSE
sarima_holiday_aic <- AIC(sarima_with_holiday)
sarima_holiday_bic <- BIC(sarima_with_holiday)
sarima_holiday_rmse <- sqrt(mean(residuals(sarima_with_holiday)^2))

print(paste("SARIMA with holiday AIC:", sarima_holiday_aic, "BIC:", sarima_holiday_bic, "RMSE:", sarima_holiday_rmse))
#a) Incorporate holiday effects as external regressors (exogenous variables) in the SARIMA model.
#b) After refitting the model with the holiday adjustment, 
#check whether the model's performance improves by comparing metrics like AIC, BIC, or RMSE.



