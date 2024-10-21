






















#2.2.R
#Q1 
#AR(1) Process
## Xt=0.5Xt−1+ϵt
#where ϵt is white noise with mean 0 and variance 1.
#1)(a) Simulate 100 observations from this AR(1) process.
#(b) Plot the time series.
#(c) Estimate the AR(1) parameter ϕ using a suitable statistical method
#2)plot acf  pacf of the series
#ii) Discuss the behaviour of acf and pacf For AR(1) process
#3)Fit AR(1)or AR(2) model to the data
#ii) forecast the next 10 observations and plot the forecast

#1a)
# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 100          # Number of observations
phi <- 0.5       # AR(1) parameter
sigma <- 1       # Standard deviation of white noise

# Initialize the series
X <- numeric(n)
epsilon <- rnorm(n, mean = 0, sd = sigma)  # Generate white noise

# Simulate AR(1) process
for (t in 2:n) {
  X[t] <- phi * X[t - 1] + epsilon[t]  # AR(1) equation
}

# Output the first few observations
head(X)

#1b)

# Plot the AR(1) time series
plot(X, type = "l", main = "Simulated AR(1) Process", xlab = "Time", ylab = "X_t", col = "blue")


#1c)
# Estimate AR(1) parameter using ARIMA
ar_model <- arima(X, order = c(1, 0, 0))
summary(ar_model)

#2)# Plot ACF and PACF
par(mfrow = c(1, 2))  # Set up the plotting area
acf(X, main = "ACF of AR(1) Process")
pacf(X, main = "PACF of AR(1) Process")

#3 1)
# Fit AR(2) model
ar_model_2 <- arima(X, order = c(2, 0, 0))
summary(ar_model_2)

#2)
# Forecast the next 10 observations using the AR(1) model
forecast_values <- forecast::forecast(ar_model, h = 10)

# Plot the original series and the forecast
plot(forecast_values, main = "Forecast of AR(1) Process", xlab = "Time", ylab = "X_t")
lines(X, col = "blue")  # Original series in blue

################
#Q2) MA(1) Process
#Consider an MA(1) Process:
# Xt = εt + 0.5εt-1
#where εt is white noise with mean 0 and variance 1.

#(a) Simulate 100 observations from this MA(1) process 
#(b) Plot the time series
#(c) Estimate the MA(1) parameter θ using a suitable statistical method
#2)(ii)Plot ACF and PACF of the series
#(ii) Discuss the behavior of ACF and PACF for MA(1) process
#3)#(i)Fit MA(1) or MA(2) model to the data
#(ii) Forecast the next 10 observations and plot the forecast
############
# Set seed for reproducibility

#1a)
set.seed(123)

# Parameters
n <- 100          # Number of observations
theta <- 0.5     # MA(1) parameter

# Initialize the series
X_ma <- numeric(n)
epsilon_ma <- rnorm(n, mean = 0, sd = 1)  # Generate white noise

# Simulate MA(1) process
for (t in 2:n) {
  X_ma[t] <- epsilon_ma[t] + theta * epsilon_ma[t - 1]  # MA(1) equation
}

# Output the first few observations
head(X_ma)


#1b)# Plot the MA(1) time series
plot(X_ma, type = "l", main = "Simulated MA(1) Process", xlab = "Time", ylab = "X_t", col = "red")

#1c)
# Estimate MA(1) parameter using ARIMA
ma_model <- arima(X_ma, order = c(0, 0, 1))
summary(ma_model)


#2)
# Plot ACF and PACF
par(mfrow = c(1, 2))  # Set up the plotting area
acf(X_ma, main = "ACF of MA(1) Process")
pacf(X_ma, main = "PACF of MA(1) Process")

#3) 1)
# Fit MA(2) model
ma_model_2 <- arima(X_ma, order = c(0, 0, 2))
summary(ma_model_2)


#2)

# Forecast the next 10 observations using the MA(1) model
forecast_values_ma <- forecast::forecast(ma_model, h = 10)
par(mfrow = c(1, 1))
# Plot the original series and the forecast
plot(forecast_values_ma, main = "Forecast of MA(1) Process", xlab = "Time", ylab = "X_t")
lines(X_ma, col = "red")  # Original series in red

