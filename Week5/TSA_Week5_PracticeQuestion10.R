# Consider the "AirPassengers" data from the R library and write the R program to forecast the
# monthly airline passenger numbers for the next 12 months.
# Clear all previous variables from the environment
rm(list=ls(all=TRUE))

# Load necessary libraries for time series analysis and forecasting
library(tseries)    # For stationarity tests like KPSS
library(forecast)   # For ARIMA, Holt-Winters, and forecasting
library(TTR)        # For moving average and other technical indicators (not used directly here)
library(lmtest)     # For various hypothesis tests (also not directly used here)

# Step 1: Load and plot the AirPassengers dataset
# Load the built-in AirPassengers dataset, which contains monthly airline passenger numbers from 1949-1960.
data("AirPassengers")
force(AirPassengers)  # Force loading of the data

# Visualizing the time series data
plot(AirPassengers, main="Monthly Airline Passengers Data", ylab="Number of Passengers", xlab="Time (Months)")

# Step 2: Time Series Decomposition
# Decompose the AirPassengers data into seasonal, trend, and random components
# We use a multiplicative decomposition because the seasonality seems to scale with the trend
AirPassengers_decompose <- decompose(AirPassengers, type="multiplicative")
AirPassengers_decompose  # Display the decomposition result

# Plot the decomposition to see the seasonal, trend, and residual components
plot(AirPassengers_decompose)

# Step 3: Checking for Stationarity
# Check if the series is stationary or non-stationary using ACF and PACF
# ACF (AutoCorrelation Function) helps us understand the autocorrelations at different lags
acf(AirPassengers, main="ACF plot for AirPassengers Data")

# PACF (Partial AutoCorrelation Function) shows the partial correlations, which help in model selection
pacf(AirPassengers, main="PACF plot for AirPassengers Data")

# Perform the KPSS (Kwiatkowski-Phillips-Schmidt-Shin) test for stationarity
# A p-value > 0.05 indicates that the series is stationary, while p-value < 0.05 indicates non-stationarity
kpss.test(AirPassengers)

# Step 4: Transformation of Data (Log Transformation)
# Often, we apply a transformation (like log) to stabilize variance in the data
# Here, we use a logarithmic transformation to deal with increasing variance over time
Air_Log <- log(AirPassengers)
plot(Air_Log, main="Log-transformed Airline Passengers Data")  # Visualize the log-transformed series

# Step 5: Model Identification
# We now try different models to forecast the data. We start with Holt-Winters models (Exponential Smoothing).

# **Double Exponential Smoothing (No Seasonal Component)**:
# Double exponential smoothing is used for data with trends but no seasonality
AirPassengers_Holtwinter2 <- HoltWinters(log(AirPassengers), gamma=F)  # Disable seasonal component
AirPassengers_Holtwinter2  # Display model details
AirPassengers_Holtwinter2$SSE  # Display the sum of squared errors (SSE) for the model
plot(AirPassengers_Holtwinter2)  # Plot the fitted model and the data

# **Triple Exponential Smoothing (Including Seasonal Component)**:
# Triple exponential smoothing accounts for both trends and seasonal variations in the data
AirPassengers_Holtwinter3 <- HoltWinters(log(AirPassengers))  # Enable seasonal component (gamma = TRUE by default)
AirPassengers_Holtwinter3  # Display model details
AirPassengers_Holtwinter3$SSE  # Display the sum of squared errors (SSE) for this model
plot(AirPassengers_Holtwinter3)  # Plot the fitted model with seasonal component

# Step 6: Model Estimation
# The Holt-Winters model has already been estimated above (in step 5). Here, we ensure that it is computed and ready to use.
AirPassengers_Holtwinter3  # Display the estimated model for triple exponential smoothing

# Step 7: Forecasting Using Triple Exponential Smoothing
# Use the fitted model to forecast the next 10 months (h = 10)
ForecastFinal <- forecast(AirPassengers_Holtwinter3, h=10)
ForecastFinal  # Display the forecast values for the next 10 months

# Plot the forecast for the next 10 months, including the original series and the forecasted values
plot(ForecastFinal, main="Forecasted Airline Passengers (Triple Exponential Smoothing)")

# Step 8: Comparison with Double Exponential Smoothing
# Forecast using the double exponential smoothing model (no seasonal component)
ForecastDummy <- forecast(AirPassengers_Holtwinter2, h=10)

# Plot the forecast from the double exponential smoothing model
plot(ForecastDummy, main="Forecasted Airline Passengers (Double Exponential Smoothing)")

