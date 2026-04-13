# Suppose we have a data set containing the monthly volume of commercial bank real estateloans (in billions of dollars) stored in a text file. Write the R code to forecast the volume of
# commercial loans for the next 20 months.
# (Remark: Data set given in text file.)
# Remove all objects from the environment to start fresh
rm(list=ls(all=TRUE))

# Load necessary libraries for time series analysis and forecasting
library(tseries)  # For stationarity test (ADF)
library(forecast) # For ARIMA model and forecasting
library(readr)    # For reading CSV files

# Step 1: Load the data
# Read the data from the bank_case.txt file
bank_case <- read_csv("D:/SumanthKamath_Labs/SumanthKamath_230957214_TSA/Week3/bank_case.txt", col_names = FALSE)
bank_case <- bank_case$X1  # Extracting the data column (assuming it's the first column)
bank_case  # Display the first few values of the data

# Step 2: Convert the data into a time series object
# Convert the raw data into a time series object (assuming monthly data)
bank_data_ts <- as.ts(bank_case)
bank_data_ts  # Display the time series object

# Step 3: Plot the data
# Visualizing the data to see the trend and any possible seasonal patterns
plot(bank_data_ts,
     main="Commercial Bank Real Estate Loan Data",
     xlab="Months",
     ylab="Loans in Billions in Dollar",
     lwd=2)
axis(1, seq(0,70,5), seq(0,70,5))  # Custom x-axis for months

# Step 4: Check if the series is stationary
# ACF (AutoCorrelation Function) plot helps us understand the serial correlation in the data
acf(bank_data_ts,
    main="ACF Plot of Commercial Bank Real Estate Loan Data",
    lag.max=10, ylim=c(-1,1))

# PACF (Partial AutoCorrelation Function) plot helps to check for partial correlations
pacf(bank_data_ts,
     main="PACF Plot of Commercial Bank Real Estate Loan Data",
     lag.max=10, ylim=c(-1,1))

# Perform ADF (Augmented Dickey-Fuller) test for stationarity
adf.test(bank_data_ts)  # p-value < 0.05 suggests that the data is stationary

# Step 5: Conversion of non-stationary data to stationary (Differencing)
# If the data is not stationary, we can make it stationary by differencing
# First, take the first difference
bank_data_ts_d1 <- diff(bank_data_ts)
adf.test(bank_data_ts_d1)  # Re-test for stationarity after differencing

# If the first difference is still non-stationary, we take the second difference
bank_data_ts_d2 <- diff(bank_data_ts_d1)
adf.test(bank_data_ts_d2)  # Check again for stationarity after second differencing

# Plot the ACF and PACF for the differenced data (after second differencing)
acf(bank_data_ts_d2,
    main="ACF Plot of Differenced Data (2nd Difference)",
    lag.max=10, ylim=c(-1,1))

pacf(bank_data_ts_d2,
     main="PACF Plot of Differenced Data (2nd Difference)",
     lag.max=10, ylim=c(-1,1))

# Step 6: Model Identification (Choose the ARIMA model order)
# We now try different ARIMA models with different combinations of p, d, q (AR, differencing, MA)
# The AIC (Akaike Information Criterion) will help us select the best model with the lowest AIC

# Try different ARIMA model orders and check AIC values for comparison
bank_fit1 <- arima(bank_data_ts, order=c(0,2,0))
bank_fit1$aic  # AIC value

bank_fit2 <- arima(bank_data_ts, order=c(1,2,1))
bank_fit2$aic  # AIC value

bank_fit3 <- arima(bank_data_ts, order=c(0,2,2))
bank_fit3$aic  # AIC value

bank_fit4 <- arima(bank_data_ts, order=c(1,2,2))
bank_fit4$aic  # AIC value

bank_fit5 <- arima(bank_data_ts, order=c(1,2,0))
bank_fit5$aic  # AIC value

bank_fit6 <- arima(bank_data_ts, order=c(0,2,1))
bank_fit6$aic  # AIC value

# **Interpretation of AIC:**
# We choose the model with the minimum AIC because it balances model fit and complexity.
# In this case, the model with order c(0,2,1) has the lowest AIC, so it is selected.

# Step 7: Model Estimation
# Fit the ARIMA(0,2,1) model and estimate its parameters
bank_fit_Est <- arima(bank_data_ts, order=c(0,2,1), method="ML")
bank_fit_Est  # Display the model summary

# Step 8: Diagnostic Checking of the Model
# Checking the residuals to ensure that the model assumptions are satisfied

res <- bank_fit_Est$residuals  # Extract the residuals

# Plot the residuals
plot(res,
     main="Residuals of Commercial Bank Real Estate Loan Data",
     type='l')
abline(h=0)  # Add a horizontal line at zero

# 1. Normality Check (Shapiro-Wilk Test)
shapiro.test(res)  # p-value > 0.05 suggests normality

# QQ Plot for normality check
qqnorm(res)
qqline(res, col="red")

# 2. Serial Autocorrelation Check (Box-Pierce Test)
Box.test(res, lag=10, type="Box-Pierce")  # p-value > 0.05 suggests no significant autocorrelation

# Plot ACF and PACF of residuals to ensure no autocorrelation is left
acf(res)
pacf(res)

# 3. Heteroscedasticity Check (White's Test for heteroscedasticity)
white.test(res)  # Test for constant variance in residuals

# Plot ACF and PACF of squared residuals to detect heteroscedasticity
acf(res^2)
pacf(res^2)

# Step 9: Forecasting
# Use the ARIMA(0,2,1) model to forecast the next 20 months
bank_fit_Forecast <- forecast(bank_fit_Est, h=20)
bank_fit_Forecast  # Display forecast results

# Plot the forecasted values
plot(bank_fit_Forecast,
     main="Forecast of Commercial Bank Real Estate Loan Data for Next 20 Months",
     xlab="Months",
     ylab="Loans in Billions in Dollar",
     lwd=2)
axis(1, seq(0,90,5), seq(0,90,5))  # Custom x-axis for months

