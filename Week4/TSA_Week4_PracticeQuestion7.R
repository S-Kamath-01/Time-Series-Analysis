# Consider the monthly volume of commercial bank real estate loans (in billions of dollars) dataset stored in a text file. Write the R program for the following:
# (f) Import data into the R environment.
# (g) Convert the data into a time series object.
# (h) Plot the data to identify the dominant component.
# (i) Check stationarity or non-stationarity using ACF/PACF plot.
# (j) Check stationarity or non-stationarity using the Augmented Dickey-Fuller (ADF) test.
# (k) If data is non-stationary, make it stationary using an appropriate operator.
# (l) Based on the dominating component, select the suitable model to fit the data and finalize the order of the model using ACF/PACF plots with model selection criteria such as AIC.
# (m) Fit the data using the selected model and estimate the parameters of the model.
# (n) Check the goodness of fit of the model. 

# Solutions
# Command to delete all the history
rm(list=ls(all=TRUE))

# Installing the required packages
install.packages(c("tseries","forecast","readr"))

# Loading all the required libraries
library(tseries)
library(forecast)
library(readr)


# Importing the file and converting it to a time series object
loan_ts <- as.ts(scan("bank_case.txt"))
loan_ts
# Plotting the time series data graph
plot(loan_ts,
     main = "Commercial Bank Real Estate Loans (Monthly)",
     ylab = "Loans in Billions of dollars",
     xlab = "Time",
     xaxt= "n",
     lwd=2)
axis(1,seq(0,70,5),seq(0,70,5))

# Checking stationarity via ACF/PACF ----
par(mfrow = c(1,2))
acf(loan_ts, main = "ACF of Commercial Bank RealEstate Loan Data",lag.max = 10,ylim=c(-1,1))
pacf(loan_ts, main = "PACF of CommercialBank RealEstate Loan Data",lag.max= 10,ylim=c(-1,1))
par(mfrow = c(1,1))


# Stationarity using Augmented Dickey–Fuller (ADF) test
adf_result <- adf.test(loan_ts)
print(adf_result)

# As per the results, we get to know that the data is non-stationary
# We proceed to use the difference  operator
# First differencing
diff1_ts <- diff(loan_ts, differences = 1)

# Plot differenced series
plot(diff1_ts,
     main = "First Differenced Series",
     ylab = "Differenced Loans",
     xlab = "Months",
     xaxt = "n")
axis(1,seq(0,70,5),seq(0,70,5))

# ACF & PACF after differencing
par(mfrow = c(1,2))
acf(diff1_ts, main = "ACF: First Difference",lag.max = 10,ylim=c(-1,1))
pacf(diff1_ts, main = "PACF: First Difference",lag.max = 10,ylim=c(-1,1))
par(mfrow = c(1,1))

# ADF test after differencing
adf.test(diff1_ts)

# Differencing again
diff2_ts <- diff(diff1_ts)

# Plot the second differenced series
plot(diff2_ts,
     main = "Second Differenced Series",
     ylab = "Differenced Loans",
     xlab = "Months",
     xaxt = "n")
axis(1,seq(0,70,5),seq(0,70,5))

# ACF & PACF after differencing
par(mfrow = c(1,2))
acf(diff2_ts, main = "ACF: Second Difference",lag.max = 10,ylim=c(-1,1))
pacf(diff2_ts, main = "PACF: Second Difference",lag.max = 10,ylim=c(-1,1))
par(mfrow = c(1,1))

# ADF test after differencing
adf.test(diff2_ts)

# Now the data is stationary

# The plot of this differenced data is horizontally aligned, which means trend has been removed from the data.
# From the statistical test we get that, the p value is less than 0.05, which means we can reject the null hypothesis and the data is stationary.
# The ACF plot cuts off after lag 2 and the PACF plot cuts off after lag 1 which also implies the data is stationary.
# Hence the data has been converted to a stationary data.
# Now, we fit a model to the data. Since trend is the dominant component, we shall use ARIMA(p,d,q) model to fit the data.
# Since we had to difference the data 2 times to make it stationary, the value of d = 2. From the ACF and PACF plots of the differenced data above, the possible values of p are 0, 1, 2 and of q are 0, 1 respectively.
# Among these values, we choose the best model with optimal parameters p, q based on the AIC value of each model.
# The model with the least AIC value is the best model.
# Here, we obtain that ARIMA(0,2,1) has the least AIC value, and hence is the best model for this data.


# Fitting the data using ARIMA(0,2,1)
bank_data_fit <- arima(loan_ts, order = c(0, 2, 1))

# Printing the estimated parameters and model summary
bank_data_fit

# Extract residuals
res <- bank_data_fit$residuals

# Quick peek
res

# Plot residuals over time
plot(res, main = "Residuals of Commercial Bank Real Estate Loan Data")

# Graphical check: histogram
hist(res, main = "Histogram of Residuals of Commercial Bank Loan Data")

# Statistical test: Shapiro–Wilk normality test
shapiro.test(res)

# Box–Pierce test for residual autocorrelation
Box.test(res, lag = 10, type = "Box-Pierce")

# Graphical checks: ACF/PACF of squared residuals
acf(res^2,  main = "ACF of Squared Residuals of Bank Loan Data")
pacf(res^2, main = "PACF of Squared Residuals of Bank Loan Data")

# White test for heteroscedasticity
white.test(res)

