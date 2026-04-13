# Consider the “AirPassengers” data from R library and write the R program for the following:
# (f) Convert the data into a time series object.
# (g) Plot the data to identify the dominant component.
# (h) Decompose the data to observe the dominating components more clearly.
# (i) Check stationarity or non-stationarity using ACF/PACF plot.
# (j) Check stationarity or non-stationarity using the KPSS test.
# (k) If data is non-stationary, make it stationary using an appropriate operator.
# (l) Based on the dominating component, select the suitable technique to fit the data.
# (m) Fit the data using the selected model and estimate the parameters of the model.
# (n) Check the goodness of fit of the model. 

# Solutions

# Command to delete all the history
rm(list=ls(all=TRUE))

# Installing the required packages
install.packages(c("tseries","forecast"))

# Loading all the required libraries
library(tseries)
library(forecast)


# Converting the data into a time series object
data("AirPassengers")
ap <- as.ts(AirPassengers)

# Checking the frequency of the dataset
frequency(ap)


# Plotting the data to identify the dominant component
plot(ap,
     main = "AirPassengers (Monthly, 1949–1960)",
     ylab = "Passengers (in thousands)",
     xlab = "Year")

# Decomposing the data to observe components more clearly
decomp_mult <- decompose(ap, type = "multiplicative")
plot(decomp_mult)


# ACF/PACF to check stationarity vs non-stationarity
par(mfrow = c(1,2))
acf(ap, main = "ACF: AirPassengers")
pacf(ap, main = "PACF: AirPassengers")
par(mfrow = c(1,1))

# KPSS test on original series
kpss.test(ap)

# Log Transform (Stabilize variance)
log_ap <- log(ap)
plot(log_ap, main="Log Transformed Series")

# First Differencing (Remove Trend)
diff1_ap <- diff(log_ap, differences = 1)

# Seasonal Differencing (Remove Seasonality)
diff_final <- diff(diff1_ap, lag = 12)

plot(diff_final, main="Log + First + Seasonal Differenced Series")

acf(diff_final)
pacf(diff_final)

# KPSS again
kpss.test(diff_final)


# The KPSS test is now applied on the log + first differenced + seasonal differenced data.
# Null Hypothesis (H0): The series is stationary.
# Alternative Hypothesis (H1): The series is non-stationary.
# If p-value > 0.05, we fail to reject H0 and conclude the series is stationary.
# This confirms that trend (d=1) and seasonality (D=1) have been successfully removed.


# (l) Model Selection using SARIMA

model <- auto.arima(log_ap,
                    seasonal = TRUE,
                    stepwise = FALSE,
                    approximation = FALSE)
# auto.arima() automatically selects the best ARIMA/SARIMA model.
# seasonal = TRUE allows seasonal components (since data is monthly).
# stepwise = FALSE performs a more thorough search.
# approximation = FALSE ensures exact likelihood estimation.
# The model is selected based on minimum AIC value.
# For AirPassengers, the usual selected model is ARIMA(0,1,1)(0,1,1)[12].

# (m) Model Summary and Parameter Estimation
summary(model)

# Goodness of Fit
checkresiduals(model)

# checkresiduals() performs:
# 1. Residual time plot → should fluctuate randomly around zero.
# 2. ACF of residuals → no significant spikes should remain.
# 3. Ljung-Box test → checks if residuals are white noise.
# If p-value > 0.05, residuals are independent and model is adequate.


# Manual Ljung-Box test
Box.test(residuals(model),
         lag = 20,
         type = "Ljung-Box")

# Null Hypothesis (H0): Residuals are independently distributed (white noise).
# If p-value > 0.05 → no autocorrelation remains → model fits well.
# If p-value < 0.05 → residual autocorrelation exists → model needs improvement.


# Normality check of residuals (optional but recommended)

hist(residuals(model), breaks = 20,
     main = "Histogram of Residuals")

# Histogram checks whether residuals are approximately normally distributed.

qqnorm(residuals(model))
qqline(residuals(model))

# Q-Q plot compares residual distribution with normal distribution.
# Points close to the straight line indicate approximate normality.
# Normal residuals are desirable for valid statistical inference.
