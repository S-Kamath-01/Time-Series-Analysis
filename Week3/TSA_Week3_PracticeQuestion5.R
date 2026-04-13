# Suppose we have a data set containing the monthly volume of commercial bank real estate loans (in billions of dollars) stored in a text file. Write the R program for the following:
# (a) Import data into the R environment.
# (b) Convert the data into a time series object.
# (c) Plot the data to identify the dominant component.
# (d) Check stationarity or non-stationarity using ACF/PACF plot.
# (e) Check stationarity or non-stationarity using the Augmented Dickey-Fuller (ADF) test. 


# Solutions
# Command to delete all the history
rm(list=ls(all=TRUE))

# Installing the required packages
install.packages(c("tseries","forecast"))

# Loading all the required libraries
library(tseries)
library(forecast)

# Setting the working directory  for importing the file
setwd("D:/SumanthKamath_230957214_TSA/Week3")

# Importing the file
file_path <- "bank_case.txt"
raw_vec <- scan(file_path, what = numeric(), quiet = TRUE)

# Checking the number of observations
length(raw_vec)

# Quick peek of the data
head(raw_vec)

# Convert the data into time series object
loans_ts <- ts(raw_vec, frequency = 12, start = c(2000, 1))

# Plotting the time series data graph
plot(loans_ts,
     main = "Commercial Bank Real Estate Loans (Monthly)",
     ylab = "Billions of dollars",
     xlab = "Time")


# Seasonal plot and decomposition (if you want more diagnostics)
seasonplot(loans_ts, main = "Seasonal Plot", ylab = "Billions", xlab = "Month")
plot(decompose(loans_ts, type = "additive"))


# Checking stationarity via ACF/PACF ----
par(mfrow = c(1,2))
acf(loans_ts, main = "ACF: Original Series")
pacf(loans_ts, main = "PACF: Original Series")
par(mfrow = c(1,1))


# Stationarity using Augmented Dickey–Fuller (ADF) test
adf_result <- adf.test(loans_ts)
print(adf_result)

