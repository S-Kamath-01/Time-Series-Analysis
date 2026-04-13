# Consider the “AirPassengers” data from R library and write the R program for the following:
# (a) Convert the data into a time series object.
# (b) Plot the data to identify the dominant component.
# (c) Decompose the data to observe the dominating components more clearly.
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

# Decomposing the data
decomposded_air = decompose(ap)
plot(decomposded_air)

# Decomposing the data to observe components more clearly
decomp_mult <- decompose(ap, type = "multiplicative")
plot(decomp_mult,
     xlab = "Year",
     main = "Multiplicative Decomposition: AirPassengers")


# ACF/PACF to check stationarity vs non-stationarity
par(mfrow = c(1,2))
acf(ap, main = "ACF: AirPassengers")
pacf(ap, main = "PACF: AirPassengers")
par(mfrow = c(1,1))


# Stationarity using Augmented Dickey–Fuller (ADF) test
adf_result <- adf.test(ap)
print(adf_result)
