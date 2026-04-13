# Practical 3
# Consider the following datasets from the R library and write R code to decompose the time-series data into its trend, seasonal, and residual components. Furthermore, identify the dominating component(s) in the dataset:
# (a) Nottem data
# (b) AirPassengers data 

# Command to delete all the history
rm(list=ls(all=TRUE))

# Loading Required Libraries
library(datasets)
library(stats)

# (a)Nottem Data

# Loading the nottem dataset
data(nottem)

# Plot the time series
plot(nottem,
     main = "Nottem Monthly Average Temperatures",
     ylab = "Temperature",
     xlab = "Year")

# Classical additive decomposition
nottem_decomp <- decompose(nottem, type = "additive")

# Plot the decomposed components
plot(nottem_decomp)

# (b) AirPassengers data
data("AirPassengers")

# Plot the time series
plot(AirPassengers,
     main = "AirPassengers Data",
     ylab = "Number of Passengers",
     xlab = "Year")

# Classical multiplicative decomposition
ap_decomp <- decompose(AirPassengers, type = "multiplicative")

# Plot the decomposed components
plot(ap_decomp)
