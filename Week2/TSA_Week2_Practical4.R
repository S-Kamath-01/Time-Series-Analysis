# Practical 4
# For a given dataset of the US Population (in millions), write the R code to:
# (a) Import the data as a time series object.
# (b) Identify the dominating component(s) in the data set.
# (c) Apply a square root transformation to the original data set.
# (d) Estimate the linear trend present in the data set.
# (e) Remove the estimated linear trend from the data set. 

# Command to delete all the history
rm(list=ls(all=TRUE))

# Loading Required Libraries
library(datasets)
library(stats)
library(readxl)

# Importing the excel file containing US Population data
us_population <- read_excel("D:/SumanthKamath_230957214_TSA/Week2/us_population.xlsx")
View(us_population)

# (a) Import the data as a time series object

# Data starts from 1970 and ends at 1990 (annual data)
us_pop_ts <- ts(us_population$Population,
                start = 1970,
                end = 1990,
                frequency = 1)

# Plot the time series
plot(us_pop_ts/1000000,
     main = "US Population Time Series",
     ylab = "Population (In Millions)",
     xlab = "Year",
     type = "o")


# (b) Identify the dominating component(s) in the data set




# (c) Apply a square root transformation to the original data set

us_pop_sqrt <- sqrt(us_pop_ts)

# Plot transformed data
plot(us_pop_sqrt,
     main = "Square Root Transformed US Population",
     ylab = "Sqrt(Population)",
     xlab = "Year",
     type="o")


# (d) Estimate the linear trend present in the data set

# Create time index
time_index <- time(us_pop_ts)

# Fit linear trend model
trend_model <- lm(us_pop_ts ~ time_index)

# Display model summary
summary(trend_model)

# Estimated trend values
trend_estimate <- ts(trend_model$fitted.values, start=1970, frequency = 1)

# Plot original data with trend line
plot(us_pop_ts,
     main = "US Population with Linear Trend",
     ylab = "Population",
     xlab = "Year",
     type = "o")
lines(trend_estimate, col = "red", lwd = 2)


# (e) Remove the estimated linear trend from the data set

# Detrended series
us_pop_detrended <- us_pop_ts - trend_estimate

# Plot detrended data
plot(us_pop_detrended,
     main = "Detrended US Population Time Series",
     ylab = "Population (Detrended)",
     xlab = "Year",
     type = "o")
abline(h = 0, col = "blue", lty = 2)

