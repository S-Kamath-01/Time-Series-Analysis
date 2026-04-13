# Practical 11
# Consider the "nottem" dataset available in R-library. Forecast the temperature values for the next two years.
#Install Required Packages if not already installed

# Remove all objects from the environment
rm(list=ls(all=TRUE))
# Install and load required package

install.packages("forecast")

library(forecast)

# Load dataset

data(nottem)

# Plot original time series

plot(nottem,
     
     main="Average Monthly Temperature at Nottingham",
     
     xlab="Year",
     
     ylab="Temperature (°F)")

# Fit ARIMA model automatically

model <- auto.arima(nottem)
summary(model)

# Forecast next 24 months (2 years)

forecast_values <- forecast(model, h=24)

# Print forecast values

print(forecast_values)

# Plot forecast

plot(forecast_values,
     
     main="Temperature Forecast for Next 2 Years")

# Extract only predicted temperatures

forecast_values$mean

