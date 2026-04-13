#Practical 2
#The “Nottem” dataset is available in the R library and contains historical time series observations.
#Using this dataset, perform the following tasks:
#(a) Write the R command to load the library in which the “Nottem” dataset is available.
#(b) Write the R command to load the “Nottem” dataset into the R working environment.
#(c) Write the appropriate R command to view the description of the dataset and briefly explain the key characteristics of the data.
#(d) Explain the nature of the data in terms of its sampling frequency.
#(e) Plot the time series data and write your observations about the pattern of the data. 

#Solutions

#Command to delete all the history
rm(list=ls(all=TRUE))

#(a)
library(datasets)

#(b)
data("nottem")
nottem

#(c)
?nottem

#(d)
frequency(nottem)

#(e)
plot(nottem,
     main="Monthly Air Temperatures at Nottingham Castle",
     xlab= "Year",
     ylab= "Temperature(°F)",
     col= "blue",
     lwd=2)

