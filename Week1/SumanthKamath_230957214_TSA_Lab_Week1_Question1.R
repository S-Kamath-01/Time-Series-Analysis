#Practical 1
#For a given dataset of the US Population (in millions), perform the following tasks in the R environment:
#(a) Entered the data into an Excel file.
#(b) Import the data from the Excel file into the R working directory.
#(c) Call the suitable package from the R library for time series analysis.
#(d) Convert the dataset into a time series object and explain the nature of the data in terms of its sampling frequency.
#(e) Plot the data and identify the dominating component(s) in the data set.
#[Data: 3929214, 5308483, 7239881, 9638453, 12860702, 17063353, 23191876, 31443321,38558371, 50189209, 62979666, 76212168, 92228496, 106021537, 123202624, 132164569,151325798, 179323175, 203302031, 226542203, 248709873] 

#Command to delete all the history
rm(list=ls(all=TRUE))

#Solutions
install.packages("readxl")
install.packages("forecast")
#(a)
#The data is entered into excel file and file named as us_population

#(b)
library(readxl)
us_population <- read_excel("us_population.xlsx")
View(us_population)
data = us_population$US_Population_Millions
data

#(c)
library(stats)
library(forecast)

#(d)
us_pop <- data/1e6
pop_ts <- ts(us_pop, frequency = 1)
print(pop_ts)

#(e)
plot(pop_ts,
     main = "US Population Time Series",
     ylab = "Population (In Millions)",
     xlab = "Year",
     col = "blue",
     lwd = 2)

