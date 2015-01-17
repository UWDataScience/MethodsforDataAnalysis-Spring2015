# Gerald Dunn
# UWNetID: gdunnjr
# Assignment 1 - Weather data

# Clear the work space
rm(list=ls())


# Global setting for plots
par(mar=c(4,4,1,1))

# Clear Console:
cat("\014")

# set working dir
setwd("C:\\Users\\jdunn1\\Documents\\UWDataScience\\DS350\\Class1\\assignment")

# read the weather data
uwWeather <- read.csv("weather_data_2000_2014.csv", header=TRUE, stringsAsFactors=FALSE)
summary(uwWeather)

# a.  Using summary statistics, histograms, boxplots, or other means identify and describe at least one  U data quality issue in the dataset.
# There is a temperature value of 9586 F in the data.
# There are missing valuses fr Pres.mbar

# b.  Filter the data to remove the questionable data you identified in part a. How much of the data is affected? 

nrow(uwWeather)
# 6992901 rows in the data frame prior to removing NA's

# remove rows with NA's
uwWeatherCleaned = na.omit(uwWeather)

# see how many rows after cleanup
nrow(uwWeatherCleaned)
# 1138910 in the data frame after removing the NA's

# 5853991 rows are affected (6992901 - 1138910)

# c. Look for and describe a monthly trend in the data.
#aggregate(uwWeatherCleaned$Temp..F, by=list(uwWeatherCleaned$Month), FUN=mean)
#Group.1        x
#1        1 40.70135
#2        2 44.35255
#3        3 45.76962
#4        4 50.79711
#5        5 56.18995
#6        6 60.13399
#7        7 64.81174
#8        8 67.07856
#9        9 62.24395
#10      10 52.59460
#11      11 46.50633
#12      12 41.36965

# One monthly trend in the data is that the mean temperature rises from Jan - Aug, reaching its 
# peak in August, then drops from Aug - Dec.