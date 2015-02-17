# Week 5 Assignment: Exploring hours
# Submitted by: Werner Colangelo (colangw)
# Date: February 17, 2015

# Clear any environment variables and clear the console as well.
rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

setwd("C:/Users/werner/Dropbox/Programming/UW_hours_science/Lectures/2_Methods/Week5/Homework")

# Q1

hours <- read.csv("hour.csv")
str(hours)
names(hours)
hours$dteday <- as.character(hours$dteday)
hours$holiday <- as.factor(hours$holiday)
hours$weekday <- as.factor(hours$weekday)
hours$workingday <- as.factor(hours$workingday)
hours$weathersit <- as.factor(hours$weathersit)

hours$dayofweek <- strptime(hours$dteday, format = "%Y-%m-%d")
#hours$dayofweek <- as.factor(as.POSIXlt(hours$dayofweek)$wday)
hours$dayofweek <- as.POSIXlt(hours$dayofweek)$wday
# Q2

contingency <- matrix(data=NA,ncol=2,nrow=7)
for(i in 1:7){
  contingency[i, 1] <- sum(hours[hours$dayofweek==(i-1),]$casual)
  contingency[i, 2] <- sum(hours[hours$dayofweek==(i-1),]$registered)
}
colnames(contingency) <- c("Casual","Registered")
rownames(contingency) <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
contingency

chisq.test(contingency)

# The chi-squared test does suggest that there is significant dependence between the each day of the week and the frequencies of riders of each category.

# Q3

ks.test(hours[hours$dayofweek > 0 & hours$dayofweek < 6,]$registered, hours[hours$dayofweek == 0 | hours$dayofweek == 6,]$registered)

# p < 2.2e-16, so we can reject the null hypothesis and conclude that the distributions are different.

# Q4

Weekdays <- hours[hours$dayofweek > 0 & hours$dayofweek < 6,]$cnt
Weekends <- hours[hours$dayofweek == 0 | hours$dayofweek == 6,]$cnt

summary(Weekdays)
summary(Weekends)

t.test(Weekdays, Weekends, alternative="greater")

# p = 0.003923 so we can reject the null hypothesis and conclude that more bikes are rented on weekdays than weekends.

# Q5

# The t-test  compares the means of distributions. We are comparing average (i.e. the mean) of ridership on weekends versus weekedays so the t-test is a correct choice.
