# Week 6 Assignment: Exploring Data
# Submitted by: Werner Colangelo (colangw)
# Date: February 24, 2015

# This assignment uses the abalone dataset from the UCI machine learning data repository. Each row in the dataset describes a different abalone, including its sex, linear dimensions, and weights. The last column contains the number of rings in an abalone’s shell. This is a proxy for the abalone’s age, just as tree rings tell us how old a tree is. The problem we face is coming up with an easy to apply model that predicts the number of rings (counting them is laborious and pretty unpleasant for the abalone, which gets sawed in half).

setwd("C:/Users/werner/Dropbox/Programming/UW_data_science/Lectures/2_Methods/Week6")

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# set the working directory

# remote file url
fileurl = "abalone.data"

# read the file
abalone <- read.csv(fileurl, header = FALSE, stringsAsFactors = TRUE)

# add names from the http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names file as follows:
# Name    Data Type	Meas.	Description
# ----		---------	-----	-----------
# Sex		nominal			M, F, and I (infant)
# Length		continuous	mm	Longest shell measurement
# Diameter	continuous	mm	perpendicular to length
# Height		continuous	mm	with meat in shell
# Whole weight	continuous	grams	whole abalone
# Shucked weight	continuous	grams	weight of meat
# Viscera weight	continuous	grams	gut weight (after bleeding)
# Shell weight	continuous	grams	after being dried
# Rings		integer			+1.5 gives the age in years
names(abalone) <- c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight", "Viscera_weight", "Shell_weight", "Rings")

# Q1.  Plot the number of rings as a function of length.

plot(abalone$Length, abalone$Rings)

# Q2.	Fit a linear model to this data (rings  = a*length + b) using R’s lm command. Examine the output of the summary table for the fit. Is length a significant factor?

fit1 <- lm(abalone$Rings ~ abalone$Length)
summary(fit1)

# It is a significant factor: R-squared = 0.3098

# Q3.	There are three sexes of abalone: male, female, and immature. Filter the data so that only the immature abalone remain. Fit the same model to this data (rings = a*length + b). Examine the output of summary: is this model a better or worse than the model fit to all of the data?

abalone.i <- subset(abalone, abalone$Sex == "I")
fit2 <- lm(abalone.i$Rings ~ abalone.i$Length)
summary(fit2)

# It is a better fit: R-squared = 0.4702


#Q4.	Still working with the immature abalone only, add Height and Diameter to the model (rings = a*length + b*height + etc.). Examine the output of summary: what are the significant factors in this new model? Compare the result to the “length only” model and explain why the two results on consistent with each other.

fit3 <- lm(abalone.i$Rings ~ abalone.i$Length + abalone.i$Height + abalone.i$Diameter)
summary(fit3)

# Height and diameter (in order) are the significant factors, not so much Length
# This model is a much better fit (R-squared =  0.5263) than just the Length since Length by itself only has an R-squared of 0.3098

# Q5.	Still working with the immature abalone only, add all of the factors to the model (except Sex: since we only have immature abalone, this value is the same for every data point) (rings = a*length + b*height + etc.). Examine the residuals and summarize your observations. Use graphical methods (histogram, qqplot, etc.) and also plot the residuals as a function of the number of rings.

fit4 <- lm(abalone.i$Rings ~ abalone.i$Length + abalone.i$Height + abalone.i$Diameter + abalone.i$Whole_weight + abalone.i$Shucked_weight + abalone.i$Viscera_weight + abalone.i$Shell_weight)
summary(fit4)
par(mfrow = c(2, 2))
plot(fit4)
plot(abalone.i$Rings, fit4$residuals)
