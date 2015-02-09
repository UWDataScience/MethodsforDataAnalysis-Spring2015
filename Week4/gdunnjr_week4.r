# Gerald Dunn
# UWNetID: gdunnjr
# Assignment 4 

# Clear the work space
rm(list=ls())

# Global setting for plots
par(mar=c(4,4,1,1))

# Clear Console:
cat("\014")

# set working dir
setwd("C:\\Users\\jdunn1\\Documents\\UWDataScience\\DS350\\Class4\\assignment")

# Question 1 
myPredictions = c(44/67,42/67,47/67,43/67,46/67)

# Hypothesis test
# Null Hypothesis is that mean of my predictions = .64
# Altertnative Hypothosesi is that the mean is greater than .64
t.test(myPredictions,alternative="greater", mu=0.64)

# Based on the output below, the p value is low, so there is moderately strong evidence to support 
# the alternaive hypothesis (The mean of predictions are higher and therefore more accurate) 
#
# One Sample t-test
#
# data:  myPredictions
# t = 1.6391, df = 4, p-value = 0.08827
# alternative hypothesis: true mean is greater than 0.64
# 95 percent confidence interval:
#  0.6331792       Inf
# sample estimates:
#   mean of x 
# 0.6626866 

x = c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
t.test(x, alternative="greater", mu=0.3)

# Question 2 - determine whether the mean due date of a second child is significantly greater than
# that of a first in mothers with two or more children

# read the data
birthdays <- read.csv("birthDataDays.csv", header=TRUE, stringsAsFactors=FALSE)

# do the t.test
t.test(birthdays$First,birthdays$Second)

# For this test, the null hypothesis is that there is no difference in the due dates in the two data sets
#
# The p value = .30 meaning that that the probability that there is no difference in the due dates is 30%.
# This is greater than the 5% standard therefore, we can accept the null hypothesis and
# conclude that there is no difference in the due dates.
#
# the results shown below
# Welch Two Sample t-test
# 
# data:  birthdays$First and birthdays$Second
# t = -1.0298, df = 952.693, p-value = 0.3033
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.8523212  0.5773212
# sample estimates:
#   mean of x mean of y 
# 277.3938  278.0312 
#
################################################################################
# Question 3 - Use a chi-squared test to see if the favored team wins any more or 
#  less often than would be expected by a random guess when the spread is (strictly) 
#  less than 3 points. 

# Read in data obtained from link

# read the data
resultsVsSpread <- read.csv("WinsVsSpread.csv", header=TRUE, stringsAsFactors=FALSE)


table(resultsVsSpread$Spread.3Result)
chisq.test(table(resultsVsSpread$Spread.3Result))

# read in the result and add a column indicating these results are from the Spread < 3 prediciton
results = resultsVsSpread$Spread.3Result
resultsVsSpread$spreadpred = "SpreadLessThan3"

# create a vector with random results and a vector to indicate these are randdom 
vSpreadRandomResult = sample(0:1,nrow(resultsVsSpread),TRUE)
vSpreadRandomResult[vSpreadRandomResult==1] = "FavWins"
vSpreadRandomResult[vSpreadRandomResult==0] = "FavLoses"
vRandomResultspreadpred = sample(1:1,nrow(resultsVsSpread),TRUE)
vRandomResultspreadpred[vRandomResultspreadpred==1] = "SpreadRandom"

# Append the actual results from the data set to the vectors of random results
vResults = c(resultsVsSpread$Spread.3Result ,vSpreadRandomResult)
vResultPred = c(resultsVsSpread$spreadpred ,vRandomResultspreadpred)

# create the table 
x2 = table(vResults,vResultPred)
# this is what the table looks like
#           vResultPred
#vResults   SpreadLessThan3 SpreadRandom
#FavLoses              27           31
#FavWins               36           32

# run the test
chisq.test(x2)

# results - 59% probability to support the null hypothesis that the random spread results and the actual spread < 3 points results
#   are independent of each other. Meaning that the spread < 3 points prediction are not random and are more accurate.
#
# Pearson's Chi-squared test with Yates' continuity correction
#
# data:  x2
# X-squared = 0.2875, df = 1, p-value = 0.5918



