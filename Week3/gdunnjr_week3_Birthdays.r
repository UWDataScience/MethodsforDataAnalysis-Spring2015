# Gerald Dunn
# UWNetID: gdunnjr
# Assignment 3 Birthdays

# command to load outliers
# library("outliers", lib.loc="C:/Users/jdunn1/Documents/R/win-library/3.1")

# Clear the work space
rm(list=ls())

# Global setting for plots
par(mar=c(4,4,1,1))

# Clear Console:
cat("\014")

# set working dir
setwd("C:\\Users\\jdunn1\\Documents\\UWDataScience\\DS350\\Class3\\assignment")

# read the data
birthdays <- read.csv("birthDataDays.csv", header=TRUE, stringsAsFactors=FALSE)

#######################

# Find probability of baby being born 24 or more days early
born24DaysEarly = 280-24
prob24DaysEarly = (length(which(birthdays$First<=born24DaysEarly)) + length(which(birthdays$Second<=born24DaysEarly)))/(nrow(birthdays)*2)
prob24DaysEarly
# results: 0.04166667
# The probability of baby being born 24 or more days early is: 0.04166667

########################

# The data might be biased because it only contains pregnancy results for women who have had 2 children

########################

# Find probability of second baby being born 24 or more days early
prob24DaysEarlyKidTwo = length(which(birthdays$Second<=born24DaysEarly))/nrow(birthdays)
prob24DaysEarlyKidTwo
# results: 0.0375
# The probability of second baby being born 24 or more days early is: 0.0375

########################

# Find probability of first child born late and second child being born 24 or more days early
probFirstLateAndSecond24DaysEalry =  ( length(which(birthdays$First>280))/nrow(birthdays) * length(which(birthdays$Second<=born24DaysEarly)) )/nrow(birthdays) 
probFirstLateAndSecond24DaysEalry
# results: 0.01625
# The probability of first child born late and second child being born 24 or more days early is: 0.01625

vPregLengthFirstKid = birthdays$First
vPregLengthSecondKid = birthdays$Second
qqplot(vPregLengthFirstKid,vPregLengthSecondKid)
abline(0,1,col="red")
# The qq plot shows that the distributions are fairly similar.

#######################

# Is the length of a pregnancy for a first child independent of the length for the second child?

# Check probability of both first and second being born early or exactly on time
length(which(birthdays$First<=280 & birthdays$Second<=280))/nrow(birthdays) * 2
# Result: .66
# Check probability of both first and second being born late
length(which(birthdays$First>280 & birthdays$Second>280))/nrow(birthdays) * 2
# Result: .53

