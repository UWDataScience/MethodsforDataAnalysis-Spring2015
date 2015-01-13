#prepare system
setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week1")

#loading data
library(RCurl)
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
data <- getURL(url)
data <- read.csv(text = data)
names(data) <- c("sex","length","diameter","height","whole_weight",
                 "shucked_weight","visera_weight","shell_weight","rings")

data$age <-  data$rings  +	1.5	
##################################
#1. Abalone Data
##################################

# 1.a
# Use the command “quantile” to find the deciles (10 groups) for height from 
# the complete data set. 
decible.breaks <- quantile(data$height,prob=seq(0, 1, length=11))

# 1.b 
# b. Use the command “cut” to assign each height value to the corresponding decile.
height.decibles <- as.numeric(cut(x=data$height, breaks=decible.breaks))

# 1.c
# Now create a table of age vs. height decile. Examine the table and 
# describe what you observe. 
table(data$age,height.decibles)
# There seems to be a positive correlation between age and height. 
# The older the abolone the larger its height. 

# d.i
# Use the commands “unique” and “sort” to find the unique 
# values of Age and store the values in ascending order to a 
# variable named “ua”.
ua <- sort(unique(data$age))

# d.ii
# Use the command “sapply” to apply a function to each value 
# in “ua”. The function should return the mean whole weight 
# of all abalone of a given age.
getMean <- function(single.age){
  mean(with(data, whole_weight[age == single.age]))
}
avg.weights.by.age <- sapply(X=ua,FUN=getMean)

# d.iii
# Finally, use the “plot” command to plot mean weight vs. age. 
# Describe the relationship revealed by the plot. Include an explanation 
# for the behavior seen in the abalone of the 25-30 year age group.
plot(avg.weights.by.age, ua, xlab="Mean Weights", ylab="Age")
abline(h = 25,col="red")
abline(h = 30, col="red")
# There seems to be a great deal of variance in in this subset, and 
# they also seem to be abnormally large, as if there was an odd growth-spurt
# after 25. 



####################
#2. Weather Data
####################

#clear up old env
rm(list=ls())
cat("\014")

####ETL
#let's use DT because of the size
library(data.table) 
#a function for cleaning up names
clean_names <- function (name) {  
  name <- tolower(name)
  name <- gsub(pattern = "[^[:alpha:]]", replacement = " ",x = name)
  name <- strsplit(x = name, split = " ")[[1]][1]
  return (name)
}

#read data with
data <- fread("weather_data_2000_2014.csv")
setnames(x=data,
         old=names(data),
         new=sapply(names(data), FUN=clean_names, USE.NAMES=F))

# 2.a. 
# Using summary statistics, histograms, boxplots, or other means identify 
#and describe at least one data quality issue in the dataset.

# Checking temperature
summary(data[, temp])
# Clearly the min and max temparatures (-100000.00 and 9596.00) were measured 
# incorrectly
boxplot(data[, temp])
# The box-plot shows how odd these measurments are, note all the extreme
# outliers make the quartile separations and the mean (the box) invisible. 
hist(data[, temp])
# The similar, extremely left-skewed image also appears in the histogram. 

# b. 
# Filter the data to remove the questionable data you identified in part 
# a. How much of the data is affected?

# Getting length before transformation
length.before <- nrow(data)
# Applying transformation
cleaned.data <- data[temp > -50 & temp < 120, ]
# Getting length after transformation
length.after <- nrow(cleaned.data)
# Calculating instances lost
instances.lost <- length.before - length.after
percentage.lost <- instances.lost / length.before
# 943 Instances were affected by the transformation, 
# which is .013% of the data

# Now the summary, boxplot and histogram are more descriptive
summary(cleaned.data[, temp])
boxplot(cleaned.data[, temp]) 
# Actually the min temp shows that it could probably be higher
hist(cleaned.data[, temp])

# c. 
# Look for and describe a monthly trend in the data.
# Get variables that we can use to look for trend
variables <- names(data)[5:length(names(data))]
# Init data frame
monthly.data <- data.frame(months=1:12)
# Get make data frame with monthly medians of each 
for (i in 1:length(variables)) {
  monthly.data <- cbind.data.frame(
    monthly.data,
    data.frame(tapply(X=data[[variables[i]]], INDEX=data[, month], FUN=median))
  )
}
names(monthly.data) <- c("month", variables)

#make plots to look for trends
par(mfrow=c(1,1))
for(var in names(monthly.data)[2:8]){
  plot(monthly.data[[var]], xlab="Month", ylab=paste("Median",var, sep=" "))
}
# It seems that Rhum, Temp, Wind, Speed, Gust, Rain, Radiation seem to all have
# monthly trends. I used median because it is not as affected by outliers.


# Contratados.org
# predatory education loans