# Week 1 Assignment: Exploring Data
# Submitted by: Werner Colangelo (colangw)
# Date: January 20, 2015


# 1) Abalone Data
# Loading and preprocessing the data
# First, clear any environment variables and clear the console as well.
rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# set the working directory
setwd("C:/Users/werner/Dropbox/Programming/UW_data_science/Lectures/2_Methods/Week1")

# remote file url
fileurl = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"

# read the remote file
abalone <- read.csv(fileurl, header = FALSE, stringsAsFactors = TRUE)

# add names from the http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names file as follows:
# Name  	Data Type	Meas.	Description
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

# add a new columns for age:
abalone$Age <- abalone$Rings + 1.5

# 1.a. Use the command “quantile” to find the deciles (10 groups) for height from the complete data set. Hint: you may find the command “seq” helpful.
height.deciles <- quantile(abalone$Height,probs=seq(0, 1, length=11))

# 1.b. Use the command “cut” to assign each height value to the corresponding decile (e.g., the smallest values are assigned to the first decile and get mapped to the value, 1). Hint: use “as.numeric” to get integer values instead of ranges.
abalone.height.deciles <- as.numeric(cut(x=abalone$Height, breaks=height.deciles))

# 1.c. Now create a table of age vs. height decile. Examine the table and describe what you observe.
table(abalone$Age, abalone.height.deciles)

# Generally, there seems to be a positive correlation between the abalone age and height, where the older abalone have greater heights than the younger ones.

# 1.d. Another way to aggregate the data is averaging. Let’s compute the average whole weight of abalone as a function of age and plot the relationship.

# 1.d.i.	Use the commands “unique” and “sort” to find the unique values of Age and store the values in ascending order to a variable named “ua”.
ua <- sort(unique(abalone$Age))

# 1.d.ii. Use the command “sapply” to apply a function to each value in “ua”. The function should return the mean whole weight of all abalone of a given age. Hint: type “help(‘function’)” to find out more about user defined functions. The quotes inside the parentheses are important.
mean.whole.weight <- function(given.age){
  mean(with(abalone, Whole_weight[Age == given.age]))
}
mean.weights.by.Age <- sapply(ua,FUN=mean.whole.weight)

# 1.d.iii. Finally, use the “plot” command to plot mean weight vs. age. Describe the relationship revealed by the plot. Include an explanation for the behavior seen in the abalone of the 25-30 year age group.
plot(mean.weights.by.Age, ua, xlab="Mean Weights", ylab="Age")
# The plot shows that generally there is a positive correlation between the Mean Weights of the abalone and their Age.
# The apparent strange behavior in the data for the 25-30 year age group seems due to the fact that there is a small data set for those ages.
# In fact, sum(abalone$Age >= 25) shows that there are only 7 abalone aged 25 or higher, out of a total number of 4177.

# 2) Weather Data
# Loading and preprocessing the data
# First, clear any environment variables and clear the console as well.
rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# set the working directory
setwd("C:/Users/werner/Dropbox/Programming/UW_data_science/Lectures/2_Methods/Week1")

# file url
fileurl = "weather_data_2000_2014.csv"

# read the file
weatherUW <- read.csv(fileurl, header = TRUE, stringsAsFactors = FALSE)

# 2.a. Using summary statistics, histograms, boxplots, or other means identify and describe at least one data quality issue in the dataset.
# Let's look at the temperature data
summary(weatherUW[, "Temp..F."])
# The temperature is expressed in fahrenheit and it shows a min of -100000.00 which clearly is impossible as absolute zero (and hence the lowest possible temperature) is -459.67 F. Also, the max temperature is 9595 F which is extremely unlikely as a recorded temperature for UW.

# 2.b. Filter the data to remove the questionable data you identified in part a. How much of the data is affected? Hint: some of the functions “length”, “nrow”, “ncol”, “is.na”, and “which” may be helpful.

# Let's see how many readings are <= -100 F and >= 200 F (a temperature range that should never be exceeded under normal circumstances)
sum(weatherUW[, "Temp..F."] <= -100 | weatherUW[, "Temp..F."] >= 200)
# It looks like there are 413 readings exceeding these limits, out of a total of 6,992,901 readings

# 2.c. Look for and describe a monthly trend in the data.
# Let's clean out the temperature outliers
weatherUW.Monthly.TempF <- subset(weatherUW, weatherUW$"Temp..F." > -100 & weatherUW$"Temp..F." < 200, select = c("Temp..F.", "Month"))
# Let's determine the mean of each month
weatherUW.Monthly.MeanTempF <- aggregate(. ~ Month, weatherUW.Monthly.TempF, FUN=mean)
# and plot it (not a particularly exciting plot...)
plot(weatherUW.Monthly.MeanTempF$Month, weatherUW.Monthly.MeanTempF$"Temp..F.", xlab = "Month", ylab = "Temp (F)", type = "l")
# Not surprisingly, the mean temperature readings are cyclic through the 12 month period, with the lowest temperatures in January (month = 1) and December (month = 12) and the highest in June (month = 6) and July (month = 7). This makes sense as UW is in Seattle which is located north of the Equator.
