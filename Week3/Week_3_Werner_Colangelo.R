# Week 3 Assignment: Exploring Data
# Submitted by: Werner Colangelo (colangw)
# Date: February 3, 2015

# Clear any environment variables and clear the console as well.
rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# set the working directory

setwd("C:/Users/werner/Dropbox/Programming/UW_data_science/Lectures/2_Methods/Week3")

filename = "birthDataDays.csv"
births <- read.csv(filename, header=TRUE, stringsAsFactors=FALSE)

# A collection of birth statistics was gathered here (http://spacefem.com/pregnant/charts/duedate0.php). The collection method is informal, but let's try to see what this data tells us about the probability of a birth being early or late. The table on this page (http://spacefem.com/pregnant/charts/duedate5.php) was used to create the birthDataDays.csv file (with the length of pregnancy converted to days).

# A mother's due date is calculated to be the 280th day of pregnancy. Based on this data set, what is the probability of a baby being born 24 or more days early?

(sum(births$First <= 280 - 24) + sum(births$Second <= 280 - 24)) / (2 * nrow(births))


# How might the use of this data set bias the result from the last question?
# The data set contains data of women that have given birth to at least two (as far as we know) children.
# It is quite possible that having one child changes the probability of a late or early birth for the second.

# Suppose that you now learn that the baby in question is a second child. Now what is the probability of our baby being born 24 or more days early?

sum(births$Second <= 280 - 24) / nrow(births)

# Furthermore, the first child was late. Now that you also know that, what is the probability of the baby being born 24 or more days early?

sum(births$First > 280 & births$Second <= 280 - 24) / nrow(births)


# Does the length of a pregnancy for a first child follow the same distribution as the length for the second child?

qqplot(births$First, births$Second)
abline(0, 1, col = "red")
# Based on the observed plot the length of a pregnancy for a first child does follow the same distribution as the length for the second child

# Is the length of a pregnancy for a first child independent of the length for the second child?
t.test(births$First, births$Second)
# Based on the t-test result (p-value = 0.3033), the length of a pregnancy for a first child IS NOT independent of the length for the second child.
