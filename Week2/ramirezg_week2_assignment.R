###### Monty Hall Simulation
monty.hall.simulator <- function(number_of_tries) {
    # This function simulates the Monty Hall Problem and prints out the percent
    # correct using both the keep the first door and change the first door strategy.
    
    original.correct = 0
    change.correct = 0
    for (i in 1:number_of_tries){
      # Setting up choices
      choices = 1:3
      # Init car door
      car <- sample(choices, 1)
      # Init original choice
      original_choice <- sample(choices , 1)
      choice <- original_choice
      # Removing original choice from choices
      choices <- choices[choices != choice]
      
      # Switching choice if it's wrong
      if (car != original_choice){
        choice <- sample(choices, 1)
      }
      # Checking if new choice worked
      if (car == choice){
        change.correct = change.correct + 1
      }
      # Keeping original 
      if (car == original_choice){
        original.correct =original.correct + 1
      }
    }
    print(paste ("Keep Original Strategy: ", original.correct/number_of_tries, sep = " ", collapse = NULL))
    print(paste ("Change Original Strategy: ", change.correct/number_of_tries, sep = " ", collapse = NULL))
}

monty.hall.simulator(100000)


###### Simulation 2
marbles.simulation <- function(total_tries){
  
    #black = 1
    #white = 0
    colors = c(1, 0)
    black.correct = 0
    white.correct = 0
    
    for (i in 1:total_tries){    
        # Choose a color for the bag
        bag <- sample(colors, 1)
        # Adding white marble to bag
        bag <- c(bag, 0)
        # Monty chooses a white marble, which is basically the marble that 
        # was just added it seems this step in not needed
        bag <- bag[1:1]
        # Betting white
        if (1 == bag){
          white.correct <- white.correct + 1 
        }
        # Betting Black
        if (0 == bag){
          black.correct <- black.correct + 1   
        }
    }
    print(paste ("Black Choice: ", black.correct/total_tries, sep = " ", collapse = NULL))
    print(paste ("White Choice: ", white.correct/total_tries , sep = " ", collapse = NULL))

}


#prepare system
setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week2")
library(data.table)
clean_names <- function (name) {  
  name <- tolower(name)
  name <- gsub(pattern = "[^[:alpha:]]", replacement = " ",x = name)
  name <- strsplit(x = name, split = " ")[[1]][1]
  return (name)
}


############
#Outliers and QQ-Plots
############
#read data with data.table
data <- fread("weather_data_2006_2009.csv")
setnames(x=data,
         old=names(data),
         new=sapply(names(data), FUN=clean_names, USE.NAMES=F))
data[,pres:=NULL] #remove extra column

#First, identify and remove outliers in these data sets. Record the 
#approach you used to identify outliers and the fraction of data 
#that contains outliers. For now, simply drop the rows of data 
#containing outliers (we will revisit this below).

# Removing outliers, first look at summary stats
summary(data[,list(temp, wind, rhum)])
# Based on the summary it appears that all 3 variables are within resonable, possible ranges.
# Wind, however, is a little strange because it had different values for 0 and 360, which
# are the same thing. In this case I would ask a domain specialist to describe the difference.

# Removing outliers, second take a look at hist and blox plots
hist(data[, rhum]) 
hist(data[, wind]) 
hist(data[, temp])
# Based on these graphs it only appears that temperature might have some outliers 

boxplot(data[, rhum]) 
boxplot(data[, wind]) 
boxplot(data[, temp])
# The graphs of humidity and temperature show a number of dots outside of the whiskers 
# show that both rhum and temp have outlires. Temperature especiallly appears to
# have outlires because it has a number of dots near or on zero that don't
# connect to the other points.

# Choosing which outliers to remove, based on overarching methods
# in the end I decided against removing outliers for humidity and wind. Both of these
# attributes appear to have reasonable rangers and since they don't have normal distributions
# using the methods we discussed in class would lead to removing important data.
# Temperature; however, appeared to have a strang gap between 20 and 0 degrees.
hist(data[temp < 40 , temp],breaks = 30)

# This odd discontinuous jump raises red flags so I decided collect the indexes for removal.
temp.outliers <- which(data[,temp] < 10)

# Looking for outliers based on runs, the value before and after.
getjumps <- function(attribute, threshold = 5){
    # get the length of the attribute
    len <- length(attribute)
    # get the difference between each point that came before
    differences <- attribute[1:len-1] - attribute[2:len]
    # get the index of these jumps
    differences <- which(differences > threshold | differences < -threshold) + 1
    # returns an indexed list of jumps
    return(differences)
}
 
# Get jumps for temp, humidity, but not wind because wind can change very quickly
# Also get jumps for day in order to remove gaps attributed to measurment gaps
# For more granularity I could use minute gaps
temp.jumps <- getjumps(data[,temp] , 5)
rhum.jumps <- getjumps(data[,rhum] , 5)
day.jumps <- getjumps(data[,day] , 0)
# remove the temp and humidity jumps associated with jumps in day
temp.jumps <- setdiff(temp.jumps, day.jumps)
rhum.jumps <- setdiff(rhum.jumps, day.jumps)

## Removing outliers
# compile a list of outliers to remove
to.remove <- unique(c(temp.jumps, temp.outliers, rhum.jumps))
data.outliers.removed <- data[!to.remove,]

#Use QQ-plots to look for trends in the data. Try to detect daily and 
# monthly trends. Describe your findings.

y <- rnorm(nrow( data.outliers.removed[month == 2, ]), 
           mean=mean(data.outliers.removed[month == 2, temp]), 
           sd=sd(data.outliers.removed[month == 2, temp]))
qqplot(x = data.outliers.removed[month == 2, temp], y = y)
abline(0,1)
# It appears that for the most part temperature has a normal distrubtion

y <- rnorm(nrow( data.outliers.removed[month == 2, ]), 
           mean=mean(data.outliers.removed[month == 2, wind]), 
           sd=sd(data.outliers.removed[month == 2, wind]))
qqplot(x = data.outliers.removed[month == 2, wind], y = y)
abline(0,1)
# Wind speed appears to have a somewhat normal distrubtion, except at the tails.
# This is probably because the distribution is more uniform.

y <- rnorm(nrow( data.outliers.removed[month == 2, ]), 
           mean=mean(data.outliers.removed[month == 2, rhum]), 
           sd=sd(data.outliers.removed[month == 2, rhum]))
qqplot(x = data.outliers.removed[month == 2, rhum], y = y)
abline(0,1)
# Rhum does not appear to have a normal distrubtion at all. 
 
### Imputing Outliers
#set the missing variables to NA
data[temp.jumps, temp:=NA]
data[rhum.jumps, rhum:=NA]
data[temp.outliers, temp:=NA]
data[,v:=NULL]

require(Amelia)
amelia.results = amelia(data, idvars=c("year", "month", "day", "time"))

# Show the distribution of the observed and imputed values
plot(amelia.results)
# Recall the relationship between missing values and temperature
plot(data[,temp] ~ data[,rhum])

# Compare means before and after imputation 
mean(data[, rhum], na.rm = TRUE)
for(i in 1:5) {
  print(mean(amelia.results$imputations[[i]]$rhum))
}

# imputing data
data[ , rhum:=amelia.results$imputations$imp1$rhum ]
data[ , temp:=amelia.results$imputations$imp1$temp ]
#checking if it worked
mean(data[, rhum])
mean(data[, temp])
# Reving changes

#checking for changes
y <- rnorm(nrow( data[month == 2, ]), 
           mean=mean(data[month == 2, temp]), 
           sd=sd(data[month == 2, temp]))
qqplot(x = data[month == 2, temp], y = y)
abline(0,1)

y <- rnorm(nrow( data[month == 2, ]), 
           mean=mean(data[month == 2, rhum]), 
           sd=sd(data[month == 2, rhum]))
qqplot(x = data[month == 2, rhum], y = y)
abline(0,1)

y <- rnorm(nrow(data[month == 2, ]), 
           mean=mean(data[month == 2, wind]), 
           sd=sd(data[month == 2, wind]))
qqplot(x = data[month == 2, wind], y = y)
abline(0,1)

# The impudations did not appear to change qqplots. 
