# Gerald Dunn
# UWNetID: gdunnjr
# Assignment 1 - Abalone Data

# Clear the work space
rm(list=ls())

# Global setting for plots
par(mar=c(4,4,1,1))

# Clear Console:
cat("\014")

# Load the avalone data and set the names
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abalone <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)
names(abalone)=c("Sex","Length","Diameter","Height","Whole.Weight","Shucked.Weight","Viscera.Weight","Shell.Weight","Rings")
summary(abalone)

# a. Use the quantile command to get the deciles for height
hvector = abalone[,"Height"]
quantile(hvector, probs = seq(0, 1, length=11), type=5)
# output:
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# 0.000 0.090 0.105 0.120 0.130 0.140 0.150 0.160 0.175 0.185 1.130 

# b. Use cut command to assign values
hq = cut(hvector, breaks = quantile(hvector, probs = seq(0, 1, length=11), type=5), include.lowest=TRUE, labels=c("1","2","3","4","5","6","7","8","9","10"))
summary(hq)
# 1   2   3   4   5   6   7   8   9  10 
# 538 350 437 371 409 449 422 564 234 403 

# c. create age vs height table.
# create age quantile age = rings+1.5
avector = abalone[,"Rings"]+1.5
quantile(avector, probs = seq(0, 1, length=11), type=5)
aq = cut(avector, breaks = unique(quantile(avector, probs = seq(0, 1, length=11)), type=5), include.lowest=TRUE, labels=c("1","2","3","4-5","6","7","8","9","10"))
summary(aq)

# create a table of age vs height
agevsheight = table(aq,hq)
agevsheight
# output:
#      hq
#aq    1   2   3   4   5   6   7   8   9  10
#1   321  73  32  12   4   2   2   1   1   0
#2   114 108  87  32  24  15   5   3   3   0
#3    50  68 131  85  80  77  38  22  12   5
#4-5  25  45  73  95 100 100 101  93  24  33
#6    17  25  36  52  74  94  90 133  50  63
#7     7  13  32  29  33  60  66 108  50  89
#8     2   8  17  18  25  28  37  53  29  50
#9     2   6  18  30  38  37  39  67  26  66
#10    0   4  11  18  31  36  44  84  39  97

# Observation - As age increases height tends to increase.

#d.i Use unique and sort to get unique vaulues of age 
ua = sort(unique(abalone[,"Rings"]))

#d.ii Use sapply and a user defined function to get mean weight for abalones of the ages in ua
meanwt = sapply(ua, function(x) ifelse(is.numeric(abalone[abalone$Rings==x,]$Whole.Weight), mean(abalone[abalone$Rings==x,]$Whole.Weight), NA))

#d iii Plot mean weight vs age
plot(meanwt,xlab="Age",ylab="Mean Wt")

#Weight grows exponentially until age 10 when it levels off.
#In the 25-30 year age group there are very few abalone.


