setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week4/hwrk")

# 1
lrmc <- c(44, 42, 47, 43, 46)
m.lrmc <- mean(lrmc)
sd.lrmc <- sd(lrmc)
pnorm(q =  67*.64, mean = m.lrmc, sd = sd.lrmc, lower.tail = T)
# The lrmc model is not significantly more accurate than the experts
qnorm(p = .05, mean = m.lrmc, sd = sd.lrmc , lower.tail = T)
# in order for the lrmc model to be significantly more accurate the
# expert would have 61% a prediction rate or less.  


#2. Use the birth data to perform a t-test to determine whether the 
# mean due date of a second child is significantly greater than that 
# of a first in mothers with two or more children.
birth_data <- read.csv("birthDataDays.csv")
t.test(birth_data$Second, birth_data$First, alternative="greater", paired = T)
# Although the mean gestation period of the second child is longer,
# we cannot reject the null hypothesis that the due date of the second
# child is not significantly greater than the due date of the first child.
# In other words it is not significantly greater. 

# Load relevant libraries, and init functions
library(XML)
xmlCleanValue <-function(x) {
  cleaned_data <- gsub("[[:blank:]\n+]", "", xmlValue(x))
  return (as.numeric(gsub("\\(.*\\)", "",cleaned_data)))
}
cleanTable <- function(table) {
    scores <- getNodeSet(doc = table, path = './/tr/td/text()') 
    return(c(xmlCleanValue(scores[[11]]),
      xmlCleanValue(scores[[13]]),
      xmlCleanValue(scores[[16]]),
      xmlCleanValue(scores[[17]])))
}
# Colect data
data <- c()
for (page in 1:17){
  site = paste("http://www.oddsshark.com/stats/scoreboardbyweek/football/nfl/",page,"/2013", sep="")
  tree = htmlTreeParse(site,useInternal = T)
  new_data <- t(xpathSApply(doc = tree, 
                        path = '//*[@id="block-system-main"]/div/div/table[@class="scores"]',
                        cleanTable))
  data <- rbind(data, new_data)
}
# Put data into data frame and set names
data <- data.frame(data)
names(data) <- c("team_1_line", "team_1_score", "team_2_line", "team_2_score")

# Getting winners
data$winner <- data$team_1_score - data$team_2_score

spread_u3 <- data[abs(data$team_1_line) <= 3,] 

fav.team.wins <- nrow(spread_u3[spread_u3$team_1_line < 0 & spread_u3$winner > 0, ])
fav.team.losses <- nrow(spread_u3[spread_u3$team_1_line < 0 & spread_u3$winner < 0, ])
unfav.team.wins <-  nrow(spread_u3[spread_u3$team_1_line > 0 & spread_u3$winner > 0, ])
unfav.team.looses <-  nrow(spread_u3[spread_u3$team_1_line > 0 & spread_u3$winner < 0, ])

ind.score <- nrow(spread_u3)/2

prob.matrix <- matrix(data = c(fav.team.wins + unfav.team.looses, 
                fav.team.losses + unfav.team.wins,
                49.5, 49.5 ), nrow = 2, ncol = 2)

chisq.test(prob.matrix, correct = F)


### 4

# I suspect that teams that have been around for longer perform better than 
# newer teams. 

# My null hypothsis is that the number of years that a team has been playing does not affect
# it's win-loss record

# Data from NCAA.R script
# Make years playing variable
outcomes$years_playing <- as.numeric(as.character(outcomes$To)) - as.numeric(as.character(outcomes$From)) 
# Clean data by removing NAs and teams that are no longer playing and isolating test variables
outcomes.cleans <- outcomes[ as.character(outcomes$To) == "2015", c("W-L%", "years_playing")]
outcomes.cleans[["W-L%"]] <- as.numeric(as.character(outcomes.cleans[["W-L%"]]))
# split 2 attributes into 4 groups in order to run chi-squared test

outcomes.cleans$split_years <- cut(x = outcomes.cleans$years_playing, 
                                   breaks = quantile(outcomes.cleans$years_playing, probs = c(0,.25, .50, .75, 1)))
outcomes.cleans$split_win_pct <- cut(x = outcomes.cleans[["W-L%"]], 
                                     breaks = quantile(outcomes.cleans[["W-L%"]], probs = c(0, .25, .50, .75, 1)))
#putting into table
yearsvswinptc <- table(outcomes.cleans$split_years, outcomes.cleans$split_win_pct )

chisq.test(yearsvswinptc)
# With a p-value of 2.2e-16 we reject the null hypothsis that years playing do no not affect a team's
# win-loss record. 
# do no perform better 