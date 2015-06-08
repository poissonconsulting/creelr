library(magrittr)
data <- read.csv("test.csv", stringsAsFactors = FALSE, sep = ";")

data$Date %<>% as.Date()
data$DayType %<>% factor(levels = c("Week", "Weekend"))
data$Period %<>% factor(levels = c("AM", "PM"))

summary(data)

#I'm leaving this here in case we'll need it for later when 
#selection probabilities are not the same
prob <- c(0.5, 0.5) 
data$Probability[data$Period == "AM"] <- prob[1]
data$Probability[data$Period == "PM"] <- prob[2]

data$daily_eff <- data$RodHours/data$Probability
data$daily_cat <- data$Catch/data$Probability

total_n <- c(20, 8)
#There's gotta be a way we can calculate the total number of days from the data
#given. Probably an R function that lets you know when a whole week is
# covered. Afterwards, the total number of week days will be weeks*5 and the
# total number of weekend days is weeks*2. In the meantime I'll just state the 
#numbers as such.



#I'll be dividing each step of the calculations for each type of day, and do different 
#calculations for the effort and catch parameters. I'm sure all of this could be simplified, 
#but I'll leave it as is for the time being. 

mean_eff <- c(mean(data$daily_eff[data$DayType == "Week"]), 
              mean(data$daily_eff[data$DayType == "Weekend"]))

mean_cat <- c(mean(data$daily_cat[data$DayType == "Week"]), 
              mean(data$daily_cat[data$DayType == "Weekend"]))

n <- c(length(data$DayType[data$DayType == "Week"]), 
       length(data$DayType[data$DayType == "Weekend"]))

var_eff <- c(var(data$daily_eff[data$DayType == "Week"]) / n[1], 
            var(data$daily_eff[data$DayType == "Weekend"]) / n[2])

var_cat <- c(var(data$daily_cat[data$DayType == "Week"]) / n[1], 
             var(data$daily_cat[data$DayType == "Weekend"]) / n[2])

var_total_eff <- total_n^2 * var_eff
var_total_cat <- total_n^2 * var_cat

se_total_eff <- sqrt(var_total_eff)
se_total_cat <- sqrt(var_total_cat)

total_eff <- total_n * mean_eff
total_cat <- total_n * mean_cat

overall_effort <- sum(total_eff)
overall_catch <- sum(total_cat)

overall_var_eff <- sum(var_total_eff)
overall_var_cat <- sum(var_total_cat)

overall_se_eff <- sqrt(overall_var_eff)
overall_se_cat <- sqrt(overall_var_cat)
