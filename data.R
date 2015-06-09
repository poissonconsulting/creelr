library(magrittr)
library(dplyr)
library(lubridate)
library(assertthat)

data <- read.csv("test.csv", stringsAsFactors = FALSE, sep = ";")

day_type <- function (x) {
  assert_that(is.date(x))
  x %<>% lubridate::wday(label = TRUE, abbr = FALSE)
  levels(x) <- list(Week = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                               Weekend = c("Saturday", "Sunday"))
  x
}

nday_type_month <- function (month, year = 2000) {
  assertthat::assert_that(is.count(month))
  assertthat::assert_that(is.count(year))
  
  first <- as.Date(paste(year, month, 01, sep = "-"))
  last <- as.Date(first + months(1) - days(1))
  x <- seq(first, last, by = "day")
  x %<>% day_type()
  c(Week = sum(x == "Week"), Weekend = sum(x == "Weekend"))
}

# now gets daytype from Date!
data$Date %<>% as.Date()
data$DayType <- day_type(data$Date)
data$Period %<>% factor(levels = c("AM", "PM"))

summary(data)

#I'm leaving this here in case we'll need it for later when 
#selection probabilities are not the same
prob <- c(0.5, 0.5) 
data$Probability[data$Period == "AM"] <- prob[1]
data$Probability[data$Period == "PM"] <- prob[2]

# uses the dplyr mutate function for faster and more readable code
data %<>% dplyr::mutate(daily_eff = RodHours / Probability, daily_cat = Catch / Probability)
#data$daily_eff <- data$RodHours/data$Probability
#data$daily_cat <- data$Catch/data$Probability

# gets number of days for month and year
total_n <- nday_type_month(month(data$Date[1]), year(data$Date[1]))
# we need to change the dates of the 
# data so that they are for a february with 28 days where the first day is a Monday
total_n <- c(20, 8)
#There's gotta be a way we can calculate the total number of days from the data
#given. Probably an R function that lets you know when a whole week is
# covered. Afterwards, the total number of week days will be weeks*5 and the
# total number of weekend days is weeks*2. In the meantime I'll just state the 
#numbers as such.



#I'll be dividing each step of the calculations for each type of day, and do different 
#calculations for the effort and catch parameters. I'm sure all of this could be simplified, 
#but I'll leave it as it is for the first draft. 

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

#----Second Approach------------

#I'm making a data frame with variab referring to effort (1) or catch (2)
# dt will be the day type, week = 1, weekend = 2
#this way I can make a couple of for cycles and run the whole procedure in less lines
#it will need the daily value for effort and catch, as well as the total number of days
#calculated above

total_n2 <- matrix(rep(total_n, 2), nrow = 2, byrow = T)

variab <- c(rep(1, nrow(data)), rep(2, nrow(data)))
dt <- rep(as.numeric(data$DayType),2)
daily <- c(data$daily_eff, data$daily_cat)
data2 <- data.frame(variab, dt, daily)

mean_est <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Effort", "Catch"), 
                                                          c("Week", "Weekend")))
var_est <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Effort", "Catch"), 
                                                         c("Week", "Weekend")))
var_total <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Effort", "Catch"), 
                                                           c("Week", "Weekend")))
se_total <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Effort", "Catch"), 
                                                          c("Week", "Weekend")))
total_est <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Effort", "Catch"), 
                                                           c("Week", "Weekend")))
overall_est <- matrix(0, nrow = 2, ncol = 1, dimnames = list(c("Effort", "Catch"), 
                                                             NULL))
overall_var <- matrix(0, nrow = 2, ncol = 1, dimnames = list(c("Effort", "Catch"), 
                                                             NULL))

for (i in 1:2) {
  for (j in 1:2) {
    mean_est[i, j] <- mean(data2$daily[data2$variab == i & data2$dt == j])
    var_est[i, j] <- var(data2$daily[data2$variab == i & data2$dt == j]) / 
      length(data2$dt[data2$dt == j & data2$variab == i])
    var_total[i, j] <- total_n2[i, j]^2 * var_est[i, j]
    se_total[i, j] <- sqrt(var_total[i, j])
    total_est[i, j] <- total_n2[i, j] * mean_est[i, j]
  }
  overall_est[i, ] <- sum(total_est[i, ])
  overall_var[i, ] <- sum(var_total[i, ])
}
overall_se <- sqrt(overall_var)

mean_est
total_est
se_total
overall_est
overall_se
