library(magrittr)
data <- read.csv("test.csv", stringsAsFactors = FALSE, sep = ",")

data$Date %<>% as.Date()
data$DayType %<>% factor(levels = c("Week", "Weekend"))

summary(data)
