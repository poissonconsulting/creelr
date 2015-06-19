library(devtools)
library(magrittr)


toa_example <- read.csv("data-raw/toa_example.csv", stringsAsFactors = FALSE, sep = ";")

toa_example$Month %<>% match(month.abb)
toa_example$Date <- paste(toa_example$Year, toa_example$Month, toa_example$Day, sep = "-")
toa_example$Date %<>% as.Date()
toa_example$DayType %<>% as.factor()
levels(toa_example$DayType) <- c("Week", "Weekend")
toa_example$Period <- factor(toa_example$Shift, levels = c("AM", "PM"))
toa_example$Catch <- toa_example$TotalCaught

toa_example %<>% subset(select = c("Date", "DayType", "Period", "RodHours", "Catch", "KOCatch", 
                                   "CTCatch", "RBCatch", "BTCatch", "LTCatch"))

use_data(toa_example, overwrite = TRUE)
