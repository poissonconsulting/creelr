library(devtools)
library(dplyr)
library(magrittr)

toa_dummy <- read.csv("data-raw/toa_dummy.csv", stringsAsFactors = FALSE, sep = ";")

toa_dummy$Date %<>% as.Date()

toa_dummy %<>% select(Date, Period, RodHours, Catch)

use_data(toa_dummy, overwrite = TRUE)
