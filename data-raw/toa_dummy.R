library(devtools)

toa_dummy <- read.csv("data-raw/toa_dummy.csv", stringsAsFactors = FALSE, sep = ";")

toa_dummy$Date %<>% as.Date()
use_data(toa_dummy, overwrite = TRUE)
