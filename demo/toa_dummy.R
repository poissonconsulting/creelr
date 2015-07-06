library(ggplot2)

data(toa_dummy)
toa_dummy
plot_creel_data(toa_dummy) + ylab("Daily Catch")

toa_dummy <- trad_one_access(toa_dummy)

toa_dummy
plot_creel_estimates(toa_dummy) + ylab("Monthly Catch")
