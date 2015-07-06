library(ggplot2)

data(toa_dummy)

plot_creel_data(toa_dummy, am = 0.5) + ylab("Daily Catch")

toa_dummy <- trad_one_access(toa_dummy)

plot_creel_estimates(toa_dummy) + ylab("Monthly Catch")
