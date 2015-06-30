data(toa_dummy)
trad_one_access(toa_dummy)

data(toa_example)
trad_one_access(toa_example, holidays = as.Date("2014-05-29"), 
                weekend = c("Friday", "Saturday", "Sunday"))
trad_one_access(toa_example)

toa_results <- trad_one_access(toa_example, weekend = c("Friday", "Saturday", "Sunday"))

plot_creel_estimates(toa_results)

sum_creel_estimates(toa_results, alpha = 0.10)
sum_creel_estimates(toa_results, by = "Month")
