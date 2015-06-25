data(toa_dummy)

trad_one_access(toa_dummy)

data(toa_example)

trad_one_access(toa_example, holidays = as.Date("2014-05-29"), 
                weekend = c("Friday", "Saturday", "Sunday"))
trad_one_access(toa_example)
