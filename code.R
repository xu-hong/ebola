data <- read.csv("country_timeseries.csv", header=T)
# prepare data for Guinea
data_gui <- data[c("Date", "Day", "Cases_Guinea")]
data_gui <- data_gui[!is.na(data_gui$Cases_Guinea),]
with(data_gui, plot(Day, Cases_Guinea))
# prepare data for Liberia
data_lib <- data[c("Date", "Day", "Cases_Liberia")]
data_lib <- data_lib[!is.na(data_lib$Cases_Liberia),]
with(data_lib, plot(Day, Cases_Liberia))
