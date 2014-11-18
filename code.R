library(ggplot2)
library(scales) 

data <- read.csv("country_timeseries.csv", header=T)
# prepare data for Guinea
data_gui <- data[c("Date", "Day", "Cases_Guinea")]
data_gui$Date <- as.Date(data_gui$Date, "%m/%d/%Y")
data_gui <- data_gui[!is.na(data_gui$Cases_Guinea),]
data_gui$Week <- format(data_gui$Date, format = "%W")
with(data_gui, plot(Date, Cases_Guinea))
#axis.Date(side = 1, data_gui$Date, format = "%b %y")

# plot with ggplot
ggplot(data = data_gui, aes(Date, Cases_Guinea)) + geom_point() + scale_x_date(labels = date_format("%Y-%m"))



mod_gui <- glm(Cases_Guinea ~ Day, family=poisson, data=data_gui)
summary(mod_gui)
coeff <- coef(mod_gui)
xvals <- sort(data_gui$Day)
log.means <- coeff[1]+coeff[2]*xvals 
mean.values <- exp(log.means)
lines(xvals, mean.values, lwd=2)

r_gui <- exp(19*coeff[2])


p_gui= data.frame(data_gui, predict(mod_gui, se.fit=TRUE, type="response"))
p_gui$ucl <- p_gui$fit + 1.96 * p_gui$se.fit
p_gui$lcl <- p_gui$fit - 1.96 * p_gui$se.fit

plot_g <- ggplot(data=data_gui, aes(Day, Cases_Guinea)) + geom_point()
plot_g + geom_line(data=p_gui, aes(x=Day, y=fit)) + geom_ribbon(aes(ymin = lcl, ymax = ucl), data=p_gui, alpha=0.2)

ggplot(data=data_gui, aes(Day, log(Cases_Guinea))) + geom_point() + geom_smooth(aes(x=Day, y=log(Cases_Guinea)), method=lm)


tail(data_gui, n=30)
dim(data_gui)









# prepare data for Liberia
data_lib <- data[c("Date", "Day", "Cases_Liberia")]
data_lib <- data_lib[!is.na(data_lib$Cases_Liberia),]
with(data_lib, plot(Day, Cases_Liberia))
mod_lib <- glm(Cases_Liberia ~ Day, family=poisson, data=data_lib)
summary(mod_lib)
