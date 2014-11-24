

library(ggplot2)
library(scales) 
library(gridExtra)
library(reshape)

data.l <- read.csv("Liberia_new_case_byweek.csv", header=T, sep=";")
data.l.new <- data.l[!is.na(data.l$New_Cases),]
bp.l <- with(data.l.new, barplot(New_Cases, xlab="Week", las=2))
bp.l.c <- with(data.l.new, barplot(Cumulative_Cases, xlab="Week", las=2))
axis(1, at=bp.s, labels=data.s.new$Week)

# week 1-14
#data.g.1 <- data.g.new[1:12,]
#data.g.2 <- data.g.new[12:18,]
#data.g.3 <- data.g.new[14:24,]
#data.g.4 <- data.g.new[25:43,]

plot.l <- ggplot(data = data.l.new, aes(x=Week, y=New_Cases)) + geom_bar(stat="identity") 
# WHO estimated Serial Interval in Liberia at 13.1 days
si <- 13.1/7
data.l.new$R <- rep(0, 32)
for (k in 1:27) {
  data.l.t <- data.l.new[k:(k+5), ] # 6 week window
  glm.l.t <- glm(New_Cases ~ Week, family=poisson, data=data.l.t)
  #summary(glm.g.t) # check significance
  coeff <- coef(glm.l.t)
  l.r.t <- exp(si*coeff[2])
  #print(s.r.t)
  data.l.new$R[(k+5)] <- l.r.t[[1]]
}

data.l.plot <- melt(data.l.new, id=c("Week", "DisplayValue"))
data.l.plot.new <- data.l.plot[!data.l.plot$variable %in% c("Cumulative_Cases"),]
plot.l.r <- ggplot(data.l.plot.new, aes(Week, value)) + facet_grid(variable~.,  scales="free_y") 
plot.l.r1 <- plot.l.r + geom_bar(data=subset(data.l.plot.new, variable=="New_Cases"), aes(Week, value), stat="identity", fill="darkorange", color="gray89") 
plot.l.r2 <- plot.l.r1 + geom_line(data=subset(data.l.plot.new, variable=="R"), aes(Week, value), alpha=0.3, lwd=1) + geom_hline(data=subset(data.l.plot.new, variable=="R"),aes(yintercept=1), colour="firebrick")
#plot.g.r3 <- plot.g.r2 + geom_smooth(data=subset(data.g.plot, variable=="R"), aes(Week, value), color="gray20")
plot.l.r2 + theme_bw()


# if we look at new cases counts in recent few weeks, when there is a trend of decreasing
data.l.n <- data.l.new[20:32,]
glm.l.n <- glm(New_Cases ~ Week, family=poisson, data=data.l.n)
#coeff <- coef(glm.l.c)
# serial interval: 13.1 days
#l.r.c <- exp(si*coeff[2]) 
plot.l.n <- ggplot(data=data.l.n, aes(Week, New_Cases)) + geom_bar(stat="identity", fill="darkorange", color="gray89")
plot.l.n

# predict

newdata.l <- matrix(rep(NA, 3*20), nrow=20, ncol=3)
newdata.l <- cbind(47:66, newdata.l)
colnames(newdata.l) <- c("Week", "DisplayValue", "New_Cases", "Cumulative_Cases")
data.l.p <- rbind(data.l.n[, c("Week", "DisplayValue", "New_Cases", "Cumulative_Cases")], newdata.l)

fitted.l <- data.frame(data.l.p, predict(glm.l.n, newdata=data.l.p, se.fit=TRUE, type="response"))
fitted.l$ucl <- fitted.l$fit + 1.96 * fitted.l$se.fit
fitted.l$lcl <- fitted.l$fit - 1.96 * fitted.l$se.fit

plot.l.his <- ggplot(data=data.l, aes(Week, Cumulative_Cases)) + geom_point(shape=17)
plot.l.stquo <- plot.l.n + geom_line(data=fitted.l, aes(x=Week, y=fit), lwd=1.5, alpha=0.8) + geom_ribbon(aes(ymin = lcl, ymax = ucl), data=fitted.l, alpha=0.2)
plot.l.stquo + theme_bw()




