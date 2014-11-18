

library(ggplot2)
library(scales) 
library(gridExtra)
library(reshape)

data.g <- read.csv("Guinea_new_case_byweek.csv", header=T, sep=";")
data.g.new <- data.g[!is.na(data.g$New_Cases),]
bp.g <- with(data.g.new, barplot(New_Cases, xlab="Week", las=2))
bp.g.c <- with(data.g.new, barplot(Cumulative_Cases, xlab="Week", las=2))
axis(1, at=bp.g,labels=data.g.new$Week)

# week 1-14
#data.g.1 <- data.g.new[1:12,]
#data.g.2 <- data.g.new[12:18,]
#data.g.3 <- data.g.new[14:24,]
#data.g.4 <- data.g.new[25:43,]

plot.g <- ggplot(data = data.g.new, aes(x=Week, y=New_Cases)) + geom_bar(stat="identity") 

for (k in 1:38) {
  data.g.t <- data.g.new[k:(k+5), ] # 6 week window
  glm.g.t <- glm(New_Cases ~ Week, family=poisson, data=data.g.t)
  #summary(glm.g.t) # check significance
  coeff <- coef(glm.g.t)
  # serial interval: 19 days
  g.r.t <- exp(2.714286*coeff[2])
  #print(g.r.t)
  data.g.new$R[(k+5)] <- g.r.t[[1]]
}

data.g.plot <- melt(data.g.new, id=c("Week", "DisplayValue"))
data.g.plot.new <- data.g.plot[!data.g.plot$variable %in% c("Cumulative_Cases"),]
plot.g.r <- ggplot(data.g.plot.new, aes(Week, value)) + facet_grid(variable~.,  scales="free_y") 
plot.g.r1 <- plot.g.r + geom_bar(data=subset(data.g.plot.new, variable=="New_Cases"), aes(Week, value), stat="identity", fill="darkorange", color="gray89") 
plot.g.r2 <- plot.g.r1 + geom_line(data=subset(data.g.plot.new, variable=="R"), aes(Week, value), alpha=0.3, lwd=1) + geom_hline(data=subset(data.g.plot.new, variable=="R"),aes(yintercept=1), colour="firebrick")
#plot.g.r3 <- plot.g.r2 + geom_smooth(data=subset(data.g.plot, variable=="R"), aes(Week, value), color="gray20")
plot.g.r2


# if we look at cumulative counts
glm.g.c <- glm(Cumulative_Cases ~ Week, family=poisson, data=data.g)
coeff <- coef(glm.g.c)
# serial interval: 19 days
#g.r.c <- exp(2.714286*coeff[2]) # 1.29
plot.g.c <- ggplot(data=data.g, aes(Week, Cumulative_Cases)) + geom_bar(stat="identity", fill="darkorange", color="gray89")
plot.g.c

# predict

newdata.g <- matrix(rep(NA, 3*20), nrow=20, ncol=3)
newdata.g <- cbind(46:65, newdata.g)
colnames(newdata.g) <- names(data.g)
data.g.p <- rbind(data.g, newdata.g)

fitted.g <- data.frame(data.g.p, predict(glm.g.c, newdata=data.g.p, se.fit=TRUE, type="response"))
fitted.g$ucl <- fitted.g$fit + 1.96 * fitted.g$se.fit
fitted.g$lcl <- fitted.g$fit - 1.96 * fitted.g$se.fit

#plot.g.p <- ggplot(data=data.g.p, aes(Week, Cumulative_Cases)) + geom_point()
plot.g.stquo <- plot.g.c + geom_line(data=fitted.g, aes(x=Week, y=fit), lwd=1.5, alpha=0.8) + geom_ribbon(aes(ymin = lcl, ymax = ucl), data=fitted.g, alpha=0.2)

#get predicted new cases
fitted.g.c.1 <- fitted.g$fit[(46-1):(65-1)]
fitted.g.c.2 <- fitted.g$fit[(46):(65)]
fitted.g.new <- fitted.g.c.2 - fitted.g.c.1
fitted.g$New_Cases[46:65] <- fitted.g.new

# CDC model: R0 for hospitalized, 0.12, home with intervention: 0.18, home w/o: 1.8
# Our estimate of R0: 1.29
# Estimate the current distribution: 0.1, 0.21, 0.69
g.sc <- data.frame(fitted.g[46:65, c("Week", "New_Cases", "fit")])
# WHO estimated hospital stay: 4.99, which means the number of beds required roughly 
# equals to the rising weekly case indidence.

# As of Week 46, November 16: New cases - 187
# Scenario 1: Bed need satisfied for all patients since Week 46, i.e. 225 beds in place
# new R0: 0.12
beta1 <- log(0.12)/2.714286
beta0 <- log(225)
g.sc$sc1 <- g.sc$New_Cases
g.sc$sc1[4:20] <- exp(beta0 + beta1*(1:17))
g.sc$sc1.c <- g.sc$fit

for (i in 4:20) {
  g.sc$sc1.c[i] <- g.sc$sc1.c[i-1] + g.sc$sc1[i]
}

# Scenario 2: half hospitalized, distribution: 0.5, 0.21, 0.29
# i.e. 113 beds in place
# new R0: 0.5*0.12 + 0.21*0.18 + 0.29*1.8 = 0.6198

beta1 <- log(0.6198)/2.714286
beta0 <- log(225)
g.sc$sc2 <- g.sc$New_Cases
g.sc$sc2[4:20] <- exp(beta0 + beta1*(1:17))
g.sc$sc2.c <- g.sc$fit

for (i in 4:20) {
  g.sc$sc2.c[i] <- g.sc$sc2.c[i-1] + g.sc$sc2[i]
}

plot.g.stquo + geom_line(data=g.sc, aes(x=Week, y=sc1.c), lwd=1.5, alpha=0.8, color="steelblue") + geom_line(data=g.sc, aes(x=Week, y=sc2.c), lwd=1.5, alpha=0.8, color="darkslategray") + geom_vline(xintercept = 53, colour="firebrick", lwd=1.5, linetype = "longdash")
