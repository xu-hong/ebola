

library(ggplot2)
library(scales) 
library(gridExtra)
library(reshape)

data.s <- read.csv("SL_new_case_byweek.csv", header=T, sep=";")
data.s.new <- data.s[!is.na(data.s$New_Cases),]
bp.s <- with(data.s.new, barplot(New_Cases, xlab="Week", las=2))
bp.s.c <- with(data.s.new, barplot(Cumulative_Cases, xlab="Week", las=2))
axis(1, at=bp.s, labels=data.s.new$Week)

# week 1-14
#data.g.1 <- data.g.new[1:12,]
#data.g.2 <- data.g.new[12:18,]
#data.g.3 <- data.g.new[14:24,]
#data.g.4 <- data.g.new[25:43,]

plot.g <- ggplot(data = data.s.new, aes(x=Week, y=New_Cases)) + geom_bar(stat="identity") 
# WHO estimated Serial Interval in Sierra Leone at 11.6 days
si <- 11.6/7
data.s.new$R <- rep(0, 27)
for (k in 1:22) {
  data.s.t <- data.s.new[k:(k+5), ] # 6 week window
  glm.s.t <- glm(New_Cases ~ Week, family=poisson, data=data.s.t)
  #summary(glm.g.t) # check significance
  coeff <- coef(glm.s.t)
  s.r.t <- exp(si*coeff[2])
  #print(s.r.t)
  data.s.new$R[(k+5)] <- s.r.t[[1]]
}

data.s.plot <- melt(data.s.new, id=c("Week", "DisplayValue"))
data.s.plot.new <- data.s.plot[!data.s.plot$variable %in% c("Cumulative_Cases"),]
plot.s.r <- ggplot(data.s.plot.new, aes(Week, value)) + facet_grid(variable~.,  scales="free_y") 
plot.s.r1 <- plot.s.r + geom_bar(data=subset(data.s.plot.new, variable=="New_Cases"), aes(Week, value), stat="identity", fill="darkorange", color="gray89") 
plot.s.r2 <- plot.s.r1 + geom_line(data=subset(data.s.plot.new, variable=="R"), aes(Week, value), alpha=0.3, lwd=1) + geom_hline(data=subset(data.s.plot.new, variable=="R"),aes(yintercept=1), colour="firebrick")
#plot.g.r3 <- plot.g.r2 + geom_smooth(data=subset(data.g.plot, variable=="R"), aes(Week, value), color="gray20")
plot.s.r2 + theme_bw()


# if we look at cumulative counts
data.s <- data.s[!is.na(data.s$Cumulative_Cases),]
glm.s.c <- glm(Cumulative_Cases ~ Week, family=poisson, data=data.s)
#coeff <- coef(glm.s.c)
# serial interval: 11.6 days
#g.r.c <- exp(si*coeff[2]) 
plot.s.c <- ggplot(data=data.s, aes(Week, Cumulative_Cases)) + geom_bar(stat="identity", fill="darkorange", color="gray89")
plot.s.c

# predict

newdata.s <- matrix(rep(NA, 3*20), nrow=20, ncol=3)
newdata.s <- cbind(47:66, newdata.s)
colnames(newdata.s) <- names(data.s)
data.s.p <- rbind(data.s, newdata.s)

fitted.s <- data.frame(data.s.p, predict(glm.s.c, newdata=data.s.p, se.fit=TRUE, type="response"))
fitted.s$ucl <- fitted.s$fit + 1.96 * fitted.s$se.fit
fitted.s$lcl <- fitted.s$fit - 1.96 * fitted.s$se.fit

plot.s.his <- ggplot(data=data.s, aes(Week, Cumulative_Cases)) + geom_point(shape=17)
plot.s.stquo <- plot.s.c + geom_line(data=fitted.s[1:37,], aes(x=Week, y=fit), lwd=1.5, alpha=0.8) + geom_ribbon(aes(ymin = lcl, ymax = ucl), data=fitted.s[1:37,], alpha=0.2)

#get predicted new cases
fitted.s.c.1 <- fitted.s$fit[(29-1):(48-1)]
fitted.s.c.2 <- fitted.s$fit[(29):(48)]
fitted.s.new <- fitted.s.c.2 - fitted.s.c.1
fitted.s$New_Cases[29:48] <- fitted.new

# CDC model: R0 for hospitalized, 0.12, home with intervention: 0.18, home w/o: 1.8
# Our estimate of R0: 1.29 (need to check)
# Estimate the current distribution: 0.1, 0.21, 0.69 (need to check)
s.sc <- data.frame(fitted.s[29:48, c("Week", "New_Cases", "fit")])
# WHO estimated hospital stay: 6.88, which means the number of beds required roughly 
# equals to the rising weekly case indidence.

# As of Week 47, November 23: estimated new cases - 1170
# Scenario 1: Bed need satisfied for all patients since Week 47, i.e. 1170 beds in place
# new R0: 0.12
beta1 <- log(0.12)/si
beta0 <- log(1170)
s.sc$sc1 <- s.sc$New_Cases
s.sc$sc1[2:20] <- exp(beta0 + beta1*(1:19))
s.sc$sc1.c <- s.sc$fit

for (i in 2:20) {
  s.sc$sc1.c[i] <- s.sc$sc1.c[i-1] + s.sc$sc1[i]
}

# Scenario 2: half hospitalized, distribution: 0.5, 0.21, 0.29
# i.e. 585 beds in place
# new R0: 0.5*0.12 + 0.21*0.18 + 0.29*1.8 = 0.6198

beta1 <- log(0.6198)/si
beta0 <- log(1170)
s.sc$sc2 <- s.sc$New_Cases
s.sc$sc2[2:20] <- exp(beta0 + beta1*(1:19))
s.sc$sc2.c <- s.sc$fit

for (i in 2:20) {
  s.sc$sc2.c[i] <- s.sc$sc2.c[i-1] + s.sc$sc2[i]
}


# As of Week 49, December 7: New cases - 1598
# Scenario 3: Bed need satisfied for all patients since Week 51, i.e. 1598 beds in place
# new R0: 0.12
beta1 <- log(0.12)/si
beta0 <- log(1598)
s.sc$sc3 <- s.sc$New_Cases
s.sc$sc3[4:20] <- exp(beta0 + beta1*(1:17))
s.sc$sc3.c <- s.sc$fit

for (i in 4:20) {
  s.sc$sc3.c[i] <- s.sc$sc3.c[i-1] + s.sc$sc3[i]
}
# Scenario 4: half hospitalized, distribution: 0.5, 0.21, 0.29
# i.e. 799 beds in place
# new R0: 0.5*0.12 + 0.21*0.18 + 0.29*1.8 = 0.6198

beta1 <- log(0.6198)/si
beta0 <- log(1598)
s.sc$sc4 <- s.sc$New_Cases
s.sc$sc4[4:20] <- exp(beta0 + beta1*(1:17))
s.sc$sc4.c <- s.sc$fit

for (i in 4:20) {
  s.sc$sc4.c[i] <- s.sc$sc4.c[i-1] + s.sc$sc4[i]
}


s.sc.p <- s.sc[1:9,]
plot.s.w46 <- plot.s.stquo + geom_line(data=s.sc.p, aes(x=Week, y=sc1.c), lwd=1.5, alpha=0.8, color="dodgerblue1") + geom_line(data=s.sc.p, aes(x=Week, y=sc2.c), lwd=1.5, alpha=0.8, color="violetred1") + geom_vline(xintercept = 53, colour="firebrick", lwd=1, linetype = "longdash")
plot.s.w49 <- plot.s.w46 + geom_line(data=s.sc.p, aes(x=Week, y=sc3.c), lwd=1.5, alpha=0.8, color="dodgerblue4") + geom_line(data=s.sc.p, aes(x=Week, y=sc4.c), lwd=1.5, alpha=0.8, color="violetred4")
plot.s.w49 + theme_bw()




