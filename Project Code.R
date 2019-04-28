######################
### math 285 final ###
######################


#### input data ####
#### input data ####
# install.packages("RCurl")
library(RCurl)
datafile <- getURL("https://raw.githubusercontent.com/TerrySitu/The-Effects-Of-Lightning-Conditions-On-Reading-Devices/master/e_reading.txt")

data <- read.table(text = datafile, header=F)
head(data)
colnames(data) <- c("devices", "lightning", "reading")
data
mean(data[,3])
data$devices <- as.factor(data$devices)
class(data$devices)

data$lightning <- as.factor(data$lightning)
class(data$lightning)

head(data)

#### basic statistics ####

### overall mean and SD ###

mean.reading.time <- mean(data$reading)
sd.reading.time <- sd(data$reading)

### devices mean and SD ###

mean.reading.time.devices <- NULL
sd.reading.time.devices <- NULL

for (i in 1:3){
	mean.reading.time.devices[i] <- mean(subset(data, devices==i)$reading)
	sd.reading.time.devices[i] <- sd(subset(data, devices==i)$reading)
}

mean.reading.time.devices[1:3]
sd.reading.time.devices[1:3]

### lightning mean and SD ###

mean.reading.time.lightning <- NULL
sd.reading.time.lightning <- NULL
for (i in 1:4) {
	mean.reading.time.lightning[i] <- mean(subset(data, lightning==i)$reading)
	sd.reading.time.lightning[i] <- sd(subset(data,lightning==i)$reading)
}
mean.reading.time.lightning[1:4]
sd.reading.time.lightning[1:4]

### basic plots ###
attach(data)
par(mfrow=c(1,2))
interaction.plot(lightning, devices, reading, type="l", xaxt="n",xlab="Lightning Conditon", ylab="Mean Reading Time", col=c(2:4), main='Interaction Plot of Reading Time under Different Illuminations')
axis(1, at=c(1,2,3,4), labels=c('200 lx', '500 lx', '1000 lx', '1500 lx'), )
boxplot(reading~devices, xlab="Devices", ylab="Reading", xaxt='n', col=c(2:4), main='Boxplot of Reading Time By Three E-readers')
axis(1, at=c(1,2,3), labels=c('Sony PRS 700','Amazon Kindle DX', 'iRex 1000S'))
par(mfrow=c(1,1))


### test homo variances ###
attach(data)
bartlett.test(reading~lightning)
bartlett.test(reading~devices)



#### ANOVA and RSM ####
attach(data)
aov.ereader.interaction <- aov(reading~devices+lightning+devices*lightning)
summary(aov.ereader.interaction)

aov.ereader <- aov(reading~devices+lightning)
summary(aov.ereader)
model.tables(aov.ereader)

reg.ereader.full <- lm(reading~(.)^2, data)
summary(reg.ereader.full)

reg.ereader <- lm(reading~devices+lightning)
summary(reg.ereader)

### for mixed effect model ###

library(nlme)
fit.lme <- lme(reading~lightning, data, random=~1|devices)
summary(fit.lme)
anova(fit.lme)
intervals(fit.lme)

BC <- boxcox(aov.ereader)
BC
BC$x[BC$y==max(BC$y)]



#### difference tests ####
bartlett.test(reading~lightning)

## tukeyhsd ##
TukeyHSD(aov.ereader, c("lightning"))

## LSD ##
install.packages("agricolae")
library(agricolae)
df <- df.residual(aov.ereader)
MSerror <- deviance(aov.ereader)/df
out <- LSD.test(reading, lightning, df, MSerror, group=F, alpha=.05)
out

xtabs(reading~lightning+devices)

#### related plots ####

par(mfrow=c(1,2), cex=0.8)
interaction.plot(lightning, devices, reading, type="l", xaxt="n",xlab="Lightning Conditon", ylab="Mean Reading Time", col=c(1:3))
axis(1, at=c(1,2,3,4), labels=c('200 lx', '500 lx', '1000 lx', '1500 lx'))
interaction.plot(lightning, devices, fitted(aov.ereader))
par(mfrow=c(1,1))


boxplot(reading~devices, xlab="Devices", ylab="Reading")


library(lattice)
print(dotplot(lightning~reading, data, group=devices, type=c('a','p'), auto.key=list(columns=1, line=T)))

#### residuals plots ####
opar <- par(mfrow=c(2,2), cex=.8)
plot(aov.ereader)
par(opar)
par(mfrow=c(1,1))

opar <- par(mfrow=c(2,2), cex=.8)
plot(reg.ereader)
par(opar)
par(mfrow-c(1,1))

#### transformation ####
library(MASS)
boxcox(aov.ereader)

BC <- boxcox(aov.ereader)
BC$x[BC$y==max(BC$y)]














#### fancy mean plot (not very important)####
mean <- aggregate(reading~devices, data, mean)
mean[1,2]
mean[2,2]
mean[3,2]

plot(cbind(devices,reading), pch=16, cex=1.5, xaxt="n", xlim=c(1, 4), ylab="mail", xlab="design", main="Comparing Means of Reading Time Between Ligthning Conditions")
axis(1, at = c(1,2,3), labels = c("1", "2", "3"))
abline(h=mean(reading), lwd=2, col="red")

points(cbind(c(0.9, 1.1),c(mean[1,2], mean[1,2])), cex = 0, type = "l", lwd = 3, col = "green")
points(cbind(c(1.9, 2.1),c(mean[2,2], mean[2,2])), cex = 0, type = "l", lwd = 3, col = "green")
points(cbind(c(2.9, 3.1),c(mean[3,2], mean[3,2])), cex = 0, type = "l", lwd = 3, col = "green")


arrows(x0 = 1.1,y0 = mean[1,2], x1 = 1.1, y1 = mean(reading), code = 3, col = "blue")
arrows(x0 = 2.1,y0 = mean[2,2], x1 = 2.1, y1 = mean(reading), code = 3, col = "blue")
arrows(x0 = 3.1,y0 = mean[3,2], x1 = 3.1, y1 = mean(reading), code = 3, col = "blue")

arrows(x0 = 1.1,y0 = mean[1,2], x1 = 2.1, y1 = mean[2,2], code = 2, col = "red")
arrows(x0 = 2.1,y0 = mean[2,2], x1 = 3.1, y1 = mean[3,2], code = 2, col = "red")
#################################



