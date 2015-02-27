# JAMES HYER SOLEY
# MACHINE LEARNING
# MATH 642
# 01/28/2015
# HOMEWORK #1

# Chapter #2 Problem #10
# Part a
library(MASS)
Boston <- Boston
?Boston
head(Boston)

#Part B
plot(Boston)

#Part C
cor(Boston)

#Part D
summary(Boston)
par(mfrow=c(2,2))
plot(Boston$crim, main="Crim v. Suburb")
plot(Boston$tax, main="Tax v. Suburb")
plot(Boston$ptratio, main="PtRatio v. Suburb")

par(mfrow=c(1,1))
boxplot(Boston,main="Boxplots of all Predictors", range=1000)
table <- matrix(0,1,2)
table <- rbind(c(min(Boston$crim),max(Boston$crim)))
table <- rbind(table,c(min(Boston$zn),max(Boston$zn)))
table <- rbind(table,c(min(Boston$indus),max(Boston$indus)))
table <- rbind(table,c(min(Boston$chas),max(Boston$chas)))
table <- rbind(table,c(min(Boston$nox),max(Boston$nox)))
table <- rbind(table,c(min(Boston$rm),max(Boston$rm)))
table <- rbind(table,c(min(Boston$age),max(Boston$age)))
table <- rbind(table,c(min(Boston$dis),max(Boston$dis)))
table <- rbind(table,c(min(Boston$rad),max(Boston$rad)))
table <- rbind(table,c(min(Boston$tax),max(Boston$tax)))
table <- rbind(table,c(min(Boston$ptratio),max(Boston$ptratio)))
table <- rbind(table,c(min(Boston$black),max(Boston$black)))
table <- rbind(table,c(min(Boston$lstat),max(Boston$lstat)))
table <- rbind(table,c(min(Boston$medv),max(Boston$medv)))
table

#Part E
charles <- Boston$chas[Boston$chas == 1]
length(charles)

#Part F
median(Boston$ptratio)

#Part G
medv.min <- min(Boston$medv)
printrows <- Boston[Boston$medv==medv.min,]
printrows

#Part H
printrows.2 <- Boston[Boston$rm>7,]
printrows.2

printrows.3 <- Boston[Boston$rm>8,]
printrows.3

#Chapter #3 Question #8
library(ISLR)

#Part A
Auto$horse.char <- as.character(Auto$horsepower)
Auto.clean <- Auto[Auto$horse.char != "?",]
Auto.clean$horse.num <- as.numeric(Auto.clean$horse.char)
auto.reg<-lm(Auto.clean$mpg~Auto.clean$horse.num)
summary(auto.reg)

#Predict

a <- seq(as.Date(tail(x, 1)$Date), by="month", length=5)
a <- data.frame(Date = a)

#data <- data.frame(horse.num=98)
predict(auto.reg, newdata=data.frame(horse.num=98), interval="confidence")

#Part B
plot(Auto.clean$mpg~Auto.clean$horse.num)
abline(auto.reg$coef)

#Part C
par(mfrow=c(2,2))
plot(auto.reg)

#Chapter #3 Question #15
#Part A

library(MASS)
reg.zn <- lm(crim~zn, data=Boston)
  summary(reg.zn)
reg.indus <- lm(crim~indus, data=Boston)
  summary(reg.indus)
reg.chas <- lm(crim~chas, data=Boston)
  summary(reg.chas)
reg.nox <- lm(crim~nox, data=Boston)
  summary(reg.nox)
reg.rm <- lm(crim~rm, data=Boston)
  summary(reg.rm)
reg.age <- lm(crim~age, data=Boston)
  summary(reg.age)
reg.dis <- lm(crim~dis, data=Boston)
  summary(reg.dis)
reg.rad <- lm(crim~rad, data=Boston)
  summary(reg.rad)
reg.tax <- lm(crim~tax, data=Boston)
  summary(reg.tax)
reg.ptratio <- lm(crim~ptratio, data=Boston)
  summary(reg.ptratio)
reg.black <- lm(crim~black, data=Boston)
  summary(reg.black)
reg.lstat <- lm(crim~lstat, data=Boston)
  summary(reg.lstat)
reg.medv <- lm(crim~medv, data=Boston)
  summary(reg.medv)

#Part B
reg.multiple <- lm(crim~., data=Boston)
summary(reg.multiple)

#Part C
table <- matrix(0,1,1)
table <- rbind(reg.zn$coef[2])
table <- rbind(table,reg.indus$coef[2])
table <- rbind(table,reg.chas$coef[2])
table <- rbind(table,reg.nox$coef[2])
table <- rbind(table,reg.rm$coef[2])
table <- rbind(table,reg.age$coef[2])
table <- rbind(table,reg.dis$coef[2])
table <- rbind(table,reg.rad$coef[2])
table <- rbind(table,reg.tax$coef[2])
table <- rbind(table,reg.ptratio$coef[2])
table <- rbind(table,reg.black$coef[2])
table <- rbind(table,reg.lstat$coef[2])
table <- rbind(table,reg.medv$coef[2])

par(mfrow=c(1,1))
plot(table, reg.multiple$coef[2-14], pch=20, col="red",xlab="Univariates",ylab="Multivariates", main="Uni v. Multi" )

# Part D
library(MASS)
poly.zn <- lm(crim~poly(zn,3), data=Boston)
summary(poly.zn)
poly.indus <- lm(crim~poly(indus,3), data=Boston)
summary(poly.indus)
poly.chas <- lm(crim~poly(chas,3), data=Boston)
summary(poly.chas)
poly.nox <- lm(crim~poly(nox,3), data=Boston)
summary(poly.nox)
poly.rm <- lm(crim~poly(rm,3), data=Boston)
summary(poly.rm)
poly.age <- lm(crim~poly(age,3), data=Boston)
summary(poly.age)
poly.dis <- lm(crim~poly(dis,3), data=Boston)
summary(poly.dis)
poly.rad <- lm(crim~poly(rad,3), data=Boston)
summary(poly.rad)
poly.tax <- lm(crim~poly(tax,3), data=Boston)
summary(poly.tax)
poly.ptratio <- lm(crim~poly(ptratio,3), data=Boston)
summary(poly.ptratio)
poly.black <- lm(crim~poly(black,3), data=Boston)
summary(poly.black)
poly.lstat <- lm(crim~poly(lstat,3), data=Boston)
summary(poly.lstat)
poly.medv <- lm(crim~poly(medv,3), data=Boston)
summary(poly.medv)

#Problem #1 Check

x <- c(3.2, 2.3, 1.0)
y <- c(7.6,5.4,3.1)

reg.p1 <- lm(y~x)
summary(reg.p1)

mean(x)
mean(y)

anova(reg.p1)