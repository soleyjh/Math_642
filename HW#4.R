# JAMES HYER SOLEY
# MACHINE LEARNING
# MATH 642
# 02/18/2015
# HOMEWORK #4

# CHAPTER #7
# QUESTION #9
# PART A
library(ISLR)
library(MASS)
head(Boston)

par(mfrow=c(1,1))
fit <- lm(dis~poly(nox,4), data=Boston)
plot(Boston$nox,Boston$dis, xlab="nox", ylab="dis", main="Cubic Fit")
summary(fit)
lines(Boston$nox,fit$fitted, col="red")

# PART B
par(mfrow=c(2,5))
error <- NULL
for (i in 1:10){
  fit <- lm(dis~poly(nox,i), data= Boston)
  plot(Boston$nox,Boston$dis, xlab="nox", ylab="dis", main="Polynomial Fit")
  summary(fit)
  lines(Boston$nox,fit$fitted, col="red")
  error[i] <- fit$residuals
}

error

# PART C
fit <- lm(dis~poly(nox,4), data= Boston)
summary(fit)

fit.1 <- lm(dis~poly(nox,1), data=Boston)
fit.2 <- lm(dis~poly(nox,2), data=Boston)
fit.3 <- lm(dis~poly(nox,3), data=Boston)
fit.4 <- lm(dis~poly(nox,4), data=Boston)

anova(fit.1,fit.2,fit.3,fit.4)

# PART D
library(splines)
dim(bs(Boston$nox,df=4))
attr(bs(Boston$nox,df=4),"knots")

fit <- lm(dis~bs(nox,knots=c(.538)), data=Boston)
summary(fit)

par(mfrow=c(1,1))
plot(Boston$nox,Boston$dis, xlab="nox", ylab="dis", main="Regression Spline")
lines(Boston$nox,fit$fitted, col="red")

# PART E
par(mfrow=c(1,1))
plot(Boston$nox,Boston$dis,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Boston$nox,Boston$dis,df=2)
fit2 <- smooth.spline(Boston$nox,Boston$dis,df=50)

rss.1 <- sum((Boston$dis-fit$y)^2)
rss.1

rss.2 <- sum((Boston$dis-fit2$y)^2)
rss.2

lines(fit,col="red",lwd=2)
lines(fit2,col="green",lwd="2")

# PART F
fit3 <- smooth.spline(Boston$nox,Boston$dis,cv=TRUE)
fit3$df
lines(fit,col="red",lwd=2)
lines(fit2,col="green",lwd=2)
lines(fit3,col="blue",lwd=2)
legend("topright",legend=c("2 DF","50 DF", "4.12"),col=c("red","green","blue"), lty=1, lwd=2, cex)
rss.3 <- sum((Boston$dis-fit3$y)^2)

# CHAPTER #7
# QUESTION #11
# PART A
library(ISLR)
head(Auto)
mysample <- Auto[sample(1:nrow(Auto), 100, replace=FALSE),]
y <- mysample$mpg
x1 <- mysample$horsepower
x2 <- mysample$weight

# PART B
beta1 <- 10

# PART C
a <- y-beta1*x1
beta2 <- lm(a~x2)$coef[2]

# PART D
a <- y-beta2*x2
beta1 <- lm(a~x1)$coef[2]

# PART E
holder <- matrix(c(10,-.39),1,2)
for (i in 1:1000){
  a <- y-beta1*x1
  beta2 <- lm(a~x2)$coef[2]
  a <- y-beta2*x2
  beta1 <- lm(a~x1)$coef[2]
  betanew <- c(beta1,beta2)
  holder <- rbind(holder,betanew)
}

debug(holder)

par(mfrow=c(1,2))
plot(holder[,1])
plot(holder[,2])

holder[1001,1]
holder[1001,2]

# PART F
reg.1 <- lm(y~x1+x2)
summary(reg.1)

# CHAPTER #8
# QUESTION #9
# PART A
library(ISLR)
head(OJ)

set.seed(10)
train <- sample(1:nrow(OJ),800)
test <- (-train)
OJ.test <- OJ[test,]
OJ.train <- OJ[train,]

nrow(OJ)
nrow(OJ.train)
nrow(OJ.test)

# PART B
tree.OJ <- tree(Purchase~.,data=OJ,subset=train)
summary(tree.OJ)

# PART C
tree.OJ

# PART D
par(mfrow=c(1,1))
plot(tree.OJ)
text(tree.OJ)

# PART E
tree.predict <- predict(tree.OJ,OJ.test,type="class")
table(tree.predict,OJ.test$Purchase)
(22+27)/270

# PART F
cv.OJ<- cv.tree(tree.OJ,FUN=prune.misclass)
names(cv.OJ)
cv.OJ

# PART G
par(mfrow=c(1,1))
plot(cv.OJ$size,cv.OJ$dev,type="b")

# PART I
prune.OJ <- prune.misclass(tree.OJ,best=5)

# PART J
summary(prune.OJ)

# PART K
tree.prune <- predict(prune.OJ,OJ.test,type="class")
table(tree.prune,OJ.test$Purchase)
(25+26)/270

# CHAPTER #8
# QUESTION #10
# PART A
library(ISLR)
nrow(Hitters)
ncol(Hitters)

Hitters <- na.omit(Hitters)
nrow(Hitters)
ncol(Hitters)

Hitters$log.sal <- log(Hitters$Salary)

# PART B
set.seed(10)
train <- sample(1:nrow(Hitters),200)
test <- (-train)
Hitters.test <- Hitters[test,]
Hitters.train <- Hitters[train,]

nrow(Hitters.train)
nrow(Hitters.test)

# PART C
library(gbm)
lambda <- NULL
MSE.holder <- NULL
for (i in seq(.1,1,.1)){
  boost.hitters <- gbm(log.sal~.-Salary,data=Hitters[train,],distribution="gaussian",n.trees=1000,interaction.depth=4, shrinkage=i,verbose=F)
  lambda <- c(lambda,i)
  MSE <- mean((boost.hitters$fit-Hitters.train$log.sal)^2)
  MSE.holder <- c(MSE.holder,MSE)
}

plot(MSE.holder~lambda, main="Training", ylab="MSE")

# PART D
library(gbm)
lambda <- NULL
MSE.holder <- NULL
for (i in seq(.1,1,.1)){
  boost.hitters <- gbm(log.sal~.-Salary,data=Hitters[train,],distribution="gaussian",n.trees=1000,interaction.depth=4, shrinkage=i,verbose=F)
  lambda <- c(lambda,i)
  yhat.boost <- predict(boost.hitters,newdata=Hitters[test,],n.trees=1000)
  MSE <- mean((yhat.boost-Hitters.test$log.sal)^2)
  MSE.holder <- c(MSE.holder,MSE)
}

plot(MSE.holder~lambda, main="Test", ylab="MSE")

# PART E
boost.hitters <- gbm(log.sal~.-Salary,data=Hitters[train,],distribution="gaussian",n.trees=1000,interaction.depth=4, shrinkage=.2,verbose=F)
yhat.boost <- predict(boost.hitters,newdata=Hitters[test,],n.trees=1000)
MSE <- mean((yhat.boost-Hitters.test$log.sal)^2)
MSE

lm.hitters <- lm(log.sal~.-Salary,data=Hitters)
yhat.lm <- predict(lm.hitters,newdata=Hitters[test,])
mean((yhat.lm-Hitters.test$log.sal)^2)

library(pls)
pcr.hitters <- pcr(log.sal~.-Salary,data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.hitters)
yhat.pcr <- predict(pcr.hitters,Hitters[test,],ncomp=3)
mean((yhat.pcr-Hitters.test$log.sal)^2)

# PART F
summary(boost.hitters)

# PART G
library(randomForest)
bag.hitters <- randomForest(log.sal~.-Salary,data=Hitters,subset=train,mtry=6,importance=TRUE)
bag.hitters

yhat.bag <- predict(bag.hitters,newdata=Hitters[test,])
plot(yhat.bag,Hitters.test$log.sal)
abline(0,1)
mean((yhat.bag-Hitters.test$log.sal)^2)