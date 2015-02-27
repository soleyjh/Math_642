# JAMES HYER SOLEY
# MACHINE LEARNING
# MATH 642
# 02/04/2015
# HOMEWORK #2

# Chapter #5 Problem #2
# Part G

holder <- NULL
for (i in 1:100000){
  ans <- 1-(1-1/i)^i
  holder <- c(holder,ans)
}

plot(holder, main="Probability that jth obs in Boot Sample")

# Part H

store=rep(NA,10000)
for (i in 1:10000){
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0
}

mean(store)

# Chapter #5 Problem #8
# Part A
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)

# Part B
plot(y~x)
data <- as.matrix()

# Part C
set.seed(1)
y<-rnorm(100)
x<-rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)
simulated<-data.frame(x,y)
cv.error<-rep(0,5)
library(boot)
for (i in 1:5){
  glm.fit<-glm(y~poly(x,i), data=simulated)
  cv.error[i]<-cv.glm(simulated,glm.fit)$delta[1]
}
cv.error
sum(cv.error)

# Part D
set.seed(2)
y<-rnorm(100)
x<-rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)
simulated<-data.frame(x,y)
cv.error<-rep(0,5)
library(boot)
for (i in 1:5){
  glm.fit<-glm(y~poly(x,i), data=simulated)
  cv.error[i]<-cv.glm(simulated,glm.fit)$delta[1]
}
cv.error

# Part E
set.seed(1)
y<-rnorm(100)
x<-rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)
simulated<-data.frame(x,y)
cv.error<-rep(0,5)

for (i in 1:5){
  glm.fit<-glm(y~poly(x,i), data=simulated)
  print(i)
  print(summary(glm.fit))
  print("&&&&&&&&&&")
  cv.error[i]<-cv.glm(simulated,glm.fit)$delta[1]
}

#Chapter 5 Problem #9
#Part A

library(MASS)
boot.mean<-function(data=Boston, index=1:506){
  mean(data$medv[index])
}
set.seed(1)
mu<-boot.mean(Boston,sample(506,506,replace=T))
mu

#Part B
boot.sd<-function(data=Boston, index=1:506){
  sd(data$medv[index])
}
set.seed(1)
x<-boot.sd(Boston,sample(506,506,replace=T))
SE<-x/sqrt(506)
SE

#Part C
boot(Boston, boot.mean,1000)

# Part D
confint1<-mu-2*SE
confint2<-mu+2*SE

confint1
confint2

t.test(Boston$medv)

#Part E
boot.median<-function(data=Boston, index=1:506){
  median(data$medv[index])
}
set.seed(1)
median<-boot.median(Boston,sample(506,506,replace=T))
median

#Part F
boot(Boston, boot.median, R=1000)
quantile(Boston$medv, c(0.1))

#Part G
boot.quantile<-function(data=Boston, index=1:506){
  quantile(data$medv[index], c(0.1))
}
set.seed(1)
mu01<-boot.quantile(Boston,sample(506,506,replace=T))
mu01

# Part H
boot(Boston, boot.quantile, R=1000)