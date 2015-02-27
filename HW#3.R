# JAMES HYER SOLEY
# MACHINE LEARNING
# MATH 642
# 02/11/2015
# HOMEWORK #3

library(ISLR)
library(leaps)
Hitters

regfit.fwd <- regsubsets(Salary~.,data=Hitters, nvmax=5, method="forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~.,data=Hitters, nvmax=5, method="backward")
summary(regfit.bwd)

# Question #8
# Part A
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100)

tail(x)
tail(eps)

# Part B
# Create the Betas
B0 <- 2
B1 <-.5
B2 <- -10.7
B3 <- 2.4

y <- B0 + B1*x + B2*x^2 + B3*x^3 + eps
tail(y)

# Part C
library(leaps)

data <- data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)
tail(data)

lm.1 <- regsubsets(y~., data=data)
bestsub <- summary(lm.1)
bestsub

bestsub$cp
bestsub$bic
bestsub$adjr2

par(mfrow=c(2,2))
plot(bestsub$cp, xlab="Number of Variables", ylab="CP")
plot(bestsub$bic, xlab="Number of Variables", ylab="bic")
plot(bestsub$adjr2, xlab="Number of Variables", ylab="Adj R^2")
plot(bestsub$rss, xlab="Number of Variables", ylab="rss")

lm.2 <- lm(y~.,data=data)
summary(lm.2)

# Part D
# Forward
lm.fwd <- regsubsets(y~., data=data, method="forward")
fwdsub <- summary(lm.fwd)
fwdsub

fwdsub$cp
fwdsub$bic
fwdsub$adjr2

par(mfrow=c(2,2))
plot(fwdsub$cp, xlab="Number of Variables", ylab="CP")
plot(fwdsub$bic, xlab="Number of Variables", ylab="bic")
plot(fwdsub$adjr2, xlab="Number of Variables", ylab="Adj R^2")
plot(fwdsub$rss, xlab="Number of Variables", ylab="rss")

coef(lm.fwd,8)

# Backward
lm.bwd <- regsubsets(y~., data=data, method="backward")
bwdsub <- summary(lm.bwd)
bwdsub

bwdsub$cp
bwdsub$bic
bwdsub$adjr2

par(mfrow=c(2,2))
plot(bwdsub$cp, xlab="Number of Variables", ylab="CP")
plot(bwdsub$bic, xlab="Number of Variables", ylab="bic")
plot(bwdsub$adjr2, xlab="Number of Variables", ylab="Adj R^2")
plot(bwdsub$rss, xlab="Number of Variables", ylab="rss")

coef(lm.bwd,8)

# Part E
# Set Environment Conditions
library(glmnet)
set.seed(1)
par(mfrow=c(1,1))

# Create Model Data Set
x <- model.matrix(y~.,data)[,-1]
y <- data$y[!is.na(data$y)]

# Create Grid
grid <- 10^seq(10,-2,length=100)
lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)

# Create Test / Train Data Sets
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# Use CV to produce Optimal Lambda
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <-cv.out$lambda.min
bestlam

# Run Lasso on Test Data set and produce MSE
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

# Test to make sure it performed Variable Selection as Lasso does
out <- glmnet(x,y,alpha=1)
predict(out,type="coefficients", s=bestlam)[1:10,]

# Part F
# Create New Model
B0 <- 2.17283
B7 <- -0.15569

# ReSet our Model
y <- B0 + B7*x^7
data <- data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)

# Best Subsets
lm.best <- regsubsets(y~., data=data)
bestsub <- summary(lm.best)
bestsub

bestsub$cp
bestsub$bic
bestsub$adjr2

par(mfrow=c(2,2))
plot(bestsub$cp, xlab="Number of Variables", ylab="CP")
plot(bestsub$bic, xlab="Number of Variables", ylab="bic")
plot(bestsub$adjr2, xlab="Number of Variables", ylab="Adj R^2")
plot(bestsub$rss, xlab="Number of Variables", ylab="rss")

# LASSO
# Create Model Data Set
x <- model.matrix(y~.,data)[,-1]
y <- data$y[!is.na(data$y)]

# Create Grid
grid <- 10^seq(10,-2,length=100)
lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)

# Create Test / Train Data Sets
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# Use CV to produce Optimal Lambda
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <-cv.out$lambda.min
bestlam

# Run Lasso on Test Data set and produce MSE
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

# Test to make sure it performed Variable Selection as Lasso does
out <- glmnet(x,y,alpha=1)
predict(out,type="coefficients", s=bestlam)[1:11,]

# Question #9
# Part A
# Set Environment Conditions
library(glmnet)
set.seed(1)
par(mfrow=c(1,1))

# Create Model Data Set
x <- model.matrix(Apps~.,College)[,-1]
y <- College$Apps[!is.na(College$Apps)]

# Create Test / Train Data Sets
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
x.test <- x[test,]
y.test <- y[test]

# Part B
lm.train <- lm(y[train]~x[train,])
lm.pred <- predict(lm.train,newx=x.test,interval="confidence")
mean((lm.pred-y.test)^2)

# Part C
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)

# Use CV to produce Optimal Lambda
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam <-cv.out$lambda.min
bestlam

# Run Ridge on Test Data set and produce MSE
ridge.pred <- predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Test to make sure it didn't performe Variable Selection
out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients", s=bestlam)[1:17,]

# Part D
grid <- 10^seq(10,-2,length=100)
lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)

# Use CV to produce Optimal Lambda
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <-cv.out$lambda.min
bestlam

# Run Ridge on Test Data set and produce MSE
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Test to make sure it didn't performe Variable Selection
out <- glmnet(x,y,alpha=1)
predict(out,type="coefficients", s=bestlam)[1:17,]

# Part E PCR Model
library(pls)
set.seed(1)
pcr.fit <- pcr(Apps~., data=College, subset=train, scale=TRUE, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred <- predict(pcr.fit,x[test,],ncomp=5)
mean((pcr.pred-y.test)^2)

# Part E PLS Model
library(pls)
set.seed(1)
pls.fit <- plsr(Apps~., data=College, subset=train, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred <- predict(pls.fit,x[test,],ncomp=6)
mean((pcr.pred-y.test)^2)
