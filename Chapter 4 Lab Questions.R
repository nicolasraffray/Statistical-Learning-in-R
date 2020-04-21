# Chapter 4 Lab Questions

# ---- Setting up working directory and Library Importing ----

setwd("/Users/nicolasraffray/Desktop/MSc/R Programming/R Statistical Learning/Chapter 4")

library(MASS)
library(ISLR)
library(class)

# ---- Question 10 ----

dim(Weekly)
?Weekly

# !!!! Question 10a !!!!

# (a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be
#     any patterns?

summary(Weekly)

pairs(Weekly)

# there do not appear to be anny patterns in the graphical summaries


# !!!! Question 10b !!!!

# (b) Use the full data set to perform a logistic regression with Direction as the response and the five 
#     lag variables plus Volume as predictors. Use the summary function to print the results. 
#     Do any of the predictors appear to be statistically significant? If so, which ones?

attach(Weekly)

log.fit <- glm(Direction ~ Lag1  + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Weekly)
summary(log.fit)

# The intercept is significant and the lag2 is mildy significant. Significant at a low level.

# !!!! Question 10c !!!!

# (c) Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion
#    matrix is telling you about the types of mistakes made by logistic regression.

log.prob <- predict(log.fit, type = 'response')

length(log.prob)
log.pred <- rep("Down", length(log.prob))
log.pred[log.prob > 0.5] <- "Up"

# Confution table
table(log.pred, Direction)

mean(log.pred == Direction)
mean(log.pred != Direction) # training error rate is 44% which is very high 

# We could also say that for weeks when the market goes up, the model is right 92.0661157% of the time 
# (557/(48+557)). For weeks when the market goes down, the model is right only 11.1570248% of the time 
# (54/(54+430)).

# !!!! Question 10d !!!!

# (d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 
#     as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions 
#     for the held out data (that is, the data from 2009 and 2010).


train <- Year < 2008
test <- Weekly[!train,]
Direction.2010 <- Direction[!train]

glm.2008 <- glm(Direction ~ Lag2, subset = train, data = Weekly, family = 'binomial')
summary(glm.fit)

glm.prob <- predict(glm.2008, test ,type = 'response')

glm.pred <- rep("Down", length(glm.prob))
glm.pred[glm.prob > 0.5] <- "Up"


table(glm.pred, Direction.2010)

mean(glm.pred == Direction.2010) # 55% success rate
mean(glm.pred != Direction.2010) # 45% Error rate


# !!!! Question 10e !!!!

# (e) Repeat (d) using LDA

lda.2008 <- lda(Direction ~ Lag2, subset = train, data = Weekly)
lda.2008

lda.pred <- predict(lda.2008, test, type = 'response')

table(lda.pred$class, Direction.2010)

mean(lda.pred$class == Direction.2010) # 55% success rate
mean(lda.pred$class != Direction.2010) # 45% Error rate

# !!!! Question 10f !!!!

# (f) Repeat (d) using QDA.

qda.fit <- qda(Direction ~ Lag2, subset = train, data = Weekly)
qda.fit

qda.probs <- predict(qda.fit, test)$class

table(qda.probs, Direction.2010)
mean(qda.probs == Direction.2010)
mean(qda.probs != Direction.2010)

# !!!! Question 10g !!!!

# (g) Repeat (d) using KNN with K = 1.

# Note you have to use the as.matrix() method
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]


set.seed(1)
knn.fit <- knn(train.X, test.X, train.Direction, k = 1) 
knn.fit

table(knn.fit, Direction.2010)
mean(knn.fit == Direction.2010)
mean(knn.fit != Direction.2010)

# In this case, we may conclude that the percentage of correct predictions on the test data is 50%. 
# In other words 50% is the test error rate. We could also say that for weeks when the market goes up, 
# the model is right 50.8196721% of the time. For weeks when the market goes down, the model is right 
# only 48.8372093% of the time.

# (h) Which of these methods appears to provide the best results on this data?

# the best results are the results with the lowest error rates which are logistic regression and lda
# followed by qda and knn with the highest error rates




# ---- QUESTION 11 ----

# This problem involves predicting whether a car will have high mpg or not from the auto data set

# !!!! Question 11a !!!!

# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, 
#     and a 0 if mpg contains a value below its median. You can compute the median using the median() 
#     function. Note you may find it helpful to use the data.frame() function to create a single data 
#     set containing both mpg01 and the other Auto variables.

attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)


# !!!! Question 11b !!!!

# (b) Explore the data graphically in order to investigate the associ- ation between mpg01 and the other
#     features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots 
#     and boxplots may be useful tools to answer this question. Describe your findings.


pairs(Auto)
plot(acceleration, mpg)
plot(horsepower, mpg)
plot(displacement, mpg)
plot(cylinders, mpg)

# All features look as though they could be reasonable predictors asside from acceleration
# which looks a little more random 

# !!!! Question 11c !!!!

# (c) Split the data into a training set and a test set.

# Lets try splitting training and testing to odd and even years
train <- (year %% 2 == 0)
auto.train <- Auto[train, ]
auto.test <- Auto[!train, ]
mpg01.test <- mpg01[!train]

dim(auto.train)
dim(auto.test)

# 55% to 45% split 

# !!!! Question 11d !!!!

# (d) Perform LDA on the training data in order to predict mpg01 using the variables that 
#     seemed most associated with mpg01 in (b). What is the test error of the model obtained?

fit.lda <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.lda

lda.pred <- predict(fit.lda, auto.test)
lda.class <- lda.pred$class

table(lda.pred$class, mpg01.test)
mean(lda.class != mpg01.test) # 12% Error Rate

# !!!! Question 11e !!!!

# (e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most 
#     associated with mpg01 in (b). What is the test error of the model obtained?
  
qda.fit <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
qda.fit

qda.pred <- predict(qda.fit,auto.test)$class

table(qda.pred, mpg01.test)
mean(qda.pred != mpg01.test) # Error rate of 13.1%

# !!!! Question 11e !!!!

# f) Perform logistic regression on the training data in order to pre-dict mpg01 using the variables 
#    that seemed most associated with mpg01 in (b). What is the test error of the model obtained?

log.fit <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train,
               family = binomial)

summary(log.fit) # significant coefficients are weight and horsepower 

log.pred <- predict(log.fit,auto.test, type = 'response')
log.prob <- rep(0, length(log.pred))
log.prob[log.pred > 0.5] <- 1

table(log.prob, mpg01.test)
mean(log.prob != mpg01.test) # Error Rate : 12.08% 

# !!!! QUESTION 11g !!!!

# (g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only 
#     the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? 
#     Which value of K seems to perform the best on this data set?

train.x <- cbind(cylinders, weight, displacement, horsepower)[train,]
test.x <- cbind(cylinders, weight, displacement, horsepower)[!train,]
train.mpg <- mpg01[train]

knn.fit <- knn(train.x, test.x, train.mpg, k = 1)

table(knn.fit, mpg01.test)
mean(knn.fit != mpg01.test) # 15.4% Error Rate

# Lets try a slightly higher K @ 3 and see what we get

knn.fit <- knn(train.x, test.x, train.mpg, k = 3)

table(knn.fit, mpg01.test)
mean(knn.fit != mpg01.test) # Test performs much better at a 13.7 Error Rate


# Hence of the tests Logistic Regression and LDA performed the best


# ---- QUESTION 12 ----

# !!!! QUESTION 12a !!!!

# (a) Write a function, Power(), that prints out the result of raising 2 to the 3rd power. In other words, 
#     your function should compute 23 and print out the results.
#     Hint: Recall that x^a raises x to the power a. Use the print() function to output the result.

Power <- function(){print(2^3)}
Power()

# !!!! QUESTION 12b !!!!

# (b) Create a new function, Power2(), that allows you to pass any two numbers, x and a, and prints out 
#     the value of x^a. You can do this by beginning your function with the line

Power2 <- function(x,y){
  x^y
}
Power2(4,5)

# !!!! QUESTION 12c !!!!

# (c) Using the Power2() function that you just wrote, compute 10^3, 8^17, and 131^3.

Power2(10,3)
Power2(8,17)
Power2(131,3)

# !!!! QUESTION 12d !!!!

# (d) Now create a new function, Power3(), that actually returns the result x^a as an R object, rather 
#     than simply printing it to the screen. That is, if you store the value x^a in an object called 
#     result within your function, then you can simply return() this result, using the following line:
#     return(result)


Power3 <- function(x,a){
  result = x^a
  return(result)
}

# !!!! QUESTION 12e !!!!

# (e) Now using the Power3() function, create a plot of f(x) = x^2. The x-axis should display a range 
#     of integers from 1 to 10, and the y-axis should display x2. Label the axes appropriately, 
#     and use an appropriate title for the figure. Consider displaying either the x-axis, the y-axis, 
#     or both on the log-scale. You can do this by using log=‘‘x’’, log=‘‘y’’, or log=‘‘xy’’ as
#     arguments to the plot() function.

x <- 1:10
plot(x, Power3(x,2), log = 'xy', xlab = "Log of x", ylab = "Log of x^2", main = "Log of x^2 vs Log of x")

# !!!! QUESTION 12f !!!!

# (f) Create a function, PlotPower(), that allows you to create a plot of x against x^a for a fixed a 
#     and for a range of values of x. For instance, if you call
#     > PlotPower (1:10 ,3)
#     then a plot should be created with an x-axis taking on values 1,2,...,10, and a y-axis taking on 
#     values 13,23,...,103.


PlotPower <- function(x,a){
  n <- x^a
  plot(x,n)
}
PlotPower(seq(-10,10,length.out = 50),3)

# ---- QUESTION 13 ----

# Using the Boston data set, fit classification models in order to predict whether a given suburb has a 
# crime rate above or below the median. Explore logistic regression, LDA, and KNN models using various 
# sub-sets of the predictors. Describe your findings.

head(Boston)
?Boston
attach(Boston)
set.seed(1)
# Creating a test and training set

crim01 <- rep(0, length(crim))
crim01[crim > median(crim)] <- 1
Boston <- data.frame(Boston, crim01)

train <- 1:(0.5 * length(crim))
test <- (0.5*length(crim) + 1):length(crim)
bos.train <- Boston[train,]
bos.test <- Boston[test,]
crim01.test <- crim01[test]


# Logistic Regression

log.fit <- glm(crim01 ~. - crim - crim01.1, family = binomial, data = Boston, subset = train)
summary(log.fit)


log.probs <- predict(log.fit, bos.test, type = 'response')
log.pred <- rep(0,length(log.probs))
log.pred[log.probs > 0.5] <- 1

table(log.pred, crim01.test)
mean(log.pred != crim01.test) # 18.2% Error Rate

# Lets remove the variables that aren't significant

log.fit <- glm(crim01 ~. - crim - crim01.1 - nox -tax -chas -lstat , family = binomial, data = Boston, 
               subset = train)
summary(log.fit) # all variables in the model are significant

log.probs <- predict(log.fit, bos.test, type = 'response')
log.pred <- rep(0,length(log.probs))
log.pred[log.probs > 0.5] <- 1

table(log.pred, crim01.test)
mean(log.pred != crim01.test) # 15.8% Error Rate

# Linear Discriminant Analysis 

lda.fit <- lda(crim01 ~. -crim - crim01.1 - nox - tax - chas - lstat, data = Boston, 
               subset = train)
lda.fit

lda.pred <- predict(lda.fit, bos.test)$class

table(lda.pred, crim01.test)
mean(lda.pred != crim01.test) # 14.6% Error Rate 

# Quatdratic Distcriminant Analysis

qda.fit <- qda(crim01 ~. -crim - crim01.1 - nox - tax - chas - lstat, data = Boston, 
               subset = train)
qda.fit

qda.pred <- predict(qda.fit, bos.test)$class

table(qda.pred, crim01.test)
mean(qda.pred != crim01.test) # 66% Error Rate

# K Nearest Neighbours 

train.x <- cbind(zn, indus, rm, age, dis, rad, ptratio, black, medv)[train,]
test.x <- cbind(zn, indus, rm, age, dis, rad, ptratio, black, medv)[test,]
train.crim <- crim01[train]

knn.fit <- knn(train.x, test.x,train.crim, k = 1)

table(knn.fit, crim01.test)
mean(knn.fit != crim01.test) # 31% Error Rate

# Lets push up K

knn.fit <- knn(train.x, test.x,train.crim, k = 3)

table(knn.fit, crim01.test)
mean(knn.fit != crim01.test) # 22% Error Rate at K = 3




