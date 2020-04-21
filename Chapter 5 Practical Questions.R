# Chapter 5 Applied Exercises

# ---- Libraries and Working Directory ----

setwd("/Users/nicolasraffray/Desktop/MSc/R Programming/R Statistical Learning/Chapter 5")

library(ISLR)
library(boot)
library(MASS)
library(class)

# ---- Question 5 ----

# In Chapter 4, we used logistic regression to predict the probability of default using income 
# and balance on the Default data set. We will now estimate the test error of this logistic 
# regression model using the validation set approach. Do not forget to set a random seed before 
# beginning your analysis.

set.seed(100)

# !!!! QUESTION 5a !!!!

# (a) Fit a logistic regression model that uses income and balance to predict default.

attach(Default)
log.fit <- glm(default ~ income + balance, family = 'binomial', data = Default)
summary(log.fit)

# !!!! QUESTION 5b !!!! 

# (b) Using the validation set approach, estimate the test error of this model.

# Split into test set and validation set

dim(Default)
train <- sample(10000, 6000)
validation <- Default[-train,]

# Fitting log model using training observations & Testing

log.fit1 <- glm(default ~ income + balance, family = 'binomial', data = Default, subset = train)
summary(log.fit1)

log.prob <- predict(log.fit1, validation, type = 'response')

log.pred <- rep("No",length(log.prob))
log.pred[log.prob > 0.5] <- "Yes"

mean(log.pred == validation$default)
mean(log.pred != validation$default) # 2.575% Error Rate
table(log.pred, validation$default)

# !!!! Question 5c !!!!

# Repeat the process in (b) three times, using three different splits of the observations into 
# a training set and a validation set. Com- ment on the results obtained.

# So I'm going to make the above (b) into a function where i can pass different splits

val.model <- function(P){
  # Pass n as 0 < n < 1. Where 1 would make the model fit 100% on the training set
  train <- sample(dim(Default)[1], (dim(Default)[1]*P))
  val <- Default[-train,]
  log.fit <- glm(default ~ income + balance, family = binomial, data = Default, subset = train)
  
  log.prob <- predict(log.fit, val, type = 'response')
  log.pred <- rep("No", dim(val)[1])
  log.pred[log.prob > 0.5] <- "Yes"
  
  return(mean(log.pred != val$default))
}

# Now we pass in different sample and validation sizes note validation size is 1-P

n <- rep(0,5)
x <- seq(0.5,0.9,length.out = 5)

n[1] <- val.model(0.5) # Error: 2.46%
n[2] <- val.model(0.6) # Error: 2.52%
n[3] <- val.model(0.7) # Error: 2.76%
n[4] <- val.model(0.8) # Error: 3.25%
n[5] <- val.model(0.9) # Error: 3.1%

plot(x, n, type = 'l', col = 'red', ylab = 'Error', xlab = "Training set Percent" )

# We can see for this model the best is a training rate circa 60%

# !!!! QUESTION 5D !!!!

# (d) Now consider a logistic regression model that predicts the probability of default using 
#     income, balance, and a dummy variable for student. Estimate the test error for this model 
#     using the val- idation set approach. Comment on whether or not including a dummy variable 
#     for student leads to a reduction in the test error rate.


val.student <- function(i){
  train <- sample(dim(Default)[1], (dim(Default)[1]*i))
  val <- Default[-train,]
  
  log.fit <- glm(default ~ income + balance + student, 
                 family = binomial, data = Default, subset = train)

  log.prob <- predict(log.fit, val, type = 'response')
  log.pred <- rep("No", dim(val)[1]) 
  log.pred[log.prob > 0.5] <- "Yes"

  return(mean(log.pred != val$default))}

error.rates <- function(){
 # i is the sample size of the validation set as a percentage of the total 
  p <- 100
  i <- 1
  X <- rep(0,p)
  while(i < 100){
    N <- val.student(i/100)
    X[i] <- N
    i <- i + 1
  }
  X <- X[X!=0.00000000]
  return(X)
}
z <- error.rates()

plot(error.rates(), ylab = "Error Rate", xlab = "Index", type = 'l', 
     col = 'red')

min(z[index(z) < 80]) 
p <- min(z[index(z) < 80])
match(p,z)

# The minimum reasonable error rate for the validation method including students is 2.33%
# The weighting is 1 - 43%, so train the model on 67% of the data set for the lowest possible
# error rate.

# Generally though there is an average error rate for the validation method

mean(z) # Average Error Rate: 2.27 %

# ---- Question 6 ----

# We continue to consider the use of a logistic regression model to predict the probability 
# of default using income and balance on the Default data set. In particular, we will now 
# compute estimates for the standard errors of the income and balance logistic regression 
# co-efficients in two different ways: (1) using the bootstrap, and (2) using the standard 
# formula for computing the standard errors in the glm() function. Do not forget to set a 
# random seed before beginning your analysis.

# !!!! QUESTION 6A !!!!

# (a) Using the summary() and glm() functions, determine the esti- mated standard errors 
#     for the coefficients associated with income and balance in a multiple 
#     logistic regression model that uses both predictors.

log.fit <- glm(default ~ income + balance, family = binomial, data = Default, 
               subset = train)
summary(log.fit)

# !!!! QUESTION 6B !!!!

# (b) Write a function, boot.fn(), that takes as input the Default data set as well as 
#     an index of the observations, and that outputs the coefficient estimates for income 
#     and balance in the multiple logistic regression model.

boot.fn <- function(data, index){
  return(coef(glm(default ~ income + balance, family = binomial, data = data,
                  subset = index)))
}

boot.fn(Default, train)

# !!!! QUESTION 6C !!!!

# (c) Use the boot() function together with your boot.fn() function to estimate the standard 
#     errors of the logistic regression coefficients for income and balance.

boot(Default, boot.fn, R = 100) 

# see  t2 and t3 standard errors

# !!!! QUESTION 6D !!!!

# (d) Comment on the estimated standard errors obtained using the glm() function and using 
#     your bootstrap function.

summary(log.fit)

# Standard errors are different by some decimal places 
# The model one is less accurate as it carries assumptions whereas bootstrap does not

# ---- Question 7 ----

# In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be used in order 
# to compute the LOOCV test error estimate. Alternatively, one could compute those 
# quantities using just the glm() and predict.glm() functions, and a for loop. You will
# now take this ap- proach in order to compute the LOOCV error for a simple logistic 
# regression model on the Weekly data set. Recall that in the context of classification 
# problems, the LOOCV error is given in (5.4).

summary(Weekly)
attach(Weekly)

# !!!! QUESTION 7A !!!!

# a) Fit a logistic regression model that predicts Direction using Lag1 and Lag2.

log.fit <- glm(Direction ~ Lag1 + Lag2, family = binomial, data = Weekly)
summary(log.fit)

# (b) Fit a logistic regression model that predicts Direction using Lag1 and Lag2 
#     using all but the first observation.

log.fit1 <- glm(Direction ~ Lag1 + Lag2, family = binomial, data = Weekly[-1,])
summary(log.fit1)

# c) Use the model from (b) to predict the direction of the first observation. 
#    You can do this by predicting that the first observation will go up if 
#    P(Direction="Up"|Lag1, Lag2) > 0.5. Was this ob- servation correctly classified?

predict.glm(log.fit1, Weekly[1,], type = 'response')

# Returns as True hence it predict that the market will go up.

# !!!! QUESTION 7D !!!!

# (d) Wriaforloopfromi=1toi=n,wherenisthenumberof observations in the data set

count <- rep(0,dim(Weekly)[1])
for(i in 1:dim(Weekly)[1]){
  log.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
  log.probs <- predict.glm(log.fit, Weekly[i,], type = 'response') > 0.5
  true_up <- Weekly[i,]$Direction == "Up"
  if (log.probs != true_up){
    count[i] <- 1}
}
sum(count) # 490 errors

# !!!! QUESTION 7E !!!!

mean(count) # 45% is the estimate of the test error rate according to LOOCV




# ---- Question 8 ----

# We will now perform cross-validation on a simulated data set.

# (a) In this data set, what is n and what is p? Write out the model used to 
#     generate the data in equation form.

set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)

# n is 100 p is the number of predictors and there are 2 in the base model 

# (b) Put the above into a scatterplot what do you see? 

plot(x,y)

# Looks a bit like an inverted quadratic function (bc that is also basically what the model says)

# (c) Set a random seed, and then compute the LOOCV errors that result from fitting the 
#     following four models using least squares:

set.seed(1)
Data <- data.frame(x,y)

# i 
glm.1 <- glm(y ~ x, data = Data)
cv.err <- cv.glm(Data, glm.1)
cv.err$delta # these are the cross validation results

# ii
glm.2 <- glm(y ~ poly(x,2), data =Data)
cv.glm(Data, glm.2)$delta

# iii
glm.3 <- glm(y ~ poly(x,3), data = Data)
cv.glm(Data, glm.3)$delta

# iv
glm.4 <- glm(y ~ poly(x,4), data = Data)
cv.glm(Data, glm.4)$delta

# delta give the cross validation estimate for the test error

# !!!! Question 8D !!!!

# (d) Repeat (c) using another random seed, and report your results.
#     Are your results the same as what you got in (c)? Why?

set.seed(3)
glm.1 <- glm(y ~ x, data = Data)
cv.err <- cv.glm(Data, glm.1)
cv.err$delta # these are the cross validation results

# ii
glm.2 <- glm(y ~ poly(x,2), data =Data)
cv.glm(Data, glm.2)$delta

# iii
glm.3 <- glm(y ~ poly(x,3), data = Data)
cv.glm(Data, glm.3)$delta

# iv
glm.4 <- glm(y ~ poly(x,4), data = Data)
cv.glm(Data, glm.4)$delta

# It is the same LOOCV since n folds are a single observation.

# !!!! QUESTION 8E !!!!

# (e) Which of the models in (c) had the smallest LOOCV error? Is this what you expected?
#     Explain your answer.

# I think the best fit will be the quadratic fit to be the best and it was
# This can be seen by just looking at the data plot and the equation

# !!!! QUESTION 8F !!!!

# (f) Comment on the statistical significance of the coefficient estimates that 
#     results from fitting each of the models in (c) using least squares. 
#     Do these results agree with the conclusions drawn based on the cross-validation results?

summary(glm.2)
# The coefficients are significant in this one 
# its likely higher orders dont have significance, same with onle one predictor
# likely it will have no significance

# ---- Question 9 ----

# We will now consider the Boston housing data set, from the MASS library.

# !!!! QUESTION 9A !!!!

# (a) Based on this data set, provide an estimate for the population mean of medv. 
#    Call this estimate μˆ.

set.seed(1)
attach(Boston)

summary(Boston)
µ <- mean(Boston$medv)

# (b) Find the sample standard deviation.

sd.medv <- sd(Boston$medv) # sample standard deviation
n <- length(Boston$medv) # number of sample observations

sd.medv/(n^0.5) # 0.4

# (c) Now estimate the standard error of μˆ using the bootstrap. How does this compare to 
#     your answer from (b)?

boot.fn <- function(data,index){return(mean(data[index]))}
boot <- boot(Boston$medv, boot.fn, R = 1000) # 0.411 standard error of µ

# They are very similar the bootstrap one is a bit higher.

# (d) Based on your bootstrap estimate from (c), provide a 95 % confidence interval for 
#     the mean of medv. Compare it to the results obtained using t.test(Boston$medv).

t.test(medv)

c(boot$t0 - 2*0.411, boot$t0 + 2*0.411)
# Bootstrap estimates of quartile range is very close to that of that stated by the 
# t-test. 

# (e) Based on this data set, provide an estimate, μˆmed, for the median value of medv 
#     in the population.

median(medv)

# (f) We now would like to estimate the standarderror of μˆmed. Unfortunately, 
#     there is no simple formula for computing the standard error of the median. 
#     Instead, estimate the standard error of the median using the bootstrap. 
#     Comment on your findings.

boot.fn <- function(data, index){return(median(data[index]))}
boot(medv, boot.fn, R = 1000) 

# The standard error of the median is 0.3801 and the median is 21.2

# (g) Based on this data set, provide an estimate for the tenth percentile of medv in
#     Boston suburbs. Call this quantity μˆ0.1. (You can use the quantile() function.)

mu.01 <- quantile(medv, 0.1)
mu.01 # 12.75

# (h) Use the bootstrap to estimate the standard error of μˆ0.1. Comment on your findings.

boot.fn <- function(data, index){return(quantile(data[index], 0.1))}
boot(medv, boot.fn, R = 1000)

# mu estimate: 12.75
# mu standard error: 0.511

# A small standard error relative to the 10th percentile value.