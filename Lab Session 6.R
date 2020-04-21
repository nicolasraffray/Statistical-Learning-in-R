# ---- CHAPTER 6 LAB SESSION ----

# ---- Lab 1: Subset Selection Methods ----

# ---- Libraries ----

library(leaps)
library(ISLR)
fix(Hitters)
names(Hitters)


# ---- 6.5.1 Best Subset Selection

dim(Hitters)
sum(is.na(Hitters)) # There are 59 NA values in the data set

Hitters = na.omit(Hitters)
dim(Hitters) # We can see now that the function is less 59 values

# regsubsets() function is part of the leaps lib that performs subset selecion
# for identifying the best model that contains a given no of predictors by RSS
# syntax is the same as lm()

attach(Hitters)

library(leaps)
regfit.full = regsubsets(Salary ~., Hitters)
summary(regfit.full) # asterisk indicated indicates that a given var is included in the corresponding model.

# arguement nvmax allows you to pass up to the xth best variable

regfit.full = regsubsets(Salary ~., Hitters, nvmax = 19 ) # up to the 19th best variable
reg.summary = summary(regfit.full)


# regsubsets also returns statistics to evaluate the mode 

names(regfit.full)
names(reg.summary)

# Lets look at the R^2 statistic for includieng extra variables in the model 

reg.summary$rsq

# plottign RSS, adjusted R^2, Cp and BIC

par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "No of variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "No of variables", ylab = "Adj. R^2", type = 'l')

# points command works like plot expect works on plots that are already created
# lets see where largest adjusted R^2 is

which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = 'red', cex = 2, pch = 20)

# lets have a look at our descriptive statistics 

plot(reg.summary$cp, xlab = "the number of variables", ylab = "Cp", type = 'l')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = 'red', cex = 2, pch = 20)

# now looking at BIC

plot(reg.summary$bic, xlab = "the number of variables", ylab = "BIC", type = 'l')
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = 'red', cex = 2, pch = 20)

# regsubsets has a built in plot funtion have a look at plot.regsubsets

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "bic")
plot(regfit.full, scale = "Cp")

coef(regfit.full, 6)

# The top row of each plot contains a blakc square for each variable selected according to the optimal model associated
# with that statistic. For instance, we see that several models share a BIC that is close to -150. However, the model with
# the lowest BIC is the six variable model that contains only the vars that are in the coef expression above. 

# ---- 6.5.2 Forward and backward stepwise selection ----

# we can pass into the method wether we want forward or backward stepwise selection. 

regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = 'forward')
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7) 

# in forward best one variable model contains only CBI and best 2 var model contatins that and Hits
# the best one=variable through siz-variable models are each identical for the best subset and foward selection. 

# for this dataset the best models 1-6 are the same for both backward forward and stepwise for 7 they are all different

# ---- 6.5.3 Choosing Among Models Using the Validation set approch and subset appoach ----

# In order for these approaches to yield accurate estimates of the test error, we must use 
# only the training observations to perform all aspects of model-fitting—including variable 
# selection. Therefore, the determination of which model of a given size is best must be 
# made using only the training observations. This point is subtle but important.

# We will split the data into training data and test data throught the creation of a 
# random vector which will hold true for the index which holds that values training data.
# same will be done for collecting the test data.

set.seed(1)
train = sample(c(T,F),nrow(Hitters), rep=TRUE)
test = !train

# now we will apply regsubsets to the training data to get best subset selection
regfit.best = regsubsets(Salary~., data = Hitters[train,], nvmax = 19)

# We now compute the validation set error for the best model of each model size. 
# We first make a model matrix from the test data.

test.mat = model.matrix(Salary~., data = Hitters[test,]) # this builds an "X" matrix from the data
test.mat

val.errors <- rep(NA,19)
# Now run a loop where for each size i, we extract the coefficients from regfit.best for the best
# model of that size, multiply them to the appropriate columns of the test model matrix to form the 
# prediction and compute the test MSE.

for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

# we find that the best model contains 10 variables 
which.min(val.errors)

coef(regfit.best,10)

# there is no predict method for regsubsets so lets make one

predict.regsubsets <- function(object, newdata, id,...)
  {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Note that we perform best subset selection on the full data set and 
# select the best ten- variable model, rather than simply using the variables 
# that were obtained from the training set, because the best ten-variable model 
# on the full data set may differ from the corresponding model on the training set.

regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

# we can see that the coef are different now we have include all of the data. 
# we now choose amoung the models of different sizes using cross validation. This means 
# we have to perform k fold crss valiation for each of the k training sets.

# First, we create a vector that allocates each observation to one of k = 10 folds, 
# and we create a matrix in which we will store the results.

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters),replace = T)
cv.errors <- matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# Now we write a for loop that performs cross-validation. In the jth fold, the elements 
# of folds that equal j are in the test set, and the remainder are in the training set.

for(j in 1:k){
  best.fit <- regsubsets(Salary~., data = Hitters[folds!=j,], nvmax = 19)
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds == j,], id=i)
    cv.errors[j,i] <- mean( (Hitters$Salary[folds == j] - pred)^2 ) 
  }
}

# This has given us a 10×19 matrix, of which the (i, j)th element corresponds to the test
# MSE for the ith cross-validation fold for the best j-variable model. We use the apply() 
# function to average over the columns of this matrix in order to obtain a vector for which 
# the jth element is the cross- validation error for the j-variable model.

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')
which.min(mean.cv.errors)

# we now perform regsubsets on the model which was selected as the best. 

reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best,id = 8) # these are the coefficients for the optimal model.


# ---- Lab 2: Ridge Regression and the Lasso ----

# ---- Libraries & Other info ----

library(ISLR)
attach(Hitters)
library(glmnet)
?glmnet

# This function (glmnet) has slightly different syntax from other model-fitting functions that 
# we have encountered thus far in this book. In particular, we must pass in an x matrix as well 
# as a y vector, and we do not use the y ∼ x syntax.

x <- model.matrix(Salary~., data = Hitters)[,-1]
y <- na.omit(Hitters$Salary)

# The model.matrix() function is particularly useful for creating x; not only does it produce a 
# matrix corresponding to the 19 predictors but it also automatically transforms any qualitative 
# variables into dummy variables.

# ---- Ridge Regression ----

# alpha argument that determines what type of model is fit. If alpha=0 then a ridge regression 
# model is fit, and if alpha=1 then a lasso model is fit.

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x,y,alpha = 0, lambda = grid) # automatically standerdizes

dim(coef(ridge.mod)) 
# 20 rows(one fo reach predictor, plus an intercept) 
# 100 columns one for each value of lambda

ridge.mod$lambda[50]
coef(ridge.mod)[,50] # these are the coefficients when lambda = 50
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# compare to the below
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# We can use the predict()
# lets obtain a ridge regression coefficient for a new value of lambda (50)

predict(ridge.mod, s=50, type = 'coefficients')[1:20,]

# as per normal split into training set and testing set

set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test = y[test]

# now fit a ridge regression on the training set and eval the MSE at lambda = 4
# note the use of the predict function again this time we predictions for a 
# test set, by replacing type="coefficients" with newx arg

ridge.mod = glmnet(x[train,],y[train],alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred-y.test)^2)

# if we had instead simplified the model with with just an intercept, we would have
# predicted each test obs using the mean of the training ovservations. In that case, 
# we could compute the test MSE as follows: 

mean((mean(y[train])-y.test)^2)

# we can get the same result by fitting a model with a large lambda

ridge.pred = predict(ridge.mod, s=1e10,newx=x[test, ])
mean((ridge.pred-y.test)^2)

# now lets check if there is any benefit to performing ridge reg with lambda = 4 instead
# of just performing ls reg. (recall ls reg lambda = 0)

# Note : when lambda = 0 use the arguement exact = T when using the predict(). Otherwise.
# the predict() function will interpolate over the grid of lambda vals used in fitting the
# glmnet() model, yeilding approx results

lm(y~x,subset =train)
predict(ridge.mod,s=0,exact=TRUE,type="coefficients")[1:20,]

# in general, instead of arbitratily choosing lambda = 4, it would be better to use cross-val
# to choose the tuning parameter lambda. We can do this using cv.glmnet(). By default, the function
# performs ten-fold cross val, though this can be changed with the folds arg

set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # lambda with the smallest cross validation error

# What is the MSE associated with this value of lambda?

ridge.pred <- predict(ridge.mod,s=bestlam,newx = x[test,])
mean((ridge.pred-y.test)^2)

# this represents a further improvement over the test MSE compared
# to when lambda = 4

# Finally, we refit our ridge regression model on the full data set, using the val
# of lambda that we have chosen by cross val

out <- glmnet(x,y,alpha=0)
predict(out,type = 'coefficients',s=bestlam)[1:20,]

# As expected, none of the coefficients are zero - ridge regression does not perform
# variable selection!!!

# ---- 6.6.2 The Lasso ----

# We now ask if the lasso can yeild either more accurate or more interpretable results.
# We use glmnet() with alpha = 1

lasso.mod = glmnet(x[train,],y[train],alpha =1 ,lambda = grid)
plot(lasso.mod)

# We can see from the coefficient plot that depending on the choice of the tuning parameter,
# some of the coefficients will be exactly equal to zero. We now perform corss val and compute 
# the associated test error

set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod,s=bestlam,newx = x[test,])
mean((lasso.pred-y.test)^2)

# This is substantially lower than the test MSE of the null model and of lease squares,
# and very similar to the test MSE of ridge regression lambda chosen via cross val

# The lasso has a substantial advantage over ridge regression in that the resulting 
# coefficient estimates are sparse. Here we see there are 12 of the 10 coefficient estimates
# are exactly zero. So the lasso model with lambda chosen by cross val only has 7 varialbes.

out<-glmnet(x,y,alpha=1,lambda = grid)
lasso.coef <- predict(out, type = 'coefficients',s = bestlam)[1:20,]
lasso.coef

# ---- LAB 3 PCR and PLS Regression ----

# ---- 6.7.1 Principal Components Regression ----

# PCR can be performed using pcr() which is part of the pls lib
# We will apply PCR to the Hitters data to try and predict salary

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale = T, validation = 'CV')

# syntax for pcr is slimilar to lm, CV does 10 fold cross val with error for 
# each possible value of M, the no pof principal components is used. 

summary(pcr.fit)

# The VB score is provided for each possible number of components, ranging rom M = 0 onwards.
# Note: pcr() reports the root mean squared error; in order to obtain the usual MSE we must square 
# this quantity.

# We can also plot the cross-val scores. using val.type="MSEP" will cause CV MSE to be plotted

validationplot(pcr.fit,val.type = "MSEP")

# smallest cross val error is around M = 16 components used (rougly the same as ls). We can also see that 
# 1 dimenstion has around the same error. This suggests that a model that uses a small amount of components
# may suffice. 

# summary() alos provides the percentage of variance explained in the predictors and in the prsponse using
# different numbers of components. For example M1 only captures 38.31% of all the variance, or information,
# the predictors. In contrast, using M = 6 increasesthe value ot 88.63%. If we were to use al M = p = 19 components
# we would get 100%

# Now lets perform PCR on the training data and evaluate its performance.

set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset= train, scale= TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred <- predict(pcr.fit, x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

# the test set MSE is competitive with the results obtainded using ridge regression and the lasso. However, as a
# result of the way PcR is impemented, the final model is more difficeult ot interpret bc it does not perfomr
# any kind of variable selection ro even directly produce coefficient estimates. 

# finally lets fit pcr on the full data set using M=7 (no of comp identified by cross val)

pcr.fit <- pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# ---- 6.72 Partial Least Squares ----

# The function used for this is also part of the pls library

set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters,subset=train, scale = T, validation = "CV")
summary(pls.fit)

# The lowest cross-validation error occurs when only M=2 partial ls are used. We now eval the corresponding test
# set MSE

pls.pred <- predict(pls.fit,x[test,],ncomp = 2)
mean((pls.pred=y.test)^2)

# The test MSE is comparable ot the MSE in lasso, ridge and PCR

# Finally we perform PLS using the full data set, using M=2, the number of components identified by cross val

pls.fit <- plsr(Salary~.,data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)

# Notice that the percentage of variance in Salary that the two-component PLS fit explains, 46.40%, is almost
# as much as that explained using the final seven-component model PcR fit, 46.69%. This is bc PCR only attempts to maximize the 
# amount of variance eplained in the predictors, while PLS serches for directions that explain varince in both the predictors
# and the response