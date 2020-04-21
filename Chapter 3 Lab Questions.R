# CHAPTER 3 LAB EXERCISES

# ---- Setting the Directory & Libraries----
setwd('/Users/nicolasraffray/Desktop/MSc/R Programming/R Statistical Learning/Chapter 3')
Libraries <- function(){
  library(ISLR)
  library(MASS)
  print('The relevant libraries have been attached')}
Libraries()

# ---- Question 8 ----
# This question involves the use of simple linear regression on the Auto data set.

# a) Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the 
# predictor. Use the summary() function to print the results. Comment on the output. For example:

attach(Auto)
lm.fit1 <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit1)

#  i. Is there a relationship between the predictor and the response?

# From the regression there seems to be a significant linear relationship between the relationship appears to be negative

#  ii. How strong is the relationship between the predictor and the response?

# There is a 15% negative relationship 

#  iii. Is the relationship between the predictor and the response positive or negative?

# The relationship is negative

# iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?

names(lm.fit1)

confint(lm.fit1, level = 0.96)
confint(lm.fit1, level = 0.90)

predict(lm.fit1, data.frame(horsepower = c(2,5)), interval = 'confidence')

# !!!! QUESTION 8B !!!!

# (b) Plot the response and the predictor. Use the abline() function to display the least squares regression line.

plot(horsepower, mpg)
abline(lm.fit1, lwd = 3, col = "red")

# !!! Question 8C !!!

# Use the plot() function to produce diagnostic plots of the least squares regression fit.
# Comment on any problems you see with the fit.

par(mfrow = c(2,2))
plot(lm.fit1)

# ---- Question 9 ----
# This question involves the use of multiple linear regression on the Auto data set.

# !!! Question 9a !!!

# (a) Produce a scatterplot matrix which includes all of the variables in the data set.

pairs(Auto)

# !!! Question 9b !!!

# (b) Compute the matrix of correlations between the variables using the function cor(). 
# You will need to exclude the name variable, which is qualitative.

Auto <- Auto[,-9]
cor(Auto)

# !!! Question 9c !!!

# (c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables 
#     except name as the predictors. Use the summary() function to print the results. Comment on the output. For instance:
#  i. Is there a relationship between the predictors and the re- sponse?
#  ii. Which predictors appear to have a statistically significant relationship to the response?
#  iii. What does the coefficient for the year variable suggest?

# Auto names were already removed but otherwise the function would look like this lm(mpg~.-name,data = Auto)

lm.fit2 <- lm(mpg~.,data = Auto)
summary(lm.fit2)

# i. There is a relationship between most of the predictors and the response
# ii. The statistically significant predictors are the intercept, displacement, weight, year and origin
# iii. The newer the car the better the miles per gallon

# !!! Qustion 9d !!!

# (d) Use the plot() function to produce diagnostic plots of the linear regression fit. 
#     Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? 
#     Does the leverage plot identify any observations with unusually high leverage?

plot(lm.fit2)

# There seems to be a quadratic relationship between the residualas and the fitted values, there are also some outliers
# There is no relationship between the fitted values and the standardized residuals
# Though the Q - Q plot distribution looks normal we can see onece agin these outliers
# Specifically the outliers are 323 327 and 326 
# None of these outliers though appear to inflict large amounts of leverage on the model.

# !!! Question 9e !!!

# (e) Use the * and : symbols to fit linear regression models with interaction effects.
#     Do any interactions appear to be statistically significant?

lm.fit3 <- lm(mpg ~ poly(displacement,3) + weight + year + origin + displacement:weight + year:origin, data = Auto)
summary(lm.fit3)
plot(lm.fit3)

# The interaction terms used are statistically significant but have low impact on the model

# !!! Question 9f !!!

# (f) Try a few different transformations of the variables, such as log(X), √X, X^2. Comment on your findings.

lm.fit3 <- lm(mpg ~ poly(displacement,3) + poly(log(weight),3) + year + I(origin^0.5) + weight:displacement, data = Auto)
summary(lm.fit3)

# none of the addtional methods used improved the model significantly

# ---- Question 10 ----
# This question should be answered using the Carseats data set.

# !!! Question 10a !!!

# (a) Fit a multiple regression model to predict Sales using Price, Urban, and US.

?Carseats
attach(Carseats)

lm.fit4 <- lm(Sales ~ Price + Urban + US, data = Carseats)

# !!! Question 10b !!!

# (b) Provide an interpretation of each coefficient in the model. Becareful—some of the variables 
#     in the model are qualitative!

summary(lm.fit4)

# The statistically signficant variables are: Price, and whether the store is in the US. 
# There is slight negative relationship between Price and sales and a positive reltionship between sales and 
# the store being located in the US. 

# !!! Question 10c !!!

# (c) Write out the model in equation form, being careful to handle the qualitative variables properly.

lm.fit5 <- lm(Sales ~ Price + US, data = Carseats)
# I'm pretu sure I've done the above wrong
summary(lm.fit5)

# !!! Question 10d !!!

# (d) For which of the predictors can you reject the null hypothesis H0 :βj =0?

# We can reject the null hypothesis that for Price and USyes because of their p-values but we have to accept
# for Urbanyes

# !!! Question 10e !!!

# (e) On the basis of your response to the previous question, 
#     fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.

# we already did this when we updataed lm.fit4 above in part c

# !!! Question 10f !!!

# (f) How well do the models in (a) and (e) fit the data?

anova(lm.fit4, lm.fit5) # Should go off R^2 not anova here

# There fit appears to be exactly the same which is making me question whether I have done this right
# And the F-statistic is very high 





 # You should return to this Q
# ---- Question 11 ----
# In this problem we will investigate the t-statistic for the null hypoth-esis H0 : β = 0 in simple linear regression without an intercept. 
# To begin, we generate a predictor x and a response y as follows.

set.seed(1)
x <- rnorm(100)
y <- 2*x+rnorm(100)

# !!! Question 11a !!!

# (a) Perform a simple linear regression of y onto x, without an in- tercept. Report the coefficient estimate βˆ,
#     the standard error of this coefficient estimate, and the t-statistic and p-value associ- ated with the null hypothesis 
#     H0 : β = 0. Comment on these results. (You can perform regression without an intercept using the command lm(y∼x+0).)

lm.fit6 <- lm(y~0 + x)
lm.fit6$coefficients
# The coefficient is 1.99

# The other stuff is reported with 
summary(lm.fit6)

# We can see that the coefficient is significant at the 1 percent level and the standard error is low.

# !!! Question 11b !!! 

# (b) Now perform a simple linear regression of x onto y without an intercept, and report the coefficient estimate, 
#     its standard error, and the corresponding t-statistic and p-values associated with the null hypothesis H0 : β = 0. 
#     Comment on these results.

lm.fit7 <- lm(x ~ 0 + y)
summary(lm.fit7)

# the coefficient is 0.3911 and it is also significant at the 1 percent level and has a high t value suggesting a strong rejection
# of the null hypothesis this is backed up with a p-value that is very close to zero. Low standard error means that the 
# fit of the model is pretty good. 

# !!! Question 11c !!!

# (c) What is the relationship between the results obtained in (a) and (b)?

# They both have the same p-value and the same test statistic (because they are both derived form a normal dist) otherwise
# I can't really spell out any other relationship 

# !!! Question 11d !!!

# (d) For the regression of Y onto X without an intercept, the t- statistic for H0 : β = 0 takes the form βˆ/SE(βˆ), 
#     where βˆ is given by (3.38), (These formulas are slightly different from those given in Sec-tions 3.1.1 and 3.1.2, 
#     since here we are performing regression without an intercept.) Show algebraically, and confirm numerically in R

(sqrt(length(x) - 1)*sum(x*y))/(sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))

# !!! Question 11e !!!

# If you swap x and y arund in the above equation you will get the same results. 

# !!! Question 11f !!!

# (f) In R, show that when regression is performed with an intercept, the t-statistic for H0 : β1 = 0 is the same for the 
#     regression of y onto x as it is for the regression of x onto y.

lm.fit6 <- lm(y~x)
lm.fit7 <- lm(x~y)

summary(lm.fit6)
summary(lm.fit7)

# You can see in the output that the t-statistics are the same. 






# ---- Question 12 ----
# This problem involves simple linear regression without an intercept.

# !!! Question 12a !!!

# (a) Recall that the coefficient estimate βˆ for the linear regression of Y onto X without an intercept is given by (3.38).
#     Under what circumstance is the coefficient estimate for the regression of X onto Y the same as the coefficient estimate
#    for the regression of Y onto X?

# When the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values.

# !!! Question 12b !!!

# (b) Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is
#     different from the coefficient estimate for the regression of Y onto X.

set.seed(100)
x <- rnorm(100)
y <- x*3 - rnorm(100, 2, 0.6)

summary(lm(y~x + 0))
summary(lm(x~y + 0))

# The regression coefficients are different for each liner equation

# !!! Question 12c !!!

# (c) Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is
#     the same as the coefficient estimate for the regression of Y onto X.

set.seed(100)
x <- rnorm(100)
y <- -sample(x,100)

# We can now look at the sum of their squares and they should be the same
sum(y^2)
sum(x^2)

summary(lm(y~x + 0))
summary(lm(x~y + 0 ))

# We can see here that the coefficients are exactly the same. 


# ---- Question 13 ----
# In this exercise you will create some simulated data and will fit simple linear regression models to it. 
# Make sure to use set.seed(1) prior to starting part (a) to ensure consistent results.

set.seed(1)

# !!! Question 13a !!! 

# (a) Using the rnorm() function, create a vector, x, containing 100 observations drawn from a N (0, 1) distribution. 
#     This represents a feature, X.

x <- rnorm(100, 0 , 1)

# !!! Question 13b !!!

# (b) Using the rnorm() function, create a vector, eps, containing 100 observations drawn from a N(0,0.25) distribution 
#    i.e. a normal distribution with mean zero and variance 0.25.

eps <- rnorm(100,0,0.25)

# !!! Queston 13c !!!

# (c) Using x and eps, generate a vector y according to the model
#     Y =−1+0.5X+ε. (3.39)
#    What is the length of the vector y? What are the values of β0 and β1 in this linear model?

y <- -1 + 0.5*x + eps

length(y)
# beta zero is cleary -1
# beta 1 is clearly 0.5

# !!! Question 13d !!! 

# (d) Create a scatterplot displaying the relationship between x and y. Comment on what you observe.

par(mfrow = c(1,1))
plot(y,x)

# There apears to be a linear relationship between x and y

# !!! Queston 13e !!!

# (e) Fit a least squares linear model to predict y using x. Comment on the model obtained. 
#     How do βˆ0 and βˆ1 compare to β0 and β1?

lm.fit8 <- lm(y~x)
summary(lm.fit8)

# The fit is almost exactly like the model that was laid out in part (c)

# !!! Question 13f !!!

# (f) Display the least squares line on the scatterplot obtained in (d). Draw the population regression line on the plot, 
#   in a different color. Use the legend() command to create an appropriate legend.

plot(x,y)
abline(lm.fit8, lwd = 2, col = 2)
abline(-1, 0.5, lwd=3, col=3)
legend(-2.5,0.8, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# !!! Question 13g !!!

# (g) Now fit a polynomial regression model that predicts y using x and x2. Is there evidence that the quadratic term 
#     improves the model fit? Explain your answer.

lm.fit9 <- lm(y ~ x + I(x^2))

summary(lm.fit8)
summary(lm.fit9)

# The fit is slightly better in the quadratic model
# There is evidence that model fit has increased over the training data given the slight increase in R2 and RSE. 
# Although, the p-value of the t-statistic suggests that there isn’t a relationship between y and x2.

# !!! Question 13h !!!

# (h) Repeat (a)–(f) after modifying the data generation process in such a way that there is less noise in the data. 
#     The model (3.39) should remain the same. You can do this by decreasing the vari-ance of the normal distribution
#     used to generate the error term ε in (b). Describe your results.

eps <- rnorm(100,0,0.1)
y <- -1 + 0.5*x + eps

plot(x,y)

lm.fit_less_noise <- lm(y ~ x)
summary(lm.fit_less_noise)
abline(lm.fit_less_noise, col = 2, lwd = 3)
abline(-1, 0.5, lwd=3, col=3)
legend(-2.5,0.2, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# !!! Question 13i !!!!

# (i) Repeat (a)–(f) after modifying the data generation process in such a way that there is more noise in the data. 
#     The model (3.39) should remain the same. You can do this by increasing the variance of the normal distribution used 
#     to generate the error term ε in (b). Describe your results.

eps <- rnorm(100,0,0.5)
y <- -1 +0.5*x +eps

lm.fit_more_noise <- lm(y~x)
summary(lm.fit_more_noise)

plot(x,y)
abline(lm.fit_more_noise, col = 2, lwd = 3)
abline(-1, 0.5, lwd=3, col=3)
legend(-2.5,1.6, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# !!! Question 13j !!!

# (j) What are the confidence intervals for β0 and β1 based on the original data set, the noisier data set,
#     and the less noisy data set? Comment on your results.

confint(lm.fit_less_noise)
confint(lm.fit_more_noise)

# The fit with the most noise has the wider confidence intervals whereas the fit with the less noise has smaller
# confidence intervals. However, both are centered around 0.5

# ---- Question 14 ----
# This question focuses on collinearity

# !!! Question 14a !!!

# (a) Perform the laid out lines. The last line corresponds to creating a linear model in which y is a function of x1 and x2.
#     Write out the form of the linear model. What are the regression coefficients?

set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y <- 2+2*x1+0.3*x2+rnorm(100)


# The regression coefficients as stated by the question are 2 and 0.3 with the error term being normally distributed errors. 

# !!! Question 14b !!!

# (b) What is the correlation between x1 and x2? Create a scatterplot displaying the relationship between the variables.

cor(x1,x2)
plot(x1,x2)

# !!! Question 14c !!! 

# (c) Using this data, fit a least squares regression to predict y using x1 and x2. Describe the results obtained. 
#    What are βˆ0, βˆ1, and βˆ2? How do these relate to the true β0, β1, and β2?
#    Can you reject the null hypothesis H0 : β1 = 0? How about the null hypothesis H0 : β2 = 0?

lm.fit_col <- lm(y ~ x1 + x2)
summary(lm.fit_col)

# The coefficients for ß0 is close to 2 for ß1 = 1.43 and ß2 = 1
# We ß1 is significant at the 5% level (rejecting the nul) but only just. ß2 there is a clear acceptance of the null at
# all levels

# !!! Question 14d !!!

# (d) Now fit a least squares regression to predict y using only x1. Comment on your results. 
#     Can you reject the null hypothesis H0 :β1 =0?

lm.fit_col2 <- lm(y~x1)
summary(lm.fit_col2)

# Fitting a model with just x1 as a predictor is much more effective the ß1 coefficient is highly significant as there is 
# a strong rejection of the null hypothesis at the 1% level

# !!! Question 14e !!!

# (e) Now fit a least squares regression to predict y using only x2. Comment on your results. 
#    Can you reject the null hypothesis H0 :β1 =0?

lm.fit_col3 <- lm(y~x2)
summary(lm.fit_col3)

# The coefficients here are also hightly significant rejecting the null at the 1% level. 

# !!! Question 14f !!!

# (f) Do the results obtained in (c)–(e) contradict each other? Explain your answer.

# These are not contradictory as explanatory power is split between the two x terms. One is unable to distinguish their 
# effects

# No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed upon together.
# When they are regressed upon separately, the linear relationship between y and each predictor is indicated more clearly.

# !!! Question 14g !!!

# (g) Now suppose we obtain one additional observation, which was unfortunately mismeasured.

x1 <- c(x1, 0.1) 
x2 <- c(x2, 0.8) 
y <- c(y,6)

# Re-fit the linear models from (c) to (e) using this new data. What effect does this new observation have on the each 
# of the models? In each model, is this observation an outlier? A high-leverage point? Both? Explain your answers.

y <- 2+2*x1+0.3*x2+rnorm(101)

cor(x1,x2)

lm.fit_col4 <- lm(y ~ x1 + x2)
summary(lm.fit_col4)

lm.fit_col5 <- lm(y~x1)
summary(lm.fit_col5)

lm.fit_col6 <- lm(y~x2)
summary(lm.fit_col6)

par(mfrow = c(2,2))
plot(lm.fit_col4)
plot(lm.fit_col5)
plot(lm.fit_col6)

plot(predict(lm.fit_col4), rstudent(lm.fit_col4))
plot(predict(lm.fit_col5), rstudent(lm.fit_col5))
plot(predict(lm.fit_col6), rstudent(lm.fit_col6))
# Everything looks pretty ok

# ---- Question 15 ----
# This problem involves the Boston data set, which we saw in the lab for this chapter. 
# We will now try to predict per capita crime rate using the other variables in this data set. 
# In other words, per capita crime rate is the response, and the other variables are the predictors.

attach(Boston)
?Boston
# Lets just do the interesting ones
pairs(Boston)

# These are nox, rm, age, dis, black, lstat, medv
lm.nox = lm(crim ~ nox)
summary(lm.nox)

lm.rm = lm(crim ~ rm)
summary(lm.rm)

lm.age = lm(crim ~ age)
summary(lm.age)

lm.dis = lm(crim ~ dis)
summary(lm.dis)

lm.black = lm(crim ~ black)
summary(lm.black)

lm.lstat = lm(crim ~ lstat)
summary(lm.lstat)

lm.medv = lm(crim ~ medv)
summary(lm.medv)

lm.medv = lm(crim ~ poly(medv, 3))
summary(lm.medv)

plot(crim, medv)
abline(lm.medv)





