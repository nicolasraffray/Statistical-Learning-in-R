# Applied Questions

# ---- Working Directory Setting ----
getwd()
setwd('/Users/nicolasraffray/Desktop/MSc/R Programming/R Statistical Learning/Data Sets')

# ---- Loading In The Data ----

College <- read.csv('College.csv')
head(College)

rownames(College) <- College[,1]
fix(College)

College <- College[,-1]
# Now all the data is loaded in and the rows are named according to the university
# We then removed the row with the university names as they are stored as rownames
# R will not perform operations on the rownames column

head(College)


# ---- Question 8C ----

summary(College)

# Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. 

pairs(College[,1:10])

# Use the plot() function to produce side-by-side boxplots of Outstate versus Private.

attach(College)
plot(Private, Outstate, varwidth = T, xlab = 'Private Education', ylab = 'Outstate')

# Create a new qualitative variable, called Elite, by binning the Top10perc variable
# We are going to divide universities into two groups based on whether or not the proportion of students
# coming from the top 10% of their high school classes exceeds 50 %.

# Create a vector that will later be added as a row to the dataframe
Elite <- rep('No', nrow(College))
Elite[College$Top10perc > 50] <- 'YES'
Elite <- as.factor(Elite)
College$Elite <- Elite

# or you could attach the data frame like College <- as.dataframe(College, Elite)

# Lets now plot the data

plot(Elite, Outstate, xlab = 'Elite School', ylab = 'Outstate')


# ? Use the hist() function to produce some histograms with differing numbers 
# of bins for a few of the quantitative vari-ables. 

par(mfrow = c(2,2))
hist(Room.Board)
hist(Books)
hist(Personal)
hist(S.F.Ratio)


# ---- Question 9 ----

auto <- read.csv('Auto.csv', header = T, na.strings = '?')
auto <- na.omit(auto)

# ---- Question 9a ----

# Which of the predictors are quantative and which of them are qualitative?

fix(Auto)

# Quantitative: mpg, horsepower, weight, acceleration, number of cylinders
# Qualitvative: Origin, Name, Year made

# ---- Question 9b ----

# What is the range of each quantitative predictor? You can an- swer this using the range() function.

attach(Auto)
range(mpg)
range(horsepower)
range(weight)
range(acceleration)
range(cylinders)

# ---- Question 9c ----

# What is the mean and standard deviation of each of the quantitative variables?
mean(mpg)
sd(mpg)

mean(horsepower)
sd(horsepower)

mean(weight)
sd(weight)

mean(acceleration)
sd(acceleration)

mean(cylinders)
sd(cylinders)

# ---- Question 9d ----

# Now remove the 10th through 85th observations. What is the range, mean, and standard deviation
# of each predictor in the subset of the data that remains?

auto[10,] <- NA
auto[85,] <- NA

auto <- na.omit(auto)

range(mpg)
mean(mpg)
sd(mpg)

range(horsepower)
mean(horsepower)
sd(horsepower)

range(weight)
mean(weight)
sd(weight)

range(acceleration)
mean(acceleration)
sd(acceleration)

range(cylinders)
mean(cylinders)
sd(cylinders)


# ---- Question 9e ----

# Using the full data set, investigate the predictors graphically, using scatterplots or other 
# tools of your choice. Create some plots highlighting the relationships among the predictors. 
# Comment on your findings.

colnames(auto)
par(mfrow = c(1,1))
plot(weight, horsepower, xlab = 'weight', ylab = 'horsepower')
plot(acceleration, horsepower, xlab = 'acceleration', ylab = 'horsepower')
plot(acceleration, weight, xlab = 'acceleration', ylab = 'weight')
plot(mpg, horsepower, xlab = 'mpg', ylab = 'horsepower')
plot(mpg, acceleration, xlab = 'mpg', ylab = 'acceleration')

pairs(auto)

# ---- Question 9f ----

# Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. 
# Do your plots suggest that any of the other variables might be useful in predicting mpg? 
# Justify your answer.

# Yes see the previous question, clear from graphs.



# ---- Question 10a ----
library(MASS)
?Boston

summary(Boston)
nrow(Boston)
head(colnames(Boston))

# ---- Question 10b ----

# Make some pairwise scatterplots of the predictors (columns) in this data set. 
# Describe your findings.

pairs(Boston)



# ---- Question 10c ----

# Are any of the predictors associated with per capita crime rate? If so, explain the relationship.

plot(Boston$age, Boston$crim)
# Older homes, more crime
plot(Boston$dis, Boston$crim)
# Closer to work-area, more crime
plot(Boston$rad, Boston$crim)
# Higher index of accessibility to radial highways, more crime
plot(Boston$tax, Boston$crim)
# Higher tax rate, more crime
plot(Boston$ptratio, Boston$crim)
# Higher pupil:teacher ratio, more crime



# ---- Question 10d ----



# the rest is extrodinarily boring and menial so I'm stopping here and moving on.

