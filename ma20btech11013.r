# Name - Prajwaldeep Kamble 
# Roll No - MA20BTECH11013

# Please find the dataset on 20 cities. A real estate investor requires your advice on which city to invest
# in. Determine the best city to live in. You may choose ‘Housing’ variable from the data as your
# dependent variable. Give justification for each and every step, and interpretation for each result.
# Include your conclusions and recommendations. Include your graphs and codes and explanation
# everything in one document. [40marks]
# a. Explore the data by examining basic plots using these variables and their descriptive statistics
# and any other EDA that you have learnt (data pre-processing).
# a. Fit an appropriate regression model to the data. You may use transformation wherever
# necessary.
# a. Explain the output. 
# a. Interpret the model coefficients.
# a. Can you make causal statements? Why or why not?
# a. Are all the variables required in the model? Find the better model? Explain and justify why.
# a. Check for outliers.
# a. Diagnose the model.
# a. Make necessary changes wherever required.
# a. Give your interpretation of the results, conclusion and business insights.

# loading the important libraries
library(lmtest)
library(RColorBrewer)
library(regclass)
library(psych)
library(factoextra)
library(olsrr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(caret)
library(MASS)

# load data
df <- read.csv("/home/locus/Documents/data.csv")

# setting the current directry as working directry for saving the fig in the current working directory
setwd("/home/locus/Documents/")

# drop the NA values
df <- na.omit(df)

# 1. doing the Exploratory Data Analysis
# 1.1. checking the structure of the data
summary(df)

# we see that some of the numerical values are in character format, so we need to convert them to numeric
# I did it using python, loading the new data
df <- read.csv("/home/locus/Documents/regression_project2/data2.csv")
summary(df)

# dropping the first column
df <- df[, -1]

# plotting the correlation matrix
png("corrplot.png")
corrplot(cor(df), method = 'number')
dev.off()

# doing scree plot with parallel analysis
png("scree_plot.png")
fa.parallel(df, fa = "pc", n.iter = 100, show.legend = FALSE)
dev.off()
# as the scree plot shows that the number of factors is 4, 4 factors explain a much larger variance than the rest of the factors

# We're avoiding the test train split as we have a very small dataset
# we're deciding to use Housing as our dependent variable and the rest of the variables as our independent variables

# checking the normality of the dependent variable
# plotting the histogram
png("housing_hist.png")
hist(df$Housing, breaks = 10, xlab = 'Housing',
     main = "Histogram of Housing", col = palette)
dev.off()
# plotting the qqnorm
png("housing_qqnorm.png")
qqnorm(df$Housing)
qqline(df$Housing, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, col = 2, lwd = 2)
dev.off()
# performing the Shapiro-Wilk test
shapiro.test(df$Housing)

# the Shapiro-Wilk test shows that the data is normal so we don't need to use the Box-Cox or the some other transformation

# 2. fitting the linear model
housing_model <- lm(Housing ~ ., data = df)
summary(housing_model)

# Checking for multicollinearity
# 2.1. VIF
VIF(housing_model)
# We see that the household Income and Income are highly correlated, so we'll drop one of them
# We'll drop the Income variable as it has a higher VIF
# and update the model 
housing_model <- lm(Housing ~ . - Income, data = df)
# checking for multicollinearity again
VIF(housing_model)
# all coloumns have a VIF less than 5 so we can proceed
# removing the Income variable from the dataset
df <- df[, -1]

# checking for the linearity of the model
# Checking for linear relationship of the model
png("linearity.png")
plot(housing_model, which = 1)
dev.off()
# the linearity plot shows that the model is not linear, so we'll try to transform the data
# we'll try to transform the data using the log transformation
df$Housing_log <- log(df$Housing)

# fitting the model again
housing_model_log <- lm(Housing_log ~ . -Housing, data = df)
# checking for linearity again
png("linearity_log.png")
plot(housing_model_log, which = 1)
dev.off()
# doing Shapiro test again 
shapiro.test(df$Housing_log)
# the Shapiro-wilk test shows better results than before, so we'll use the log transformed data

# checking for the homoscedasticity of the model
# using the Breusch-Pagan test
bptest(housing_model_log)
# as the p-value is 0.28 and BP is 9.7 we can say that the model is homoscedastic

# checking for the Heteroscedasticity of the model
# using the Goldfeld-Quandt test
gqtest(housing_model_log)
# as the p-value is 0.817 and GQ is 0.09 we can say that the model is not hereoscedastic

# checking for the normality of the residuals
# using the Shapiro-Wilk test
shapiro.test(housing_model_log$residuals)
# as the p-value is more than 0.05 (around 0.15) we can say that the residuals are normal

# checking for the autocorrelation 
# using the Durbin-Watson test
dwtest(housing_model_log)
# as the Durbin-Watson test is 2 and p-value 0.34 we can say that there is no autocorrelation

# checking for the outliers
# using the Cook's distance
png("outliers.png")
plot(housing_model_log, which = 4)
dev.off()
# we see that 18 is an outlier, so we'll remove it
df <- df[-18, ]

# fitting the model again
housing_model_log <- lm(Housing_log ~ . -Housing, data = df)

# plotting the cook's distance again
png("outliers2.png")
plot(housing_model_log, which = 4)
dev.off()

# Pairs plot
png("pairs_plot.png")
pairs(df)
dev.off()

# 3. Interpretation of the model
# 3.1. Interpretation of the coefficients
# the coefficients of the model are
summary(housing_model_log)$coefficients
# the accuracy of the model is
summary(housing_model_log)$r.squared
# the accuracy is around 0.83 which is good
# the adjusted accuracy is
summary(housing_model_log)$adj.r.squared

# printing the summmaary of the model
summary(housing_model_log)
