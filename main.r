#including Libraries needed
library(lmtest)
library(RColorBrewer)
library(regclass)
library(ridge)
library(psych)
library(factoextra)
library(olsrr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(caret)


# load data
df <- read.csv("/home/locus/Documents/regression_project2/Country_data2.csv")

# suggested optimmization
# converting the exports, imports and health columns to percentages
df['exports'] <- df['exports'] * df['gdpp'] / 100
df['imports'] <- df['imports'] * df['gdpp'] / 100
df['health'] <- df['health'] * df['gdpp'] / 100

# doing the test/train split
set.seed(123)
train_index <- createDataPartition(df$life_expec, p = 0.8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]
df <- train

# summarizing the data
summary(df)

# Data cleaning
# removing the first column
df <- df[, -1]
# dropping the NA values
df <- na.omit(df)

df

# saving the fig in the current working directory
setwd("/home/locus/Documents/regression_project2")
# saving the fig in the current working directory
png("life_expec_hist.png")
# plottin the histogram of dependent variable life_expec
n_colors <- 9
# size of the fig is 8 x 6
options(repr.plot.width = 8, repr.plot.height = 6)
palette <- brewer.pal(n_colors, "YlGnBu")
hist(df$life_expec, breaks = 10, xlab = 'life_expec',
     main = "Histogram of life_expec", col = palette)
dev.off()

# checking the normality of the dependent variable
# saving the fig in the current working directory
png("life_expec_qqnorm.png")
qqnorm(df$life_expec)
qqline(df$life_expec, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, col = 2, lwd = 2)
shapiro.test(df$life_expec)
dev.off()

# plotting the correlation matrix heatmap
corrplot(cor(df), method = 'ellipse', order = 'AOE', type = 'upper')
corrplot(cor(df), method = 'number')

# linear regression with chid mortality as the dependent variable
fit_life_expec = lm(life_expec ~ child_mort, data = df)
summary(fit_life_expec)
plot(df$child_mort, df$life_expec, main = "life_expec vs child_mort",
     xlab = "child_mort", ylab = "life_expec")
abline(fit_life_expec, col = 'blue', lwd = 3)

# linear regression with total_fer as the dependent variable
fit_life_expec = lm(life_expec ~ total_fer, data = df)
summary(fit_life_expec)
plot(df$total_fer, df$life_expec, main = "life_expec vs total_fer",
     xlab = "total_fer", ylab = "life_expec")
abline(fit_life_expec, col = 'red', lwd = 3)

# linear regression with income as the dependent variable
fit_life_expec = lm(life_expec ~ income, data = df)
summary(fit_life_expec)
plot(df$income, df$life_expec, main = "life_expec vs income",
     xlab = "income", ylab = "life_expec")
abline(fit_life_expec, col = 'green', lwd = 3)

# multiple linear regression life_expec as the dependent variable Vs all the other variables
fit_life_expec = lm(life_expec ~ child_mort + total_fer + income, data = df)
summary(fit_life_expec)
fit_life_expec = step(fit_life_expec, direction = "backward", trace = 1)
plot(fitted(fit_life_expec), residuals(fit_life_expec), main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)
mse <- summary(fit_life_expec)$sigma^2
mse

# linearity of variables
# the pairs function plots the scatter plot of all the variables
pairs(~ life_expec + total_fer + income + child_mort, data = df, main = "Scatterplot Matrix")

# autocorrelation
# the Durbin-Watson test is used to check for autocorrelation
# the value of the test statistic ranges from 0 to 4
# the closer the value to 2, the more confident we are that there is no autocorrelation
dwtest(fit_life_expec)

# Homoscadasticity
# the gqtest function is used to check for homoscadasticity
# the null hypothesis is that the variance is constant
# the alternative hypothesis is that the variance is not constant
# the p-value is less than 0.05, so we reject the null hypothesis
gqtest(fit_life_expec, data=df, alternative = "two.sided")
plot(fitted(fit_life_expec), rstudent(fit_life_expec), main = "Scale-Location",
     xlab = "Fitted values", ylab = "Standardized residuals")
abline(h = 0, col = "red", lwd = 2)

# checking for multicollinearity
# the vif function is used to check for multicollinearity
# the vif value is greater than 10, so we reject the null hypothesis
columns <- c("child_mort", "total_fer", "income")
dfc <- df[, columns]
# the vif function is used to check for multicollinearity
VIF(fit_life_expec)

# using PCA on the Variables 
new_df <- df[,c("child_mort", "income", "total_fer")]
# transform the data in new_df to principal components df of two components
pca <- prcomp(new_df, scale = TRUE)
# the summary of the pca
pca

fit_life_expec = lm(life_expec ~ PC1 + PC2, data = df)
summary(fit_life_expec)
fit_life_expec = step(fit_life_expec, direction = "backward", trace = 1)
plot(fitted(fit_life_expec), residuals(fit_life_expec), main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)

# checking for outliers
# the cooksd function is used to check for outliers
# the p-value is less than 0.05, so we reject the null hypothesis
# removing the outliers by using the outlier function
# infl <- influence(fit)
# cd <- infl$cooks
infl <- influence(fit_life_expec)
cd <- infl$cooks
threshold <- 4/nrow(fit_life_expec)
high_cd <- which(cd > threshold)
# Conlclusion: no outliers




dev.off()