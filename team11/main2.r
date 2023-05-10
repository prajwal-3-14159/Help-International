#including Libraries needed
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
hous
# load data
df <- read.csv("/home/locus/Documents/regression_project2/Country_data2.csv")

# suggested optimmization
# converting the exports, imports and health columns to percentages
df['exports'] <- df['exports'] * df['gdpp'] / 100
df['imports'] <- df['imports'] * df['gdpp'] / 100
df['health'] <- df['health'] * df['gdpp'] / 100
# summarizing the data
summary(df)

# doing the test/train split
set.seed(123)
train_index <- createDataPartition(df$life_expec, p = 0.8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]
df <- train


# saving the fig in the current working directory
setwd("/home/locus/Documents/regression_project2")

# We're deciding to use Life-expectancy as our dependent variable
# and the rest of the variables as our independent variables
# we're going to use the lm() function to fit a linear model for WLS regression

# plottin the histogram of dependent variable life_expec
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
# plotting the qqnorm
png("life_expec_qqnorm.png")
qqnorm(df$life_expec)
qqline(df$life_expec, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, col = 2, lwd = 2)
dev.off()
# performing the Shapiro-Wilk test
shapiro.test(df$life_expec)

# As it's not normal we'll use the Box-Cox transformation
bc = boxcox(life_expec ~ gdpp + income + child_mort + total_fer + health, lambda = seq(-3, 3, by = 0.2), data=df)
# we get lambda around 2.3
df$life_expec_bc <- (df$life_expec^2.4 - 1) / 2.4
# plotting the qqplot
png("life_expec_qqnorm_bc.png")
qqnorm(df$life_expec_bc)
qqline(df$life_expec_bc, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, col = 2, lwd = 2)
dev.off()
# performing the Shapiro-Wilk test
shapiro.test(df$life_expec_bc)
# plotting the histogram
png("life_expec_hist_bc.png")
hist(df$life_expec_bc, breaks = 10, xlab = 'life_expec_bc',
     main = "Histogram of life_expec_bc", col = palette)
dev.off()

# doing the log transform on the dependent varaible 
df$life_expec_log <- log(df$life_expec)
# shapiro wilk test
shapiro.test(df$life_expec_log)

# plotting cook's distance plot
png("life_expec_cooks.png")
plot(df$life_expec, which = 4)
dev.off()



# we're using columns gdpp, income, chid_mort, toatl_fer, health for our multiple predictor variables
# we're using the life_expec column as our dependent variable for multiple linear regression
life_expec_multi = lm(life_expec ~ gdpp + income + child_mort + total_fer + health, data = df)
summary(life_expec_multi)
pairs(~ life_expec_bc + gdpp + income + child_mort + total_fer + health, data = df)
# Checking for linear relationship of the model
plot(life_expec_multi, which = 1)
# checking for normality of residuals
# plotting the qqplot
png("life_expec_resi_qqnorm_multi.png")
qqnorm(residuals(life_expec_multi))
qqline(residuals(life_expec_multi), datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, col = 2, lwd = 2)
dev.off()
plot(life_expec_multi, which = 2)
# shapiro-wilk test for normality of residuals
shapiro.test(residuals(life_expec_multi))

# homoscedasticity test
# we're using the Breusch-Pagan test
bptest(life_expec_multi)
# no significant heteroscedasticity detected

# doing Goldfeld-Quandt test to check for heteroscedasticity
# we're using the gqtest() function
gqtest(life_expec_multi)

# checking for multicollinearity
# we're using the Variance Inflation Factor (VIF)
VIF(life_expec_multi)
# there was high multicollinearity between the variables gdpp, income and health
# so we're going to drop the gdpp column in WLS regression
life_expec_multi = lm(life_expec_bc ~ income + child_mort + total_fer + health, data = df, weights = weights )
summary(life_expec_multi)

# performing PCA on the independent variables
# we're using the prcomp() function
pca <- prcomp(df[, c('gdpp', 'income', 'child_mort', 'total_fer', 'health')], center = TRUE, scale. = TRUE)
pca
# making a dataframe of the principal components
pca_df <- data.frame(pca$x)
pca_df

# plot the cooks distance plot
png("life_expec_cooks_multi.png")
plot(life_expec_multi, which = 4)
dev.off()

ols_plot_cooksd_bar(life_expec_multi, which = 3, threshold = 0.5)

# plot the influence plot
png("life_expec_influence_multi.png")
ols_plot_cooksd_bar(life_expec_multi, which = 4, threshold = 0.5)
dev.off()

# autocorrelation test
# we're using the Durbin-Watson test
dwtest(life_expec_multi)
# if the autocorrelation is high we'll use the Cochrane-Orcutt method

summary(life_expec_multi)

# testing the model on the test data
# we're using the predict() function
predicted_values <- predict(life_expec_multi, test)
# actual values
# convering  the boxcox values to actual values
predicted_values <- (predicted_values * 2.4 + 1)^(1/2.4)
actual_values <- test$life_expec
# calculating the mean squared error
mse <- mean((actual_values - predicted_values)^2)

# calculating the root mean squared error
rmse <- sqrt(mse)

# printing the mse adn rmse
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("mean of train values: ", mean(actual_values)))
