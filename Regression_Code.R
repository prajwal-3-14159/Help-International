df = read.csv("C:/Users/Anita Dash/Downloads/Happiness_Dataset_2017.csv")
new_df = read.csv("C:/Users/Anita Dash/Downloads/2018.csv")
df$Trust <- as.numeric(as.character(df$Trust))  # Convert one variable to numeric

#including Libraries needed
library(lmtest)
library(RColorBrewer)
library(regclass)
library(ridge)
library(psych)
library(factoextra)
library(olsrr)


#Plotting histogram of dependent variable
n_colors <- 9
palette <- brewer.pal(n_colors, "YlGnBu")
hist(df$Happiness.Score, breaks = 10, xlab = 'Happiness.Score', 
     main = "Histogram of happiness score", col = palette)

#Checking normality of the dependent variable
qqnorm(df$Happiness.Score)
qqline(df$Happiness.Score, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7,col=2,lwd=2)
shapiro.test(df$Happiness.Score)

# Simple Linear Regression
fit_Happiness.Score = lm(Happiness.Score ~ Economy, data= df)
summary(fit_Happiness.Score)
plot(df$Economy, df$Happiness.Score, main = "Happiness Score vs Economy",
     xlab = "Economy", ylab = "Happiness Score")
abline(fit_Happiness.Score, col = 'red', lwd = 3)

#Multiple Linear Regression
fit_data = lm( Happiness.Score ~ Economy +  Family + Life.Expectancy + Freedom + Generosity + Trust,
               data= df)
summary(fit_data)
fit_data = step(fit_data,direction="backward",trace=1)
plot(fitted(fit_data), residuals(fit_data),main = "Residuals vs Fitted (Multiple Linear Regression)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = 'red')
mse <- summary(fit_data)$sigma^2
mse

#Linearity of variables
pairs(~ Happiness.Score + Economy + Family, data = df, col = palette)
pairs(~ Happiness.Score + Life.Expectancy + Freedom + Trust, data = df, col = palette)

#AutoCorrelation
dwtest(fit_data)

#Homoscedasticity
gqtest(fit_data, data = df, alternative = "two.sided")
plot(fitted(fit_data), rstandard(fit_data), ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0, lty = 5, col = 'red')  # Add a horizontal line at y=0

#Multicollinearity
df_col<- df[,c("Economy", "Family", "Life.Expectancy", "Freedom", "Trust")]
df_col
round(cor(df_col), 2)
VIF(fit_data)

# Remedy 1 - Ridge regression
fit2 = linearRidge(Happiness.Score ~ Economy +  Family + Life.Expectancy + Freedom +Trust , data= df)
summary(fit2)
fitted_values <- predict(fit2)
residuals <- df$Happiness.Score - fitted_values
plot(fitted_values, residuals, main = "Residuals vs Fitted (Linear Ridge Regression)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = 'red')
mse <- mean((residuals)^2)
mse

#Remedy 2 - Using PCA on the variables
pca_df <- df[,c("Economy", "Family", "Life.Expectancy", "Freedom", "Trust")]
pca_df <- principal(pca_df, nfactors=2, score=TRUE)
pca_df <- data.frame(pca_df$scores)
df <- cbind.data.frame(df,pca_df)
round(cor(df[,c("RC1", "RC2")]), 2)
fit3 = linearRidge(Happiness.Score ~ RC1+RC2, data= df)
summary(fit3)
predicted_ridge = predict(fit3)
residuals_ridgepca = df$Happiness.Score - predicted_ridge
plot(predicted_ridge, residuals_ridgepca, 
     main = "Residuals vs Fitted (Ridge Regression on Principal Components)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = 'red')
mse <- mean((residuals_ridgepca)^2)
mse

#Looking at influential points
ols_plot_cooksd_bar(fit_data)
outlier = c(35, 71, 80, 93, 106, 132, 142, 151)
outlier
rem_df <- df[-outlier,]
rem_df
fit_data = lm( Happiness.Score ~ Economy +  Family + Life.Expectancy + Freedom + Trust,
               data= rem_df)
summary(fit_data)
mse <- summary(fit_data)$sigma^2
mse


### Predictions with 2018 data
newdata=new_df
newdata$Actual_score=new_df$Happiness.Score
#Predicting based on the fit_data model
pred_score = predict.lm(fit_data,newdata,interval="p")
residuals_lm = newdata$Happiness.Score - pred_score
mse_1 <- mean((residuals_lm)^2)
mse_1
#Indias Happiness Score
newdata[132,]
#Predicted Happiness Score for India
pred_score[132]
#Predicting based on Fit2 model
pred_score_2 = predict(fit2,newdata = newdata, interval="p")
residuals_ridge_newdf = newdata$Happiness.Score - pred_score_2
mse_2 <- mean((residuals_ridge_newdf)^2)
mse_2
pred_score_2[132]

 

