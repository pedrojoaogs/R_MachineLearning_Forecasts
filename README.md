# R_MachineLearning_Forecast
Brazilian Load Generation - Machine Learning

# Clear R environment 

rm(list=ls())

# Find the working directory

getwd()

# Change the working directory

setwd("C:/Users/João Pedro Gonçalves/Desktop/Macroeconometrics")

# The dataset is already prepared. But if it weren't, we could assign variables

b <- 2
c <- 5
b-c

# Remove a variable from the environment 

rm(b)
rm(c)

# The command below is used to request help to understand the function of RM (Remove objects). But we could ask for help for other commands as well.

?rm

# Next, let's install packages to upload documents and manipulate data

install.packages("tidyverse")
install.packages("readxl")
install.packages("openxlsx")

# Then, let's load the packages to use them

library(tidyverse)
library(readxl)
library(openxlsx)

# Import file (in this case an xlsx)

dataset <- read.xlsx("Dataset_30052024_Sem Mês.xlsx")

# Since we don't have the dates in the dataset, let's add this column

datas <- seq.Date(from = as.Date("2003-01-01"), 
                  to = as.Date("2021-12-01"), 
                  by = "month")

dataset$Data <- datas

# Let's check the mean of all variables

mean(dataset$load)
mean(dataset$policy)
mean(dataset$income)


# Let's check the standard deviation of all variables

sd(dataset$load)
sd(dataset$policy)
sd(dataset$income)


# Let's summarize the dataset

summary(dataset)


# Let's calculate the log of all variables 

dataset$log_load <- log(dataset$load)
dataset$log_income <- log(dataset$income)
dataset$log_policy <- log(dataset$policy)


# To delete a specific column, use the command below. In this case, delete the Log of Income column. The command was not executed in R

dataset$log_income <- NULL


# Imagine we are going to work only with the analysis of the variables "load" and "income". Thus, we load the "dplyr" package and then create this new dataset with the SELECT command

library(dplyr)
novo_dataset <- select(dataset, load, income)


# It is possible to perform analysis based on filters. Let's create a dataset where we only consider data of "income" above R$2000.00

novo_dataset_income <- filter(dataset, income > 2000)

# Simple scatter plot

plot(dataset$income,
     dataset$load,
     xlab = "Income",
     ylab = "Load",
     main = "Relationship between Income and Load")
abline(lm(load ~ income, data = dataset))

# Install and load a new graphics package

install.packages("ggplot2")
library(ggplot2)

# Continue with data visualization. Let's construct the histogram

hist(dataset$load)
hist(dataset$income)
hist(dataset$policy)

# Check correlation between variables 

# Get correlation in numbers

correlation_income_load <- cor(dataset$income, dataset$load)
correlation_income_policy <- cor(dataset$income, dataset$policy)
correlation_load_policy <- cor(dataset$load, dataset$policy)

cat("Correlation between Income and Load:", correlation_income_load, "\n")
cat("Correlation between Income and Policy:", correlation_income_policy, "\n")
cat("Correlation between Load and Policy:", correlation_load_policy, "\n")

correlation_matrix <- cor(dataset[, c("income", "policy", "load")])
print(correlation_matrix)


# Install and load package for creating correlation plots

install.packages("corrplot")
library(corrplot)


# Create Correlation Plot

correlation_matrix <- cor(dataset[, c("income", "policy", "load")])

corrplot(correlation_matrix, method = "circle", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix", mar = c(0,0,2,0))

# Or we can use the GGally package

library ("GGally")

GGally::ggcorr(correlation_matrix)

# Estimate the linear regression model between the variables load, income, and policy

modelo1 <- lm(load ~ income + policy, data = dataset)

# Summarize the results

summary(modelo1)


# Estimate the regression model without a constant

modelo2 <- lm(load ~ income + policy - 1, data = dataset)

# Summarize the results

summary(modelo2)

# Create the residuals and summarize them

dataset$res2 <- residuals(modelo1)
dataset$res1 <- residuals(modelo2)

summary(dataset$res1)
summary(dataset$res2)

dataset$hat_load <- fitted(modelo1)
dataset$hat_load <- fitted(modelo2)

# Install packages for forecasts and time series plots

install.packages("fpp2")
install.packages("performance")
install.packages("tidyverse")

# Load packages

library(fpp2)
library(performance)
library(tidyverse)

# If you have any doubts, access the functions of a package. See example below

# fpp2::


# Create the time series object for the variables 'load', "income", and "policy"

load_ts <- ts(dataset$load, start = c(2001, 1), frequency = 12)
income_ts <- ts(dataset$income, start = c(2001, 1), frequency = 12)
policy_ts <- ts(dataset$policy, start = c(2001, 1), frequency = 12)

# Create the time series plots 'load_ts' 'income_ts' using autoplot

autoplot(load_ts) +
  ggtitle("Monthly Load Evolution") +
  ylab("Load (MWh)") +
  xlab("Year") +
  theme_minimal()

autoplot(income_ts) +
  ggtitle("Monthly Income Evolution") +
  ylab("Monthly Income (R$)") +
  xlab("Year") +
  theme_minimal()

autoplot(policy_ts) +
  ggtitle("Economic and Political Uncertainty Index Evolution") +
  ylab("Uncertainty Index") +
  xlab("Year") +
  theme_minimal()


# Calculate the first difference of the time series 'load_ts' 'income_ts'

D_load_ts <- diff(load_ts)
D_income_ts <- diff(income_ts)
D_policy_ts <- diff(policy_ts)

# Plot the first difference of the time series 'load_ts' 'income_ts'

autoplot(D_load_ts) +
  ggtitle("First Difference of Monthly Load") +
  ylab("Difference in Load (MWh)") +
  xlab("Year") +
  theme_minimal()

autoplot(D_income_ts) +
  ggtitle("First Difference of Monthly Income") +
  ylab("Difference in Monthly Income (R$)") +
  xlab("Year") +
  theme_minimal()

autoplot(D_policy_ts) +
  ggtitle("First Difference of the Economic and Political Uncertainty Index") +
  ylab("Difference in Uncertainty Index") +
  xlab("Year") +
  theme_minimal()

# Let's create a seasonality plot, considering the data created from the first difference for the variables 'D_load_ts' 'D_income_ts' 'D_policy_ts'

ggseasonplot(D_load_ts) +
  ggtitle("Seasonal Analysis of the First Difference in Load") +
  ylab("Difference in Load (MWh)") +
  xlab("Month") +
  theme_minimal()

ggseasonplot(D_income_ts) +
  ggtitle("Seasonal Analysis of the First Difference in Monthly Income") +
  ylab("Difference in Monthly Income (R$)") +
  xlab("Month") +
  theme_minimal()

ggseasonplot(D_policy_ts) +
  ggtitle("Seasonal Analysis of the First Difference in the Economic and Political Uncertainty Index") +
  ylab("Difference in Uncertainty Index") +
  xlab("Month") +
  theme_minimal()


# Create the seasonal subseries plot for the first difference for the variables 'D_load_ts' 'D_income_ts' 'D_policy_ts'

ggsubseriesplot(D_load_ts) +
  ggtitle("Seasonal Subseries Analysis of the First Difference in Load") +
  ylab("Difference in Load (MWh)") +
  xlab("Month") +
  theme_minimal()

ggsubseriesplot(D_income_ts) +
  ggtitle("Seasonal Subseries Analysis of the First Difference in Monthly Income") +
  ylab("Difference in Monthly Income (R$)") +
  xlab("Month") +
  theme_minimal()

ggsubseriesplot(D_policy_ts) +
  ggtitle("Seasonal Subseries Analysis of the First Difference in the Economic and Political Uncertainty Index") +
  ylab("Difference in Uncertainty Index") +
  xlab("Month") +
  theme_minimal()

# Load packages and run random walk ARIMA model (0.1.0) 'D_load_ts' 'D_income_ts' 'D_policy_ts'

library(forecast)

# Fit the random walk model with drift (Seasonal Naive) and summarize

fit_load <- snaive(D_load_ts)
summary(fit_load)

fit_income <- snaive(D_income_ts)
summary(fit_income)

fit_policy <- snaive(D_policy_ts)
summary(fit_policy)

# Check residuals for 'fit_load' 'fit_income' 'fit_policy'

checkresiduals(fit_load)
checkresiduals(fit_income)
checkresiduals(fit_policy)


# Fit the seasonal exponential smoothing model for 'fit_load' 'fit_income' 'fit_policy'

fit_ae_load <- ets(D_load_ts

)
summary(fit_ae_load)
checkresiduals(fit_ae_load)

fit_ae_income <- ets(D_income_ts)
summary(fit_ae_income)
checkresiduals(fit_ae_income)

fit_ae_policy <- ets(D_policy_ts)
summary(fit_ae_policy)
checkresiduals(fit_ae_policy)

# Automatic Arima for 'fit_load' 'fit_income' 'fit_policy, summarize and check the residuals 

fit_arima_load <- auto.arima(D_load_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace=TRUE)
summary(fit_arima_load)
checkresiduals(fit_arima_load)

fit_arima_income <- auto.arima(D_income_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace=TRUE)
summary(fit_arima_income)
checkresiduals(fit_arima_income)

fit_arima_policy <- auto.arima(D_policy_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace=TRUE)
summary(fit_arima_policy)
checkresiduals(fit_arima_policy)


# Now let's make a forecast with the specific ARIMA for each variable. We consider "fit_arima_load", "fit_arima_income", "fit_arima_policy" for 24 months

previsao_load <- forecast(fit_arima_load, h=24)
summary(previsao_load)
autoplot(previsao_load) +
  ggtitle("Forecast for fit_arima_load") +
  ylab("Difference in Load (MWh)") +
  xlab("Year")

previsao_income <- forecast(fit_arima_income, h=24)
summary(previsao_income)
autoplot(previsao_income) +
  ggtitle("Forecast for fit_arima_income") +
  ylab("Difference in Monthly Income (R$)") +
  xlab("Year")

previsao_policy <- forecast(fit_arima_policy, h=24)
summary(previsao_policy)
autoplot(previsao_policy) +
  ggtitle("Forecast for fit_arima_policy") +
  ylab("Difference in Uncertainty Index") +
  xlab("Year")

# Model a forecast with the Holt-Winters seasonal method considering the dataset with the first difference

holt_load <- holt(D_load_ts, h=24)
summary(holt_load)

autoplot(holt_load) +
  ggtitle("Forecast for D_load_ts with Holt") +
  ylab("Difference in Load (MWh)") +
  xlab("Year")

holt_income <- holt(D_income_ts, h=24)
summary(holt_income)

autoplot(holt_income) +
  ggtitle("Forecast for D_income_ts with Holt") +
  ylab("Difference in Monthly Income (R$)") +
  xlab("Year")

holt_policy <- holt(D_policy_ts, h=24)
summary(holt_policy)

autoplot(holt_policy) +
  ggtitle("Forecast for D_policy_ts with Holt") +
  ylab("Difference in Uncertainty Index") +
  xlab("Year")

# Fit the Holt-Winters model with damping for load_ts, income_ts, and policy_ts, summarize and plot

holtmodel_load_ts_2 <- hw(load_ts, h=15, damped=TRUE)
previsao_load_ts_2 <- forecast(holtmodel_load_ts_2, h=15)
summary(previsao_load_ts_2)
autoplot(holtmodel_load_ts_2) +
  ggtitle("Forecast for load_ts with Damped Holt-Winters") +
  ylab("Load (MWh)") +
  xlab("Year")


holtmodel_income_ts_2 <- hw(income_ts, h=15, damped=TRUE)
previsao_income_ts_2 <- forecast(holtmodel_income_ts_2, h=15)
summary(previsao_income_ts_2)
autoplot(previsao_income_ts_2) +
  ggtitle("Forecast for income_ts with Damped Holt-Winters") +
  ylab("Monthly Income (R$)") +
  xlab("Year")

holtmodel_policy_ts_2 <- hw(policy_ts, h=15, damped=TRUE)
previsao_policy_ts_2 <- forecast(holtmodel_policy_ts_2, h=15)
summary(previsao_policy_ts_2)
autoplot(previsao_policy_ts_2) +
  ggtitle("Forecast for policy_ts with Damped Holt-Winters") +
  ylab("Economic and Political Uncertainty Index") +
  xlab("Year")


# Command to correct missing fields and outliers. Let's use the original dataset, i.e., load_ts, income_ts, and policy_ts

clean_load_ts <- tsclean(load_ts)
clean_income_ts <- tsclean(income_ts)
clean_policy_ts <- tsclean(policy_ts)

# autoregressive neural network model for load_ts, income_ts, and policy_ts, 24 period forecast and plot

nn_load <- nnetar(clean_load_ts)
revisao_nn_load <- forecast(nn_load, h=24)
summary(revisao_nn_load)
autoplot(revisao_nn_load) +
  ggtitle("Forecast for load_ts with Autoregressive Neural Network") +
  ylab("Load (MWh)") +
  xlab("Year")

nn_income <- nnetar(clean_income_ts)
previsao_nn_income <- forecast(nn_income, h=24)
summary(previsao_nn_income)
autoplot(previsao_nn_income) +
  ggtitle("Forecast for income_ts with Autoregressive Neural Network") +
  ylab("Monthly Income (R$)") +
  xlab("Year")

nn_policy <- nnetar(clean_policy_ts)
previsao_nn_policy <- forecast(nn_policy, h=24)
summary(previsao_nn_policy)
autoplot(previsao_nn_policy) +
  ggtitle("Forecast for policy_ts with Autoregressive Neural Network") +
  ylab("Economic and Political Uncertainty Index") +
  xlab("Year")


# Now let's model a machine learning project 

# First, let's install and load the caTools package

install.packages("caTools")  
library (caTools)

# First, let's set the seed for the random number generator of the sample 

set.seed(123)

# Then we split the sample into a test sample and a training sample 

split <- sample.split(dataset$load, SplitRatio = 2/3)

# Create the training set
training_set <- subset(dataset, split == TRUE)

# Create the test set
teste_set <- subset(dataset, split == FALSE)

# Create the linear regression model
regressor <- lm(load ~ income + policy, data = training_set)
summary(regressor)

# Predict the test sample results and store them in pred_load
pred_load <- predict(regressor, newdata = teste_set)

# Plot the points of the training data with income and policy on the x-axis and load on the y-axis

ggplot() +
  geom_point(aes(x = training_set$income, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$income, y = predict(regressor, newdata = training_set)), colour = "blue") +
  ggtitle("Load vs Income - Training") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

ggplot() +
  geom_point(aes(x = training_set$policy, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$policy, y = predict(regressor, newdata = training_set)), colour = "blue") +
  ggtitle("Load vs Policy - Training") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()


# Visualize the test sample results for income
ggplot() +
  geom_point(aes(x = teste_set$income, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$income, y = predict(regressor, newdata = teste_set)), colour = "blue") +
  ggtitle("Load vs Income - Test") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the test sample results for policy
ggplot() +
  geom_point(aes(x = teste_set$policy, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$policy, y = predict(regressor, newdata = teste_set)), colour = "blue") +
  ggtitle("Load vs Policy - Test") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

# Add polynomial terms to the dataset
dataset$income2 <- dataset$income^2
dataset$policy2 <- dataset$policy^2

# Fit the polynomial regression model
poly_reg <- lm(load ~ income + policy + income2 + policy2, data = dataset)
summary(poly_reg)

# Visualize the results of simple linear regression
ggplot(dataset, aes(x = income, y = load)) +
  geom_point(colour = "red") +
  geom_line(aes(y = predict(regressor, newdata = dataset)), colour = "blue") +
  ggtitle("Linear Regression - Load x Income ^2") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the results of polynomial regression
ggplot(dataset, aes(x = income, y = load)) +
  geom_point(colour = "red") +
  geom_line(aes(y = predict(poly_reg, new

data = dataset)), colour = "blue") +
  ggtitle("Polynomial Regression - Policy - Income^2") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()


# Add cubic terms to the dataset
dataset$income3 <- dataset$income^3
dataset$policy3 <- dataset$policy^3

# Fit the cubic polynomial regression model
poly_reg_cubic <- lm(load ~ income + policy + income3 + policy3, data = dataset)
summary(poly_reg_cubic)

# Visualize the results of simple linear regression
ggplot(dataset, aes(x = income, y = load)) +
  geom_point(colour = "red") +
  geom_line(aes(y = predict(regressor, newdata = dataset)), colour = "blue") +
  ggtitle("Linear Regression - Load x Income^3") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the results of cubic polynomial regression
ggplot(dataset, aes(x = income, y = load)) +
  geom_point(colour = "red") +
  geom_line(aes(y = predict(poly_reg_cubic, newdata = dataset)), colour = "blue") +
  ggtitle("Polynomial Regression - Load x Income^3") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the results of cubic polynomial regression for policy
ggplot(dataset, aes(x = policy, y = load)) +
  geom_point(colour = "red") +
  geom_line(aes(y = predict(poly_reg_cubic, newdata = dataset)), colour = "blue") +
  ggtitle("Polynomial Regression - Load x Policy^3") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()



# Estimate Support Vector Regression (SVR)

# Install and load the necessary package

install.packages("e1071")
library(e1071)

# Assuming your dataset is already loaded as 'dataset', follow the steps below

# Split the dataset using the 'load' column
set.seed(123)
split <- sample.split(dataset$load, SplitRatio = 2/3)

# Create the training set
training_set <- subset(dataset, split == TRUE)

# Create the test set
teste_set <- subset(dataset, split == FALSE)


# Estimate SVR
regressor <- svm(formula = load ~ income + policy,
                 data = training_set,
                 type = "eps-regression",
                 kernel = "radial")

pred_load_svr <- predict(regressor, newdata = teste_set)


# Visualize the test sample results for income
ggplot() +
  geom_point(aes(x = teste_set$income, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$income, y = pred_load_svr), colour = "blue") +
  ggtitle("SVR - Load vs Income - Test") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the test sample results for policy
ggplot() +
  geom_point(aes(x = teste_set$policy, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$policy, y = pred_load_svr), colour = "blue") +
  ggtitle("SVR - Load vs Policy - Test") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

pred_train_svr <- predict(regressor, newdata = training_set)

# Visualize the training sample results for income
ggplot() +
  geom_point(aes(x = training_set$income, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$income, y = pred_train_svr), colour = "blue") +
  ggtitle("SVR - Load vs Income (Training)") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the training sample results for policy
ggplot() +
  geom_point(aes(x = training_set$policy, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$policy, y = pred_train_svr), colour = "blue") +
  ggtitle("SVR - Load vs Policy (Training)") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

# Create x_grid for income and policy
income_grid <- seq(min(dataset$income), max(dataset$income), 0.1)
policy_grid <- seq(min(dataset$policy), max(dataset$policy), 0.1)

# Visualize the test sample results for income with x_grid
ggplot() +
  geom_point(aes(x = dataset$income, y = dataset$load), colour = "darkred") +
  geom_line(aes(x = income_grid, y = predict(regressor, newdata = data.frame(income = income_grid, policy = mean(dataset$policy)))), colour = "blue") +
  ggtitle("SVR - Load vs Income") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the test sample results for policy with x_grid
ggplot() +
  geom_point(aes(x = dataset$policy, y = dataset$load), colour = "darkred") +
  geom_line(aes(x = policy_grid, y = predict(regressor, newdata = data.frame(income = mean(dataset$income), policy = policy_grid))), colour = "blue") +
  ggtitle("SVR - Load vs Policy") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()


# Create a Load forecast for May 2035

# Assuming projections for May 2035
income_2035 <- 4000  # Replace this value with the actual income projection for May 2035
policy_2035 <- 600   # Replace this value with the actual policy projection for May 2035

# Predict the load value for May 2035
load_2035_pred <- predict(regressor, newdata = data.frame(income = income_2035, policy = policy_2035))

# Display the forecast
print(paste("Load forecast for May 2035:", load_2035_pred))


### Decision Tree 

# Install the necessary packages 
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# Split the dataset using the 'load' column
set.seed(123)
split <- sample.split(dataset$load, SplitRatio = 2/3)

# Create the training set
training_set <- subset(dataset, split == TRUE)

# Create the test set
teste_set <- subset(dataset, split == FALSE)

# Estimate the decision tree
regressor <- rpart(load ~ income + policy,
                   data = training_set,
                   control = rpart.control(minsplit = 1))

# Predict the test sample results
pred_load_tree <- predict(regressor, newdata = teste_set)

# Visualize the test sample results for income
ggplot() +
  geom_point(aes(x = teste_set$income, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$income, y = pred_load_tree), colour = "blue") +
  ggtitle("Decision Tree - Load vs Income (Test)") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the test sample results for policy
ggplot() +
  geom_point(aes(x = teste_set$policy, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$policy, y = pred_load_tree), colour = "blue") +
  ggtitle("Decision Tree - Load vs Policy (Test)") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

# Predict the training sample results
pred_train_tree <- predict(regressor, newdata = training_set)

# Visualize the training sample results for income
ggplot() +
  geom_point(aes(x = training_set$income, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$income, y = pred_train_tree), colour = "blue") +
  ggtitle("Decision Tree - Load vs Income (Training)") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the training sample results for policy
ggplot() +
  geom_point(aes(x = training_set$policy, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$policy, y = pred_train_tree), colour = "blue") +
  ggtitle("Decision Tree - Load vs Policy (Training)") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()


# Fit the decision tree to your dataset
regressor <- rpart(load ~ income + policy, data = dataset, control = rpart.control(minsplit = 1))
plot(regressor)
text(regressor)


# Visualize the decision tree
fancyRpartPlot(regressor)

# Check for missing values and remove rows with missing values
dataset <- na.omit(dataset)

# Fit the decision tree to your dataset with adjusted parameters
regressor <- rpart(load ~ income + policy, data = dataset, control = rpart.control(minsplit = 10, cp = 0.01))

# Check if the tree was created correctly


printcp(regressor) 
plot(regressor)
text(regressor)


# Estimate Random Forest 

# Install packages

install.packages("randomForest")
library(randomForest)

# Import file (in this case an xlsx)

dataset_randomforest <- read.xlsx("Dataset_30052024_Sem Mês.xlsx")


# Prepare the data
set.seed(123)
split <- sample.split(dataset_randomforest$load, SplitRatio = 2/3)
training_set <- subset(dataset_randomforest, split == TRUE)
teste_set <- subset(dataset_randomforest, split == FALSE)

# Fit the Random Forest model with 10 trees
set.seed(1234)
regressor <- randomForest(x = training_set[, c("income", "policy")],
                          y = training_set$load,
                          ntree = 10)

# Predict the test set results
pred_load_rf <- predict(regressor, newdata = teste_set[, c("income", "policy")])

# Predict the training set results
pred_train_rf <- predict(regressor, newdata = training_set[, c("income", "policy")])

# Visualize the test sample results for income
library(ggplot2)
ggplot() +
  geom_point(aes(x = teste_set$income, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$income, y = pred_load_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Income (Test)") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the test sample results for policy
ggplot() +
  geom_point(aes(x = teste_set$policy, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$policy, y = pred_load_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Policy (Test)") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

# Visualize the training sample results for income
ggplot() +
  geom_point(aes(x = training_set$income, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$income, y = pred_train_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Income (Training)") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the training sample results for policy
ggplot() +
  geom_point(aes(x = training_set$policy, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$policy, y = pred_train_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Policy (Training)") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

# Fit the Random Forest model with 300 trees
set.seed(1234)
regressor <- randomForest(x = training_set[, c("income", "policy")],
                          y = training_set$load,
                          ntree = 300)

# Predict the test set results
pred_load_rf <- predict(regressor, newdata = teste_set[, c("income", "policy")])

# Predict the training set results
pred_train_rf <- predict(regressor, newdata = training_set[, c("income", "policy")])

# Visualize the test sample results for income
library(ggplot2)
ggplot() +
  geom_point(aes(x = teste_set$income, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$income, y = pred_load_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Income (Test) - 300 trees") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the test sample results for policy
ggplot() +
  geom_point(aes(x = teste_set$policy, y = teste_set$load), colour = "darkred") +
  geom_line(aes(x = teste_set$policy, y = pred_load_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Policy (Test) - 300 trees") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

# Visualize the training sample results for income
ggplot() +
  geom_point(aes(x = training_set$income, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$income, y = pred_train_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Income (Training) - 300 trees") +
  xlab("Income") +
  ylab("Load") +
  theme_minimal()

# Visualize the training sample results for policy
ggplot() +
  geom_point(aes(x = training_set$policy, y = training_set$load), colour = "darkred") +
  geom_line(aes(x = training_set$policy, y = pred_train_rf), colour = "blue") +
  ggtitle("Random Forest - Load vs Policy (Training) - 300 trees") +
  xlab("Policy") +
  ylab("Load") +
  theme_minimal()

