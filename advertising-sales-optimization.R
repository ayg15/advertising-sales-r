# libraries
library(dplyr)
library(tidyverse)
library(viridis)
library(caret)

# The data
advertising_df <- read.csv("Advertising_Budget_and_Sales.csv", check.names = FALSE)
head(advertising_df)

# EDA
# print column names
column_names <- colnames(advertising_df)
print(column_names)

# remove the extra unnecessary column
advertising_df <- advertising_df[, colnames(advertising_df) != ""] 
head(advertising_df)

# get the summary statistics of the dataframe
summary(advertising_df)

# check and print if there are null values in the dataframe
any_null <- colSums(is.na(advertising_df)) > 0
print(names(advertising_df)[any_null])

# scatterplot for TV ad budget and Sales
p1 <- ggplot(data = advertising_df) +
  geom_point(aes(x = `TV Ad Budget ($)`, y = `Sales ($)`), color = "green") +
  ggtitle("TV Ad budget vs Sales") + 
  xlab("TV Ad Budget") +
  ylab("Sales")
print(p1)
ggsave("TV_Ad_Budget_and_Sales.png", plot = p1, width = 6, height = 4, dpi = 300)

# scatterplot for Radio ad budget and Sales
p2 <- ggplot(data = advertising_df) +
  geom_point(aes(x = `Radio Ad Budget ($)`, y = `Sales ($)`), color = "green") +
  ggtitle("Radio Ad budget vs Sales") + 
  xlab("Radio Ad Budget") +
  ylab("Sales")
print(p2)
ggsave("Radio_Ad_Budget_and_Sales.png", plot = p2, width = 6, height = 4, dpi = 300)

# scatterplot for Newspaper ad budget and Sales
p3 <- ggplot(data = advertising_df) +
  geom_point(aes(x = `Newspaper Ad Budget ($)`, y = `Sales ($)`), color = "green") +
  ggtitle("Newspaper Ad budget vs Sales") + 
  xlab("Newspaper Ad Budget") +
  ylab("Sales")
print(p3)
ggsave("Newspaper_Ad_Budget_and_Sales.png", plot = p3, width = 6, height = 4, dpi = 300)

# heatmap
corr_matrix <- cor(advertising_df)

# convert the correlation matrix to a dataframe
corr_df <- as.data.frame(as.table(corr_matrix))
names(corr_df) <- c("Var1", "Var2", "Correlation")

# create the heatmap plot using ggplot2
p4 <- ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 3)), color = "black", size = 3) +
  scale_fill_viridis() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(p4)
ggsave("Heatmap.png", plot = p4, width = 6, height = 4, dpi = 300)

# Linear Regression model
# partition data frame into training and testing sets
train_indices <- createDataPartition(
  advertising_df$`Sales ($)`, times=1, p=.8, list=FALSE)

# create training set
train_df <- advertising_df[train_indices , ]

# create testing set
test_df  <- advertising_df[-train_indices, ]

# top 5 rows of train and test dataframes
head(train_df)
head(test_df)

lm_model <- lm(`Sales ($)` ~ ., data = train_df)
summary(lm_model)

# predictions on test data
predictions <- predict(lm_model, newdata = test_df)
head(predictions)

# model rmse
rmse_model <- sqrt(mean(lm_model$residuals^2))
print(paste("Root Mean Squared Error (RMSE) from model:", rmse_model))

# evaluate the model (e.g., calculate RMSE) based on test_df
rmse <- sqrt(mean((test_df$`Sales ($)` - predictions)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# r2_score
r_squared <- summary(lm_model)$r.squared
print(paste("R-squared:", r_squared))

# get the coefficients
lm_model_coef <- coef(lm_model)
print(lm_model_coef)

# Minimization using optim
# the minimum and maximum bounds for optimization
min_values <- c(unname(apply(advertising_df, 2, min)))
print(min_values)
max_values <- c(unname(1.2 * apply(advertising_df, 2, max)))
print(max_values)

# define the objective function to be minimized
objective_function <- function(x) {
  return (lm_model_coef[1] + (lm_model_coef[2]*x[1]) + 
            (lm_model_coef[3]*x[2]) + (lm_model_coef[4]*x[3]))
}

# initial guess
x0 <- c(0, 0, 0)

# perform minimization
result <- optim(par = x0, fn = objective_function, method = "L-BFGS-B", 
                lower = min_values, upper = max_values, control = list(trace=1))

# extract the optimized parameters
optimized_params <- result$par
print("Optimized parameters:")
print(optimized_params)

# print the value of the objective function at the optimum
print("Value of objective function at optimum:")
print(result$value)

# define the objective function to be maximized
objective_function <- function(x) {
  return (-lm_model_coef[1] - (lm_model_coef[2]*x[1]) - 
            (lm_model_coef[3]*x[2]) - (lm_model_coef[4]*x[3]))
}

# perform maximization
result <- optim(par = x0, fn = objective_function, method = "L-BFGS-B", 
                lower = min_values, upper = max_values, control = list(trace=1))

# extract the optimized parameters
optimized_params <- result$par
print("Optimized parameters:")
print(optimized_params)

# print the value of the objective function at the optimum
print("Value of objective function at optimum:")
print(-result$value)

