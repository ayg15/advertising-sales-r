# libraries
library(dplyr)
library(tidyverse)
library(viridis)

# The data
advertising_df <- read.csv("Advertising_Budget_and_Sales.csv", check.names = FALSE)
head(advertising_df)

# EDA
# print column names
column_names <- colnames(advertising_df)
print(column_names)

# remove the extra unnecessary column
# advertising_df <- select(advertising_df, -``)
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

# Convert the correlation matrix to a dataframe
corr_df <- as.data.frame(as.table(corr_matrix))
names(corr_df) <- c("Var1", "Var2", "Correlation")

# Create the heatmap plot using ggplot2
p4 <- ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 3)), color = "black", size = 3) +
  scale_fill_viridis() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(p4)
ggsave("Heatmap.png", plot = p4, width = 6, height = 4, dpi = 300)
