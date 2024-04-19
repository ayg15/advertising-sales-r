# libraries
library(dplyr)
library(tidyverse)

# The data
advertising_df <- read.csv("Advertising_Budget_and_Sales.csv")
head(advertising_df)

# EDA
# print column names
column_names <- colnames(advertising_df)
print(column_names)

# remove the extra unnecessary column
advertising_df <- select(advertising_df, -X)
head(advertising_df)

# get the summary statistics of the dataframe
summary(advertising_df)

# check and print if there are null values in the dataframe
any_null <- colSums(is.na(advertising_df)) > 0
print(names(advertising_df)[any_null])

# scatterplot for TV ad budget and Sales
p1 <- ggplot(data=advertising_df) +
  geom_point(aes(x=`TV.Ad.Budget....`, y=`Sales....`), color = "green") +
  ggtitle("TV Ad budget vs Sales") + 
  xlab("TV Ad Budget") +
  ylab("Sales")

ggsave("TV_Ad_Budget_and_Sales.png", plot = p1, width = 6, height = 4, dpi = 300)



