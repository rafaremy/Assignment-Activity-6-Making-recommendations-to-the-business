################################################################################

#Assignment Activity 5: Clean, manipulate, and visualise the data
# Continue with assignement 4.

################################################################################
# 2-- Load and explore the data and continue to use the data frame.

# Install tidyverse.
install.packages('tidyverse')

# Import tidyverse library.
library(tidyverse) 

# Import the data set (wages_plots.csv).
turtle_sales.csv <- read.csv(file.choose(), header=TRUE) 


# View the data frame.
View(turtle_sales.csv) 
as_tibble(turtle_sales.csv)

# View a summary of the data frame.
summary(turtle_sales.csv)

# 3--Determine the impact on sales per product_id

# Load the required libraries
library(dplyr)

# Read the CSV file into a data frame
turtle_sales <- read.csv("turtle_sales.csv")

# Group the data by product_id
grouped_sales <- turtle_sales %>%
  group_by(Product)

# Define a function to sum the sales values
sum_sales <- function(x) {
  return(sum(x, na.rm = TRUE))
}

# 4-- Create plots to review and determine insights.

# Load the required libraries
library(ggplot2)

# Read the CSV file into a data frame
turtle_sales <- read.csv("turtle_sales.csv")

# Create a scatterplot
scatterplot <- ggplot(data = turtle_sales, aes(x = Product, y = NA_Sales)) +
  geom_point() +
  labs(title = "Scatterplot of Sales by Product",
       x = "Product",
       y = "NA_Sales")

# Create a histogram
histogram <- ggplot(data = turtle_sales, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Sales",
       x = "NA_Sales",
       y = "Frequency")

# Create a boxplot
boxplot <- ggplot(data = turtle_sales, aes(x = Product, y = NA_Sales)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sales by Product",
       x = "Product",
       y = "NA_Sales")

# Display the plots
print(scatterplot)
print(histogram)
print(boxplot)

#5--Determine the normality of the data set (sales data).

# Load required libraries
library(ggplot2)

# a.Create Q-Q plot for Global_Sales.
ggplot(turtle_sales, aes(sample = Global_Sales)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Q-Q Plot of Sales Data",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# b.Perfom a Shapiro-Wilk test on all the sales data.

shapiro_test <- shapiro.test(turtle_sales$Global_Sales)
cat("Shapiro-Wilk Test Results for Global_Sales Data:\n")
cat("Test statistic:", shapiro_test$statistic, "\n")
cat("p-value:", shapiro_test$p.value, "\n")

# Above 0.05 indicates that the data is normally distributed.

# c.Determine the Skewness and Kurtosis of all the sales data.
# Load required libraries
library(moments)

# Load data from turtle_sales.csv
turtle_sales <- read.csv("turtle_sales.csv")

# Calculate skewness and kurtosis of sales data

# Load required libraries
library(moments)

# Load the data into a dataframe
df <- read.csv("turtle_sales.csv")

# Extract the "Global_Sales" column
global_sales <- df$Global_Sales

# Calculate skewness
skewness <- moments::skewness(global_sales)
cat("Skewness: ", round(skewness, 2), "\n")

# Calculate kurtosis
kurtosis <- moments::kurtosis(global_sales)
cat("Kurtosis: ", round(kurtosis, 2), "\n")

# Where a skewness value close to 0 indicates a symmetric distribution, 
#and positive or negative values indicate right or left skewness, respectively.
# Kurtosis measures the tailness of a distribution, where higher positive 
#values indicate heavy tails and lower negative values indicate light tails
#compared to a normal distribution.

# d-- Calculate correlation between columns of NA_Sales, EU_Sales, and Global_Sales data
correlation_matrix <- cor(turtle_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")])

print(correlation_matrix)

# Positive values close to 1 indicate positive correlation, negative values 
#close to -1 indicate negative correlation, and values close to 0 indicate
#little or no correlation. 

# 6-Create plots to gain insights into the sales data.
# Load required libraries
library(ggplot2)

# Scatterplot of NA_Sales vs EU_Sales
ggplot(turtle_sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(x = "NA Sales", y = "EU Sales") +
  ggtitle("Scatterplot of NA Sales vs EU Sales")

# Histogram of Global_Sales
ggplot(turtle_sales, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Global Sales", y = "Frequency") +
  ggtitle("Histogram of Global Sales")

# Boxplot of Sales by Genre
ggplot(turtle_sales, aes(x = Genre, y = Global_Sales)) +
  geom_boxplot() +
  labs(x = "Genre", y = "Global Sales") +
  ggtitle("Boxplot of Sales by Genre")

# Bar chart of Sales by Publisher
ggplot(turtle_sales, aes(x = Publisher, y = Global_Sales)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(x = "Publisher", y = "Total Global Sales") +
  ggtitle("Bar Chart of Sales by Publisher")

# Line chart of Sales by Year
ggplot(turtle_sales, aes(x = Year, y = Global_Sales, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Global Sales") +
  ggtitle("Line Chart of Sales by Year")

################################################################################
# a---Choosing the plots who suits my Analysys.
# Line chart of Sales by Year
ggplot(turtle_sales, aes(x = Year, y = Global_Sales, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Global Sales") +
  ggtitle("Line Chart of Sales by Year")

# Line chart of Sales by Year with customizations
ggplot(turtle_sales, aes(x = Year, y = Global_Sales, group = 0.2)) +
  geom_line(color = "darkgreen", size = 1.5) +  # Custom line color and size
  geom_point(color = "darkgreen", size = 2) +  # Custom point color and size
  labs(x = "Year", y = "Global Sales") +
  ggtitle("Line Chart of Sales by Year") +
  theme_minimal() +  # Custom theme
  theme(plot.title = element_text(hjust = 0.5)) +  # Centered plot title
  scale_x_continuous(breaks = seq(2000, 2010, by = 1)) +  # Custom x-axis breaks
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))  
# Custom y-axis limits and breaks

# Line chart is better as I can see the movements thought the years.

#b---Compare all the sales data (columns) for any correlation(s).

library(ggplot2)

# Read the CSV file into a data frame
turtle_sales <- read.csv("turtle_sales.csv")

# Extract the relevant columns for analysis
sales_columns <- c("NA_Sales", "EU_Sales", "Global_Sales")

# Loop through each sales column and create scatter plots
for (col in sales_columns) {
  # Create scatter plot
  ggplot(turtle_sales.csv, aes_string(x = col, y = "Ranking")) +
    geom_point() +  # Add points to the plot
    labs(x = col, y = "Ranking", title = paste("Scatter plot of", col, "vs. Ranking")) +
    theme_minimal() +  # Set the plot theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
    coord_cartesian(clip = "out") +  # Set the coordinate system
    scale_x_continuous(labels = scales::comma)  # Format x-axis labels with commas for thousands
}
###########################################################
# The correlation here is telling that is there a strong positive correlation.
#############################################################

#c-- Add a trend line to the plots for ease of interpretation.

# Load the ggplot2 library
library(ggplot2)

# Specify the columns for analysis
sales_columns <- c("NA_Sales", "EU_Sales", "Global_Sales")

# Create scatter plots with trend lines
for (col in sales_columns) {
  # Create scatter plot with trend line
  plot <- ggplot(df, aes(x = turtle_sales.csv[[col]], y = Ranking)) +
    geom_point() +  # Add scatter plot
    labs(x = col, y = "Ranking") +  # Set x-axis and y-axis labels
    ggtitle(paste("Scatter plot of", col, "vs. Ranking")) +  # Set plot title
    geom_smooth(method = "lm", se = FALSE, color = "red")  # Add trend line with red color
  
  print(plot)  # Show the plot
}

##############################################################################

# Assignment Activity 6: Making recommendations to the business#


# 2-- Load and explore the data and continue to use the data frame.

# Install tidyverse.
install.packages('tidyverse')

# Import tidyverse library.
library(tidyverse) 

# Import the data set (wages_plots.csv).
turtle_sales.csv <- read.csv(file.choose(), header=TRUE) 

# View the data frame.
View(turtle_sales.csv) 
as_tibble(turtle_sales.csv)

# View a summary of the data frame.
summary(turtle_sales.csv)

################################################################################

# 3-- Create a simple linear regression model

# Create a simple linear regression model
lm_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = turtle_sales.csv)

# Determine the correlation between the sales columns
correlation <- cor(turtle_sales.csv[c("NA_Sales", "EU_Sales", "Global_Sales")])

# View the correlation output
print(correlation)

# View the model output
summary(lm_model)

# Create plots to visualize the linear regression
# Scatter plot of Global Sales vs. NA Sales
ggplot(turtle_sales.csv, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Global Sales vs. NA Sales",
       x = "NA Sales",
       y = "Global Sales")

# Scatter plot of Global Sales vs. EU Sales
ggplot(turtle_sales.csv, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Global Sales vs. EU Sales",
       x = "EU Sales",
       y = "Global Sales")

################################################################################
# 4--Create a multiple lineae regression model.

# Select only the numeric columns
numeric_cols <- select_if(turtle_sales.csv, is.numeric)

# Create a multiple linear regression model
mlm_model <- lm(Global_Sales ~ ., data = numeric_cols)

# Determine the correlation between the sales columns
correlation <- cor(numeric_cols[c("NA_Sales", "EU_Sales", "Global_Sales")])

# View the correlation output
print(correlation)

# View the model output
summary(mlm_model)

################################################################################
#--5 Predict global sales based on provided values.
# Compare your prediction to the observed values.

# Create a data frame with the predictor variables for prediction
new_data <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                       EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52),
                       Ranking = c(1, 2, 3, 4, 5),
                       Product = c(1, 1, 1, 1, 1),
                       Year = c(2000, 2000, 2000, 2000, 2000))

# Predict global sales using the mlm_model
global_sales_pred <- predict(mlm_model, new_data)

# Print the predicted global sales
print(global_sales_pred)

################################################################################

