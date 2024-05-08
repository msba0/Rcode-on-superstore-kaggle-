# library(ggplot2)
# library(plotly)

# This is to view the whole dataset of 'superstore'
View(superstore)

# Display the structure of the dataset
str(superstore)

# FIRST INVESTIGATION - Histogram of Quantity(items purchased in single transaction )
# Load ggplot2 libraries
library(ggplot2)

# Plot histogram of the quantity of products purchased in a single transaction
ggplot(superstore, aes(x = Quantity)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Quantity Purchased in a Single Transaction",
       x = "Quantity",
       y = "Frequency") +
  theme_minimal()





# SECOND INVESTIGATION - T-test:Perform Perform t-test for profit between two segments.
# Load dplyr packages
library(dplyr)

# Define the segments
segment1 <- "Consumer"
segment2 <- "Corporate"

# Filter the data for the two segments
data_segment1 <- superstore %>% filter(Segment == segment1)
data_segment2 <- superstore %>% filter(Segment == segment2)

# Perform the t-test
t_test_result <- t.test(data_segment1$Profit, data_segment2$Profit)


# Print the results of the t-test
print(t_test_result)



# THIRD INVESTIGATION - Profit Margin Analysis: Analyze profit margins for different product categories
# Load dplyr, ggplot2 packages
library(dplyr)
library(ggplot2)

# Calculate profit margin if not already in the dataset
if (!"Profit_Margin" %in% colnames(superstore)) {
  superstore$Profit_Margin <- (superstore$Profit / superstore$Sales) * 100
}

# Filter out rows with zero or negative Sales to avoid division by zero and unrealistic profit margins
superstore_filtered <- superstore %>%
  filter(Sales > 0)

# Group data by category and calculate summary statistics for profit margin
category_profit_margin <- superstore_filtered %>%
  group_by(Category) %>%
  summarise(
    mean_profit_margin = mean(Profit_Margin, na.rm = TRUE),
    median_profit_margin = median(Profit_Margin, na.rm = TRUE),
    sd_profit_margin = sd(Profit_Margin, na.rm = TRUE)
  )

# Print summary statistics
print(category_profit_margin)

# Visualize profit margin for each category using a boxplot
ggplot(superstore_filtered, aes(x = Category, y = Profit_Margin)) +
  geom_boxplot() +
  labs(title = "Profit Margin Analysis by Product Category",
       x = "Product Category",
       y = "Profit Margin (%)") +
  theme_minimal()



# FOURTH INVESTIGATION - Sales vs. Profit (scatter plot)
# Load ggplot2 packages
library(ggplot2)

# Investigation of Sales vs. Profit
ggplot(superstore, aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot: Sales vs. Profit",
       x = "Sales",
       y = "Profit") +
  theme_minimal()


# FIFTH INVESTIGATION - Shipping Cost vs. Profit (scatter plot)
# Load ggplot2 packages
library(ggplot2)

# Investigation of Shipping Cost vs. Profit
ggplot(superstore, aes(x = Shipping.Cost, y = Profit)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot: Shipping Cost vs. Profit",
       x = "Shipping Cost",
       y = "Profit") +
  theme_minimal()


# SIXTH INVESTIGATION - Discount vs. Profit (scatter plot)
# Load ggplot2 packages
library(ggplot2)

# Investigation of Discount vs. Profit
ggplot(superstore, aes(x = Discount, y = Profit)) +
  geom_point(alpha = 0.6, color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot: Discount vs. Profit",
       x = "Discount",
       y = "Profit") +
  theme_minimal()


# SEVENTH INVESTIGATION - Correlation Analysis: Calculate and visualize the correlation matrix of key numerical variables.
# Load ggplot2,corrplot packages
library(ggplot2)
library(corrplot)

# Select key numerical variables for correlation analysis
key_numeric_vars <- superstore[, c("Sales", "Profit", "Discount", "Quantity", "Shipping.Cost")]

# Calculate the correlation matrix
correlation_matrix <- cor(key_numeric_vars)

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix using a heatmap
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.srt = 45)





# EIGHTH INVESTIGATION - Segment Analysis: Analyze sales, profit, and quantity across different segments.
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Group the data by segment and calculate mean profit
mean_profit_by_segment <- superstore %>%
  group_by(Segment) %>%
  summarize(mean_profit = mean(Profit, na.rm = TRUE))

# Print mean profit by segment
print(mean_profit_by_segment)

# Visualize mean profit across segments
ggplot(mean_profit_by_segment, aes(x = Segment, y = mean_profit, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Profit across Segments", x = "Segment", y = "Mean Profit") +
  theme_minimal()





# NINTH INVESTIGATION - Mean profit across category
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Group data by category and calculate mean profit
category_mean_profit <- superstore %>%
  group_by(Category) %>%
  summarize(mean_profit = mean(Profit, na.rm = TRUE))

# Print mean profit by category
print(category_mean_profit)

# Visualize mean profit across categories
ggplot(category_mean_profit, aes(x = Category, y = mean_profit, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Profit across Categories", x = "Category", y = "Mean Profit") +
  theme_minimal()




# TENTH INVESTIGATION - Mean sales across category
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Group data by category and calculate mean sales
category_mean_sales <- superstore %>%
  group_by(Category) %>%
  summarize(mean_sales = mean(Sales, na.rm = TRUE))

# Print mean sales by category
print(category_mean_sales)

# Visualize mean sales across categories
ggplot(category_mean_sales, aes(x = Category, y = mean_sales, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Sales across Categories", x = "Category", y = "Mean Sales") +
  theme_minimal()




# ELEVENTH INVESTIGATION - Product Category Distribution
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Group data by Category and count the number of observations in each category
category_data <- superstore %>%
  group_by(Category) %>%
  summarize(count = n())

# Calculate the percentage of each category
category_data <- category_data %>%
  mutate(percentage = count / sum(count) * 100)

# Plot a pie chart of product category distribution
ggplot(category_data, aes(x = "", y = percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Product Category Distribution") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))



# TWELFTH INVESTIGATION -  Region Distribution
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Group data by Region and count the number of observations in each region
region_data <- superstore %>%
  group_by(Region) %>%
  summarize(count = n())

# Calculate the percentage of each region
region_data <- region_data %>%
  mutate(percentage = count / sum(count) * 100)

# Plot a pie chart of region distribution
ggplot(region_data, aes(x = "", y = percentage, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Region Distribution") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))



# THIRTEENTH INVESTIGATION - Outlier detection in sales (standard deviation method)
# Calculate the mean and standard deviation of sales
mean_sales <- mean(superstore$Sales)
sd_sales <- sd(superstore$Sales)

# Define the number of standard deviations to use for outlier detection 
 num_sd <- 3

# Identify outliers in sales
outliers <- superstore$Sales[
  superstore$Sales < mean_sales - num_sd * sd_sales |
    superstore$Sales > mean_sales + num_sd * sd_sales
]

# Print the number of outliers found
 cat("Number of outliers in sales:", length(outliers), "\n")


# FOURTEENTH  INVESTIGATION - Sales Performance Analysis
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Aggregate total sales by segment and region
sales_by_segment_region <- superstore %>%
  group_by(Segment, Region) %>%
  summarize(total_sales = sum(Sales), .groups = 'drop')

# Print the aggregated data
print(sales_by_segment_region)


# Create a bar plot of sales by segment and region
ggplot(sales_by_segment_region, aes(x = Segment, y = total_sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales Performance by Segment and Region",
       x = "Segment",
       y = "Total Sales") +
  theme_minimal()




# FIFTEENTH INVESTIGATION - Regional Analysis
# Load dplyr,ggplot2 packages
library(dplyr)
library(ggplot2)

# Aggregate total sales by region
sales_by_region <- superstore %>%
  group_by(Region) %>%
  summarize(total_sales = sum(Sales), .groups = 'drop')

# Print the aggregated data
print(sales_by_region)

# Create a bar chart of sales by region
ggplot(sales_by_region, aes(x = Region, y = total_sales, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Region",
       x = "Region",
       y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# SIXTEENTH INVESTIGATION - Average Profit Over Time by Segment
# Load dplyr,lubridate,ggplot2 libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Define cohorts by extracting year and month from 'Order.Date'
superstore <- superstore %>%
  mutate(year_month = floor_date(Order.Date, "month"))

# Group data by year-month and segment
cohorts <- superstore %>%
  group_by(year_month, Segment)

# Calculate average profit for each cohort
cohort_profit <- cohorts %>%
  summarise(avg_profit = mean(Profit), .groups = 'drop')

# Plot average profit over time for each segment
ggplot(cohort_profit, aes(x = year_month, y = avg_profit, color = Segment)) +
  geom_line(size = 1) +
  labs(title = "Average Profit Over Time by Segment",
       x = "Year-Month",
       y = "Average Profit") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



# SEVENTEENTH INVESTIGATION - ANOVA test for profit across different regions.
# Load dplyr,ggplot2 libraries
library(dplyr)
library(ggplot2)

# Conduct ANOVA test for profit across different regions
anova_result <- aov(Profit ~ Region, data = superstore)

# Print the ANOVA results
summary(anova_result)

# If there is a significant result, you may want to conduct a post-hoc test
# to find out which specific regions differ significantly
if (summary(anova_result)[[1]][1, "Pr(>F)"] < 0.05) {
  # Perform Tukey's HSD post-hoc test
  posthoc_result <- TukeyHSD(anova_result, conf.level = 0.95)
  
  # Print the results of the post-hoc test
  print(posthoc_result)
}

# Visualize the profit means across different regions using a boxplot
ggplot(superstore, aes(x = Region, y = Profit)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Profit Across Different Regions", x = "Region", y = "Profit") +
  theme_minimal()



# EIGHTEENTH INVESTIGATION - Analyze cumulative profit over time.
# install.packages("lubridate")
# Load dplyr,lubridate,ggplot2 libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Convert the 'Order.Date' column to Date type 
superstore$Order.Date <- as.Date(superstore$Order.Date)

# Group data by year instead of month
# Calculate the total profit and cumulative profit
cumulative_profit_data <- superstore %>%
  group_by(Year = floor_date(Order.Date, "year")) %>%
  summarize(Total_Profit = sum(Profit)) %>%
  mutate(Cumulative_Profit = cumsum(Total_Profit))

# Plot the cumulative profit over time
ggplot(cumulative_profit_data, aes(x = Year, y = Cumulative_Profit)) +
  geom_line(color = "steelblue") +
  labs(title = "Cumulative Profit Over Time",
       x = "Year",
       y = "Cumulative Profit") +
  theme_minimal()



# NINETEENTH INVESTIGATION - Test the association between categories(segment and region)
# Load dplyr,ggplot2 libraries
library(dplyr)
library(ggplot2)


# Define the variables segment and region 
variable1 <- superstore$Segment
variable2 <- superstore$Region

# Create a contingency table
contingency_table <- table(variable1, variable2)

# Perform the Chi-Square test
chi_square_test <- chisq.test(contingency_table)

# Print the test result
print(chi_square_test)






# TWENTIETH INVESTIGATION - Density Plot of Discount Levels.
# Load ggplot2,dplyr libraries
library(ggplot2)
library(dplyr)

# Plot the density of discount levels across all products
ggplot(superstore, aes(x = Discount)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Density of Discount Levels Across All Products",
       x = "Discount",
       y = "Density") +
  theme_minimal()


