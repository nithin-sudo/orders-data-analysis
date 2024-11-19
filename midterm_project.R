library(tidyr)
library(tidyverse)
library(ggplot2)

#Load from csv
centralData <- read.csv("N:/DA/Orders_Central.csv", header=TRUE)
westData <- read.csv("N:/DA/Orders_West.csv", header=TRUE)
eastData <- read.delim("N:/DA/Orders_East.txt", header=TRUE, sep='\t')

# lowercase column names 
centralData <- centralData %>% rename_with(tolower)
westData <- westData %>% rename_with(tolower)
eastData <- eastData %>% rename_with(tolower)

# check column names
colnames(centralData)
colnames(westData)
colnames(eastData)

# Add the Region column for 'Central Data'
centralData <- centralData %>% mutate(region = "central")

# rename 'product' to 'product name'
centralData <- centralData %>% rename(product.name = product)

# Combine year, month, and day into proper Date columns
centralData <- centralData %>%
  mutate(order.date = as.Date(paste(order.year, order.month, order.day, sep = "-"), format = "%Y-%m-%d"),
         ship.date = as.Date(paste(ship.year, ship.month, ship.day, sep = "-"), format = "%Y-%m-%d"))

# change data type
centralData <- centralData %>%
  mutate(discounts = as.numeric(discounts))

# Remove columns
centralData <- centralData %>% select(-order.year, -order.month, -order.day, -ship.year, -ship.month, -ship.day)



# remove unneccessary column for west data
westData <- westData %>% select(-starts_with("right_"))

# Rename columns
westData <- westData %>%
  rename(discounts = discount)

# Rename columns 
eastData <- eastData %>%
  rename(discounts = discount)


# Standardize `order.date` and `ship.date` as Date objects in all datasets
centralData <- centralData %>%
  mutate(order.date = as.Date(order.date, format = "%Y-%m-%d"),
         ship.date = as.Date(ship.date, format = "%Y-%m-%d"))

# Correctly parse order.date and ship.date in westData
westData <- westData %>%
  mutate(order.date = as.Date(order.date, format = "%Y-%m-%d %H:%M:%S"),
         ship.date = as.Date(ship.date, format = "%Y-%m-%d %H:%M:%S"))

eastData <- eastData %>%
  mutate(order.date = as.Date(order.date, format = "%m/%d/%Y"),
         ship.date = as.Date(ship.date, format = "%m/%d/%Y"))

#convert Sales column in eastData
eastData <- eastData %>%
  mutate(sales = as.numeric(gsub("USD","",sales)))

# Merge the datasets
merged_data <- bind_rows(centralData, westData, eastData)

#1. which region, on average, ships products faster
merged_data <- merged_data %>%
  mutate(time.to.ship = as.numeric(difftime(ship.date,order.date,units = "days")))

# Remove rows with missing or blank regions
merged_data <- merged_data %>%
  filter(region != "" & !is.na(region))

average_shipping_time <- merged_data %>%
  group_by(region) %>%
  summarise(avg_shipping_time = mean(time.to.ship, na.rm = TRUE)) %>%
  arrange(avg_shipping_time)

print(average_shipping_time)

#2. Calculate average shipping time by region and product
slowest_shipping_products <- merged_data %>%
  group_by(region, product.name) %>%
  summarise(avg_shipping_time = mean(time.to.ship, na.rm = TRUE), .groups = "drop") %>%
  group_by(region) %>%
  slice_max(avg_shipping_time, n = 1, with_ties = FALSE)

print(slowest_shipping_products)

#3.Plot time to ship by category, by year.

merged_data = merged_data %>% 
  mutate(order_year = as.numeric(format(order.date,"%Y")))

time_to_ship_by_category_year <- merged_data %>%
  group_by(category,order_year) %>%
  summarise(avg_shipping_time = mean(time.to.ship, na.rm = TRUE), .groups = "drop")

ggplot(time_to_ship_by_category_year, aes(x = order_year, y = avg_shipping_time, color = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Time to Ship by Category, by Year",
       x = "Year",
       y = "Average Time to Ship (days)",
       color = "Category") +
  theme_minimal()



# 4.which categories have highest profit by region, chain-wide?

# Calculate total profit by region and category
highest_profit_categories <- merged_data %>%
  group_by(region, category) %>%
  summarise(total_profit = sum(profit, na.rm = TRUE), .groups = "drop") %>%
  arrange(region, desc(total_profit)) %>%
  group_by(region) %>%
  slice_max(total_profit, n = 1, with_ties = FALSE)

# Calculate total profit by category (chain-wide)
highest_profit_chain_wide <- merged_data %>%
  group_by(category) %>%
  summarise(total_profit = sum(profit, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_profit)) %>%
  group_by(category) %>%
  slice_max(total_profit, n = 1, with_ties = FALSE)

print(highest_profit_categories)

print(highest_profit_chain_wide)


# 5.which segments have the lowest profit by region?

# Calculate total profit by region and segment
lowest_profit_segments <- merged_data %>%
  group_by(region, segment) %>%
  summarise(total_profit = sum(profit, na.rm = TRUE), .groups = "drop") %>%
  arrange(region, total_profit) %>%
  group_by(region) %>%
  slice_min(total_profit, n = 1, with_ties = FALSE)

# View the results
print(lowest_profit_segments)

#6.What are yearly sales by region?

yearly_sales_by_region <- merged_data %>%
  group_by(region, order_year) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(region, order_year)

# View the results
print(yearly_sales_by_region)
