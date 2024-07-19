# Muhammad Umer Mirza, 15/10/2023, ALY 6000

# Clear the Console
cat("\014")  # Clears the console
rm(list = ls())  # Clears the global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)  # Clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)  # Clears packages
options(scipen = 100)  # Disables scientific notation for the entire R session

# Load Packages
library(pacman)
p_load(tidyverse, janitor, lubridate, ggthemes, ggeasy)

# Load Data
ttc_bus_delay <- read.csv("ttc-bus-delay-data-2023.csv")

# Data Cleaning
ttc_bus_delay <- ttc_bus_delay %>% clean_names() # standardize column names
ttc_bus_delay <- ttc_bus_delay %>%
  mutate_all(~ifelse(. == "", NA, .)) # replace empty strings with NA

ttc_bus_delay$date <- dmy(ttc_bus_delay$date)    # parse dates using lubridate                   
ttc_bus_delay$month <- month(ttc_bus_delay$date) # extract month using month function and create new column 

missing_values <- colSums(is.na(ttc_bus_delay)) # check null values in each column
print(missing_values) 

clean_ttc_bus_delay <- ttc_bus_delay %>%
  select(-direction, -vehicle, -min_gap) %>% # remove direction, vehicle, min_gap columns
  filter(!is.na(route) & min_delay != 0) # remove rows with 0 delay and missing route values

missing_values_clean <- colSums(is.na(clean_ttc_bus_delay)) # check null values in the clean data frame
print(missing_values_clean) 

# Data Analysis
glimpse(clean_ttc_bus_delay)
summary(clean_ttc_bus_delay)

# Creating a bar chart for the distribution of incidents
bar_chart_incidents <- ggplot(clean_ttc_bus_delay, aes(x = incident)) + # plot count of each incident
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    x = "Incidents",
    y = "Count",
    title = "Distribution of Incidents", face = "bold"
       ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_incidents)

# Creating a bar chart for the distribution of min_delay 
bar_chart_min_delay <- ggplot(clean_ttc_bus_delay, aes(x = min_delay)) +
  geom_histogram(binwidth = 25, fill = "skyblue", color = "black") +
  labs(
    x = "Min Delay (Minutes)",
    y = "Frequency",
    title = "Distribution of Minutes Delay",
    face = "bold"
        ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5)  
        )
print(bar_chart_min_delay)

# Grouping Data

# Grouping data by incident and calculate the sum of min_delay in hours
incident_sum <- clean_ttc_bus_delay %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE)) %>%
  arrange(desc(total_delay_hours)) # Sort by total_delay_hours in descending order

# Creating a bar chart to visualize the sum of min_delay in hours by incident
bar_chart_incident_sum <- ggplot(incident_sum, aes(x = reorder(incident, -total_delay_hours), y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Incident",
    y = "Total Delay (Hours)",
    title = "Total Delay by Incident",
    face = "bold"
       ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_incident_sum)

# Grouping data by route and calculate the sum of min_delay in hours
route_sum <- clean_ttc_bus_delay %>%
  group_by(route) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE)) %>%
  arrange(desc(total_delay_hours)) # Sort by total_delay_hours in descending order

# Select the top 10 routes with the most delay
top_10_routes <- head(route_sum, 10)

# Define a vector with the route number replacements
route_replacements <- c(
  "36" = "Finch West",
  "32" = "Eglinton West",
  "52" = "Lawrence West",
  "7" = "Bathurst South",
  "29" = "Dufferin South",
  "37" = "Islington North",
  "96" = "Wilson West",
  "102" = "Markham South",
  "54" = "Lawrence East",
  "47" = "Lansdowne North"
                          )
# Replacing route numbers with names in the top_10_routes data frame
top_10_routes <- top_10_routes %>%
  mutate(route = factor(route, levels = names(route_replacements), labels = route_replacements))

# Creating a bar chart to visualize the total delay for the top 10 routes
bar_chart_top_10_routes <- ggplot(top_10_routes, aes(x = reorder(route, -total_delay_hours), y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Route",
    y = "Total Delay (Hours)",
    title = "Top 10 Routes with the Most Delay (in Hours)",
    face = "bold"
      ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_top_10_routes)

# Check causes of delay at route 36 (Finch West)
# Filter the data for route 36
finch_west_36 <- clean_ttc_bus_delay %>%
  filter(route == "36")

# Calculate the sum of Min Delay for each incident and sort by Min Delay in descending order
finch_west_36_delay <- finch_west_36 %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60)) %>%
  arrange(desc(total_delay_hours))
print(finch_west_36_delay)

# Creating a bar chart to visualize the total delay for Finch West (route 36)
bar_chart_finch_west_36_delay <- ggplot(finch_west_36_delay, aes(x = reorder(incident, -total_delay_hours), y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Incident",
    y = "Total Delay (Hours)",
    title = "Total Delay by Incident for Finch West (Route 36)",
    face = "bold"
       ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_finch_west_36_delay)

# Grouping the data by day and calculate the sum of min_delay in hours
delay_by_day <- clean_ttc_bus_delay %>%
  group_by(day) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))

# Calculate the percentage of total delay for each day
delay_by_day <- delay_by_day %>%
  mutate(percentage = (total_delay_hours / sum(total_delay_hours)) * 100)

# Create a pie chart with percentage labels
pie_chart_delay_by_day <- ggplot(delay_by_day, aes(x = "", y = percentage, fill = factor(day), label = scales::percent(percentage / 100))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_stack(vjust = 0.5)) +
  labs(
    x = "",
    y = "",
    fill = "Day",
    title = "Total Delay by Day",
    face = "bold"
  ) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(plot.title = element_text(hjust = 0.5))
print(pie_chart_delay_by_day)

delays_by_month <- clean_ttc_bus_delay %>%
  group_by(month) %>%
  summarise(count = n())
# Create a line graph for delays by month
custom_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
line_chart_delays_by_month <- ggplot(delays_by_month, aes(x = month, y = count, group = 1)) +
  geom_line(color = "skyblue") +
  geom_point(color = "skyblue", size = 2) +
  labs(
    x = "Month",
    y = "Count",
    title = "Delays by Month",
    face = "bold"
  ) +
  scale_x_continuous(breaks = 1:12, labels = custom_labels) +  # include custom labels
  theme_bw()
print(line_chart_delays_by_month)

month_1_data <- clean_ttc_bus_delay %>%
  filter(month == 1)
# Calculate the sum of min_delay in hours
month_1_data_summary <- month_1_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 1
bar_chart_month_1 <- ggplot(month_1_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "January",
    face = "bold"
       ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_1)

month_2_data <- clean_ttc_bus_delay %>%
  filter(month == 2)
# Calculate the sum of min_delay in hours
month_2_data_summary <- month_2_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 2
bar_chart_month_2 <- ggplot(month_2_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "darkorchid1", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "February",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
plot.title = element_text(hjust = 0.5))
print(bar_chart_month_2)

month_3_data <- clean_ttc_bus_delay %>%
  filter(month == 3)
# Calculate the sum of min_delay in hours
month_3_data_summary <- month_3_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 3
bar_chart_month_3 <- ggplot(month_3_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "March",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_3)

month_4_data <- clean_ttc_bus_delay %>%
  filter(month == 4)
# Calculate the sum of min_delay in hours
month_4_data_summary <- month_4_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 4
bar_chart_month_4 <- ggplot(month_4_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "April",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_4)

month_5_data <- clean_ttc_bus_delay %>%
  filter(month == 5)
# Calculate the sum of min_delay in hours
month_5_data_summary <- month_5_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 5
bar_chart_month_5 <- ggplot(month_5_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "May",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_5)

month_6_data <- clean_ttc_bus_delay %>%
  filter(month == 6)
# Calculate the sum of min_delay in hours
month_6_data_summary <- month_6_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 6
bar_chart_month_6 <- ggplot(month_6_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "June",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_6)

month_7_data <- clean_ttc_bus_delay %>%
  filter(month == 7)
# Calculate the sum of min_delay in hours
month_7_data_summary <- month_7_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 7
bar_chart_month_7 <- ggplot(month_7_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "July",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_7)

month_8_data <- clean_ttc_bus_delay %>%
  filter(month == 8)
# Calculate the sum of min_delay in hours
month_8_data_summary <- month_8_data %>%
  group_by(incident) %>%
  summarise(total_delay_hours = sum(min_delay / 60, na.rm = TRUE))
# Create a bar chart for month 8
bar_chart_month_8 <- ggplot(month_8_data_summary, aes(x = incident, y = total_delay_hours)) +
  geom_bar(stat = "identity", fill = "cornsilk3", color = "black") +
  labs(
    x = "Incidents",
    y = "Total Delay (Hours)",
    title = "August",
    face = "bold"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(bar_chart_month_8)

# Summary Statistic for min_delay
mean_delay <- mean(clean_ttc_bus_delay$min_delay, na.rm = TRUE)
median_delay <- median(clean_ttc_bus_delay$min_delay, na.rm = TRUE)
sd_delay <- sd(clean_ttc_bus_delay$min_delay, na.rm = TRUE)
variance_delay <- var(clean_ttc_bus_delay$min_delay, na.rm = TRUE)
min_delay <- min(clean_ttc_bus_delay$min_delay, na.rm = TRUE)
max_delay <- max(clean_ttc_bus_delay$min_delay, na.rm = TRUE)

# Create a tibble to store the results
results <- tibble(
  Statistic = c("Mean", "Median", "Standard Deviation", "Variance", "Minimum", "Maximum"),
  Value = c(mean_delay, median_delay, sd_delay, variance_delay, min_delay, max_delay)
)
print(results)

# END OF PROJECT