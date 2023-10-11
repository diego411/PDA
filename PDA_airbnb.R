rm(list = ls())
# load tidyverse packages
library(tidyverse)

# set your working directory
setwd("C://Users//Steffen Elferich//OneDrive//Desktop//Soziologie//Master//III WS 23-24//Programming R//Assignment")
getwd()

# save csv-file in working directory
# read dataset from working directory
airbnb_data <- read_csv("airbnb_data.csv")
View(airbnb_data)

# calculate average monthly earning
# we assume, that every visitor leaves a review; thus, one review = one stay
# one stay is defined as the fixed minimum number of nights
# using, monthly review count and minimum nights, we calculate the total number of nights
# using the amount of nights and price, we calculate a listing's earnings
airbnb_df <- airbnb_data %>%
  mutate(tot_nights_stayed_M = (reviews_per_month * minimum_nights)) %>%
  mutate(tot_nights_stayed_Y = (number_of_reviews * minimum_nights)) %>%
  mutate(earnings_M = (tot_nights_stayed_M * price)) %>%
  mutate(earnings_total = (tot_nights_stayed_Y * price)) %>%
  mutate(avg_price_hoodgroup = mean(price, na.rm = TRUE), .by = neighbourhood_group)
View(airbnb_df) 

# create barplot showing the average monthly earnings in the neighbourhood groups
price_table <- airbnb_df %>%
  group_by(neighbourhood_group) %>%
  summarize(mean_monthly_earn = mean(earnings_M, na.rm = TRUE),
            mean_price = mean(price, na.rm = TRUE),
            total_earned = sum(earnings_M, na.rm = TRUE))
price_table
ggplot(price_table, aes(x=reorder(neighbourhood_group, +mean_monthly_earn), y=mean_monthly_earn)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = as.integer(mean_monthly_earn)), vjust = -0.5, size = 3) +
  scale_y_continuous(breaks = seq(0, max(price_table$mean_monthly_earn), by = 100)) +
  labs(title="Listing's Average Monthly Earnings by Neighbourhood Group") +
  xlab("Neighbourhood Group") +
  ylab("Earnings in Dollar") 
ggsave("Avg_Earn_by_Hoodgroup.pdf")

ggplot(price_table, aes(x=reorder(neighbourhood_group, +total_earned), y=total_earned)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = comma_format(scale = 1e-6)(total_earned)), vjust = -0.5, size = 3) +
  scale_y_continuous(breaks = seq(0, max(price_table$total_earned), by = 1000000),
                     labels = label_comma(scale = 1e-6)) +
  labs(title="Total Earnings by Neighbourhood Group in Millions") +
  xlab("Neighbourhood Group") +
  ylab("Earnings in Million Dollar") 
ggsave("tot_earn_by_hoodgroup.pdf")

attach(airbnb_data)
rm(airbnb_data)

# describe sample
airbnb_df %>%
  ggplot(aes(x=price)) +
  geom_histogram(binwidth=25)


# calculate mean prices for each neighbourhood and group neighbourhood
mean_price_by_hood <- airbnb_df %>%
  group_by(neighbourhood, neighbourhood_group) %>%
  summarize(avg_price_hood = mean(price, na.rm = TRUE), 
            avg_earn_hood = mean(earnings_total, na.rm = TRUE))
mean_price_by_hoodgroup <- airbnb_df %>%
  group_by(neighbourhood_group) %>%
  summarize(avg_price_hoodgroup = mean(price, na.rm = TRUE),
            avg_earning_hoodgroup = mean(earnings_total, na.rm = TRUE))
mean_price_earn <- full_join(mean_price_by_hood, mean_price_by_hoodgroup)
mean_price_by_hood %>%
  sort(neighbourhood_group, decreasing = TRUE)
mean_price_earn %>%
  ggplot(aes(x=neighbourhood, y=avg_earn_hood, color=neighbourhood_group)) +
  geom_point()
