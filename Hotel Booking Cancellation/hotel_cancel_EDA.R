rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

getwd()
# setwd() set your work directory

library(tidyverse)
library(lubridate)
library(countrycode)

# Read csv
hotel_bookings = read_csv('hotel_bookings.csv')
glimpse(hotel_bookings)

# Check missing values
sum(is.na(hotel_bookings))
colSums(is.na(hotel_bookings))

# Replace missing values of children with 0
hotel_bookings$children[is.na(hotel_bookings$children)] <- 0
sum(is.na(hotel_bookings))

# Covert to factor
hotel_bookings$is_canceled <- as.factor(hotel_bookings$is_canceled)
hotel_bookings$is_repeated_guest <- as.factor(hotel_bookings$is_repeated_guest)
is.factor(hotel_bookings$is_canceled)
is.factor(hotel_bookings$is_repeated_guest)


# Data Visualization
# Total bookings by hotel type
table(hotel_bookings$hotel)
total_bookings_plot = ggplot(data = hotel_bookings, 
                             aes(x = hotel, fill = hotel))+
  geom_bar()+
  labs(title = 'Number of Bookings by Hotel Type',
       x = 'Hotel',
       y = 'Number of Bookings')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat = "count", aes(label = ..count..),
            vjust = -0.5,
            size = 3.5) +
  scale_fill_manual(name = 'Hotel', values = c('#FFCB77','#FE6D73'))+
  theme(legend.position = 'none')
total_bookings_plot


# Percentage of total bookings by hotel type
total_bookings_percent_plot = ggplot(data = hotel_bookings, 
                                        aes(x = "", y = prop.table(stat(count)),
                                            fill = hotel))+
  geom_bar(stat="count", width=1,color="white")+
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = 'Percentage Distribution of Bookings by Hotel Type')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat = "count", aes(label = scales::percent(prop.table(stat(count)))),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(name = 'Hotel', values = c('#FFCB77','#FE6D73'))
total_bookings_percent_plot


# Total bookings by hotel type and country
top10_country <- hotel_bookings %>%
  group_by(country) %>% 
  tally(sort = TRUE) %>% 
  filter(row_number() <= 10)
top10_country

sub_hotel_bookings <- hotel_bookings %>%
  filter(country %in% top10_country$country)
table(sub_hotel_bookings$country)

sub_hotel_bookings$country_name <- countrycode(sub_hotel_bookings$country, 
                                               origin = "iso3c",
                                               destination = "country.name")

bookings_country_plot = ggplot(sub_hotel_bookings, aes(country_name, fill = hotel))+
  geom_bar(position = position_dodge())+
  labs(title = 'Number of Bookings by Country',
       x = 'Country',
       y = 'Number of Bookings')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(.9),
            vjust = -0.5,
            size = 3.5) +
  scale_fill_manual(name = 'Hotel', values = c('#FFCB77','#FE6D73'))+
  theme(legend.position="bottom")
bookings_country_plot 


# The overall booking status
table(hotel_bookings$is_canceled)
booking_status_plot = ggplot(data = hotel_bookings, 
                             aes(x = "", y = prop.table(stat(count)),
                                 fill = factor(is_canceled)))+
  geom_bar(stat="count", width=1,color="white")+
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = 'Percentage Distribution of Booking Status')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat = "count", aes(label = scales::percent(prop.table(stat(count)))),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(name = 'Cancellation Status', labels = c("Not Cancelled", "Cancelled"), 
                    values = c('#457B9D','#A8DADC'))
booking_status_plot


# The of booking status by hotel type
table(hotel_bookings$is_canceled, hotel_bookings$hotel)
bookig_status_type_plot = ggplot(data = hotel_bookings, 
                                  aes(x = hotel, y = prop.table(stat(count)), fill = factor(is_canceled),
                                      label = scales::percent(prop.table(stat(count)))))+
  geom_bar(position = position_dodge())+
  labs(title = 'Percentage Distribution of Booking Status by Hotel Type',
       x = 'Hotel',
       y = 'Percentage')+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat = "count",
            position = position_dodge(.9),
            hjust = 1,
            size = 3.5) +
  scale_fill_manual(name = 'Cancellation Status',
                    labels = c("Not Cancelled", "Cancelled"),
                    values = c('#457B9D','#A8DADC'))+
  theme(legend.position="bottom")
bookig_status_type_plot


# Number of bookings by months
# Rearranging the factors to months
hotel_bookings <- hotel_bookings %>% 
  mutate(Months = fct_relevel(arrival_date_month,"January",
                              "February",
                              "March",
                              "April",
                              "May",
                              "June",
                              "July",
                              "August",
                              "September",
                              "October",
                              "November",
                              "December"))

bookings_month_plot = ggplot(data = hotel_bookings,aes(Months,fill = (hotel))) +
  geom_bar(position = position_dodge(.9)) +
  facet_grid(hotel_bookings$arrival_date_year) +
  labs(title = 'Number of Bookings by Month',
       x = 'Month',
       y = 'Number of Bookings')+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(0.9), vjust = -0.5, size = 3)+
  scale_fill_manual(name = 'Hotel', values = c('#FFCB77','#FE6D73'))+
  theme(legend.position="bottom")
bookings_month_plot


# The average weekly bookings
# Convert data to the per unit value
paged_table(hotel_bookings %>% 
              group_by(arrival_date_year) %>%
              summarize(minimum = min(arrival_date_week_number),
                        maximum = max(arrival_date_week_number),
                        weeks = maximum - minimum + 1))

# The average weekly number of bookings and average weekly revenue
hotelbooking.summary <- hotel_bookings %>%
  filter(is_canceled == 0) %>% 
  group_by(hotel,arrival_date_year) %>%
  summarize(hotel.count = n()) %>%
  transmute(arrival_date_year,
            avg.weekly.bookings = case_when(arrival_date_year == '2015' ~ round(hotel.count/27),
                                            arrival_date_year == '2017' ~ round(hotel.count/35),
                                            TRUE ~ round(hotel.count/53)))
paged_table(hotelbooking.summary)

#Plotting the average weekly bookings
avg_week_bookings_plot = ggplot(hotelbooking.summary,
                                aes(x = hotel, y = avg.weekly.bookings, fill = hotel)) +
  geom_col(width = 0.2) +
  facet_grid(hotelbooking.summary$arrival_date_year) +
  labs(title = 'The Average Weekly Number of Bookings by Hotel Type',
       x = 'Hotel',
       y = 'Number of Bookings')+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(name = 'Hotel', values = c('#FFCB77','#FE6D73'))+
  geom_text(stat = "identity",aes(label = hotelbooking.summary$avg.weekly.bookings),
            position = position_dodge(0.9), vjust = 1.5, size = 3.5)+
  theme(legend.position="bottom")
avg_week_bookings_plot


# Meal package preferences
meal.summary <- hotel_bookings %>%
  filter(is_canceled == 0) %>% 
  group_by(meal) %>%
  summarize(meal_count = n()) %>% 
  arrange(-meal_count) %>%  
  transmute(Meal=case_when(meal == 'BB' ~ 'Bed & Breakfast',
                           meal=='HB' ~ 'Half board (breakfast and one other meal)',
                           meal=='FB' ~ 'Full board (breakfast, lunch and dinner)',
                           meal=='SC'~ 'No meal package',      
                           TRUE ~ 'No Selection'),meal_count)
paged_table(meal.summary)

# plot
meal_plot = ggplot(meal.summary,aes(x = Meal ,y = meal_count, fill = Meal)) +
  geom_col(width = 0.2) + 
  coord_flip() +
  scale_y_continuous(name = "Number of Meal Packages Sold")+
  xlab("Meal Packages") +
  ggtitle("Number of Meal Packages Sold by Type") +
  geom_text(stat = "identity",aes(label = meal_count),
            position = position_dodge(0.9), hjust = -0.2, size = 3.5)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
meal_plot


# Percentage of booking Status in Weekday
# Weekday stay
weekday.summary <- hotel_bookings %>%
  filter(stays_in_week_nights > 0, stays_in_weekend_nights == 0) %>% 
  group_by(hotel,arrival_date_year) %>%
  summarize(is_canceled) 
paged_table(weekday.summary)

# Weekday stay plot
bookig_status_weekdays_plot = ggplot(data = weekday.summary,
                                     aes(x = hotel, y = prop.table(stat(count)),
                                         fill = factor(is_canceled),
                                         label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = position_dodge()) +
  coord_flip()+
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    hjust = 1,
    size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Percentage Distribution of Booking Status on Weekdays",
       x = "Hotel",
       y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    name = "Cancellation Status",
    labels = c("Not Cancelled", "Cancelled"),
    values = c('#457B9D','#A8DADC'))+
  theme(legend.position="bottom")
bookig_status_weekdays_plot


# Weekend stay
weekend.summary <- hotel_bookings %>%
  filter(stays_in_week_nights == 0, stays_in_weekend_nights > 0) %>% 
  group_by(hotel,arrival_date_year) %>%
  summarize(is_canceled) 
paged_table(weekend.summary)

# Weekend stay plot
bookig_status_weekends_plot = ggplot(data = weekend.summary,
                                    aes(x = hotel,y = prop.table(stat(count)),
                                        fill = factor(is_canceled),
                                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = position_dodge()) +
  coord_flip()+
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    hjust = 1,
    size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Percentage Distribution of Booking Status on Weekends",
       x = "Hotel",
       y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    name = "Cancellation Status",
    labels = c("Not Cancelled", "Cancelled"),
    values = c('#457B9D','#A8DADC'))+
  theme(legend.position="bottom")
bookig_status_weekends_plot
