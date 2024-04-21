library("readxl")
library("magrittr")
library("tidyverse")
library(lubridate)
# loading
bikes_tbl <- read_excel("content/data/01_bike_sales/01_raw_data/bikes.xlsx")
order_lines<-read_excel("content/data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("content/data/01_bike_sales/01_raw_data/bikeshops.xlsx")
# joining
bikes_orderlines_bikeshops <-order_lines %>%
  left_join(bikes_tbl, by=c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by=c("customer.id" = "bikeshop.id"))

bikes_orderlines_bikeshops %>% glimpse()
bikes_orderlines_bikeshops
# wrangling
bikes_orderlines_bikeshops<-bikes_orderlines_bikeshops %>%  
  mutate(total.price = price * quantity) %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") 


# grouping etc


sales_by_location_and_year<-bikes_orderlines_bikeshops[, c("order.date", "state", "city", "total.price")]
sales_by_location_and_year_grouped<- sales_by_location_and_year %>%
  mutate(order.date = year(order.date)) %>%
  group_by(order.date, state) %>%
  summarize(sales=sum(total.price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_location<- sales_by_location_and_year %>%
  mutate(order.date = year(order.date)) %>%
  group_by(state) %>%
  summarize(sales=sum(total.price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# visualizing
sales_by_location %>%
  # color by category_1
  ggplot(aes(x = state, y = sales, fill=state)) +
  # stack
  geom_bar(stat = "identity") +
  geom_label(aes(label = sales_text), position = position_stack(vjust = 0.95), size=2.5
  ) + # Adding labels to the bars

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # changing direction of labels
  labs(title = "Revenue and state",
       subtitle = "North Rhine-Westphalia has most revenue, then Bremen, followed by Bavaria.", x = "State", y = "Revenue", fill = "State")



sales_by_location_and_year_grouped %>%
  # color by category_1
  ggplot(aes(x = order.date, y = sales, fill = state)) +
  # stack
  geom_bar(stat = "identity") +
  geom_label(aes(label = sales_text), position = position_stack(vjust = 0.5), size=2.5
  ) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~state) +  # Facet by state
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(title = "Revenue by year and state",
       subtitle = "Some states have an upwards trend, some stagnate and few have a slight downward trend.", x = "Year", y = "Revenue", fill = "State")

sales_by_location_and_year_grouped %>%
  # color by category_1
  ggplot(aes(x = order.date, y = sales, fill = state)) +
  # stack
  geom_bar(stat = "identity") +
  geom_label(aes(label = sales_text), position = position_stack(vjust = 0.5), size=2.5
  ) + # Adding labels to the bars

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(title = "Revenue by year and state",
       subtitle = "Some states have an upwards trend, some stagnate and few have a slight downward trend.", x = "Year", y = "Revenue", fill = "State")
  


sales_by_location_and_year_grouped %>%
  group_by(state) %>%
  summarize(total_sales = sum(sales)) %>%
  arrange(desc(total_sales))
