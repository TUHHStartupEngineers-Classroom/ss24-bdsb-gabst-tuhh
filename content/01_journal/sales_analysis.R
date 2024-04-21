# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("readxl")
library("magrittr")
library("tidyverse")
library(lubridate)
# 2.0 Importing Files ----
bikes_tbl <- read_excel("content/data/01_bike_sales/01_raw_data/bikes.xlsx")
order_lines<-read_excel("content/data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("content/data/01_bike_sales/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----
bikes_tbl
order_lines
bikeshops_tbl
summary(bikes_tbl)
glimpse(bikes_tbl)

# 4.0 Joining Data ----

bikes_orderlines_bikeshops <-order_lines %>%
  left_join(bikes_tbl, by=c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by=c("customer.id" = "bikeshop.id"))

bikes_orderlines_bikeshops %>% glimpse()
# 5.0 Wrangling Data ----
bikes_orderlines_bikeshops %>% select(category) %>% filter(str_detect(category, "^Mountain")) %>% unique()

bikes_orderlines_bikeshops <- bikes_orderlines_bikeshops %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bikes_orderlines_bikeshops %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_")) 
  
  
  
  
  names(bikes_orderlines_bikeshops)
  
  
# 6.0 Business Insights ----
# 6.1 Sales by Year ----

  sales_by_year<-bikes_orderlines_bikeshops[, c("order_date", "total_price")]
  sales_by_year_grouped<- sales_by_year %>% mutate(order_date = year(order_date))  %>% group_by(order_date) %>% summarize(sales=sum(total_price)) %>% mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                                                                                                                                                                         decimal.mark = ",", 
                                                                                                                                                                                         prefix = "", 
                                                                                                                                                                                         suffix = " €"))
  sales_by_year_grouped %>%
    # Setup canvas with the columns year (x-axis) and sales (y-axis)
    ggplot(aes(x = order_date, y = sales)) +
    
    # Geometries
    geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
    geom_label(aes(label = sales_text)) + # Adding labels to the bars
    geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
    
    # Formatting
    # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
    # Again, we have to adjust it for euro values
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                      decimal.mark = ",", 
                                                      prefix = "", 
                                                      suffix = " €")) +
    labs(
      title    = "Revenue by year",
      subtitle = "Upward Trend",
      x = "", # Override defaults for x and y
      y = "Revenue"
    )


# 6.2 Sales by Year and Category 2 ----
  # Step 1 - Manipulate
  names(bikes_orderlines_bikeshops)
  sales_by_year_and_category<-bikes_orderlines_bikeshops[, c("order_date", "category_1", "total_price")]
  sales_by_year_and_category_grouped<- sales_by_year_and_category %>%
    mutate(order_date = year(order_date)) %>%
    group_by(order_date, category_1) %>%
    summarize(sales=sum(total_price)) %>%
    mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €"))


# Step 2 - Visualize
  sales_by_year_and_category_grouped %>%
    ggplot(aes(x = order_date, y = sales, fill = category_1)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_label(aes(label = sales_text), position = position_stack(vjust = 0.5), # Position labels at the center of each stack
               color = "black", size = 4)
    #facet_wrap(~category_1) +  # Facet by category_1
    labs(x = "Year", y = "Count", fill = "Category")


  sales_by_year_and_category_grouped %>%
    # color by category_1
    ggplot(aes(x = order_date, y = sales, fill = category_1)) +
    # stack
    geom_bar(stat = "identity") +
    geom_label(aes(label = sales_text), position = position_stack(vjust = 0.5), size=2.5
                                        ) + # Adding labels to the bars
    geom_smooth(method = "lm", se = FALSE)+
    facet_wrap(~category_1) +  # Facet by category_1
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                      decimal.mark = ",", 
                                                      prefix = "", 
                                                      suffix = " €")) +
    labs(title = "Revenue by year and main category",
         subtitle = "Each product category has an upward trend", x = "Year", y = "Revenue", fill = "Category")
  #########################################
  #########################################
  #########################################
  #########################################
  #########################################
  #########################################
  # from course:
  sales_by_year_cat_1_tbl <- bikes_orderlines_bikeshops %>%
    
    # Select columns and add a year
    select(order_date, total_price, category_1) %>%
    mutate(year = year(order_date)) %>%
    
    # Group by and summarize year and main catgegory
    group_by(year, category_1) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    
    # Format $ Text
    mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €"))
  
  sales_by_year_cat_1_tbl
  
  sales_by_year_cat_1_tbl %>%
    
    # Set up x, y, fill
    ggplot(aes(x = year, y = sales, fill = category_1)) +
    
    # Geometries
    geom_col() + # Run up to here to get a stacked bar plot

    # Facet
    facet_wrap(~ category_1) +
    geom_smooth(method = "lm", se = FALSE)+
    # Formatting
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                      decimal.mark = ",", 
                                                      prefix = "", 
                                                      suffix = " €")) +
    labs(
      title = "Revenue by year and main category",
      subtitle = "Each product category has an upward trend",
      fill = "Main category" # Changes the legend name
    )

# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
