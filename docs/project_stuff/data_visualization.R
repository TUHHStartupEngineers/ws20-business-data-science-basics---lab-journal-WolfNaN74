# Data Visualization with ggplot2

# 0.0 Libraries ----

library(tidyverse) # loads ggplot2
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----

sales_by_year_tbl <- bike_orderlines_tbl %>%

  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%

  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%

  # € Format Text
  mutate(sales_text = scales::dollar(sales,
                                     big.mark     = ".",
                                     decimal.mark = ",",
                                     prefix       = "",
                                     suffix       = " €"))

sales_by_year_tbl


# Step 2: Plot ----
sales_by_year_tbl %>%

  # Canvas
  ggplot(aes(x = year, y = sales, color = sales))

# Without piping
ggplot(data = sales_by_year_tbl,
       aes(x     = year,
           y     = sales,
           color = sales))

# 2.0 Various Plots ----

# 2.1 Scatter Plot with Trendline ----
sales_by_year_tbl %>%

  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +

  # Geometries
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE)

# Different aes() mapping
sales_by_year_tbl %>%

  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +

  # Geometries
  geom_line(size = 1) +
  geom_point(aes(size = sales), color = "blue") +
  geom_smooth(method = "lm", se = FALSE)


# 2.2 Point / Scatter Plots ----
# Great for Continuous vs Continuous
# Also good for Lollipop Charts (more on this later)
# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%

  select(order_id, order_line, total_price, quantity) %>%

  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>%
  ungroup()

# Scatter Plot
order_value_tbl %>%

  ggplot(aes(x = total_quantity, y = total_price)) +

  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)


# 2.3 Line Plots ----
# Great for time series
# Goal: Describe revenue by month, expose cyclic nature

# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%

  select(order_date, total_price) %>%

  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%

  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%

  ggplot(aes(year_month, revenue)) +

  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)


# 2.4 Bar / Column Plots ----
# Great for categories
# Goal: Sales by Descriptive Category

# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>%

  select(category_2, total_price) %>%

  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%

  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%

  ggplot(aes(category_2, revenue)) +

  geom_col(fill = "#2c3e50") +
  coord_flip()


# 2.5 Histogram / Density Plots ----
# Great for inspecting the distribution of a variable
# Goal: Unit price of bicycles

# Histogram

bike_orderlines_tbl %>%

  distinct(model, price) %>%

  ggplot(aes(price)) +

  geom_histogram(bins = 25, fill = "blue", color = "white")

# Goal: Unit price of bicylce, segmenting by frame material

# Histogram
bike_orderlines_tbl %>%

  distinct(price, model, frame_material) %>%

  ggplot(aes(price, fill = frame_material)) +

  geom_histogram() +

  facet_wrap(~ frame_material, ncol = 1)

# Density
bike_orderlines_tbl %>%

  distinct(price, model, frame_material) %>%

  ggplot(aes(price, fill = frame_material)) +

  geom_density(alpha = 0.5) +
  # facet_wrap(~ frame_material, ncol = 1) +

  theme(legend.position = "bottom")


# 2.6 Box Plot / Violin Plot ----
# Great for comparing distributions
# Goal: Unit price of model, segmenting by category 2

# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%

  select(category_2, model, price) %>%
  distinct() %>%

  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%

  ggplot(aes(category_2, price)) +

  geom_boxplot() +
  coord_flip()


# 2.7 Violin Plot & Jitter Plot ----

unit_price_by_cat_2_tbl %>%

  ggplot(aes(category_2, price)) +

  geom_jitter(width = 0.15, color = "#2c3e50") +
  geom_violin(alpha = 0.5) +

  coord_flip()


# 2.8 Text & Labels -----
#
# Goal: Exposing sales over time, highlighting outliers

# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>%

  select(order_date, total_price) %>%

  mutate(year = year(order_date)) %>%

  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>%

  ggplot(aes(year, revenue)) +

  geom_col(fill = "#2c3e50") +
  geom_smooth(method = "lm", se = FALSE) +

  geom_text(aes(label =  scales::dollar(revenue,
                                        scale  = 1e-6,
                                        prefix = "",
                                        suffix = "M")),
            vjust = 1.5, color = "white") +

  geom_label(label =  "Major Demand This Year",
             vjust = -0.5,
             size  = 5,
             fill  = "#1f78b4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>%
               filter(year %in% c(2019))) +

  expand_limits(y = 2e7)


# 3.0 Formating ----
# See page 2 of the visualization Cheatsheet

sales_by_year_tbl %>%

  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +

  # Geometries
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +

  # same as above, with explicit scales
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_continuous()

# 3.1 Scales with coloring and Labels ----

sales_by_year_tbl %>%

  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +

  # Geometries
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +

  # Formatting
  expand_limits(y = 0) +
  # You can also type "red", "black" etc. for the colors
  scale_color_continuous(low    = "#95E1EA", high = "#2097A3",
                         labels = scales::dollar_format(scale  = 1/1e6,
                                                        prefix = "",
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M €")) +

  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )

# 3.2 Theme, custom legend pos.+dir., breaks, angle axis ----

library(ggthemes)
## DATA PREPARATION
sales_by_month_2015 <- bike_orderlines_tbl %>%

  # Selecting columns to focus on and adding a month column
  select(order_date, total_price) %>%
  mutate(year  = year(order_date)) %>%
  mutate(month = month(order_date)) %>%

  filter(year == "2015") %>%

  # Grouping by month, and summarizing sales
  group_by(month) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%

  # $ Format Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",
                                     suffix          = " €"))

## PLOTTING
# Canvas
g <- sales_by_month_2015 %>%
  ggplot(aes(x = month, y = sales, color = sales)) +

  # Geometries
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +

  # Formatting
  expand_limits(y = 0) +
  scale_color_continuous(low = "red", high = "black",
                         labels = scales::dollar_format(scale = 1/1e6,
                                                        prefix = "",
                                                        suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month,
                     labels = month(sales_by_month_2015$month, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M")) +
  labs(
    title = "Monthly sales (2015)",
    subtitle = "April is the strongest month!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers are dropping towards the end of the year."
  )  +
  theme_economist() +
  theme(legend.position  = "right",
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))
g
View(g)

# 3.3 Some Data Manipulation ----

# Let’s create a new subset of the data for some examples
# of formatting:

# Data Manipulation

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%

  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%

  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%

  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

# Uncover the factor levels (just for demonstration)
# sorted by years and the highest revenues
sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)

# 3.4 Colors ----

# 3.4.1 Color Conversion ----
# Named Colors. This returns a long list of colors that can be used by name
colors()

# Example
sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue)) +

  geom_col(fill = "slateblue")

# To RGB
col2rgb("slateblue")

col2rgb("#2C3E50")

# To HEX (this function should be provided to a geom)
rgb(44, 62, 80, maxColorValue = 255)


# 3.4.2 Color Palettes: Colors that are typically work well together.----

### Brewer. Comes with basic R.
#Primarly for discrete data.

# We can use those palletes by just calling their names (e.g. "Blues")
# Display the colors
RColorBrewer::display.brewer.all()
# Get information
RColorBrewer::brewer.pal.info
# Get the HEX codes
RColorBrewer::brewer.pal(n = 8, name = "Blues")[1]

# Example
sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue)) +

  geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])


### Viridis
viridisLite::viridis(n = 20)
# The last two characters indicate the transparency (e.g. FF makes it 100% transparent)

# Example
sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue)) +

  geom_col(fill = viridisLite::viridis(n = 20)[10])


# 3.5 Color: Used with line and points, Outlines of rectangular objects ----

sales_by_year_category_1_tbl %>%

  # Put the aes color mapping here, to apply it to geom_line and geom_point
  ggplot(aes(year, revenue, color = category_1)) +

  # Or you could do it locally in each geom
  # (aes mapping only necessary if you map it to a column)
  geom_line(size = 1) + # geom_line(aes(color = category_1))
  geom_point(color = "dodgerblue", size = 5)

# 3.6 Color Fill ----
#Used with fill of rectangular objects
# (stacked column chart in this case)

sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue)) +
  geom_col(aes(fill = category_1))
# You could use color = ... to color the outlines

# 3.7 Size: Typically used with points ----
sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, size = revenue)) +

  # The local size overrides the global size
  geom_line(aes(color = category_1), size = 1) +
  geom_point()


# 3.8 Faceting ----

# facet_wrap() separates a plot with groups into multiple plots (aka facets)
# Great way to tease out variation by category

sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, color = category_1)) +
  geom_line(color = "black") +
  geom_smooth(method = "lm", se = FALSE) +

  # Break out stacked plot
  facet_wrap(~ category_1, ncol = 3, scales = "free_y") +

  expand_limits(y = 0)


# 3.9.1 Position Adjustments (Stack & Dodge) ----

# Using the position argument to plot Stacked Bars & Side-By-Side Bars

sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, fill = category_1)) +
  # geom_col(position = "stack") # default
  # geom_col(position = "dodge")
  geom_col(position = position_dodge(width = 0.9), color = "white")


# 3.9.2 Stacked Area ----

sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black")


# 3.10.0 Scales (Color, Fills, Axis) ----
# Continuous (e.g. Revenue): Changes color via gradient palette
# Categorical (e.g. category_2): Changes color via discrete palette

# 3.10.1 Faceted Plot, Color = Continuous Scale ----
# Plot 1: Faceted Plot, Color = Continuous Scale
g_facet_continuous <- sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, color = revenue)) +
  geom_line(size = 1) +
  geom_point(size = 3) +

  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +

  theme_minimal()

g_facet_continuous

# 3.10.2  Plot 2: Faceted Plot, Color = Discrete Scale ----

g_facet_discrete <- sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, color = category_1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +

  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +

  theme_minimal()

g_facet_discrete

# 3.10.3 Plot 3: Stacked Area Plot ----

g_area_discrete <- sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black") +

  theme_minimal()

g_area_discrete

# 3.11.0 Scale Color & Fills ----
# Awesome way to show variation by groups (discrete)
# and by values (continuous).

# 3.11.1 Color by Revenue (Continuous Scale) ----
g_facet_continuous +

  # scale_color_continuous(
  #     low   = "black",
  #     high  = "cornflowerblue"
  # )
  # This is basically like adding a theme
  scale_color_viridis_c(option = "E", direction = -1)

# 3.11.2 Color by Category 1 (Discrete Scale) ----
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n = 8, name = "Blues")

g_facet_discrete +
  scale_color_brewer(palette = "Set3") +
  theme_dark()

g_facet_discrete +
  scale_color_viridis_d(option = "D") +
  theme_dark()

# 3.11.3  Fill by Category 1 ----
g_area_discrete +
  scale_fill_brewer(palette = "Set3")

g_area_discrete +
  scale_fill_viridis_d()

# 3.12 Axis Scales ----
g_facet_continuous +
  scale_x_continuous(breaks = seq(2015, 2019, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6,
                                                    preix = "",
                                                    suffix = "M"))

# 3.13 Labels ----
g_facet_continuous +

  scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6,
                                                    suffix = "M")) +

  geom_smooth(method = "lm", se = FALSE) +

  scale_color_viridis_c() +
  theme_dark() +

  labs(
    title = "Bike Sales",
    subtitle = "Sales are trending up",
    caption = "5-year sales trends\ncomes from our ERP Database",
    x = "Year",
    y = "Revenue (M €)",
    color = "Revenue" # Legend text
  )

# 3.13 Themes ----
# Run View(g_facet_continuous) and expand theme to see
# which elements of the plots can be changed with theme().

g_facet_continuous +

  theme_light() +

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.background = element_rect(
      color = "black",
      fill  = "cornflowerblue",
      size  = 1
    ),
    strip.text = element_text(
      face  = "bold",
      color = "white"
    )
  )

# 3.14 Example graph: Putting It All Together ----

sales_by_year_category_1_tbl %>%

  ggplot(aes(year, revenue, fill = category_1)) +

  geom_area(color = "black") +

  # Scales
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " €")) +

  # Labels
  labs(
    title = "Sales Over Year by Category 1",
    subtitle = "Sales Trending Upward",
    x = "",
    y = "Revenue (M €)",
    fill = "2nd Category",
    caption = "Bike sales trends look strong heading into 2020"
  ) +

  # Theme
  theme_light() +
  theme(
    title = element_text(face = "bold", color = "#08306B")

  )

# 4. Factors ----

library(tidyverse)
# without factors
starwars %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE)

# with factors
starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = as_factor(species) %>%
           fct_lump(n = 3)) %>%
  count(species)

# fct_reorder(): Reordering a factor by another variable.
f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
f

fct_reorder(f, c(2,3,1,4))


# fct_relevel(): allows you to move any number of levels to any location.
fct_relevel(f, "a")
fct_relevel(f, "b", "a")

# Move to the third position
fct_relevel(f, "a", after = 2)

# Relevel to the end
fct_relevel(f, "a", after = Inf)

fct_relevel(f, "a", after = 3)


# 5.0 Business case ----

# 5.1 Case 1 Lollipop Chart: Top N Customers ----
# Question: How much purchasing power is in top 5 customers (bikeshops)?
# Goal: Lollipop Chart. Visualize top N customers in
# terms of Revenue, include cumulative percentage.

# 5.1.1 Load libraries and data
library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# 5.1.2 Data manipulation

n <- 10
top_customers_tbl <- bike_orderlines_tbl %>%

  # Select relevant columns
  select(bikeshop, total_price) %>%

  # Collapse the least frequent values into “other”
  mutate(bikeshop = as_factor(bikeshop) %>%
           fct_lump(n = n, w = total_price)) %>%

  # Group and summarize
  group_by(bikeshop) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%

  # Reorder the column customer_city by revenue
  mutate(bikeshop = bikeshop %>% fct_reorder(revenue)) %>%
  # Place "Other" at the beginning
  mutate(bikeshop = bikeshop %>% fct_relevel("Other", after = 0)) %>%
  # Sort by this column
  arrange(desc(bikeshop)) %>%

  # Add Revenue Text
  mutate(revenue_text = scales::dollar(revenue,
                                       scale  = 1e-6,
                                       prefix = "",
                                       suffix = "M €")) %>%

  # Add Cumulative Percent
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>%
  mutate(cum_pct_text = scales::percent(cum_pct)) %>%

  # Add Rank
  mutate(rank = row_number()) %>%
  mutate(rank = case_when(
    rank == max(rank) ~ NA_integer_,
    TRUE ~ rank
  )) %>%

  # Add Label text
  mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))

glimpse(top_customers_tbl)

# 5.1.3 Data visualization

top_customers_tbl %>%

  # Canvas
  ggplot(aes(revenue, bikeshop)) +

  # Geometries
  geom_segment(aes(xend = 0, yend = bikeshop),
               color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
               size  = 1) +

  geom_point(aes(size = revenue),
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +

  geom_label(aes(label = label_text),
             hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +

  # Formatting
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6,
                                                    prefix = "",
                                                    suffix = "M €")) +
  labs(
    title = str_glue("Top {n} Customers"),
    subtitle = str_glue(
      "Start: {year(min(bike_orderlines_tbl$order_date))}
               End:  {year(max(bike_orderlines_tbl$order_date))}"),
    x = "Revenue (M €)",
    y = "Customer",
    caption = str_glue("Top 6 customers contribute 52% of purchasing power.")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )


# 5.2 Case 2 Heatmap ----
# Question: Do specific customers have a purchasing preference?
# Goal: Visualize heatmap of proportion of sales by Secondary
# Product Category

# 5.2.1 Data manipulation
# Select columns and filter categories
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%

  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain","Road")) %>%

  # Group by category and summarize
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%

  # Add missing groups (not necessarily mandatory, but we'd get holes in the plot)
  # complete() creates NAs. We need to set those to 0.
  complete(bikeshop, nesting(category_1, category_2)) %>%
  mutate(across(total_qty, ~replace_na(., 0))) %>%

  # Group by bikeshop and calculate revenue ratio
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%

  # Reverse order of bikeshops
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  # Just to verify
  mutate(bikeshop_num = as.numeric(bikeshop))


# 5.2.2 Data visualization

pct_sales_by_customer_tbl %>%

  ggplot(aes(category_2, bikeshop)) +

  # Geometries
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)),
            size = 3) +
  facet_wrap(~ category_1, scales = "free_x") +

  # Formatting
  scale_fill_gradient(low = "white", high = "#2C3E50") +
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road:
        To be discussed ...

        Customers that prefer Mountain:
        To be discussed ...")
  ) +

  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )



