# Visualization of Covid Data

# 0.0 Loading Libraries + data ----
library(tidyverse)
library(lubridate)
library(scales)
library(ggsn)
library(maptools)
library(grid)
library(maps)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

glimpse(covid_data_tbl)

# 1.0 Challenge 1 ----

# Goal: Map the time course of the cumulative Covid-19 cases!
# Cases in Germany, UK, France, Spain, US, Europe
cum_covid_cases <- covid_data_tbl %>%

  # Select needed cols:
  select(dateRep, cases, countriesAndTerritories) %>%

  # Filter Countries
  filter(countriesAndTerritories %in% c("France",
                                        "Germany",
                                        "United_Kingdom",
                                        "Spain",
                                        "United_States_of_America")) %>%
  mutate(date = dmy(dateRep)) %>%
  filter(year(date) == "2020") %>%
  group_by(countriesAndTerritories) %>%
  arrange(date) %>%

  mutate(cum_cases = cumsum(cases)) %>%
  ungroup() %>%

  glimpse()

# Cum cases Europe:
cum_covid_cases_europe <- covid_data_tbl %>%
  
  select(dateRep, cases, continentExp, countriesAndTerritories) %>%
  filter(continentExp == "Europe") %>%
  mutate(date = dmy(dateRep)) %>%
  filter(year(date) == "2020") %>%
  arrange(date) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cum_cases_country = cumsum(cases)) %>%
  ungroup() %>%
  
  group_by(date) %>%
  mutate(cum_cases = sum(cum_cases_country)) %>%
  # # mutate(cum_cases = cumsum(cases)) %>%
  ungroup() %>%
  filter(countriesAndTerritories == "Germany") %>%
  mutate(countriesAndTerritories = "Europe") %>%
  select(!continentExp) %>% select(!cum_cases_country) %>%
  glimpse()

# Creating new to have europe cases as well!
cum_cas_all <- bind_rows(cum_covid_cases,cum_covid_cases_europe) %>%
  arrange(date)



# Data Visualization
cum_cas_all %>%

  # Canvas
  ggplot(aes(x=date, y = cum_cases), color = countriesAndTerritories) +

  geom_line(aes(color = countriesAndTerritories), size=1, linetype = 1) +

  # Get Months as x axis (%B for full name, more info: strftime())
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               limits = c(as.Date("2020/01/01"), Sys.Date()-1)) +

  scale_y_continuous(n.breaks = 8, labels = unit_format(unit = "Mio.", scale = 1e-6)) +

  scale_colour_manual(values = c("blue", "green", "purple", "black", "red", "cyan")) +

  labs(
    title = "Confirmed cummulative COVID-19 Cases",
    subtitle = "Cases are still rising dramatically!",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Country",
    caption = "Europe overtook the USA but keep in mind that Russia is counted to Europe in this data set."
  ) +

  theme_bw() +

  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic"),
        axis.text.x = element_text(angle=45, vjust=0.6))


# 2.0 Challenge 2 ----
# Visualize the distribution of the mortality rate (deaths / population)
# with geom_map(). The necessary longitudinal and lateral data can be
# accessed with this function:

# Data Manipulation:
world <- map_data("world")

glimpse(world)

cum_covid_deaths <- covid_data_tbl %>%

  # Select needed cols:
  select(deaths, countriesAndTerritories,popData2019) %>%

  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  )) %>%
  group_by(countriesAndTerritories) %>%
  summarise(sum_deaths = sum(deaths), pop_2019 = mean(popData2019)) %>%
  mutate(deaths_per_pop = sum_deaths/pop_2019) %>%
  ungroup() %>%

  left_join(world, ., by = c("region" = "countriesAndTerritories")) %>%

  select(!subregion) %>%
  glimpse()

# Plotting the stuff
x <- ggplot(data = cum_covid_deaths) +
  geom_map(map = world,
    aes(x = long,
        y = lat,
        map_id = region,
        fill = deaths_per_pop)) +
  # scale_fill_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(-150,-100,-50,0,50,100,150),
                     limits = c(-180, 180)) +
  scale_y_continuous(breaks = c(-90,-60,-30,0,30,60,90),
                     limits = c(-90, 90))+
  scale_fill_viridis_c(direction = -1,
                       n.breaks = 8,
                       labels = scales::percent,
                       option = "B",
                       alpha = 1,
                       begin = 0.2,
                       end = 0.6,
                       na.value = "grey",
                       guide = "colourbar", # "coloursteps"
                       aesthetics = c("colour", "fill")) +
  expand_limits(x = cum_covid_deaths$long, y = cum_covid_deaths$lat) +
  labs(
    title = "Confirmed cummulative COVID-19 Deaths Relative to the size of the population",
    subtitle = "Looking grim for Americas and Europe",
    x = "Longitude",
    y = "Latitude",
    fill = "Mortality Rate",
    color = "Country",
    caption = str_glue("As of: {max(cum_covid_cases$date)}")
  ) +

  # ggsn::scalebar(data = world,
  #                transform = T,
  #                dist_unit = "km",
  #                dist = 2000,
  #                location = "bottomright",
  #                box.fill = c('black','white'))+

  # map scale
  # ggsn::scalebar(data = world,
  #                transform = T,
  #                dist_unit = "km",
  #                dist = 1000,
  #                location = "bottomright",
  #                # st.dist = 0.1,
  #                st.bottom = F,
  #                st.size = 2,
  #                border.size = 1,
  #                box.fill = c('black','white'),
  #                model = "WGS84") +

  theme_minimal() +
  theme(plot.title = element_text(face = "bold",color = "blue"),
        axis.title.x = element_text(face = "italic"),
        axis.title.y = element_text(face = "italic"))
x

x +
  blank() +
  north(world) +
  scalebar(world, dist = 1000, dist_unit = "km", transform = TRUE, model = 'WGS84')
