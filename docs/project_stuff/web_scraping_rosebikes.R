# Web Scaping rosebike.de

# Goal: get model names, categories and prices of all the bikes

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 2.0 GET PRODUCT FAMILIES ----

url_home <- "https://www.rosebikes.de"
# xopen(url_home)  # to open in the browser from R

# Read the HTML of the home adress
html_home         <- read_html(url_home)

# maybe: ADD WARNING IF THE URL WAS NOT LOADED...

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  html_nodes(css = ".main-navigation-category-with-tiles__link") %>% # extract nodes
  html_attr('href') %>% # get the urls
  # kick out e-bike, kinder and sale
  discard(.p = ~stringr::str_detect(.x,"/fahrräder/sale|/fahrräder/e-bike|/fahrräder/kinder")) %>% # get rid of sale
  # If discard does not work:
  # .[-6] %>% .[-(9:10)] %>%
  str_remove('/fahrräder/') %>%
  enframe(name = "position", value = "family_name") %>% # Convert vector to tibble
  mutate(url = str_glue("https://www.rosebikes.de/fahrräder/{family_name}")) # Create url


# 3.0 GET BIKE MODELS ----
# We have: bike_family_tbl

# 3.1 BIKE MODELS FUNCTION ----

get_bike_mdl <- function(url){
  fam_url <- url
  html_bike_family<- read_html(url)

  # Getting Bike Model Names
  bike_model_name <- html_bike_family %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text() %>%
    str_extract(pattern = "(?<=\\n).*(?=\\n)") %>%
    enframe(name = "position", value = "bikeName") %>%
    glimpse()

  # Getting Bike Model URLs
  bike_model_url <- html_bike_family %>%
    html_nodes(css = ".catalog-category-bikes__button") %>%
    html_attr('href') %>%
    enframe(name = "position", value = "url") %>%
    mutate(url = glue("https://www.rosebikes.de{url}")) %>%
    mutate(bike_family = str_extract(fam_url, pattern = "(?<=/)[a-z]*$")) %>%
    glimpse()

  # Join Model Names and URLs
  bike_models_tbl <- left_join(bike_model_name, bike_model_url)
}

# 3.2 TRY BIKE SUBCATEGORIES FUNCTION ----
# test_url_subcat <- bike_family_tbl$url[1]
# bike_subcat_tbl <- get_bike_mdl(url = test_url_subcat)

# 3.3 RUN BIKE MODELS FUNCTION ON ALL URLS ----
# 3.3.1 Extract all family URLs
family_url_list <- bike_family_tbl %>%
  pull(url) %>%
  as.character()

# 3.3.2 Map the model function against all family URLs
bike_mdl_tbl <- map(family_url_list, get_bike_mdl) %>%
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble
  as_tibble() %>%
  # Kick out position from above
  select(!position) %>%
  # Create new pos to have a number count
  rowid_to_column(var = "position")


# 4.0 GET BIKE VERSIONS AND PRICES ----
# We have: bike_family_tbl, bike_mdl_tbl

# 4.1 BIKE VERSIONS AND PRICES FUNCTION ----
get_bike_vrsn_and_price <- function(url){

  html_bike_mdl <- read_html(url)

  # Bike Model
  bike_mdl <- html_bike_mdl %>%
    html_nodes(css = '.catalog-category-title-with-logo__title-text') %>%
    html_text() %>%
    str_extract(pattern = "(?<=\\n).*(?=\\n)") %>%
    glimpse()

  # Getting Versions
  bike_vrsn_tbl <- html_bike_mdl %>%
    html_nodes(css = '.catalog-category-model__title') %>%
    html_text() %>%
    str_extract(pattern = "(?<=\\n).*(?=\\n)") %>%
    enframe(name = "position", value = "version") %>%
    mutate(model = bike_mdl) %>%
    mutate(family = str_extract(url, pattern = "(?<=fahrräder/)[a-z]*")) %>%
    glimpse()

  # Getting the price of Versions
  bike_vrsn_price_tbl <- html_bike_mdl %>%
    html_nodes(css = '.product-tile-price__current-value.catalog-category-model__price-current-value') %>%
    html_text() %>%
    str_extract(pattern = "(?<=\\n).*(?=\\n)") %>%
    enframe(name = "position", value = "price") %>%
    glimpse()

  # Join Version and Price
  left_join(bike_vrsn_tbl , bike_vrsn_price_tbl)
}

# 4.2 TRY BIKE VERSIONS AND PRICES FUNCTION ----
# test_url_vrsn <- bike_mdl_tbl$url[1]
# bike_vrsn_tbl <- get_bike_vrsn_and_price(url = test_url_vrsn)

# 4.3 RUN BIKE VERSIONS AND PRICES FUNCTION ON ALL MODEL URLs ----
# 4.3.1 Extract all model urls
mdl_url_list <- bike_mdl_tbl %>%
  pull(url) %>%
  as.character()

# 4.3.2 Map the vers and price function against all model URLs
bike_mdl_vers_price_tbl <- purrr::map(mdl_url_list, get_bike_vrsn_and_price) %>%
  bind_rows() %>%
  as_tibble() %>%
  select(!position) %>%
  rowid_to_column(var = "position") %>%
  mutate(price = price %>% str_remove(pattern = ",.*$")) %>% 
  mutate(price = price %>%  str_remove(pattern = "\\.")) %>% 
  mutate(price = as.numeric(price))
bike_mdl_vers_price_tbl


