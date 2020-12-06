# Guardian API ----
# https://open-platform.theguardian.com/documentation/search

# Known Bugs... Problem with the Decoding
# Does not handle ' and -

# example query:
# https://content.guardianapis.com/search?q=12%20years%20a%20slave&
# format=json&tag=film/film,tone/reviews&from-date=2010-01-01&
# show-tags=contributor&show-fields=starRating,headline,thumbnail,short-url&
# show-refinements=all&order-by=relevance

# 1.0 Loading Libraries ----
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)

# 2.0 Creating the query ----

# 2.1 Base URL ----
cont_endpoint_url = "https://content.guardianapis.com/search"

# 2.2 Search Item
# AND, AND NOT, OR can be used here
search_item <- "Hydrogen"  # Some User Input should be used here
page_size <- '20'  # possible form 1-50
ordering <- "newest"  # possilbe: newest, oldest, relevance

# 3.0 Connecting to the API ----
resp <- GET(cont_endpoint_url, query = list(
            q = search_item,
            "order-by" = ordering,
            format = 'json',
            "page-size" = page_size,
            "api-key" = Sys.getenv('guard_api_key')))
resp
# 3.1 Check if the Status is okay, else fire a warning ----
# status <- status_code(resp)
http_status <- http_status(resp)
http_status$message
warn_for_status(resp)
stop_for_status(resp)
# header <- headers(resp)

# 4.0 Convert the API Content ----

# 4.1 Check encoding ----
stringi::stri_enc_detect(content(resp, "raw"))

# 4.2 Convert to nice and tidy JSON ----
# rawToChar(resp$content)
# glimpse(resp)
json_resp <- resp %>%
  .$content %>%
  rawToChar() %>%
  fromJSON()
# 4.3 Derive the Data ----
my_info <- tibble(totalFindings = json_resp$response$total,
                  pageSize = json_resp$response$pageSize,
                  currentPage = json_resp$response$currentPage,
                  numberPages = json_resp$response$pages,
                  order = json_resp$response$orderBy)
glimpse(my_info)


my_data <- tibble(webTitle = json_resp$response$results$webTitle,
                  webUrl = json_resp$response$results$webUrl,
                  webPublicationDate = json_resp$response$results$webPublicationDate,
                  section = json_resp$response$results$sectionName)
glimpse(my_data)

# 5.0 Work with the data ----
# Split Date and Time
