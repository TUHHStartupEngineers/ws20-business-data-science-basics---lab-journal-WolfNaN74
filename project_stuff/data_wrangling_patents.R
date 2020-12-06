# Doing some data wrangling with patents

# Website: https://www.patentsview.org/download/

# Answer the question with data.table or dplyr
# Tables needed for each question:
# 1: assignee, patent_assignee
# 2: assignee, patent_assignee, patent
# 3: assignee, patent_assignee, uspc


# 0.0 Loading the needed libraries ----
library(vroom)
library(data.table)
library(tidyverse)
library(lubridate)

# 1.0 Importing the data ----
# col_skip() als "datatype" to minimize the read things

# 1.1 Loading assignee ----
col_types <- list(
  id =           col_character(),
  type =         col_double(),
  name_first =   col_skip(),  #! col_skip since we dont need it here.
  name_last =    col_skip(),  #!
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = unz("00_data/patents/assignee.tsv.zip", "assignee.tsv"),
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL", " ", "na", "N/A")
)

# Print information on the dataset:
# Number of observations, column variables, data type for each variable,
# and number of distinct values for non-numeric variables.

head(assignee_tbl)
ncol(assignee_tbl)
nrow(assignee_tbl)
str(assignee_tbl)
# summary(assignee_tbl, na.rm=TRUE)


# 1.2 Loading patent_assignee ----
col_types <- list(
  patent_id =   col_character(),
  assignee_id = col_character(),
  location_id = col_skip()  #!
)

patent_assignee_tbl <- vroom(
  file       = unz("00_data/patents/patent_assignee.tsv.zip", "patent_assignee.tsv"),
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL", " ", "na", "N/A")
)

# Print information on the dataset:
head(patent_assignee_tbl)
ncol(patent_assignee_tbl)
nrow(patent_assignee_tbl)
str(patent_assignee_tbl)
# summary(patent_assignee_tbl, na.rm=TRUE)

# 1.3 Loading patent ----
col_types <- list(
  id =         col_character(),
  type =       col_skip(),  #!
  number =     col_skip(),  #!
  country =    col_skip(),  #!
  date =       col_date("%Y-%m-%d"),
  abstract =   col_skip(),  #!
  title =      col_skip(),  #!
  kind =       col_skip(),  #!
  num_claims = col_skip(),  #!
  filename =   col_skip(),  #!
  withdrawn =  col_skip()   #!
)

patent_tbl <- vroom(
  file       = unz("00_data/patents/patent.tsv.zip", "patent.tsv"),
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL", " ", "na", "N/A")
)

# Print information on the dataset:
head(patent_tbl)
ncol(patent_tbl)
nrow(patent_tbl)
str(patent_tbl)
# summary(patent_tbl, na.rm=TRUE)

# 1.4 Loading uspc ----
col_types <- list(
  uuid =         col_character(),
  patent_id =    col_character(),
  mainclass_id = col_character(),
  subclass_id =  col_character(),
  sequence =     col_double()
)

uspc_tbl <- vroom(
  file       = unz("00_data/patents/uspc.tsv.zip", "uspc.tsv"),
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL", " ", "na", "N/A")
)

# Print information on the dataset:
head(uspc_tbl)
ncol(uspc_tbl)
nrow(uspc_tbl)
str(uspc_tbl)
# summary(uspc_tbl, na.rm=TRUE)

# 2.0 Answering Questions ----

# 2.1 Patent Dominance: US company / corporation with the most patents ----
# List the 10 US companies with the most assigned/granted patents
# assignee, patent_assignee
# link patent_assignee(assignee_id) with assigneee(id)
# not needed: location, first and last name
# further info assingee(type = 2) == US Company or Corporation

# Converting the tables into data.table format
class(assignee_tbl)
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
# Preview for Structure etc.
assignee_tbl %>% glimpse()
patent_assignee_tbl %>% glimpse()

# Merging the data.tables
patent_domiance_data <- merge(x = patent_assignee_tbl, y = assignee_tbl,
                              by.x  = "assignee_id",
                              by.y  = "id",
                              all.x = TRUE,
                              all.y = FALSE)
patent_domiance_data %>% glimpse()

# Get the top 10 Patent Companies in the US
patent_domiance_data[type == 2,
                     .N,
                     by = organization][order(-N)] %>%
  head(.,10)

# When using .N can i somehow keep the other columns. e.g.
#patent_domiance_data[type == 2, .N, by = assignee_id] but still have col org as well?

# 2.2 Recent patent activity: US company with most patents granted in 2019 ----
# List top 10 companies with the most new granted patents for 2019
# assignee, patent_assignee, patent

# Creating the Data Tables
#setDT(assignee_tbl)         # created above already
#setDT(patent_assignee_tbl)  # created above already
setDT(patent_tbl)

assignee_tbl %>% glimpse()
patent_assignee_tbl %>% glimpse()
patent_tbl %>% glimpse()

# Merging the data.tables
# take existing merged table "patent_domiance_data"
recent_patent_activity_data <- merge(x = patent_domiance_data, y = patent_tbl,
                                    by.x  = "patent_id",
                                    by.y  = "id",
                                    all.x = TRUE,
                                    all.y = FALSE)
recent_patent_activity_data %>% glimpse()

# Get the top 10 Organizations with patent numbers in 2019
recent_patent_activity_data[type == 2 & year(ymd(date)) == "2019",
                            .N,
                            by = organization][order(-N)] %>%
  head(.,10)

# X.3 Innovation in Tech: Most innovative sector ----
# Top 10 companies (worldwide) with most patents,
# what are the top 5 USPTO tech main classes?
# 3: assignee, patent_assignee, uspc

setDT(uspc_tbl)
uspc_tbl %>% glimpse()

# Merging the data.tables
# take existing merged table "patent_domiance_data"
patent_innovation_data <- merge(x = patent_domiance_data, y = uspc_tbl,
                                     by    = "patent_id",
                                     all.x = TRUE,
                                     all.y = FALSE)
patent_innovation_data %>% glimpse()

# Top 10 worldwide
most_patents_world <- patent_innovation_data[!is.na(organization), .N, by = organization][order(-N)] %>%
  head(.,10)
most_patents_world

# Main classes of the top 10
top_5_main_classes <-  patent_innovation_data[!is.na(mainclass_id) &
                          organization %in% most_patents_world$organization,
                          .N,
                          by = mainclass_id
                          ][order(-N)] %>%
  head(.,5)
glimpse(top_5_main_classes)

# Investigating the mainclasses further:

# Load mainclass_current
col_types <- list(
  id =     col_character(),
  title =  col_character()
)

mainclass_tbl <- vroom(
  file       = unz("00_data/patents/mainclass_current.tsv.zip", "mainclass_current.tsv"),
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL", " ", "na", "N/A")
)
glimpse(mainclass_tbl)
setDT(mainclass_tbl)
mainclass_tbl[id %in% top_5_main_classes$mainclass_id]

# I guess it would have been more clever to first filter and use another
# join-mechanism to save computational time and have smaller tables to work with
