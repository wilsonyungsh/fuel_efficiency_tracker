library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

con <- dbConnect(SQLite(), "data/fuel.sqlite")

raw <- readxl::read_xlsx("data/2010 Subaru Impreza.xlsx",sheet = "Fuel consumption",skip = 2) %>% 
  rename(date= "...2") %>% .[,1:8] %>% filter(!is.na(date)) %>% janitor::clean_names()
%>% 
  mutate(across(-c(year,date,odometer),.fns = ~ifelse(.x %in% c("NA","-"),NULL,.x)))

clean <- raw %>%
  transmute(
    date = as.character(date),
    odometer = odometer,
    litres = fuel_liter_filled,
    fuel_type = case_when(
      grepl("98", fuel_type) ~ "98",
      grepl("95", fuel_type) ~ "95",
      grepl("91", fuel_type) ~ "91",
      grepl("E10", fuel_type, ignore.case = TRUE) ~ "E10",
      TRUE ~ "98"
    )
  )

dbWriteTable(
  con,
  "fuel",
  clean,
  append = TRUE
)

dbDisconnect(con)
