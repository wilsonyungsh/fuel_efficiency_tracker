library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

con <- dbConnect(SQLite(), "data/car_log.sqlite")

raw <- readxl::read_xlsx("data/2010 Subaru Impreza.xlsx", sheet = "Fuel consumption", skip = 2) %>%
  rename(date = "...2") %>% .[, 1:8] %>% filter(!is.na(date)) %>% janitor::clean_names() %>%
  mutate(lag_odometer = lag(odometer)) %>% relocate(lag_odometer, .before = odometer) %>%
  filter(!is.na(lag_odometer)) %>%
  mutate(date = as.character(date), across(contains("liter"), .fns = as.numeric),
    fuel_type = str_remove(toupper(fuel_type), pattern = "UNLEAD "))


clean <- raw %>% select(date, odometer, fuel_liter_filled, fuel_type) %>%
  rename(litres = fuel_liter_filled)
dbWriteTable(
  con,
  "fuel",
  clean,
  append = FALSE
)

dbDisconnect(con)
