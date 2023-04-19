library(tidyverse)
library(openxlsx)
library(anytime)
library(MMWRweek)
library(lubridate)

load(file = 'data-raw//data_reading.rda')

letters <- data %>%
  filter(str_detect(Week, "[:alpha:]")) %>%
  mutate(Year = str_extract(Week, "[:digit:]{4}"),
         Week = str_extract(Week, ".{2,3}(?=[:blank:])")) %>%
  filter(str_detect(Week, "[:digit:]")) %>%
  mutate(Month = str_extract(Week, "[:alpha:]{2}"),
         Week = str_extract(Week, "[:digit:]")) %>%
  mutate(Month = case_when(Month == "JN" ~ 6,
                           Month == "JL" ~ 7,
                           Month == "AU" ~ 8,
                           Month == "SE" ~ 9,
                           Month == "OC" ~ 10,
                           Month == "NO" ~ 11,
                           Month == "DE" ~ 12)) %>%
  # mutate(month = lubridate::ym(paste(Year, Month, sep="-"))) %>%
  mutate(yearweek = as.numeric(strftime(my(paste0(Month,"-",Year)), format = "%U")) + as.numeric(Week)) %>%
  mutate(StartDate = MMWRweek2Date(as.numeric(Year), as.numeric(yearweek))) %>%
  mutate(EndDate = StartDate + days(7)) %>%
  select(Parish, StartDate, EndDate, Cases)

week_num <- data %>%
  filter(str_detect(Week, "[:digit:]{2} [:digit:]{4}")) %>%
  filter(!str_detect(Week, "-")) %>%
  mutate(Year = str_extract(Week, "[:digit:]{4}"),
         Week = str_extract(Week, "[:digit:]{1,2}(?=[:blank:])"))%>%
  mutate(StartDate = MMWRweek2Date(as.numeric(Year), as.numeric(Week))) %>%
  mutate(EndDate = StartDate + days(7)) %>%
  select(Parish, StartDate, EndDate, Cases)

with_dash <- data %>%
  filter(str_detect(Week, "-")) %>%
  mutate(Year = str_extract(Week, "[:digit:]{4}"),
         Week = str_extract(Week, "[:digit:]{1,2}-[:digit:]{1,2}")) %>%
  mutate(Week = str_extract(Week, "[:digit:]{1,2}(?=-)")) %>%
  mutate(StartDate = MMWRweek2Date(as.numeric(Year), as.numeric(Week))) %>%
  mutate(EndDate = StartDate + days(7)) %>%
  select(Parish, StartDate, EndDate, Cases)


dig4 <- data %>%
  filter(str_detect(Week, "(?<![:digit:]{2}[:blank:])[:digit:]{4}")) %>%
  filter(str_detect(Week, "[:alpha:]")==FALSE & str_detect(Week, "-")==FALSE) %>%
  mutate(Year = str_extract(Week, "[:digit:]{2}"),
         Week = str_sub(Week, start = -2, end = -1)) %>%
  mutate(StartDate = MMWRweek2Date(as.numeric(paste0("20",Year)), as.numeric(Week))) %>%
  mutate(EndDate = StartDate + days(7)) %>%
  select(Parish, StartDate, EndDate, Cases)

corrected <- week_num %>%
  full_join(with_dash) %>%
  full_join(dig4) %>%
  full_join(letters)


save(corrected, file ="data-raw//time_correction.rda")
