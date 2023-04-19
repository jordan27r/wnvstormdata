library(tidyverse)
library(openxlsx)
library(hurricaneexposuredata)
library(lubridate)
library(stormwindmodel)
library(tigris)
library(sf)

louisiana <- counties(state="LA", class = "sf", cb = T) %>%
  mutate(fips = paste0(STATEFP,COUNTYFP))

counties <- county_points %>%
  filter(gridid %in% louisiana$fips)


#wind speeds - set 34 knots threshold - vmax_sustained

load("data-raw//fips_linking.rda")

wnv <- wnv %>%
  left_join(louisiana, by = c("county_code" = "COUNTYFP")) %>%
  mutate(Fips = paste0(STATEFP,county_code),
         record_date = interval(start = StartDate, end = EndDate, tzone = "UTC")) %>%
  select(Parish:county_code, Fips, StartDate, EndDate, record_date)

#get_grid_winds(hurr_tracks[1:100,], county_points)

storm_time <- hurr_tracks %>%
  mutate(ymd_date = lubridate::ymd_hm(date)) %>%
  filter(ymd_date %within% lubridate::interval(start = min(wnv$StartDate), end = max(wnv$EndDate), tzone = "UTC") & wind > 30) %>%
  group_by(storm_id) %>%
  group_modify(~get_grid_winds(hurr_track = .x, grid_df = counties)) %>%
  mutate(vmax_sust_knots = vmax_sust * 1.9) %>%
  filter(vmax_sust_knots > 34) %>%
  select(storm_id, gridid, date_time_max_wind, vmax_sust_knots)


wnv$Exposure = 0
wnv$Storm = "NA"

for (i in 1:nrow(wnv)){
  for (j in 1:nrow(storm_time)){
    if(storm_time$gridid[j] == wnv$Fips[i]){
      if (storm_time$date_time_max_wind[j] %within% wnv$record_date[i]){
        wnv$Exposure[i] = 1
        wnv$Storm[i] = storm_time$storm_id[j]
      }
    }
  }
}


wnv_2002_2021_stormexposure <- wnv %>%
  select(Parish, Fips, StartDate, EndDate, Cases, Exposure, Storm)


save(wnv_2002_2021_stormexposure, file ="data//wnv_2002_2021_stormexposure.rda")
usethis::use_data(wnv_2002_2021_stormexposure, overwrite = TRUE)
