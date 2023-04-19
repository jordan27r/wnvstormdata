library(tidyverse)
library(openxlsx)
library(tigris)

load("data-raw//time_correction.rda")


lousiana <- list_counties(state="LA") %>%
  mutate(Parish = str_extract(county, ".{1,20}(?=[:blank:]Parish)")) %>%
  mutate(Parish = if_else(str_detect(Parish, "St."),paste0("St",str_extract(Parish,"(?<=[:punct:]).{1,40}")), Parish))


data_w_fips <- corrected %>%
  mutate(Parish = str_squish(Parish))

for (i in 1:nrow(data_w_fips)){
  if (data_w_fips$Parish[i] == "EBatonRouge"){
    data_w_fips$Parish[i] = "East Baton Rouge"
  }else if (data_w_fips$Parish[i] == "EFeliciana"){
    data_w_fips$Parish[i] = "East Feliciana"
  }else if (data_w_fips$Parish[i] == "WBatonRouge" | data_w_fips$Parish[i] == "West Baron Rouge"){
    data_w_fips$Parish[i] = "West Baton Rouge"
  }else if (data_w_fips$Parish[i] == "W Feliciana"){
    data_w_fips$Parish[i] = "West Feliciana"
  }else if (data_w_fips$Parish[i] == "St. Charles"){
    data_w_fips$Parish[i] = "St Charles"
  }else if (data_w_fips$Parish[i] == "St John" | data_w_fips$Parish[i] == "St. John the Baptist"){
    data_w_fips$Parish[i] = "St John the Baptist"
  }else if(data_w_fips$Parish[i] == "Vermillion"){
    data_w_fips$Parish[i] = "Vermilion"
  }else if (data_w_fips$Parish[i] == "Jeff Davis"){
    data_w_fips$Parish[i] = "Jefferson Davis"
  }else if (data_w_fips$Parish[i] == "Lasalle"){
    data_w_fips$Parish[i] = "La Salle"
  }else if(data_w_fips$Parish[i] == "DeSoto"){
    data_w_fips$Parish[i] = "De Soto"
  }else if(data_w_fips$Parish[i] == "West Caroll"){
    data_w_fips$Parish[i] = "West Carroll"
  }else if(data_w_fips$Parish[i] == "StTammany" | data_w_fips$Parish[i] == "St. Tammany" | data_w_fips$Parish[i] == "St.Tammany"){
    data_w_fips$Parish[i] = "St Tammany"
  }else if (data_w_fips$Parish[i] == "St. Landry"){
    data_w_fips$Parish[i] = "St Landry"
  }else if (data_w_fips$Parish[i] == "St. Mary"){
    data_w_fips$Parish[i] = "St Mary"
  }else if (data_w_fips$Parish[i] == "St. Bernard"){
    data_w_fips$Parish[i] = "St Bernard"
  }else if (data_w_fips$Parish[i] == "St. Landry"){
    data_w_fips$Parish[i] = "St Landry"
  }else if (data_w_fips$Parish[i] == "St. Martin"){
    data_w_fips$Parish[i] = "St Martin"
  }else if (data_w_fips$Parish[i] == "St. Helena"){
    data_w_fips$Parish[i] = "St Helena"
  }else if (data_w_fips$Parish[i] == "St. James"){
    data_w_fips$Parish[i] = "St James"
  }else if (data_w_fips$Parish[i] == "Pointe Coupe"){
    data_w_fips$Parish[i] = "Pointe Coupee"
  }else if (data_w_fips$Parish[i] == "St Landryy"){
    data_w_fips$Parish[i] = "St Landry"
  }else{

  }
}

wnv <- data_w_fips %>%
  left_join(lousiana, by = c("Parish"="Parish")) %>%
  filter(!str_detect(Parish, "<")&!str_detect(Parish, "LDH"))

save(wnv, file = "data-raw//fips_linking.rda")
