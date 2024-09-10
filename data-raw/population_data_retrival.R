library(tidyverse)

years <- list(year = c(2015:2022))
pop_data = list()

for (i in 1:length(years$year)){
county_pops <- tidycensus::get_estimates(geography = "county", state = "LA", product = "population", year = years$year[i]) %>%
  mutate(year = years$year[i])
pop_data[[i]] = county_pops
}


pop_data_2015_2023 <- bind_rows(pop_data) %>%
  filter(variable=='POP'| variable=="POPESTIMATE") %>%
  select(-c(variable))

save(pop_data_2015_2023, file ="data-raw//pop_data_2015_2023.rda")
