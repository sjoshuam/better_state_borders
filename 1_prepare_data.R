##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M.
## creation: 2021-03-07
## version: R v4.0.4
## description: prepares county and msa data for analysis

## environment set up
remove(list = objects())
options(dplyr.summerise.inform = FALSE, width = 80, scipen = 2, digits = 6)
library(tidyverse)
library(foreign)
library(sp)

## READ IN DATA ================================================================

## read in census datasets
cbsa_data <- read.dbf("A_Inputs/tl_2020_us_cbsa.dbf", as.is = TRUE)
county_data <- read.dbf("A_Inputs/tl_2020_us_county.dbf", as.is = TRUE)
state_data <- read.delim("A_Inputs/state.txt", sep = "|", as.is = TRUE,
  colClasses = "character")

## read in population data
unzip("A_Inputs/ACSDT5Y2019.B01003_2021-03-08T131331.zip",
 files = "ACSDT5Y2019.B01003_data_with_overlays_2021-03-04T082306.csv",
  exdir = "A_Inputs")
population <- read_csv(skip = 1,
  "A_Inputs/ACSDT5Y2019.B01003_data_with_overlays_2021-03-04T082306.csv")
colnames(population) <- c("county", "county_name", "population", "moe")
file.remove(
  "A_Inputs/ACSDT5Y2019.B01003_data_with_overlays_2021-03-04T082306.csv")

## read in county and state maps
county_map <- map_data("county")
state_map  <- map_data("state")

## SHAPE DATA ==================================================================

## shape state data
state_data <- state_data %>%
  select(-STATENS) %>%
  rename(state_fips = STATE, state = STUSAB, state_name = STATE_NAME) %>%
  as_tibble()

## shape CBSA data
cbsa_data <- cbsa_data %>%
  as_tibble() %>%
  select(GEOID, NAME, AWATER, LSAD, INTPTLON, INTPTLAT, ALAND) %>%
  rename(cbsa = GEOID, cbsa_name = NAME, state = AWATER, city_type = LSAD,
    area = ALAND, lon = INTPTLON, lat = INTPTLAT) %>%
  mutate(
    city_type = if_else(city_type == "M1", "metro", "micro"),
    state = str_remove_all(cbsa_name, ".+, ") %>% str_remove_all("-.*"),
    cbsa_name_all = cbsa_name,
    cbsa_name = str_remove_all(cbsa_name, ", .+") %>% str_remove_all("-.*"),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
    )

## shape county data
county_data <- county_data %>%
  as_tibble() %>%
  select(GEOID, NAME, STATEFP, CBSAFP, INTPTLON, INTPTLAT, ALAND) %>%
  rename(county = GEOID, county_name = NAME, state = STATEFP, cbsa = CBSAFP,
    area = ALAND, lon = INTPTLON, lat = INTPTLAT) %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    state = state_data$state[match(state, str_to_lower(state_data$state_fips))],
    county_name = paste(str_to_title(county_name), state)
    ) %>%
  filter(!(state %in% c("AS", "AK", "HI", "PR", "VI", "MP", "GU")))

## shape county map data
county_map <- county_map %>%
  as_tibble() %>%
  mutate(
    state = state_data$state[match(region, str_to_lower(state_data$state_name))],
    county_name = paste(str_to_title(subregion), state)
    ) %>%
  select(-region, -subregion) %>%
  rename(lon = long)

## shape state map data
state_map <- state_map %>%
  as_tibble() %>%
  mutate(
    state = state_data$state[match(region, str_to_lower(state_data$state_name))],
    ) %>%
  select(-region, -subregion) %>%
  rename(lon = long) %>%
  filter(!(state %in% c("AS", "AK", "HI", "PR", "VI", "MP", "GU")))

remove(state_data)

## shape population data; incorporate into 
population <- population %>%
  mutate(county = str_remove_all(county, ".*US"))
county_data <- county_data %>%
  left_join(select(population, county, population), by = "county")

## add county fips codes to county map data
county_map <- county_map %>%
  mutate(
    county_name = county_name %>%
      str_replace_all("^De ", "De") %>%
      str_replace_all("^Du ", "Du") %>%
      str_replace_all("^La ", "La") %>%
      str_replace_all("^St ", "St. ") %>%
      str_replace_all("^Ste ", "Ste. ") %>%
      str_remove_all(" [A-Z][A-Z]$") %>%
      str_to_title() %>%
      paste(state)
    )
county_map$county_name <- recode(
  county_map$county_name,
  "Queen Annes MD" = "Queen Anne's MD",
  "St. Marys MD" = "St. Mary's MD",
  "Prince Georges MD" = "Prince George's MD",
  "Washington DC" = "District Of Columbia DC",
  "Laplata CO" = "La Plata CO",
  "Dewitt IL" = "De Witt IL",
  "Obrien IA" = "O'brien IA",
  "Desoto LA" = "De Soto LA",
  "Debaca NM" = "De Baca NM",
  "Dona Ana NM" = "Doña Ana NM",
  "Shannon SD" = "Oglala Lakota SD",
  "Lasalle TX" = "La Salle TX",
  "Lapaz AZ" = "La Paz AZ",
  "Shannon SD" = "La Crosse WI",
  "Lacrosse WI" = "La Crosse WI",
  "Baltimore City MD" = "Baltimore MD",
  "St. Louis City MO" = "St. Louis MO"
  )

county_map <- county_map %>%
  left_join(select(county_data, county_name, county), by = "county_name") %>%
  filter(!is.na(county))

# ## DETERMINE BASIC COUNTY RELATIONSHIPS ========================================
# 
# ## calculate geographic distance between counties
# distance_matrix <- county_data %>%
#   select(lon, lat) %>%
#   as.matrix %>%
#   spDists(longlat = TRUE)
# colnames(distance_matrix) <- county_data$county
# distance_matrix <- distance_matrix %>%
#   as_tibble() %>%
#   mutate(origin = county_data$county) %>%
#   pivot_longer(cols = county_data$county, names_to = "dest") %>%
#   mutate(key = paste(origin, dest, sep = "_")) %>%
#   arrange(key)
# 
# ## determine which counties share a border
# adjacent_counties <- county_map %>%
#   mutate(key = paste(round(lon, 2), round(lat, 2))) %>%
#   select(county, key)
# adjacent_counties <- left_join(adjacent_counties, adjacent_counties,
#   by = "key") %>%
#   select(-key) %>%
#   unique() %>%
#   rename(origin = county.x, dest = county.y) %>%
#   mutate(value = 1, key = paste(origin, dest, sep = "_")) %>%
#   arrange(key)
# 
# ## determine which counties are part of the same state
# same_state <- outer(county_data$state, county_data$state, FUN = "==")
# colnames(same_state) <- county_data$county
# same_state <- as_tibble(same_state) %>%
#   mutate(origin = county_data$county) %>%
#   pivot_longer(cols = county_data$county, names_to = "dest") %>%
#   filter(value) %>%
#   mutate(value = as.numeric(value), key = paste(origin, dest, sep = "_")) %>%
#   arrange(key)

## EXPORT DATA =================================================================

## export area data
saveRDS(county_data, file = "B_Intermediates/county_data.RData")
saveRDS(cbsa_data,   file = "B_Intermediates/cbsa_data.RData")

## export map data
saveRDS(county_map, file = "B_Intermediates/county_map.RData")
saveRDS(state_map,  file = "B_Intermediates/state_map.RData")

# ## export area relationships data
# saveRDS(distance_matrix,   file = "B_Intermediates/distance_matrix.RData")
# saveRDS(adjacent_counties, file = "B_Intermediates/adjacent_counties.RData")
# saveRDS(same_state,        file = "B_Intermediates/same_state.RData")

##########==========##########==========##########==========##########==========

