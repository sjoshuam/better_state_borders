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
library(mapproj)
library(sf)

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
us_map     <- map_data("usa")

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
    state_all = str_remove_all(cbsa_name, ".+, "),
    state = state_all %>% str_remove_all("-.*"),
    cbsa_name_all = cbsa_name,
    cbsa_name = str_remove_all(cbsa_name, ", .+") %>% str_remove_all("-.*"),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    short_name = str_replace_all(cbsa_name,
      pattern = "([A-Z][a-z])[^ ]+ ([A-Z][a-z]).*",
      replacement = "\\1\\2") %>%
      str_sub(end = 4)
    )
cbsa_data$short_name[cbsa_data$short_name == "Wash"] <- "DC"

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

## shape us map data
us_map <- us_map %>%
  as_tibble() %>%
  filter(region == "main") %>%
  rename(lon = long)

## shape population data; incorporate into county object
population <- population %>%
  mutate(county = str_remove_all(county, ".*US"))
county_data <- county_data %>%
  left_join(select(population, county, population), by = "county")

## incorporate population into cbsa object
temp <- county_data %>%
  filter(!is.na(cbsa)) %>%
  group_by(cbsa) %>%
  summarize(population = sum(population))
cbsa_data <- cbsa_data %>%
  left_join(temp, by = "cbsa") %>%
  arrange(desc(population))
remove(temp)

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

## exclude counties that make map rendition more difficult
exclude_list <- c("53055", "53029")
county_data <- filter(county_data, !(county %in% exclude_list))
county_map <- filter(county_map, !(county %in% exclude_list))

## PRECALCULATE ALTERNATIVE STATES ASSIGNMENTS AS MUCH AS POSSIBLE =============

## move metro's that are split evenly between cities into less populous state
cbsa_data$unified_state <- cbsa_data$state
cbsa_data$unified_state[cbsa_data$cbsa == "28140"] <- "KS"
cbsa_data$unified_state[cbsa_data$cbsa == "19340"] <- "IA"
cbsa_data$unified_state[cbsa_data$cbsa == "45500"] <- "AR"
cbsa_data$unified_state[cbsa_data$cbsa == "49020"] <- "WV"
cbsa_data$unified_state[cbsa_data$cbsa == "14140"] <- "VA"
cbsa_data$unified_state[cbsa_data$cbsa == "25180"] <- "WV"
cbsa_data$unified_state[cbsa_data$cbsa == "19060"] <- "WV"

  # "48181", # Grayson TX
  # "48147", # Fannin TX
  # "48097", # Cooke TX
  # "48337", # Montague TX

county_data <- county_data %>%
  left_join(select(cbsa_data, cbsa, unified_state), by = "cbsa") %>%
  mutate(unified_state = if_else(!is.na(unified_state), unified_state, state))

## solve TX enclave problem
county_data[county_data$county == "48181", "cbsa"] <- "19100"


## move counties as needed to make geographic areas more contiguous
county_data <- county_data %>% mutate(contiguous_state = NA)
FindCounties <- function(counties, code, c_data = county_data) {
  counties <- as.character(counties)
  counties <- c_data$county %in% counties
  c_data$contiguous_state[counties] <- code
  return(c_data)
}
county_data <- FindCounties(
  counties = c(26003, 26013, 26033, 26041, 26043, 26053, 26061, 26071, 26083,
    26095, 26097, 26103, 26109, 26131, 26153),
  code = "WI->MI"
  )
county_data <- FindCounties(
  counties = c(12033, 12113, 12091, 12131, 12059, 12133, 12005, 12063, 12013,
    12045),
  code = "AL->FL"
  )
county_data <- FindCounties(counties = c(51001, 51131), code = "MD->VA")
county_data <- FindCounties(counties = c(24023, 24001), code = "WV->MD")
county_data <- FindCounties(counties = c(24015), code = "MD->MD")
county_data <- FindCounties(counties = c(24037), code = "DC->MD")
county_data <- FindCounties(counties = c(40007, 40025, 40139), code = "KS->OK")

## split mega-cities off from their states
mega_population <- cbsa_data %>%
  filter(population >= 5 * 10^6) %>%
  select(cbsa, population, cbsa_name) %>%
  rename(mega_population = population) %>%
  mutate(
    mega_name = str_replace_all(cbsa_name, "([A-Z][a-z]{3,3})[a-z]*", "\\1") %>%
      str_replace_all("( [A-Z][a-z]).*", "\\1") %>%
      str_replace_all("^([A-Z][a-z])[^ ]* ", "\\1")
    )

county_data <- county_data %>%
  left_join(mega_population, by = "cbsa") %>%
  mutate(mega_population = if_else(is.na(mega_population), 0, mega_population))
remove(mega_population)

## DECLARE INEQUALITY MEASUREMENT FUNCTION =====================================

MeasureInequality <- function(state_col, c_data = county_data) {
  inequality <- c_data %>%
    rename(state_col = starts_with(state_col)) %>%
    group_by(state_col) %>%
    summarize(area = sum(area), population = sum(population)) %>%
    mutate(
      area = area / sum(area),
      population = population / sum(population),
      weight = 1 / length(population)
      ) %>%
    arrange(area) %>%
    mutate(
      area_cum = 1 - cumsum(weight),
      area_gini = area * (weight + 2 * area_cum)
      ) %>%
    arrange(population) %>%
    mutate(
      pop_cum = 1 - cumsum(weight),
      pop_gini = population * (weight + 2 * pop_cum)
      ) %>%
    summarize(area = 1 - sum(area_gini), pop = 1 - sum(pop_gini)) %>%
    unlist() %>%
    round(digits = 3)

  return(inequality)
}

## GENERATE PROJECTED COORDINATESS =============================================

## move cbsa center closer to core city's center for exceptional cases
i <- match(c("Riverside", "Flagstaff", "Phoenix"), cbsa_data$cbsa_name)
cbsa_data[i, "lon"] <- c(-117.396, -111.631, -112.067)
cbsa_data[i, "lat"] <- c(33.948,     35.199,   33.45)

## generate projected coordinates for county centroids
temp <- mapproject(x = county_data$lon, y = county_data$lat,
  projection = "sinusoidal", orientation = c(90, 0, -97.5))
county_data$x <- temp$x
county_data$y <- temp$y
remove(temp)

## generate projected coordinates for county centroids
temp <- mapproject(x = cbsa_data$lon, y = cbsa_data$lat,
  projection = "sinusoidal", orientation = c(90, 0, -97.5))
cbsa_data$x <- temp$x
cbsa_data$y <- temp$y
remove(temp)

## GENERATE BRIDGES BETWEEN DISCONNECTED GEOGRAPHIES ===========================

IncorporateBridge <- function(the_county, bridge_poly, c_map = county_map){

  ## combine polygons
  all_poly <- county_map %>%
    filter(county == the_county) %>%
    select(lon, lat) %>%
    list(bridge_poly) %>%
    lapply(as.matrix) %>%
    lapply(list) %>%
    lapply(st_polygon) %>%
    st_sfc() %>%
    st_combine() %>%
    st_union(by_feature = TRUE) %>%
    st_coordinates() %>%
    as_tibble()

  ## format for reintroduction
  all_poly <- all_poly[, 1:2]
  colnames(all_poly) <- c("lon", "lat")
  all_poly$county <- the_county
  all_poly$order <- seq(nrow(all_poly)) + max(c_map$order)
  all_poly <- all_poly %>%
    left_join(
      filter(c_map, county == the_county) %>% select(-lon, -lat, -order),
      by = "county")
  all_poly <- all_poly[, colnames(c_map)]
  
  ## reintroduce the new data
  c_map <- c_map %>%
    filter(county != the_county) %>%
    bind_rows(all_poly)

  return(c_map)
  }

## MI Upper Peninsula (26097)
bridge_polygon = tibble(
    "lon" = c(-84.721703, -84.731913, -84.732425, -84.726080, -84.721703),
    "lat" = c( 45.8579, 45.7882, 45.7882, 45.8579, 45.8579)
    )
county_map <- IncorporateBridge("26097", bridge_poly = bridge_polygon)

# ## NC Chowan (37041)
# bridge_polygon = tibble(
#     "lon" = c(-76.520715, -76.516389, -76.478931, -76.484562, -76.520715),
#     "lat" = c(36.016351, 36.016183, 35.942971, 35.941078, 36.016351)
#     )
# county_map <- IncorporateBridge("37041", bridge_poly = bridge_polygon)

## VA Northampton (51131)
bridge_polygon = tibble(
    "lon" = c(-75.948098,-76.008485,-76.040573,-75.966278,-75.948098),
    "lat" = c(37.120555+10^-2,36.912550,36.914110,37.120654+10^-2,37.120555+10^-2)
    )
county_map <- IncorporateBridge("51131", bridge_poly = bridge_polygon)

## PRE-CALCULATE SF FORMAT COUNTY POLYGONS =====================================

## pre-calculate sf-format county polygons
extra_buffer <- c(
  "37041", # Chowan NC
  "37139", # Pasquotank NC
  "37143", # Perquimans NC
  "37015", # Bertie NC
  "37053", # Currituck NC
  "37187", # Washington NC
  "51810", # Virginia Beach NC
  "37083", # Halifax NC,
  "37029", # Camden NC
  "37073", # Gates NC
  "37091", # Hertford NC
  # "48181", # Grayson TX
  # "48147", # Fannin TX
  # "48097", # Cooke TX
  # "48337", # Montague TX
  "99999"
  )

## convert map polygons into sf-format polygons
county_polygon <- as.data.frame(county_map[, c("lon", "lat", "group")]) 
county_polygon <- split(select(county_polygon, lon, lat), f = county_polygon$group)
county_polygon <- lapply(county_polygon, as.matrix)
county_polygon <- lapply(county_polygon, list)
county_polygon <- lapply(county_polygon, st_polygon)
county_polygon <- lapply(county_polygon, st_buffer, dist = 10^-3)

if (!is.null(extra_buffer)) {
  extra_buffer <- county_map$group[county_map$county %in% extra_buffer] %>%
    unique() %>%
    as.character()
  county_polygon[extra_buffer] <- lapply(
    county_polygon[extra_buffer], st_buffer, dist = 10^-1.2)
}

## divide polygon list into states
county_map <- left_join(
  county_map,
  select(county_data, county, unified_state),
  by = "county"
  ) 


i <- county_map$unified_state[match(names(county_polygon),
  as.character(county_map$group))]
state_polygon <- tapply(county_polygon, i, list)
remove(i)
  
## unify polygons in each state
Unify <- function(x) {
  x <- sf::st_sfc(x)
  x <- sf::st_combine(x)
  x <- sf::st_union(x, by_feature = TRUE)
  return(x)
}
state_polygon <- lapply(X = state_polygon, FUN = Unify)

## EXPORT DATA =================================================================

## export area data
saveRDS(county_data, file = "B_Intermediates/county_data.RData")
saveRDS(cbsa_data, file = "B_Intermediates/cbsa_data.RData")

## export map data
saveRDS(county_map, file = "B_Intermediates/county_map.RData")
saveRDS(state_map, file = "B_Intermediates/state_map.RData")
saveRDS(us_map, file = "B_Intermediates/us_map.RData")

## export sf polygons
saveRDS(county_polygon, file = "B_Intermediates/county_polygon.RData")
saveRDS(state_polygon, file = "B_Intermediates/state_polygon.RData")

##########==========##########==========##########==========##########==========

