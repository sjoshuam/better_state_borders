##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M.
## creation: 2021-03-07
## version: R v4.0.4
## description: 

## environment set up
remove(list = objects())
options(dplyr.summarise.inform = FALSE, width = 80, scipen = 2, digits = 6,
  start_time = Sys.time())
library(tidyverse)
library(sp)
library(mapproj)

## READ IN DATA ================================================================

## import area data
county_data <- readRDS(file = "B_Intermediates/county_data.RData")
cbsa_data   <- readRDS(file = "B_Intermediates/cbsa_data.RData")

## import map data
us_map <- map_data("usa")
state_map   <- readRDS(file = "B_Intermediates/state_map.RData")
county_map  <- readRDS(file = "B_Intermediates/county_map.RData")
cell_map <- readRDS(file = "B_Intermediates/cell_map.RData")

## import area relationship data
adjacent_counties   <- readRDS(file = "B_Intermediates/adjacent_counties.RData")
same_state   <- readRDS(file = "B_Intermediates/same_state.RData")

## PREPARE OBJECTS =============================================================

## prepare map objects
us_map <- us_map %>%
  filter(region == "main") %>%
  as_tibble() %>%
  rename(lon = long)

## create points centers for zones
cell_centers <- cell_map %>%
  group_by(group) %>%
  summarize(lon = mean(lon), lat = mean(lat))

## generate county to zone distance matrix
if(!file.exists("B_Intermediates/cell_county_distance.RData")) {

cell_county_distance <- spDists(
  select(county_data, lon, lat) %>% as.matrix(),
  select(cell_centers, lon, lat) %>% as.matrix,
  longlat = TRUE
  )

colnames(cell_county_distance) <- cell_centers$group

cell_county_distance <- cell_county_distance %>%
  as_tibble() %>%
  mutate(county = county_data$county) %>%
  pivot_longer(cols = cell_map$group, names_to = "zone") %>%
  mutate(value = round(value))

saveRDS(cell_county_distance,
  file = "B_Intermediates/cell_county_distance.RData")

} else cell_county_distance <- readRDS(
  "B_Intermediates/cell_county_distance.RData")

## calculate total cell population
cell_population <- cell_county_distance %>%
  arrange(value) %>%
  filter(!duplicated(county)) %>%
  left_join(select(county_data, county, population), by = "county") %>%
  group_by(zone) %>%
  summarize(population = sum(population))

cell_centers <- cell_centers %>%
  left_join(cell_population, by = c("group" = "zone")) %>%
  mutate(population = if_else(is.na(population), 0, population))

remove(cell_population)

## sum county populations to cbsa object
cbsa_data <- cbsa_data %>%
  left_join(
    county_data %>% group_by(cbsa) %>% summarize(population = sum(population)),
    by = "cbsa") %>%
  filter(!is.na(population)) %>%
  arrange(desc(population))

## FIND CURRENT STATE ==========================================================

## unpack state polygons into list
state_list <- split(
  state_map %>% select(lon, lat) %>% as.data.frame,
  state_map$group
  )

## determine which centroids are in which state
InPoly <- function(the_poly, all_points = cell_centers) {
  point.in.polygon(
    pol.x = the_poly$lon,
    pol.y = the_poly$lat,
    point.x = all_points$lon,
    point.y = all_points$lat) %>%
  as.logical()
}

state_list <- sapply(state_list, InPoly) %>%
  apply(MARGIN = 1, which) %>%
  as_tibble() %>%
  left_join(
    state_map %>% select(group, state) %>% filter(!duplicated(group)),
    by = c("value" = "group")
    )

remove(InPoly)

##  incorporate results into data
cell_centers <- cell_centers %>% mutate(state = state_list$state)
cell_map <- cell_map %>%
  left_join(select(cell_centers, group, state), by = "group")
remove(state_list)

## FIND NEIGHBOR'S STATE MAJORITY SOLUTION =====================================

majority_state <- cell_county_distance %>%
  filter(value < 100) %>%
  left_join(select(county_data, population, state, county), by = "county") %>%
  group_by(zone, state) %>%
  summarize(population = sum(population)) %>%
  arrange(desc(population)) %>%
  filter(!duplicated(zone)) %>%
  rename(neighbor_majority = state)

##  incorporate results into data
cell_centers <- cell_centers %>%
  left_join(select(majority_state, zone, neighbor_majority),
    by = c("group" = "zone")) %>%
  mutate(
 neighbor_majority = if_else(is.na(neighbor_majority), state, neighbor_majority)
    )

cell_map <- cell_map %>%
  left_join(select(cell_centers, group, neighbor_majority), by = "group")
remove(majority_state)

## FIND EQUAL AREA SOLUTION ====================================================

## divide cells into roughly equal group
cell_centers$equal_area <- select(cell_centers, lon, lat) %>%
  as.matrix() %>%
  kmeans(centers = 48, iter.max = 10^3, nstart = 10^2) %>%
  pluck("cluster")

## link clusters to perfect fit state where possible
state_crosswalk <- cell_centers %>%
  select(state, equal_area, population) %>%
  group_by(state, equal_area) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>%
  arrange(desc(population)) %>%
  rename(state_ea = state) %>%
  mutate(selected = !duplicated(state_ea) & !duplicated(equal_area))

## link remaining clusters to next best choice
NextBest <- function(dat) {
  dat2 <- dat
  
  ## exclude first choice clusters
  i <- dat2$state_ea %in% dat2$state_ea[dat2$selected]
  j <- dat2$equal_area %in% dat2$equal_area[dat2$selected]
  dat2$state_ea[i | j]      <- "XX"
  dat2$equal_area[i | j]     <- 0L
  dat2$selected[i | j] <- FALSE

  ## find next best candidates
  dat2 <- dat2 %>%
    mutate(selected = !duplicated(state_ea) & !duplicated(equal_area))
  dat <- dat %>%
    mutate(selected = selected | dat2$selected) %>%
    arrange(desc(selected), desc(population))
  
  return(dat)
}

state_crosswalk <- state_crosswalk %>%
  NextBest() %>%
  NextBest() %>%
  NextBest() %>%
  filter(!duplicated(equal_area))
remove(NextBest)

## add numbers to duplicate states
state_crosswalk <- state_crosswalk %>%
  arrange(state_ea) %>%
  mutate(
    num = seq(length(state_ea)),
    num = num - tapply(num, state_ea, min)[state_ea],
    state_ea = paste0(state_ea, if_else(num == 0, "", as.character(num)))
    )

## incorporate into cell objects
cell_centers <- cell_centers %>%
 left_join(select(state_crosswalk, state_ea, equal_area), by = "equal_area") %>%
  mutate(equal_area = state_ea) %>%
  select(-state_ea)

remove(state_crosswalk)

cell_map <- cell_map %>%
  left_join(select(cell_centers, group, equal_area), by = "group")

## FIND EQUAL POPULATION SOLUTION ==============================================

## convert county data to dot density format
equal_pop <- rep(
  seq(nrow(county_data)),
  round(county_data$population / 10^2)
  )

## calculate kmeans centers
equal_pop <- select(county_data, lon, lat)[equal_pop, ] %>%
  as.matrix() %>%
  kmeans(centers = 48, iter.max = 10^3, nstart = 10^2) %>%
  pluck("centers")

## assign cells to each center
cell_centers$cluster <- spDists(
  cell_centers %>% select(lon, lat) %>% as.matrix(), equal_pop,
  longlat = TRUE) %>%
  apply(MARGIN = 1, FUN = which.min)

remove(equal_pop)

## link clusters to existing states
InitialAssignment <- function(dat) {
  cell_centers %>%
    group_by(state, cluster) %>%
    summarize(population = sum(population)) %>%
    ungroup() %>%
    arrange(desc(population)) %>%
    mutate(excluded = FALSE, selected = FALSE)
  }

StateAssignment <- function(dat) {
  dat %>%
    mutate(
      selected = if_else(
        !duplicated(state) & !duplicated(cluster) & !excluded,
        TRUE, selected),
      excluded = (state %in% state[selected]) | (cluster %in% cluster[selected])
      ) %>%
  arrange(selected | excluded, desc(population))
}

NewStates <- function(dat) {
  dat %>%
    arrange(desc(selected), desc(population)) %>%
    filter(!duplicated(cluster)) %>%
    arrange(state) %>%
    mutate(
      num = seq(length(state)),
      num = num - tapply(num, state, min)[state],
      new_state = if_else(num == 0, "", as.character(num)),
      new_state = paste0(state, new_state)
      ) %>%
    select(-excluded, -selected, -num)
  }

crosswalk <- InitialAssignment(cell_centers) %>%
  StateAssignment() %>%
  StateAssignment() %>%
  StateAssignment() %>%
  NewStates()

## incorporate results into data objects
cell_centers <- cell_centers %>%
  left_join(select(crosswalk, cluster, new_state), by = "cluster") %>%
  rename(equal_pop = new_state) %>%
  select(-cluster)

cell_map <- cell_map %>%
  left_join(select(cell_centers, group, equal_pop), by = "group")

remove(crosswalk)

## FIND METROPOLIS-CENTERED SOLUTION ===========================================

## calculate cbsa to cbsa distance
cbsa_distance <- cbsa_data %>%
  select(lon, lat) %>%
  as.matrix() %>%
  spDists(longlat = TRUE)

colnames(cbsa_distance) <- cbsa_data$cbsa

cbsa_distance <- cbsa_data %>%
  as_tibble() %>%
  mutate(origin = cbsa_data$cbsa)

cbsa_distance <- cbsa_distance %>%
  pivot_longer(names_to = "dest", cols = cbsa_data$cbsa) %>%
  filter(value <= 200, origin != dest)

## drop cbsa that are close to larger cbsa
cbsa_data$primary_city <- TRUE

iter <- cbsa_data$cbsa[1]
for (iter in cbsa_data$cbsa) {
  not_outranked <- cbsa_data %>% filter(cbsa == iter) %>% pull(primary_city)
  if (!not_outranked) next
  i <- cbsa_distance %>% filter(origin == iter) %>% pull(dest)
  cbsa_data[cbsa_data$cbsa %in% i, "primary_city"] <- FALSE
}

remove(not_outranked, i, iter, cbsa_distance)

## assign cells to cbsa
cbsa_data <- cbsa_data %>% 
  mutate(primary_city = primary_city & (cumsum(primary_city) < 49))

cell_centers$cluster <- spDists(
  cbsa_data %>% filter(primary_city) %>% select(lon, lat) %>% as.matrix(),
  select(cell_centers, lon, lat) %>% as.matrix(),
  longlat = TRUE
  ) %>%
  apply(MARGIN = 2, which.min)

## assign clusters to states
crosswalk <- InitialAssignment(cell_centers) %>%
  StateAssignment() %>%
  StateAssignment() %>%
  StateAssignment() %>%
  NewStates()

## incorporate results into data objects
cell_centers <- cell_centers %>%
  left_join(select(crosswalk, cluster, new_state), by = "cluster") %>%
  rename(central_city = new_state) %>%
  select(-cluster)

cell_map <- cell_map %>%
  left_join(select(cell_centers, group, central_city), by = "group")

remove(crosswalk)

## CALCULATE COMPOSITE =========================================================

## extract all results and reshape as a long dataset
composite_score <- cell_centers %>%
  select(state, neighbor_majority, equal_area, equal_pop, central_city,
    group) %>%
  pivot_longer(
    cols = c("state", "neighbor_majority", "equal_area", "equal_pop",
      "central_city"),
    names_to = "criteria",
    values_to = "state"
    ) %>%
  mutate(state = paste(state, criteria, sep = "-"))



## EXPORT DATA =================================================================

## VISUALLY INSPECT ============================================================

## generate a color palette
color_v <- seq(from = 4 / 12, to = 12 / 12, by = 2 / 12)
color_h <- seq(from = 0 / 12, to = 11 / 12, by = 1/ 12)
color_palette <- sapply(color_h, FUN = hsv, s = 0.7, v = color_v) %>%
  cbind(gray(color_v - 2 / 12)) %>%
  as.vector()

temp <- arrange(cell_centers, lon)
temp <- c(temp$state, temp$central_city) %>% unique() # 1/4
names(color_palette)[seq(temp)] <- temp
remove(color_h, color_v, temp)

color_palette <- color_palette[!is.na(names(color_palette))]

## visually inspect
pdf("~/Desktop/central_city.pdf", width = 10, height = 5) # 2/4

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = us_map,
    mapping = aes(x = lon, y = lat),
    fill = "transparent", color = "black"
    ) +
  geom_polygon(
    data = cell_map,
    mapping = aes(x = lon, y = lat, group = group,
      fill = central_city, color = central_city), # 3/4
    size = 0.1
    ) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "none")

graphics.off()

##########==========##########==========##########==========##########==========
Sys.time() - options()$start_time


## Before
quantile(sort(round(tapply(cell_centers$population/10^6, cell_centers$state,
  sum),1)))
sd(sort(round(tapply(cell_centers$population/10^6, cell_centers$state,
  sum),1)))

## equal area
quantile(sort(round(tapply(cell_centers$population/10^6, cell_centers$equal_area,
  sum),1)))
sd(sort(round(tapply(cell_centers$population/10^6, cell_centers$equal_area,
  sum),1)))

## equal population
quantile(sort(round(tapply(cell_centers$population/10^6, cell_centers$equal_pop,
  sum),1)))
sd(sort(round(tapply(cell_centers$population/10^6, cell_centers$equal_pop,
  sum),1)))

## central city
quantile(sort(round(tapply(cell_centers$population/10^6, cell_centers$central_city,
  sum),1)))
sd(sort(round(tapply(cell_centers$population/10^6, cell_centers$central_city,
  sum),1)))

## neigbor majority
quantile(sort(round(tapply(cell_centers$population/10^6, cell_centers$neighbor_majority,
  sum),1)))
sd(sort(round(tapply(cell_centers$population/10^6, cell_centers$neighbor_majority,
  sum),1)))

