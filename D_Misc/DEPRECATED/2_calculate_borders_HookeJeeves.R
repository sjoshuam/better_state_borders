##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M.
## creation: 2021-03-07
## version: R v4.0.4
## description: prepares county and msa data for analysis

## environment set up
remove(list = objects())
options(dplyr.summarise.inform = FALSE, width = 80, scipen = 2, digits = 6)
library(tidyverse)
library(readxl)
library(optimx)

warning("todo: fix cross-wiring in Inequality function")

## READ IN DATA ================================================================

## import area data
county_data <- readRDS(file = "B_Intermediates/county_data.RData")
cbsa_data <- readRDS(file = "B_Intermediates/cbsa_data.RData")

## import map data
county_map <- readRDS(file = "B_Intermediates/county_map.RData")
#state_map <- readRDS(file = "B_Intermediates/state_map.RData")
us_map <- map_data("usa") %>% as_tibble() %>% rename(lon = long)

## convert all coordinates to flat plate approximation
county_data$lon_fp <- county_data$lon * 55 / 70
cbsa_data$lon_fp <- cbsa_data$lon * 55 / 70

## DEAL WITH STATES THAT ARE DOUBLE/HALF THE MEDIAN POPULATION & AREA ==========

## reassign states with less than half the median population and area
county_data <- county_data %>%
  mutate(
    basic_fixes = recode(
      state,
      "RI" = "CT",
      
      "DE" = "MD",
      "WV" = "MD",
      "DC" = "MD",
      
      "VT" = "NH",
      "ME" = "NH"
      )
    )

## split California and Texas (has more than double median population/area)
PartitionState <- function(dat, the_state, k) {
  state_data <- as.data.frame(dat)
  rownames(state_data) <- state_data$county
  
  state_data <- state_data %>%
    filter(state == the_state) %>%
    select(lon, lat) %>%
    as.matrix() %>%
    dist() %>%
    hclust(method = "ward.D2") %>%
    cutree(k = k) %>%
    tibble("county" = names(.), "part" = .) %>%
    mutate("part" = paste0(the_state, part))
  
  dat <- dat %>%
    left_join(state_data, by = "county") %>%
    mutate(basic_fixes = if_else(state == the_state, part, basic_fixes)) %>%
    select(-part)

  return(dat)
  }

county_data <- county_data %>% mutate(state_merges = basic_fixes)
county_data <- PartitionState(county_data, "CA", 4)
county_data <- PartitionState(county_data, "TX", 3)

county_map <- county_map %>%
  left_join(select(county_data, county, basic_fixes), by = "county")

remove(PartitionState)

## DECLARE GENERIC OBJECTS TO INTERACT WITH FUNCTIONS ==========================

## generate object to hold retraction solution
state_data <- county_data %>%
  group_by(basic_fixes) %>%
  summarize(
    state = unique(basic_fixes),
    population = sum(population),
    area = sum(area)
    ) %>%
  select(-basic_fixes) %>%
  mutate(
    basic_fixes = if_else(state %in% c("CT", "MD", "NH"), 2, 1) %>%
      if_else(nchar(state) > 2, 0, .),
    equal_population = median(population) / population,
    equal_population = (equal_population / max(equal_population)) * 0.5 + 0.5,
    equal_area = median(area) / area,
    equal_area = (equal_area / max(equal_area)) * 0.5 + 0.5,
    whole_cities = 1
    ) %>%
  select(state, basic_fixes, equal_population, equal_area, whole_cities,
    population, area)

## generate object to hold point approximations of state-county relationships
point_approximation <- split(
  x = county_data %>% select(lon_fp, lat) %>% as.data.frame,
  f = pull(county_data, basic_fixes)
    )

Kmeans <- function(x) {
  kmeans(x, centers = 50)$centers
  }

too_many <- sapply(point_approximation, nrow) > 50
point_approximation[too_many] <- lapply(point_approximation[too_many], Kmeans)

point_approximation <- tibble(
  "basic_fixes" = rep(
    names(point_approximation), sapply(point_approximation, nrow)),
  do.call(what = rbind, point_approximation)
  ) %>%
  mutate(
    center_lon_fp = tapply(lon_fp, basic_fixes, mean)[basic_fixes],
    center_lat = tapply(lat, basic_fixes, mean)[basic_fixes],
    lon_fp = lon_fp - center_lon_fp,
    lat = lat - center_lat,
    radius = sqrt(lon_fp^2 + lat^2),
    angle  = atan2(lat, lon_fp),
    lon_fp = lon_fp + center_lon_fp,
    lat = lat + center_lat 
    )

remove(too_many, Kmeans)

## DECLARE GENERIC TOOL FUNCTIONS ==============================================

## measure inequality --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
MeasureInequality <- function(
  dat = county_data,
  the_column,
  category,
  weight = NULL
) {
  ## if no weight the_column provided, assume equal weight
  if (is.null(weight)) {
    dat$weight <- NA
    weight  <- "weight"
  }
  
  ## create object with necessary variables
  dat <- select(dat,
    starts_with(the_column), starts_with(category), starts_with(weight)) %>%
    rename("the_column" = starts_with(the_column),
      "category" = starts_with(category), "weight" = starts_with(weight)) %>%
    group_by(category) %>%
    summarize(
      "the_column"   = sum(the_column),
      "categories" = unique(category),
      "weight"     = sum(weight)
      ) %>%
    arrange(the_column)
  dat$weight <- ifelse(all(is.na(dat$weight)), 1, dat$weight)

  ##  calculate inequality
  equality <- dat %>% mutate(
      weight = weight / sum(weight),
      the_column = the_column / sum(the_column),
      has_more = 1 - cumsum(weight),
      equality = the_column * (weight + 2 * has_more)
      )
  equality <- equality %>% pull(equality) %>% sum()
  
  ## add population weights if not provided
 return(1 -  equality)
}

## infer state assignments from nearest point --- --- --- --- --- --- --- --- --

InferState <- function(p_approx = point_approximation,
  c_data = county_data, the_column) {
  dist_mat <- outer(p_approx$lon_fp, c_data$lon_fp, FUN = "-")^2
  dist_mat <- dist_mat + outer(p_approx$lat, c_data$lat, FUN = "-")^2
  dist_mat <- sqrt(dist_mat)
  dist_mat <- apply(dist_mat, 2, which.min)
  c_data[[the_column]] <- p_approx$basic_fixes[dist_mat]
  return(c_data)
}

## retract points to centroids --- --- --- --- --- --- --- --- --- --- --- --- =

RetractStates <- function(
  p_approx = point_approximation, s_data = state_data, the_column) {
  s_data <- select(s_data, state, starts_with(the_column))
  p_approx <- p_approx %>%
    left_join(s_data, by = c("basic_fixes" = "state")) %>%
    rename("the_column" = starts_with(the_column)) %>%
    mutate(
      radius = radius * the_column,
      lon_fp = radius * cos(angle) + center_lon_fp,
      lat = radius * sin(angle) + center_lat
      )
  return(p_approx)
  }

## adjust shrink factors to optimize a score  --- --- --- --- --- --- --- --- --

ScoreFunction <- function(par, the_column, s_data = state_data, the_category,
  score_function = MeasureInequality) {
  s_data[[the_column]] <- par
  p_approx <- RetractStates(s_data = s_data, the_column = the_column)
  c_data <- InferState(p_approx = p_approx, the_column = the_column)
  score_function(the_column = the_category, category = the_column,
    dat = c_data)
}

## CREATE ALTERNATIVE SCORING FUNCTION FOR CITY CONTIGUITY =====================

CityScore <- function(
  dat,
  ...
) {
  ## sum the number of people in each state for each metro area
  the_category = "population"
  the_column = "whole_cities"
  x <- dat %>%
    rename(
      the_column = starts_with(the_column),
      the_category = starts_with(the_category)
      ) %>%
    filter(!is.na(cbsa)) %>%
    group_by(cbsa, the_column) %>%
    summarize(the_category = sum(the_category)) %>%
    group_by(cbsa) %>%
    summarize(
      max = max(the_category),
      sum = sum(the_category)
      )
  ## calculate wrong state score
  x <- sum(x$max) / sum(x$sum)
  return(1 - x)
}


## CALCULATE EQUAL AREA AND EQUAL POPULATION ALTERNATIVE STATE BORDERS =========

## generate equal population solution
state_data$equal_population <- hjn(
  par = state_data$equal_population,
  fn = ScoreFunction,
  the_column = "equal_population",
  the_category = "population",
  lower = if_else(state_data$state %in% c("CT", "MA"), 0.5, 0.3),
  upper = 1
  )$par

## generate equal area solution
state_data$equal_area <- hjn(
  par = state_data$equal_area,
  fn = ScoreFunction,
  the_column = "equal_area",
  the_category = "area",
  lower = if_else(state_data$state %in% c("CT", "MA"), 0.5, 0.3),
  upper = 1
  )$par

## generate one state per metro area solution
state_data$whole_cities <- hjn(
  par = state_data$equal_area,
  fn = ScoreFunction,
  score_function = CityScore,
  the_column = "whole_cities",
  the_category = "population",
  lower = if_else(state_data$state %in% c("CT", "MA"), 0.5, 0.3),
  upper = 1
  )$par

## GENERATE A TABLE  Of SCORES =================================================

UnpackSolutions <- function(
  s_data = state_data,
  p_approx = point_approximation,
  c_data = county_data,
  c_map = county_map,
  the_column
  ){
  p_approx <- RetractStates(s_data = s_data, the_column = the_column)
  c_data <- InferState(p_approx = p_approx, the_column = the_column)
  c_map <- c_map %>%
    left_join(select(c_data, county, starts_with(the_column)))
  return(c_map)
}

## compile results into the county map
county_map <- UnpackSolutions(the_column = "original")
county_map <- UnpackSolutions(the_column = "basic_fixes")
county_map <- UnpackSolutions(the_column = "equal_population")
county_map <- UnpackSolutions(the_column = "equal_area")
county_map <- UnpackSolutions(the_column = "whole_cities")

county_map$merges_only <- str_sub(county_map$basic_fixes, 1, 2)
county_map$splits_only <- if_else(
  nchar(county_map$basic_fixes) > 2,
  county_map$basic_fixes,
   county_map$state
  )

county_data$merges_only <- str_sub(county_data$basic_fixes, 1, 2)
county_data$splits_only <- if_else(
  nchar(county_data$basic_fixes) > 2,
  county_data$basic_fixes,
   county_data$state
  )

## create container to hold results
county_data$original <- county_data$state
inequality_scorecard <- vector(mode = "list")

## standard states
inequality_scorecard$original <- c(
  MeasureInequality(the_column = "population", category = "original"),
  MeasureInequality(the_column = "area", category = "original")
  )

## Basic merges
inequality_scorecard$merges_only <- c(
  MeasureInequality(the_column = "population", category = "merges_only"),
  MeasureInequality(the_column = "area", category = "merges_only")
)

## Basic splits
inequality_scorecard$splits_only <- c(
  MeasureInequality(the_column = "population", category = "splits_only"),
  MeasureInequality(the_column = "area", category = "splits_only")
)

## Basic fixes
inequality_scorecard$basic_fixes <- c(
  MeasureInequality(the_column = "population", category = "basic_fixes"),
  MeasureInequality(the_column = "area", category = "basic_fixes")
)

## Equal area
inequality_scorecard$equal_area <- c(
ScoreFunction(
  par = state_data$equal_area,
  the_category = "population", the_column = "equal_area"),
ScoreFunction(
  par = state_data$equal_area,
  the_category = "area", the_column = "equal_area")
)

## Equal population
inequality_scorecard$equal_population <- c(
ScoreFunction(
  par = state_data$equal_population,
  the_category = "population", the_column = "equal_population"),
ScoreFunction(
  par = state_data$equal_population,
  the_category = "area", the_column = "equal_population")
)

## whole cities
inequality_scorecard$whole_cities <- c(
ScoreFunction(
  par = state_data$whole_cities,
  the_category = "population", the_column = "whole_cities"),
ScoreFunction(
  par = state_data$whole_cities,
  the_category = "area", the_column = "whole_cities")
)

## assemble scores
inequality_scorecard <- as.data.frame(t(simplify2array(inequality_scorecard)))
colnames(inequality_scorecard) <- c("Population", "Area")
inequality_scorecard$Combined <- sqrt(inequality_scorecard$Population *
  inequality_scorecard$Area)

## Assemble plot titles
inequality_scorecard$title <- paste0(
  c("Current States", "Basic Merges", "Basic Splits", "Basic Merges And Splits",
    "Equal Area", "Equal Population", "Whole Cities"),
  " (Pop: ",
  (1 - round(inequality_scorecard$Population, 2)) * 100,
  "%, Area: ",
  (1 - round(inequality_scorecard$Area, 2)) * 100,
  "%)"
  )

##########==========##########==========##########==========##########==========

color_palettte <- seq(from = 0.4, to = 1.0, length.out = 6)
color_palettte <- c(
  hsv(h = 6/12, s = 0.7, v = color_palettte),
  hsv(h = 7/12, s = 0.7, v = color_palettte),
  hsv(h = 8/12, s = 0.7, v = color_palettte),
  hsv(h = 9/12, s = 0.7, v = color_palettte),
  hsv(h = 10/12, s = 0.7, v = color_palettte),
  hsv(h = 11/12, s = 0.7, v = color_palettte),
  hsv(h = 12/12, s = 0.7, v = color_palettte),
  hsv(h = 1/12, s = 0.7, v = color_palettte),
  hsv(h = 3/12, s = 0.7, v = color_palettte)
  )
set.seed(9017)
color_palettte <- sample(color_palettte)

pdf("C_Outputs/RoughBorderReference.pdf", width = 10, height = 6)

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = state)
    ) +
  labs(title = inequality_scorecard["original", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = merges_only)
    ) +
  labs(title = inequality_scorecard["merges_only", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = splits_only)
    ) +
  labs(title = inequality_scorecard["splits_only", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = basic_fixes)
    ) +
  labs(title = inequality_scorecard["basic_fixes", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = whole_cities)
    ) +
  labs(title = inequality_scorecard["whole_cities", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = equal_area)
    ) +
  labs(title = inequality_scorecard["equal_area", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat, group = group, fill = equal_population)
    ) +
  labs(title = inequality_scorecard["equal_population", "title"]) +
  scale_fill_manual(values = color_palettte) +
  theme(legend.position = "none")

graphics.off()

save(county_data, state_data, us_map, county_map, point_approximation,
  inequality_scorecard, file = "B_Intermediates/border_calculations_pack.RData"
  )
