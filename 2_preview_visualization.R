##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M
## creation: 2021-03-22
## version: 4.0.4 Lost Library Book
## contact: github.com/sjoshuam

## environment set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, dplyr.summarise.inform = FALSE)
library(tidyverse)
library(sf)

## IMPORT DATA =================================================================

## load geographic data
cbsa_data   <- readRDS("B_Intermediates/cbsa_data.RData")
county_data <- readRDS("B_Intermediates/county_data.RData")

## load map data
county_map  <- readRDS("B_Intermediates/county_map.RData")
state_map   <- readRDS("B_Intermediates/state_map.RData")
us_map      <- readRDS("B_Intermediates/us_map.RData")

# ## drop small islands from county_map
# i <- county_map %>%
#   group_by(group) %>%
#   summarize(
#     county = unique(county),
#     group_size = length(group)
#     ) %>%
#   arrange(desc(group_size)) %>%
#   group_by(county) %>%
#   slice_head(n = 1) %>%
#   pull(group)
# mean(county_map$group %in% i)
# county_map <- filter(county_map, group %in% i)

## drop some island counties that add geographic complexity to the map
# i <- c("53055", "53029", "25019", "25007") ## 
# county_map <- filter(county_map, !(county %in% i))
# county_data <- filter(county_data, !(county %in% i))

## POLYGON UNION PRACTICE ======================================================

# ## make overlapping triangles
# x1 <- list(
#   cbind(
#     c(1, 2, 3, 1),
#     c(2, 1, 2, 2)
#     )
# ) %>% st_polygon
# 
# x2 <- list(
#   cbind(
#     c(1, 2, 3, 1) + 0.5,
#     c(2, 1, 2, 2)
#     )
#   ) %>% st_polygon
# 
# ## convert to polygon
# y <- st_union(x1, x2)
# 
# plot(y)

## declare split state assignment function
SplitStates <- function(the_state, n, state_col, c_data = county_data,
  m_data = cbsa_data) {
  
  if (the_state == "-") return(rep(NA, nrow(c_data)))
  
  ## generate state population grid
  k_data <- c_data %>%
    mutate(state_col = state_col) %>%
    filter(state_col == the_state) %>%
    mutate(population = round(population / 10^3))
  k_centers <- k_data[rep(seq(nrow(k_data)), k_data$population), ] %>%
    select(x, y) %>%
    as.matrix

  ## generate population-weighted k-means clusters
  m_data <- m_data %>%
    select(cbsa_name, state, x, y, population) %>%
      filter(state == the_state) %>%
    arrange(desc(population)) %>%
    slice_head(n = n) %>%
    select(x, y) %>%
    as.matrix()
  k_centers <- kmeans(k_centers, centers = m_data, iter.max = 10^3, nstart = 1)
  k_centers <- k_centers$centers

  ## assign counties to state partitions
  partition <- outer(k_data$x, k_centers[, "x"], FUN = "-")^2 +
    outer(k_data$y, k_centers[, "y"], FUN = "-")^2
  partition <- apply(partition, MARGIN = 1, FUN = which.min)
  partition <- paste0(the_state, partition)

  ## expand vector and return
  the_result <- rep(NA, nrow(c_data))
  the_result[state_col == the_state] <- partition
  return(the_result)
}

## declare inequality measurement function
MeasureInequality <- function(state_col, c_data = county_data) {
  
  ##
  state_col <- str_remove_all(state_col, "1$")
  
  ## calculate inequality
  inequality <- c_data %>%
    mutate(state_col = state_col) %>%
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
    unlist()
  
  ## calculate change from current states
  inequality["change"] <- 1 -  mean(c_data$state == state_col)
  inequality["combined"] <- sqrt(inequality["pop"] * inequality["area"])
  inequality["combined"] <- inequality["combined"]
  inequality <- round(inequality, 2) * 100
  
  inequality <- paste0(
    "Population inequality:  ", inequality["pop"], "%\n",
    " Land area inequality:  ", inequality["area"], "%\n",
    " Combined inequality:  ", inequality["combined"], "%\n",
    "Change from current borders:  ", inequality["change"], "%"
    )

  return(inequality)
}

## UNIFY METRO AREAS ===========================================================
county_data$map_one <- county_data$unified_state

## SPLIT OFF LARGE MSA =========================================================
county_data <- county_data %>%
  mutate(map_two = if_else(mega_population >= 10 * 10^6, mega_name, map_one))

## MERGE SMALL STATES: VT,ME->NH, RI->CT, WV,DE,DC->MD, NJ -> PA  ==============

county_data$map_three <- recode(
  county_data$map_two,
  "VT" = "NH",
  "ME" = "NH",
  "RI" = "CT",
  "WV" = "MD",
  "DE" = "MD",
  "DC" = "MD",
  "NJ" = if_else("Phil" %in% county_data$map_two, "Phil", "PA")
  )
  
## SPLIT CA AND TX =============================================================

state_split <- SplitStates("CA", 3, county_data$map_three)
tx_split <- SplitStates("TX", 3, county_data$map_three)
state_split <- if_else(is.na(state_split), tx_split, state_split)
state_split <- if_else(is.na(state_split), county_data$map_three, state_split)
county_data$map_four <- state_split
remove(state_split, tx_split)

## INCORPORATE INTO MAP OBJECT =================================================

county_map <- county_map %>%
  left_join(select(county_data, county, map_one, map_two, map_three, map_four),
    by = "county")

## GENERATE COMPOSITE BORDERS ==================================================


GenerateStatePolygons <- function(
  new_states, c_data = county_data, c_map = county_map) {
  
  ## incorporate state assignments into objects
  c_data$new_states <- new_states
  remove(new_states)
  c_map <- c_map %>%
    left_join(select(c_data, county, new_states), by = "county")

  ## convert map polygons into sf-format polygons
  c_poly <- c_map %>% select(lon, lat, group) %>% as.data.frame()
  c_poly <- split(select(c_poly, lon, lat), f = c_poly$group) %>%
    lapply(as.matrix) %>%
    lapply(list) %>%
    lapply(st_polygon)
  
  ## divide polygon list into states
  i <- c_map$new_states[match(names(c_poly), as.character(c_map$group))]
  c_poly <- tapply(c_poly, i, list)
  remove(i)
  
  ## unify polygons in each state
  Unify <- function(x) {
    x %>% st_sfc() %>% st_combine() %>% st_union(by_feature = TRUE)
  }
  c_poly <- lapply(c_poly, Unify)
  
  ## simplify polygon data to tidy format and express
  c_poly <- lapply(c_poly, st_coordinates) %>%
    lapply(function(x){x[, c("X", "Y")]})
  c_poly <- data.frame(
    "State" = rep(names(c_poly), sapply(c_poly, nrow)),
    do.call(what = rbind, args = c_poly)
  ) %>% as_tibble()
  colnames(c_poly)[2:3] <- c("lon", "lat")
  
  return(c_poly)
}



temp <- GenerateStatePolygons(county_data$map_four)


##########==========##########==========##########==========##########==========






