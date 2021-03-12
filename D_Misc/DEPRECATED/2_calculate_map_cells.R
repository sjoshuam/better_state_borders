##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## Author: Josh M.
## Creation: 2021-03-06
## Version: R v4.0.4
## Description: Generates map borders based on the data gathered

## environment set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, dplyr.summarize.inform = FALSE)
library(sp)
library(tidyverse)

## READ DATA ===================================================================

us_border <- map_data("usa") %>%
  as_tibble() %>%
  rename(lon = long) %>%
  filter(region == "main") %>%
  select(-region, -subregion)

## GENERATE A POINT LATTICE FOR THE CONTINGUOUS US =============================

## calculate bounds
grid_space <- c((50 / 50), 1) * 0.5
us_lattice <- apply(select(us_border, lon, lat), 2, range)
us_lattice <- expand.grid(
  seq(from = us_lattice[1, "lon"], to = us_lattice[2, "lon"],
    by = grid_space[1]),
  seq(from = us_lattice[1, "lat"], to = us_lattice[2, "lat"],
    by = grid_space[2])
  )  %>%
  as_tibble()
colnames(us_lattice) <- c("lon", "lat")

## offset into hexagonal lattice
i <- us_lattice$lat %in% unique(us_lattice$lat)[c(TRUE, FALSE)]
us_lattice$lon[i] <- us_lattice$lon[i] + grid_space[1] / 2
remove(i)

## exclude points outside US borders
i <- point.in.polygon(point.x = us_lattice$lon, point.y = us_lattice$lat,
  pol.x = us_border$lon, pol.y = us_border$lat)
us_lattice <- filter(us_lattice, as.logical(i))
remove(i)

## DECLARE BORDER CALCULATION SUB-FUNCTIONS ====================================

## function that generates the initial radii --- --- --- ---
GenerateInitialBorders <- function(the_points, the_radius = 0,
  circle_resolution = 90) {
  
  ## generate unit circle
  theta <- seq(from = 0, to = 2 * pi, length.out = circle_resolution + 1)
  theta <- theta[-length(theta)]
  radius <- rep(the_radius, length(theta))
  coords <- cbind("theta" = theta, "radius" = radius)
  remove(theta, radius)
  
  ## replicate unit circle for each point
  i <- seq(nrow(coords))
  i <- rep(i, nrow(the_points))
  coords <- coords[i, ]
  remove(i)
  
  ## replicate centers for each circle
  j <- rep(seq(nrow(the_points)), each = circle_resolution)
  coords <- cbind(coords, "id" = j, the_points[j,])
  remove(j)
  colnames(coords)[4:5] <- c("center_x", "center_y")
  
  ## convert radial points to Cartesian
  coords <- cbind(
    coords, 
    x = coords[, "radius"] * cos(coords[, "theta"]),
    y = coords[, "radius"] * sin(coords[, "theta"])
    )
  coords[, c("x", "y")] <- coords[, c("x", "y")] +
    coords[, c("center_x", "center_y")]

  return(coords)
}

## function that modifies radii based on a vector of TRUE/FALSE --- --- --- ---
RetractRadii <- function(radii_data, tf_vector, nudge_size = 0.01) {
  
  ## adjust radii
  radii_data[tf_vector, "radius"] <- radii_data[tf_vector, "radius"] +
    nudge_size
  
  ## convert radial points to Cartesian
  radii_data[, "x"] <- radii_data[, "radius"] * cos(radii_data[, "theta"])
  radii_data[, "y"] <- radii_data[, "radius"] * sin(radii_data[, "theta"])
  radii_data[, c("x", "y")] <- radii_data[, c("x", "y")] +
    radii_data[, c("center_x", "center_y")]

  return(radii_data)
}

## function that detect overlapping radii --- --- --- ---
OverlapCheck <- function(radii_data) {
  
  ## extract center points
  center_points <- radii_data[, c("id", "center_x", "center_y")]
  center_points <- center_points[!duplicated(center_points[, "id"]), ]
  
  ## determine nearest centerpoint
  x_dist <- outer(radii_data[, "x"], center_points[, "center_x"], FUN = "-")^2
  y_dist <- outer(radii_data[, "y"], center_points[, "center_y"], FUN = "-")^2
  x_dist <- sqrt(x_dist + y_dist)
  remove(y_dist)
  x_dist <- apply(x_dist, 1, which.min)
  
  ## check for overlap
  return(radii_data[, "id"] == x_dist)
}


## function that detect out of bounds radii
BoundsCheck <- function(radii_data, bounds_poly) {
  x <- sp::point.in.polygon(
    point.x = radii_data[, "x"],
    point.y = radii_data[, "y"],
    pol.x = bounds_poly[, 1],
    pol.y = bounds_poly[, 2]
    )
  return(as.logical(x))
}

## CALCULATE MAP BORDERS =======================================================

## prepare outer borders
us_border <- us_border %>% filter(group == 1) %>% select(lon, lat) %>% as.matrix

## calculate zone borders
zone_borders <- us_lattice %>% select(lon, lat) %>% as.matrix
zone_borders <- GenerateInitialBorders(zone_borders, the_radius = 0)

i <- OverlapCheck(zone_borders)
iteration <- 0

while (any(i)) {
  iteration <- iteration + 1

  zone_borders <- RetractRadii(zone_borders, i, nudge_size = 0.01)
  i <- OverlapCheck(zone_borders)
  i <- i & BoundsCheck(zone_borders, us_border)

  sum_i <- sum(i)
  if (iteration > 2^7) break
}

remove(i, iteration)

## label results
zone_borders <- zone_borders %>% 
  as_tibble() %>%
  select(id, x, y) %>%
  rename(lon = x, lat = y, group = id) %>%
  mutate(group = str_sub(10^4 + group, 1))

## SAVE RESULTS =======================================================

cell_map <- as_tibble(zone_borders)
saveRDS(cell_map, file = "B_Intermediates/cell_map.RData")
file.remove("B_Intermediates/cell_county_distance.RData")

##########==========##########==========##########==========##########==========
