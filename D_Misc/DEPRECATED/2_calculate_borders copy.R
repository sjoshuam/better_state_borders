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
library(sp)
library(readxl)
library(optimx)

## READ IN DATA ================================================================

## import area data
county_data <- readRDS(file = "B_Intermediates/county_data.RData")
cbsa_data <- readRDS(file = "B_Intermediates/cbsa_data.RData")

## import map data
county_map <- readRDS(file = "B_Intermediates/county_map.RData")
state_map <- readRDS(file = "B_Intermediates/state_map.RData")
us_map <- map_data("usa") %>% as_tibble() %>% rename(lon = long)

## import state color scheme
state_color <- read_xlsx("A_Inputs/state_colors.xlsx")

## DEAL WITH STATES THAT ARE DOUBLE/HALF THE MEDIAN POPULATION & AREA ==========

## declare function to calculate inequality (Gini coefficient)

Inequality <- function(dat, statistic, label){
  label <- paste0(label, "$")
  x <- dat %>%
    rename(statistic = matches(statistic), label = matches(label)) %>%
    select(statistic, label) %>%
    group_by(label) %>%
    summarize(statistic = sum(statistic)) %>%
    arrange(statistic) %>%
    mutate(
      weight = 1 / length(statistic),
      statistic = statistic / sum(statistic),
      has_more = 1 - cumsum(weight),
      equality = statistic * (weight + 2 * has_more)
      ) %>%
    pull(equality)
    
    
  return(round(1 - sum(x), 3))
}

## reassign states with less than half the median population and area
county_data <- county_data %>%
  mutate(
    first_cut = recode(
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
    mutate(first_cut = if_else(state == the_state, part, first_cut)) %>%
    select(-part)

  return(dat)
  }

county_data <- county_data %>% mutate(just_merges = first_cut)
county_data <- PartitionState(county_data, "CA", 4)
county_data <- PartitionState(county_data, "TX", 3)

county_map <- county_map %>%
  left_join(select(county_data, county, first_cut), by = "county")

## CALCULATE POLYGON REPRESENTATION OF CURRENT STATES ==========================

## split dataset into states
state_literal <- county_data %>%
  select(lon, lat, first_cut) %>%
  as.data.frame %>%
  split(
    x = select(., lon, lat),
    f = .$first_cut
    )

## calculate kmeans centers for each state
k <- 2^5
set.seed(0318)
i <- sapply(state_literal, nrow) > k
state_literal[i] <- lapply(state_literal[i], FUN = kmeans,
  centers = k, iter.max = 10^3, nstart = 10^2) %>%
  lapply(., function(x){x$centers})
remove(i, k)

state_literal <- data.frame(
    "state_literal" = rep(names(state_literal), sapply(state_literal, nrow)),
    do.call(what = rbind, args = state_literal)
  ) %>% as_tibble

## declare function to infer labels from the point approximation 
ImpliedLabels <- function(solution, label, c_data = county_data){
  implied_labels <- spDists(
    select(solution, lon, lat) %>% as.matrix(),
    select(county_data, lon, lat) %>% as.matrix()
    ) %>%
    apply(2, which.min)
  implied_labels <- pull(solution, label)[implied_labels]
  implied_labels
}

## incorporate results into county object
county_data$state_literal <- ImpliedLabels(state_literal, "state_literal")

county_map <- county_map %>%
  left_join(select(county_data, county, state_literal), by = "county")

## CALCULATE EQUAL AREA AND EQUAL POPULATION ALTERNATIVE STATE BORDERS =========



## convert literal state border representation to polar coordinates
state_polar <- state_literal %>%
  mutate(
    center_lon = tapply(lon, state_literal, mean)[state_literal],
    center_lat = tapply(lat, state_literal, mean)[state_literal],
    radius = sqrt((lon - center_lon)^2 + (lat - center_lat)^2),
    angle = atan2(lat - center_lat, lon - center_lon)
    )

## generate scoring function suitable for optim()
InequalityScore <- function(
  radii,
  representation = state_polar,
  ground_truth = county_data,
  score_variable = "population",
  return_labels = FALSE
) {
  ## format input parameters for merge
  radii <- pmax(pmin(radii, 1), 0)
  radii <- tibble(
    state_literal = names(radii),
    new_radius = radii
    )

  ## inject the new radii into the border representation object
  representation <- representation %>%
    left_join(radii, by = "state_literal") %>%
    mutate(
      radius = if_else(!is.na(new_radius), new_radius * radius, radius),
      lon = radius * cos(angle) + center_lon,
      lat = radius * sin(angle) + center_lat
      ) %>%
    rename(state_proposed = state_literal) %>%
    select(-new_radius)
  
  ## infer labels from the new points
  ground_truth$state_proposed <- ImpliedLabels(solution = representation,
    "state_proposed", c_data = ground_truth)
  if (return_labels) return(ground_truth)
  
  ## score based on inequality
  inequality_score <- Inequality(
    dat = ground_truth,
    statistic = score_variable,
    label = "state_proposed"
    )
  
  ## return results
  if (rbinom(n = 1, size = 1, prob = 0.01) == 1) {
    print(Sys.time())
    print(inequality_score)
  }
  return(inequality_score)
  
}

## alter map to reduce population inequality
InitialRadii <- function(value, label) {
  y <- rep(1, length(unique(label)))
  names(y) <- unique(label)
  return(y)
  }

initial_radii <- InitialRadii(county_data$population, county_data$first_cut)

equal_population <- hjn(
  par = initial_radii,
  fn = InequalityScore,
  score_variable = "population",
  upper = rep(1, length(initial_radii)),
  lower = rep(1 / 3, length(initial_radii))
  )


county_data <- InequalityScore(
  radii = equal_population$par,
  return_labels = TRUE)

county_map <- county_map %>%
  left_join(select(county_data, county, state_proposed), by = "county")

Inequality(
    dat = county_data,
    statistic = "population",
    label = "first_cut"
    )

Inequality(
    dat = county_data,
    statistic = "population",
    label = "state_proposed"
    )

# ## alter map to reduce land area inequality
# equal_area <- optim(
#   par = state_polar$radius,
#   fn = InequalityScore,
#   score_variable = "area",
#   method = "L-BFGS-B",
#   upper = state_polar$radius,
#   lower = 0
#   )

## GENERATE A COLOR PALETTE ====================================================

## generate label data
state_labels <- county_data %>%
  select(first_cut, lon, lat) %>%
  group_by(first_cut) %>%
  summarize(lon = mean(range(lon)), lat = mean(range(lat))) %>%
  mutate(
    lon = if_else(first_cut %in% c("FL"), lon + 2, lon),
    lon = if_else(first_cut %in% c("VA", "MI", "TX3", "NC", "KY"),
      lon + 1, lon),
    lon = if_else(first_cut %in% c("LA", "MD", "MN", "ID"), lon - 1, lon),
    lat = if_else(first_cut %in% c("MA"), lat + 0.4, lat)
    )

## extract color data
state_color <- state_color %>%
  mutate(
    key = (Y %% 2) %>% paste0(Color, .) %>% as.factor %>% as.numeric,
    h = 11 / 12,
    s = 0.7,
    v = seq(from = 0.4, to = 0.9, length.out = 6)[key],
    main_color = hsv(h = h, s = s, v = v)
    )

main_color <- set_names(
  pull(state_color, main_color),
  pull(state_color, State)
)

i <- names(main_color) %in% c("MD", "NC", "NH")
main_color[i] <- main_color[names(main_color) == "UT"]
remove(i)

##########==========##########==========##########==========##########==========

pdf("~/Desktop/temp.pdf", height = 5, width = 10)
ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49)) +
  geom_polygon(
    data = county_map,
    mapping = aes(x = lon, y = lat,
      fill = state_proposed, color = state_proposed,
      group = group)
    ) +
  geom_polygon(
    data = us_map,
    mapping = aes(x = lon, y = lat, group = group),
    fill = "transparent", color = "black"
    )  +
  geom_label(
    data = state_labels,
    mapping = aes(x = lon, y = lat, label = first_cut, fill = first_cut),
    color = "#FFFFFFFF", fill = "#00000050",
    size = 2, label.padding = unit(0.1, "lines")
    ) +
  geom_point(
    data = state_literal,
    mapping = aes(x = lon, y = lat),
    color = "transparent", size = 0.4
    ) +
  geom_point(
    data = state_literal,
    mapping = aes(x = lon, y = lat), # , color = state_literal
    color = "transparent",
    size = 0.1
    ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = main_color) +
  scale_color_manual(values = main_color)
graphics.off()

Inequality(dat = county_data, statistic = "population", label = "state")
Inequality(dat = county_data, statistic = "area", label = "state")

Inequality(dat = county_data, statistic = "population", label = "just_merges")
Inequality(dat = county_data, statistic = "area", label = "just_merges")

Inequality(dat = county_data, statistic = "population", label = "first_cut")
Inequality(dat = county_data, statistic = "area", label = "first_cut")

Inequality(dat = county_data, statistic = "population", label = "state_proposed")
Inequality(dat = county_data, statistic = "area", label = "state_proposed")




