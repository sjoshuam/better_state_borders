##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta information
## author: Josh M
## contact: https://github.com/sjoshuam
## creation: 2021-03-19
## version: 4.0.4 Lost Library Book
## description: interactive tool exploring ways to improve US state borders

## environment set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, dpylr.summarise.inform = FALSE)
library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)
library(sf)

warning("TODO: Change-based color coding")

## READ IN DATA ================================================================

## read in datasets
county_data <- readRDS("B_Intermediates/county_data.RData")
cbsa_data <- readRDS("B_Intermediates/cbsa_data.RData")
county_map <- readRDS("B_Intermediates/county_map.RData")
us_map <- readRDS("B_Intermediates/us_map.RData")
county_polygon <- readRDS("B_Intermediates/county_polygon.RData")
state_polygon <- readRDS("B_Intermediates/state_polygon.RData")

## generate initial list of selectable states
selectable_states <- c(
  "-", sort(c("DC", state.abb)),
  sort(unique(county_data$mega_name[!is.na(county_data$mega_name)]))
  )
selectable_states <- selectable_states[!(selectable_states %in% c("HI", "AK"))]

## UI === === === === === === === === === === === === === === === === === === ==
## UI === === === === === === === === === === === === === === === === === === ==

## UI - INTRO ==================================================================

ui_intro <- column(width = 12,
  h3("Small Changes To The United States' State Borders Can Yield Big Improvements"),
  p(
    "The United States\' state borders do not divide the United States",
    "according to a grand plan.  The borders reflect historic events,",
    "political bargains, and other considerations that are less relevant",
    "in modern times.  As result, US states contain unequal numbers of people",
    "and unequal land area. In addition, state borders cross through major",
    "metropolitan areas.  Together, these inequalities may complicate",
    "governance."
    ),
  p(
    "However, three kinds of border changes could potentially make states",
    "less unequal.  Using this tool, you can apply those changes in various",
    "ways and see the results.  The three changes are:",
    br(), br(),
    "1. Move counties from one state to another, so that states are",
    "geographically contiguous and each metropolitan area falls entirely in",
    "one state's jurisdiction",
    br(),
    "2. Merge small states into other states, so that states are more equal",
    "in terms of population and land area",
    br(),
    "3. Split large states into multiple smaller states, so that states are",
    "more equal in terms of population and land area"
    ),
  p(
    "For each change, you can choose whether to implement the change and how",
    "it will be implemented.  Once you make the change, the maps will update",
    "to show the new borders.  In addition, the inequality scores (gini",
    "coefficients) will update, so that you can measure how your changes",
    "affected inequality between states.  A score of 0% indicates perfect",
    "equality between states and a score of 100% indicates egregeous",
    "inequality between states. To implements changes, the tool moves whole",
    "counties from one state to another.  As a result, the new state borders",
    "may seem ragged because county borders are not as smooth as state borders."
    ),
  p(
    "To illustrate how different options affect the results, the tool starts",
    " with some changes pre-loaded. After these changes, 85% of counties are",
    "in the same state that they are currently in.  However, population and",
    "land area are more equally distributed, state borders are more",
    "contiguous, and metropolitan areas are no longer split between different",
    "state jurisdictions.  In short, small changes to the current state",
    "borders can improve them significantly."
    ),
  p(
    "This tool takes about five seconds to load.   The tool is ready when",
    "the maps are visible.  Likewise, the tool takes up to five seconds to",
    "implement changes when you select new options.  The tool is done",
    "recalculating when the maps no longer look faded."
    )
  )

## UI - PANEL 1 ================================================================

## make panel title and explanatory text
ui_q1_title <- column(width = 12,
  h3(
    "How should state borders change to improve contiguity?"
    ),
  p(style = "max-width: 33%;",
    "Select how state borders should change to make states geographically",
    "contiguous.  Also select a population threshold.  Metropolitan areas",
    "with a population above the threshold will split off into separate",
    "states.  I recommend select all of the checkboxes and setting the",
    "threshold at 10 million.  A threshold of 10 million splits the New York",
    "City and Los Angeles metropolitan areas into separate states."
    )
  )

## make control panel
ui_q1_controls <- sidebarPanel(
  checkboxInput(
    inputId = "metro_unifier",
    label = "Place each metropolitan area entirely in one state",
    value = TRUE
    ),
  checkboxInput(
    inputId = "mi_upper_peninsula_check",
    label = "Make MI's upper peninsula part of WI",
    value = TRUE
    ),
  checkboxInput(
    inputId = "fl_panhandle_check",
    label = "Make FL's panhandle (west of Tallahassee) part of AL",
    value = TRUE
    ),
  checkboxInput(
    inputId = "va_delmarva_check",
    label = "Make VA's section of the Delmarva peninsula part of MD",
    value = TRUE
    ),
  checkboxInput(
    inputId = "md_defrag_check",
    label = "Defragment the MD border",
    value = TRUE
    ),
  checkboxInput(
    inputId = "ok_panhandle",
    label = "Make the OK panhandle part of KS",
    value = TRUE
    ),
  sliderInput(inputId = "mega_slider",
    label = "Make separate states of metropolitan areas above this population size",
    min = 5, max = 20, value = 10, post = "m"
    )
)

## generate reactive output
ui_q1_output <- mainPanel(plotOutput("map_one"))

## UI - PANEL 3 ================================================================

## make panel title and explanatory text
ui_q3_title <- column(width = 12,
  h3("Which states should merge into bigger states?"),
  p(style = "max-width: 33%;",
    "Select which states should merge into other states.",
    " The states on the left will be merged into the states on the right.",
    " I recommend selecting all of the checkboxes."
    )
  )

## make control panel
ui_q3_controls1 <- sidebarPanel(width = 2,
  
  checkboxInput(inputId = "merge_check1", label = "Merge VT and ME into NH",
    value = TRUE),
  
  checkboxInput(inputId = "merge_check2", label = "Merge WV, DC, DE into MD",
    value = TRUE),
  
  checkboxInput(inputId = "merge_check3", label = "Merge RI into CT",
    value = TRUE),
  
  checkboxInput(inputId = "merge_check4",
    label = paste(
      "If you unified the New York City metropolitan area, divide NJ between",
      "Philadelphia and New York City"
      ),
    value = TRUE)
  
  )

ui_q3_controls2 <- sidebarPanel(width = 2,

    selectInput(inputId = "merge_select1", label = "Merge state #1...",
    choices = selectable_states, selected = "-"
    ),
  
  selectInput(inputId = "merge_dest1", label = "...into this state",
    choices = selectable_states, selected = "-"
    ),
  
  selectInput(inputId = "merge_select2", label = "Merge state #2...",
    choices = selectable_states, selected = "-"
    ),
  
  selectInput(inputId = "merge_dest2", label = "... into this state",
    choices = selectable_states, selected = "-"
    ),
  
  selectInput(inputId = "merge_select3", label = "Merge state #3...",
    choices = selectable_states, selected = "-"
    ),
  
  selectInput(inputId = "merge_dest3", label = "... into this state",
    choices = selectable_states, selected = "-"
    )
  
  )


## generate reactive output
ui_q3_output <- mainPanel(plotOutput("map_three"))

## UI - PANEL 4 ================================================================

## make panel title and explanatory text
ui_q4_title <- column(width = 12,
  h3("Which states should split into smaller states?"),
  p(style = "max-width: 33%;",
    "Select which states should split into 2-4 smaller states",
    "  I recommend selecting all of the checkboxes."
    )
  )

## make control panel
ui_q4_controls1 <- sidebarPanel(width = 2,

  checkboxInput(inputId = "split_check1", label = "Split CA into 3 parts",
    value = TRUE),
  
  checkboxInput(inputId = "split_check2",
    label = paste("Split TX into 3 parts if you did not split Dallas and",
    "Houston off from TX"),
    value = TRUE)
  
  )

ui_q4_controls2 <- sidebarPanel(width = 2,
  
  selectInput(inputId = "split_state1", label = "Split State #1...",
    choices = selectable_states, selected = "-"
    ),
  sliderInput(inputId = "split_n1", label = "...into __ parts",
    min = 2, max = 4, value = 2),
  
  selectInput(inputId = "split_state2", label = "Split State #2...",
    choices = selectable_states, selected = "-"
    ),
  sliderInput(inputId = "split_n2", label = "...into __ parts",
    min = 2, max = 4, value = 2),
  
  selectInput(inputId = "split_state3", label = "Split State #3...",
    choices = selectable_states, selected = "-"
    ),
  sliderInput(inputId = "split_n3", label = "...into __ parts",
    min = 2, max = 4, value = 2)
  
  )

## generate reactive output
ui_q4_output <- mainPanel(plotOutput("map_four"))

## SERVER === === === === === === === === === === === === === === === === === ==
## SERVER === === === === === === === === === === === === === === === === === ==

## declare state polygon generation function

GenerateStatePolygons <- function(new_states, s_poly = state_polygon,
  c_data = county_data, c_map = county_map, c_poly = county_polygon) {
  
  ## incorporate state assignments into objects
  c_data$new_states <- new_states
  remove(new_states)
  
  c_map <- left_join(c_map, c_data[, c("county", "new_states")],
    by = "county")
  
  ## divide polygon list into states
  i <- c_map$new_states[match(names(c_poly), as.character(c_map$group))]
  c_poly <- tapply(c_poly, i, list)
  remove(i)
  
  ## replace county polygons with fast-load state polygons where possible
  county_count    <- tapply(c_map$new_states, c_map$new_states, length)
  unchanged_count <- tapply(c_map$unified_state, c_map$unified_state, length)
  unchanged_count <- unchanged_count[names(county_count)]
  unchanged_count[is.na(unchanged_count)] <- -Inf
  county_count <- names(county_count)[county_count == unchanged_count]
  c_poly[county_count] <- NULL
  remove(unchanged_count)
  
  ## unify polygons in each state
  Unify <- function(x) {
    x <- sf::st_sfc(x)
    x <- sf::st_combine(x)
    x <- sf::st_union(x, by_feature = TRUE)
    return(x)
  }
  c_poly <- lapply(X = c_poly, FUN = Unify)
  c_poly[county_count] <- s_poly[county_count]
  
  ## simplify polygon data to tidy format and express
  c_poly <- lapply(c_poly, st_coordinates) %>%
    lapply(function(x){x[, c("X", "Y")]})
  c_poly <- data.frame(
    "state" = rep(names(c_poly), sapply(c_poly, nrow)),
    do.call(what = rbind, args = c_poly)
  ) %>% as_tibble()
  colnames(c_poly)[2:3] <- c("lon", "lat")
  
  return(c_poly)
}

## declare split state assignment function
SplitStates_Tidy <- function(the_state, n, state_col, c_data = county_data,
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

SplitStates_Untidy <- function(the_state, n, state_col, c_data = county_data,
  m_data = cbsa_data) {

  if (the_state == "-") return(rep(NA, nrow(c_data)))
  
  ## generate state population grid
  k_data <- c_data
  k_data$state_col <- state_col
  k_data <- k_data[k_data$state_col == the_state, ]
  k_data$population <- round(k_data$population / 10^3)
  
  k_centers <- k_data[rep(seq(nrow(k_data)), k_data$population), c("x", "y")]
  k_centers <- as.matrix(k_centers)
  
  ## generate population-weighted k-means clusters
  m_data <- m_data[, c("cbsa_name", "state", "x", "y", "population")]
  m_data <- m_data[m_data$state == the_state, ]
  m_data <- arrange(m_data, desc(population))
  m_data <- m_data[seq(n), c("x", "y")]
  m_data <- as.matrix(m_data)
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
MeasureInequality_Tidy <- function(state_col, c_data = county_data) {
  
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

MeasureInequality_Untidy <- function(state_col, c_data = county_data) {
  
  ##
  state_col <- str_remove_all(state_col, "1$")
  
  ## calculate inequality
  c_data$state_col <- state_col
  
  inequality <- group_by(c_data, state_col) %>%
    summarize(area = sum(area), population = sum(population))
  
  inequality$area <- inequality$area / sum(inequality$area)
  inequality$population <- inequality$population / sum(inequality$population)
  inequality$weight <- 1 / nrow(inequality)
  
  inequality <- arrange(inequality, area)
  
  inequality$area_cum <- 1 - cumsum(inequality$weight)
  inequality$area_gini <- inequality$area * (
    inequality$weight + 2 * inequality$area_cum)
  
  inequality <- arrange(inequality, population)
  
  inequality$pop_cum <- 1 - cumsum(inequality$weight)
  inequality$pop_gini <- inequality$population * (
    inequality$weight + 2 * inequality$pop_cum)
  
  inequality <- c(
    area = 1 - sum(inequality$area_gini),
    pop = 1 - sum(inequality$pop_gini)
    )
  
  
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

SplitStates <- SplitStates_Tidy
MeasureInequality <- MeasureInequality_Tidy

## initialize server
server <- function(input, output) {
output$start_time <- reactive({Sys.time()})

## DECISION-MAKING =============================================================

y1 <- reactive({
  
  ## pre-set action
  if (input$metro_unifier) new_states <- county_data$unified_state else {
    new_states <- county_data$state}
  contiguous <- county_data$contiguous_state
  if (input$mi_upper_peninsula_check) new_states[contiguous == "WI->MI"] <- "WI"
  if (input$fl_panhandle_check) new_states[contiguous == "AL->FL"] <- "AL"
  if (input$va_delmarva_check) new_states[contiguous == "MD->VA"] <- "MD"
  if (input$md_defrag_check) new_states[contiguous == "WV->MD"] <- "WV"
  if (input$md_defrag_check) new_states[contiguous == "MD->MD"] <- "MD"
  if (input$md_defrag_check) new_states[contiguous == "DC->MD"] <- "DC"
  if (input$ok_panhandle) new_states[contiguous == "KS->OK"] <- "KS"
  if (input$mi_upper_peninsula_check & input$metro_unifier) {
    new_states[county_data$county == "55037"] <- "WI"
    }
  
  ## custom actions
  new_states <- if_else(county_data$mega_population > input$mega_slider * 10^6,
    county_data$mega_name, new_states)

  
  return(new_states)
  
  })

y3 <- reactive({
  
  ## object preparation
  new_states <- y1()
  ReplaceState <- function(x, y, dat = new_states) {
    dat[dat %in% x] <- y
    return(dat)
    }
  
  ## pre-set actions
  if (input$merge_check1) new_states[new_states %in% c("VT", "ME")] <- "NH"
  if (input$merge_check2) new_states[new_states %in% c("WV", "DE", "DC")] <- "MD"
  if (input$merge_check3) new_states <- ReplaceState("RI", "CT")
  if (input$merge_check4) {
    if (("NeYo" %in% new_states) | (input$metro_unifier == "Yes")) {
      if ("Phil" %in% new_states) {
        new_states <- ReplaceState("NJ", "Phil")} else {
          new_states <- ReplaceState("NJ", "PA") 
          }
      }
  
  }
  
  ## custom actions
  if (input$merge_select1 != "-") {
    new_states <- ReplaceState(input$merge_select1, input$merge_dest1)}
  if (input$merge_select2 != "-") {
    new_states <- ReplaceState(input$merge_select2, input$merge_dest2)}
  if (input$merge_select3 != "-") {
    new_states <- ReplaceState(input$merge_select3, input$merge_dest3)}
  
  return(new_states)
  })

y4 <- reactive({
  
  ## prepare objects
  y3_obj <- y3()
  all_splits <- matrix(data = as.character(NA), nrow = length(y3_obj), ncol = 5)

  ## preset actions
  if (input$split_check1) all_splits[, 1] <- SplitStates("CA", 3, y3_obj)
  if (input$split_check2 & (input$mega_slider > 7)) {
    all_splits[, 2] <- SplitStates("TX", 3, y3_obj)}

  ## custom actions
  all_splits[, 3] <- SplitStates(input$split_state1, input$split_n1, y3_obj)
  all_splits[, 4] <- SplitStates(input$split_state2, input$split_n2, y3_obj)  
  all_splits[, 5] <- SplitStates(input$split_state3, input$split_n3, y3_obj)
  
  NotNA <- function(x) {
    if (all(is.na(x))) return(NA) else return(x[!is.na(x)])
    }
  all_splits <- apply(all_splits, 1, NotNA)
  all_splits <- if_else(is.na(all_splits), y3_obj, all_splits)
  return(all_splits)
  })

## COMMON MAP COMPONENTS =======================================================

## map set up
map_base <- reactive({
  ggplot() +
    coord_map(projection = "lambert",
      orientation = c(90, 0, -98), parameters = c(25, 49),
      expand = FALSE, xlim = -97 + c(-1, 1) * 21) +
    theme(legend.position = "none")
  })

## calculate inequality
i1 <- reactive({MeasureInequality(y1())})
i3 <- reactive({MeasureInequality(y3())})
i4 <- reactive({MeasureInequality(y4())})

## RENDER MAPS =================================================================

map_one <- reactive({GenerateStatePolygons(y1())})
map_three <- reactive({GenerateStatePolygons(y3())})
map_four <- reactive({GenerateStatePolygons(y4())})

output$map_one <- renderPlot({
  map_base() +
    geom_polygon(
      data = map_one(),
      mapping = aes(x = lon, y = lat, group = state),
      color = hsv(h = 196 / 360, s = 1.0, v = 0.5),
      fill =  hsv(h = 196 / 360, s = 0.1, v = 1.0),
      size = 0.3
      ) +
    geom_label(
      data = as_tibble(NA),
      x = -120.5, y = 27,
      vjust = 1, hjust = 0,
      label = MeasureInequality(y1())
      )
})

output$map_three <- renderPlot({
  map_base() +
    geom_polygon(
      data = map_three(),
      mapping = aes(x = lon, y = lat, group = state),
      color = hsv(h = 196 / 360, s = 1.0, v = 0.5),
      fill =  hsv(h = 196 / 360, s = 0.1, v = 1.0),
      size = 0.3
      ) +
    geom_label(
      data = as_tibble(NA),
      x = -120.5, y = 27,
      vjust = 1, hjust = 0,
      label = MeasureInequality(y3())
      )
  })

output$map_four <- renderPlot({
  map_base() +
    geom_polygon(
      data = map_four(),
      mapping = aes(x = lon, y = lat, group = state),
      color = hsv(h = 196 / 360, s = 1.0, v = 0.5),
      fill =  hsv(h = 196 / 360, s = 0.1, v = 1.0),
      size = 0.3
      ) +
    geom_label(
      data = as_tibble(NA),
      x = -120.5, y = 27,
      vjust = 1, hjust = 0,
      label = MeasureInequality(y4())
      )
  })

output$end_time <- reactive({Sys.time()})
} # end of server function

## EXECUTION === === === === === === === === === === === === === === === === ===
## EXECUTION === === === === === === === === === === === === === === === === ===

## FINAL ASSEMBLY ==============================================================

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  ## introduction
  ui_intro,
  
  ## panel 1
  hr(),
  fluidRow(ui_q1_title),
  br(), br(),
  sidebarLayout(
    sidebarPanel = ui_q1_controls,
    mainPanel = ui_q1_output
    ),
  
  ## panel 3
  hr(),
  fluidRow(ui_q3_title),
  br(), br(),
  sidebarLayout(
    sidebarPanel = list(ui_q3_controls1, ui_q3_controls2),
    mainPanel = ui_q3_output
    ),
  
  ## panel 4
  hr(),
  fluidRow(ui_q4_title),
  br(), br(),
  sidebarLayout(
    sidebarPanel = list(ui_q4_controls1, ui_q4_controls2),
    mainPanel = ui_q4_output
    ),
  
  textOutput("start_time"),
  textOutput("end_time")  
  )

shinyApp(ui, server)

##########==========##########==========##########==========##########==========
