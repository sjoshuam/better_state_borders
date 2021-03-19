##########==========##########==========##########==========##########==========

## ENVIRONMENT SET UP ==========================================================

## meta-information
## author: Josh M
## contact: github.com/sjoshuam
## creation: 2021-03-16
## version: 4.0.4 Lost Library Book
## description: generates an interactive for exploring alternatives to the
   ## current set of state borders.

## prepare the environment
remove(list = objects())
options(digits = 3, scipen = 2, width = 80, dplyr.summarise.inform = FALSE)
library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)

## Alegany - 24001
## Garrett - 24023
## Washington - 24043

## READ IN DATA ================================================================

county_data <- readRDS("B_Intermediates/county_data.RData")
cbsa_data <- readRDS("B_Intermediates/cbsa_data.RData")
county_map <- readRDS("B_Intermediates/county_map.RData")
us_map <- readRDS("B_Intermediates/us_map.RData")
selectable_states <- c("-", sort(c("DC", state.abb)))

## DO FINAL ASSEMBLY ===========================================================

county_data <- county_data %>%
  mutate(
    mega_population = if_else(is.na(mega_population), 0, mega_population)
    )

county_map <- county_map %>%
  left_join(
    select(county_data, county, unified_state, mega_name, mega_population))


## DECLARE INEQUALITY MEASUREMENT FUNCTION =====================================

MeasureInequality <- function(state_col, c_data = county_data, as_title = T) {
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
    unlist()
    
    if(as_title) {
      inequality <- paste(
        "Gini Inequality Scores (Lower Is Better)\nLand Area:",
        round(inequality[1], 3),
        "    Population:",
        round(inequality[2], 3),
        "    Combined:",
        round(sqrt(prod(inequality)), 3)
        )
      }

  return(inequality)
}

ExtractValues <- function(x, i = input) {
  y <- vector(mode= "list", length = length(x))
  names(y) <- x
  for(iter in x) {
    y[[iter]] <- i[[iter]] 
  }
  return(y)
  }

MeasureInequality("state")

## BUILD THE UI ================================================================

## render basic county maps
basic_map <- ggplot() +
  coord_map(projection = "lambert", orientation = c(90, 0, -99),
    parameters = c(25, 49), expand = FALSE, ylim = c(25, 49),
    xlim = -97.5 + c(-1, 1) * 21
    )

## Step 0: Introduction --- --- --- --- --- --- --- --- --- --- --- --- --- ---
intro0 <- column(width = 12,
  h3("A Four Step Process For Drawing Better State Borders"),
  p(
    "The United States\' state borders do not divide the United States",
    "according to a grand plan.  The borders reflect historic events,",
    "political bargains, and other considerations that are less relevant",
    "in modern times.  As result, US states contain unequal numbers of people",
    "and unequal land area. In addition, state borders cross through major",
    "metropolitan areas.  Together, these inequities complicate governance."
    ),
  p(
    "However, four kinds of border changes could potentially make states",
    "less unequal.  Using this page, you can apply those changes in various",
    "ways and see the results.  The four changes are:",
    br(),
    "1. Consolidate each metropolitan areas into the jurisdiction of just one",
    "state",
    br(),
    "2. Split the largest metropolitan areas into separate states",
    br(),
    "3. Merge small states into other states",
    br(),
    "4. Split large states into multiple smaller states"
    ),
  p(
    "For each change, you can choose whether to implement the change and how",
    "it will be implemented.  Once you make the change, the maps will update",
    "to show the new borders.  In addition, the inequality scores (gini",
    "coefficients) will update, so that you can measure how your changes",
    "affected inequality between states.  A score of 0.000 indicates perfect",
    "equality between states and a score of 1.000 indicates egregeous",
    "inequality between states."
    )
  )


## Step 1: Unify Cities --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## section text
metro0 <- column(width = 12,
  h3("Step 1: Should each metropolitan area be in the jurisdiction of just",
  "one state?"),
  p(style = "max-width: 33%;",
    "Many large metropolitan areas span across multiple states. Select \"Yes\"",
    " to unite these areas into one state's jurisdiction. I recommend this",
    " action, even though it increases inequality slightly."
    )
  )

## section control widgets
metro1 <- column(width = 4,
  selectInput(inputId = "metro_unifier",
    label = "Only One State Within Each Metropolitan Area",
    choices = c("Yes", "No")
    )
  )

## map
metro3 <- column(width = 8, plotOutput("map_one"))

## Step 2: Split Mega-Cities --- --- --- --- --- --- --- --- --- --- --- --- ---

## section text
mega0 <- column(width = 12,
  h3("Step 2: Should populous metropolitan areas be separate states?"),
  p(style = "max-width: 33%;",
    "Select a population threshold.  Metropolitan areas with a population",
    " greater than the threshold will split off from their state.",
    "  I recommend setting the threshold at 7 million, which splits ",
    "New York City NY, Los Angeles CA, and Chicago IL from their",
    " states."
    )
  )

## section control widgets
mega1 <- column(width = 4,
  sliderInput(inputId = "mega_slider",
    label = "Split Off All Metropolitan Areas Above This Population Size",
    min = 5, max = 20, value = 9, post = "m"
    )
  )

## map
mega3 <- column(width = 8, plotOutput("map_two"))

## Step 3: Merge Small States --- --- --- --- --- --- --- --- --- --- --- --- --

## section text
merge0 <- column(width = 12,
  h3("Step 3: Which states should merge into bigger states?"),
  p(style = "max-width: 33%;",
    "Select up to 8 states that should merge into other states.",
    " The states on the left will be merged into the states on the right.",
    "  I recommend merging: (1) ME and VT into NH, (2) RI into CT,",
    " (3) DE, DC, and WV into MD, and",
    " (4) if you split New York City from New York state, NJ into PA."
    )
  )

## section control widgets
merge1 <- column(width = 2,
  selectInput(inputId = "merge1_state", label = "Merge State #1",
    choices = selectable_states, selected = "ME"
    ),
  selectInput(inputId = "merge2_state", label = "Merge State #2",
    choices = selectable_states, selected = "VT"
    ),
  selectInput(inputId = "merge3_state", label = "Merge State #3",
    choices = selectable_states, selected = "RI"
    ),
  selectInput(inputId = "merge4_state", label = "Merge State #4",
    choices = selectable_states, selected = "DE"
    ),
  selectInput(inputId = "merge5_state", label = "Merge State #5",
    choices = selectable_states, selected = "WV"
    ),
  selectInput(inputId = "merge6_state", label = "Merge State #6",
    choices = selectable_states, selected = "DC"
    ),
  selectInput(inputId = "merge7_state", label = "Merge State #7",
    choices = selectable_states, selected = "NJ"
    ),
  selectInput(inputId = "merge8_state", label = "Merge State #8",
    choices = selectable_states, selected = "-"
    ),
  selectInput(inputId = "merge9_state", label = "Merge State #9",
    choices = selectable_states, selected = "-"
    ),
  selectInput(inputId = "merge10_state", label = "Merge State #10",
    choices = selectable_states, selected = "-"
    )
  )

merge2 <- column(width = 2,
  selectInput(inputId = "merge1_dest", label = "Destination #1",
    choices = selectable_states, selected = "NH"
    ),
  selectInput(inputId = "merge2_dest", label = "Destination #2",
    choices = selectable_states, selected = "NH"
    ),
  selectInput(inputId = "merge3_dest", label = "Destination #3",
    choices = selectable_states, selected = "CT"
    ),
  selectInput(inputId = "merge4_dest", label = "Destination #4",
    choices = selectable_states, selected = "MD"
    ),
  selectInput(inputId = "merge5_dest", label = "Destination #5",
    choices = selectable_states, selected = "MD"
    ),
  selectInput(inputId = "merge6_dest", label = "Destination #6",
    choices = selectable_states, selected = "MD"
    ),
  selectInput(inputId = "merge7_dest", label = "Destination #7",
    choices = selectable_states, selected = "PA"
    ),
  selectInput(inputId = "merge8_dest", label = "Destination #8",
    choices = selectable_states, selected = "-"
    ),
  selectInput(inputId = "merge9_dest", label = "Destination #9",
    choices = selectable_states, selected = "-"
    ),
  selectInput(inputId = "merge10_dest", label = "Destination #10",
    choices = selectable_states, selected = "-"
    )
  )

## map
merge3 <- column(width = 8, plotOutput("map_three"))

## Step 4: Split Large States --- --- --- --- --- --- --- --- --- --- --- --- --

## section text
split0 <- column(width = 12,
  h3("Step 4: Which states should split into smaller states?"),
  p(style = "max-width: 33%;",
    "Select up to 4 states that should split into 2-4 states",
    "  I recommend splitting CA and TX into 3 states each."
    )
  )

## section control widgets
split1 <- column(width = 2,
  selectInput(inputId = "split1_state", label = "Split State #1",
    choices = selectable_states, selected = "CA"
    ),
  br(),
  selectInput(inputId = "split2_state", label = "Split State #2",
    choices = selectable_states, selected = "TX"
    ),
  br(),
  selectInput(inputId = "split3_state", label = "Split State #3",
    choices = selectable_states),
  br(),
  selectInput(inputId = "split4_state", label = "Split State #4",
    choices = selectable_states)
  )

split2 <- column(width = 2,
  sliderInput(inputId = "split1_n", label = "Total Parts #1",
    min = 2, max = 4, value = 3),
  sliderInput(inputId = "split2_n", label = "Total Parts #2",
    min = 2, max = 4, value = 3),
  sliderInput(inputId = "split3_n", label = "Total Parts #3",
    min = 2, max = 4, value = 2),
  sliderInput(inputId = "split4_n", label = "Total Parts #4",
    min = 2, max = 4, value = 2)
  )

## map
split3 <- column(width = 8, plotOutput("map_four"))

## declare function to select map colors --- --- --- --- --- --- --- --- --- ---
StateColor <- function(the_map, state_col,
  first_hue = 3 / 12, highlight_hue = 6 / 12, first_bright = 7 / 12) {
  
  ## filter down county data
  the_map$key <- paste(round(the_map$lon, 1), round(the_map$lat, 1))
  the_map <- filter(the_map, !duplicated(paste(the_map$key, the_map$state))) %>%
    rename(the_state = starts_with(state_col))
  
  
  ## find which states are adjacent
  adj_list <- the_map %>%
    select(key, the_state)
  adj_list <- left_join(adj_list, adj_list, by = "key") %>%
    select(the_state.x, the_state.y) %>%
    unique()
  
  ## generate a starting list of colors
  color_list <- as.matrix(adj_list) %>% as.vector() %>% unique()
  color_list <- setNames(
    rep(seq(12), times = 8)[seq(length(color_list))],
    color_list
    )
  
  ## detect adjacent states with the same color
  adj_list <- adj_list %>%
    filter(the_state.x != the_state.y) %>%
    mutate(
      color.x = color_list[the_state.x],
      color.y = color_list[the_state.y],
      same_color = color.x == color.y
      )

  needs_revision <- unique(adj_list$the_state.x[adj_list$same_color])
  
  while(length(needs_revision) > 0) {
    
    color_options <- adj_list$color.y[adj_list$the_state.x == needs_revision[1]]
    color_options <- seq(12)[!(seq(12) %in% color_options)]
    color_list[needs_revision[1]] <- color_options[1]
    
    adj_list <- adj_list %>%
      mutate(
        color.x = color_list[the_state.x],
        color.y = color_list[the_state.y],
        same_color = color.x == color.y
        )
    needs_revision <- unique(adj_list$the_state.x[adj_list$same_color])
    
  }
  
  ## generate colors
  h <- seq(from = first_hue, by = 2 / 12, length.out = 3) %>% rep(times = 4)
  h <- c(1 / 12, 9 / 12, 7 / 12) %>% rep(times = 4)
  hh <- seq(from = highlight_hue, by = 1 / 12, length.out = 3) %>%
    rep(times = 4)
  v <- seq(from = first_bright, by = 1 / 12, length.out = 4) %>% rep(each = 3)
  
 color_list <- list(
   "main" = setNames(
     hsv(h = h, s = 0.7, v = v)[color_list],
     names(color_list)
   ),
   "highlight" = setNames(
     hsv(h = hh, s = 0.7, v = v)[color_list],
     names(color_list)
   )
   )
   
 return(color_list) 
}

## Assemble UI elements --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## assemble the IU
the_ui <- fluidPage(theme = shinytheme("cerulean"),
  
  intro0,
  
  hr(),
  fluidRow(metro0),
  br(), br(),
  fluidRow(metro1, metro3),
  
  hr(),
  fluidRow(mega0),
  br(), br(),
  fluidRow(mega1, mega3),
  
  hr(),
  fluidRow(merge0),
  br(), br(),
  fluidRow(merge1, merge2, merge3),
  
  hr(),
  fluidRow(split0),
  br(), br(),
  fluidRow(split1, split2, split3),
  
  hr(),
  br(),
  br()
  )

## BUILD CORE MANIPULATION FUNCTIONS ===========================================

  # ## assemble new state assignments
  # county_data$map_zero <- county_data$state # 0
  # 
  # if (input$metro_unifier == "Yes") { # 1
  #   county_data$map_one <- county_data$unified_state
  # } else{
  #   county_data$map_one <- county_data$map_zero
  # }
  # 
  # county_data <- county_data %>% # 2
  #   mutate(
  #     mega_population = ifelse(
  #       mega_population < input$mega_slider * 10^6, 0, mega_population),
  #       map_two = if_else(mega_population == 0, map_one, mega_name)
  #       )
  # 
  # merge_state <- paste0("merge", 1:10, "_state") %>% # 3
  #   ExtractValues(i = input) %>% unlist
  # merge_dest  <- paste0("merge", 1:10, "_dest") %>%
  #   ExtractValues(i = input) %>% unlist
  # merge_state <- tibble("origin" = merge_state, "dest" = merge_dest) %>%
  #   filter(merge_state != "-", merge_dest != "-", !duplicated(merge_state))
  # remove(merge_dest)
  # 
  # merge_state <- select(county_data, map_two) %>%
  #   left_join(merge_state, by= c("map_two" = "origin"))
  # county_data <- county_data %>%
  #   mutate(
  #     map_three = if_else(!is.na(merge_state$dest), merge_state$dest, map_two)
  #     )
  # remove(merge_state)

i1 <- sample(c("Yes", "No"), 1)
i2 <- sample(5:10, 1)
i3a <- list(
  "merge1_state" = "ME",
  "merge2_state" = "VT"
  )
i3b <- list(
  "merge1_dest" = "NH",
  "merge2_dest" = "NH"
  )

MakeDecisions <- function(d1 = i1, d2 = i2, d3a = i3a, d3b = i3b,
  cd = county_data, cm = county_map) {
  
  ## decision 1
  cd$map_one <- cd$map_zero <- cd$state
  if (d1 == "Yes") cd$map_one <- cd$unified_state
  
  ## decision 2
  
select(cd, map_zero, map_one)
}

MakeDecisions()

## BUILD THE SERVER ============================================================

the_server <- function(input, output, ...){
  
  dat <- reactive({
    MakeDecisions(
      d1 = input$metro_unifier,
      d2 = input$mega_slider,
      d3a = paste0("merge", 1:10, "_state") %>% 
        ExtractValues(i = input) %>% unlist,
      d3b = paste0("merge", 1:10, "_dest") %>% 
        ExtractValues(i = input) %>% unlist,
      )
    })
  

  ## map one --- --- --- --- --- --- --- ---
  
  ## generate metro area plot
  output$map_one <- renderPlot({
    
    print(dat)
    
  ## calculate colors
  color_palette <- StateColor(county_map, state_col = "map_one")$main
  
  ## render plot
    basic_map +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_palette) + 
      scale_color_manual(values = color_palette) +
      geom_polygon(
        data = county_map,
        mapping = aes(x = lon, y = lat, group = group, color = map_one,
          fill = map_one),
        size = 0.2
        ) +
      geom_polygon(
        data = us_map,
        mapping = aes(x = lon, y = lat, group = group),
          fill = "transparent", color = "black"
        ) + 
      ggtitle(MeasureInequality(state_col = "map_one", c_data = county_data))

    })
  
  ## map two --- --- --- --- --- --- --- ---

  ## generate mega-cities map
  output$map_two <- renderPlot({
    
    ## generate colors
    color_palette <- StateColor(county_map, state_col = "map_two")
    color_palette <- color_palette$main
      
    ## render plot
    basic_map +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_palette) +
      scale_color_manual(values = color_palette) +
      geom_polygon(
        data = county_map,
        mapping = aes(x = lon, y = lat, group = group, color = map_two,
          fill = map_two),
        size = 0.2
        ) +
      geom_polygon(
        data = us_map,
        mapping = aes(x = lon, y = lat, group = group),
          fill = "transparent", color = "black"
        ) + 
      ggtitle(MeasureInequality(state_col = "map_two", c_data = county_data))
    
    })

  ## map three --- --- --- --- --- --- --- ---
  
  ## generate state split map
  output$map_three <- renderPlot({
    
    ## generate colors
    color_palette <- StateColor(county_map, state_col = "map_three")
    color_palette <- color_palette$main
      
    ## render plot
    basic_map +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_palette) +
      scale_color_manual(values = color_palette) +
      geom_polygon(
        data = county_map,
        mapping = aes(x = lon, y = lat, group = group, color = map_three,
          fill = map_three),
        size = 0.2
        ) +
      geom_polygon(
        data = us_map,
        mapping = aes(x = lon, y = lat, group = group),
          fill = "transparent", color = "black"
        ) + 
      ggtitle(MeasureInequality(state_col = "map_three", c_data = county_data))
    })
  
  ## map four --- --- --- --- --- --- --- ---
  
  ## generate state merge map
  output$map_four <- renderPlot({
    basic_map
    })
  }

##########==========##########==========##########==========##########==========
shinyApp(the_ui, the_server)