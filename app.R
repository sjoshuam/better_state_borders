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

## READ IN DATA ================================================================

county_data <- readRDS("B_Intermediates/county_data.RData")
cbsa_data <- readRDS("B_Intermediates/cbsa_data.RData")
county_map <- readRDS("B_Intermediates/county_map.RData")
us_map <- readRDS("B_Intermediates/us_map.RData")
selectable_states <- c(
  "-", sort(c("DC", state.abb)),
  sort(unique(county_data$mega_name[!is.na(county_data$mega_name)]))
  )
selectable_states <- selectable_states[!(selectable_states %in% c("HI", "AK"))]

## generate crosswalk index for transferring county_data to county_map
data_to_map <- match(county_map$county, county_data$county)

## UI === === === === === === === === === === === === === === === === === === ==
## UI === === === === === === === === === === === === === === === === === === ==

## UI - INTRO ==================================================================

ui_intro <- column(width = 12,
  h3("A Four Step Process For Drawing Better State Borders"),
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
    "However, four kinds of border changes could potentially make states",
    "less unequal.  Using this tool, you can apply those changes in various",
    "ways and see the results.  The four changes are:",
    br(), br(),
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
    "affected inequality between states.  A score of 0% indicates perfect",
    "equality between states and a score of 100% indicates egregeous",
    "inequality between states. To implements changes, the tool moves whole",
    "counties from one state to another.  As a result, the new state borders",
    "may seem ragged because county borders are not as smooth as state borders."
    ),
  p(
    "To illustrate how different options affect the results, the tool starts",
    " with some changes pre-loaded:",
    br(), br(),
    "\t1. Make the New York City and Los Angeles metropolitan areas into",
    "separate states (step 2)",
    br(),
    "    2. Merge Vermont and Maine into New Hampshire (step 3)",
    br(),
    "    3. Merge West Virginia, Deleware, and the District of Columbia into",
    "Maryland (step 3)",
    br(),
    "    4. Merge the remainder of New Jersey into Pennsylvania (step 3)",
    br(),
    "    5. Split the remainder of California into three states (step 4)",
    br(),
    "    6. Split Texas into three states (step 4)",
    br(), br(),
    "After these changes, 90% of counties remain in the state that they are",
    "currently in.  However, moving the other 10% of counties to a different",
    "state reduces inequality by more than a quarter.  This suggests that",
    "small changes to the current state borders can accomplish significant",
    "results."
    ),
  p(
    "This tool may take a few seconds to fully load.   The tool is ready when",
    "the maps are visible.  Likewise, the tool may take a few seconds to",
    "implement changes when you select new options.  The tool is done",
    "recalculating when the maps are no longer looked faded."
    )
  )

## UI - PANEL 1 ================================================================

## make panel title and explanatory text
ui_q1_title <- column(width = 12,
  h3("Step 1: Should each metropolitan area be in the jurisdiction of just",
  "one state?"),
  p(style = "max-width: 33%;",
    "Many large metropolitan areas span across multiple states. Select \"Yes\"",
    "to unite these areas into one state's jurisdiction."
    )
  )

## make control panel
ui_q1_controls <- sidebarPanel(
  selectInput(
    inputId = "metro_unifier",
    label = "Only One State Within Each Metropolitan Area",
    choices = c("No", "Yes")
    )
)

## generate reactive output
ui_q1_output <- mainPanel(plotOutput("map_one"))

## UI - PANEL 2 ================================================================

## make panel title and explanatory text
ui_q2_title <- column(width = 12,
  h3("Step 2: Should populous metropolitan areas be separate states?"),
  p(style = "max-width: 33%;",
    "Select a population threshold.  Metropolitan areas with a population",
    " greater than the threshold will split off from their state.",
    "  I recommend setting the threshold at 10 million."
    )
  )

## section control widgets
ui_q2_controls <- sidebarPanel(
  sliderInput(inputId = "mega_slider",
    label = "Split Off All Metropolitan Areas Above This Population Size",
    min = 5, max = 20, value = 10, post = "m"
    ),
  renderTable("largest_metros")
  )

## generate reactive output
ui_q2_output <- mainPanel(plotOutput("map_two"))

## UI - PANEL 3 ================================================================

## make panel title and explanatory text
ui_q3_title <- column(width = 12,
  h3("Step 3: Which states should merge into bigger states?"),
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
      "If you unified metropolitan areas in step 1, divide NJ between",
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
  h3("Step 4: Which states should split into smaller states?"),
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

## declare color generation function
StateColor <- function(state_col, the_map = county_map) {
  
  ## incorporate new state assignments into count
  the_map$the_state <- state_col
  
  ## filter down county data
  the_map$key <- paste(round(the_map$lon, 1), round(the_map$lat, 1))
  the_map <- filter(the_map, !duplicated(paste(the_map$key, the_map$state)))
  
  
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
  h <- c(1 / 12, 9 / 12, 7 / 12) %>% rep(times = 4)
  v <- seq(from = 0.5, to = 0.9, length.out = 4) %>% rep(each = 3)
  
 color_list <- setNames(hsv(h = h, s = 0.7, v = v)[color_list],
   names(color_list))
   
 return(color_list) 
}

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

## initialize server
server <- function(input, output) {
  
## DECISION-MAKING =============================================================
  
y0 <- reactive({county_data$state})
y1 <- reactive({
  if (input$metro_unifier == "Yes") county_data$unified_state else y0()
  })

y2 <- reactive({
  if_else(county_data$mega_population > input$mega_slider * 10^6,
    county_data$mega_name, y1())
  })

y3 <- reactive({
  
  ## object preparation
  new_states <- y2()
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

## color scheme
c1 <- reactive({StateColor(y1()[data_to_map])[y1()[data_to_map]]})
c2 <- reactive({StateColor(y2()[data_to_map])[y2()[data_to_map]]})
c3 <- reactive({StateColor(y3()[data_to_map])[y3()[data_to_map]]})
c4 <- reactive({StateColor(y4()[data_to_map])[y4()[data_to_map]]})

## calculate inequality
i1 <- reactive({MeasureInequality(y1())})
i2 <- reactive({MeasureInequality(y2())})
i3 <- reactive({MeasureInequality(y3())})
i4 <- reactive({MeasureInequality(y4())})

## RENDER MAPS =================================================================

output$map_one <- renderPlot({
  map_base() +
    geom_polygon(
      data = county_map,
      mapping = aes(x = lon, y = lat, group = group),
      color = c1(), fill = c1(),
      size = 0.2
      ) +
    geom_label(
      data = as_tibble(NA),
      x = -120.5, y = 27,
      vjust = 1, hjust = 0,
      label = MeasureInequality(y1())
      )
})

output$map_two <- renderPlot({
  map_base() +
    geom_polygon(
      data = county_map,
      mapping = aes(x = lon, y = lat, group = group),
      color = c2(), fill = c2(),
      size = 0.2
      ) +
    geom_label(
      data = as_tibble(NA),
      x = -120.5, y = 27,
      vjust = 1, hjust = 0,
      label = MeasureInequality(y2())
      )
  })

output$map_three <- renderPlot({
  map_base() +
    geom_polygon(
      data = county_map,
      mapping = aes(x = lon, y = lat, group = group),
      color = c3(), fill = c3(),
      size = 0.2
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
      data = county_map,
      mapping = aes(x = lon, y = lat, group = group),
      color = c4(), fill = c4(),
      size = 0.2
      ) +
    geom_label(
      data = as_tibble(NA),
      x = -120.5, y = 27,
      vjust = 1, hjust = 0,
      label = MeasureInequality(y4())
      )
  })

  
} # end of function

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
  
  ## panel 2
  hr(),
  fluidRow(ui_q2_title),
  br(), br(),
  sidebarLayout(
    sidebarPanel = ui_q2_controls,
    mainPanel = ui_q2_output
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
  
  )


shinyApp(ui, server)
# warning(
#   paste("todo: program MSA table, drill out cm() issue",
#     "generate static image with smooth borders",
#     "Make NJ merge into PA if either separate NYC, or unified metros"
#     ))

##########==========##########==========##########==========##########==========
