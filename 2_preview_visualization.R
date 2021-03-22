##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M
## creation: 2021-03-22
## version: 4.0.4 Lost Library Book
## contact: github.com/sjoshuam

## environment set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, dplyr.summarize.inform = FALSE)
library(tidyverse)

## IMPORT DATA =================================================================

## load geographic data
cbsa_data   <- readRDS("B_Intermediates/cbsa_data.RData")
county_data <- readRDS("B_Intermediates/county_data.RData")

## load map data
county_map  <- readRDS("B_Intermediates/county_map.RData")
state_map   <- readRDS("B_Intermediates/state_map.RData")
us_map      <- readRDS("B_Intermediates/us_map.RData")

## GENERATE RECOMMENDED SOLUTION ===============================================

## unify metro areas

## split off large msa

## merge small states: VT,ME->NH, RI->CT, WV,DE,DC->MD, NJ -> PA

## split CA and TX

## GENERATE COMPOSITE BORDERS ==================================================

## RENDER EXPLANER VISUALIZATION ===============================================

##########==========##########==========##########==========##########==========
