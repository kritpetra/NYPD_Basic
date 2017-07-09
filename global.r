
#Load shapefile and data ----
library(readr)

library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)

library(maptools)
library(rgeos)

if(!exists("arrestData")) arrestData <- read_csv("arrestData.csv")
if(!exists("precincts1")) precincts1 <- readShapeSpatial("precincts1/nypp")

exp_minus_one <- function(x) { round( exp(x)-1 ) }
