
#Load shapefile and data ----
library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)

library(maptools)
library(rgeos)

if(!exists("arrestData")) arrestData <- read.csv("arrestData.csv")
if(!exists("precincts1")) precincts1 <- readShapeSpatial("precincts1/nypp")

exp_minus_one <- function(x) { round( exp(x)-1 ) }
