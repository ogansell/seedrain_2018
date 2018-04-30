
library(shiny)
library(dplyr)
library(shinyjs)
library(dygraphs)
library(data.table)
#library(plotly)
#library(leaflet)
#library(mapview)
library(DT)
library(crosstalk)
library(tidyselect)


source("shiny_extras.R")
source("dygraph-extra-shiny.R")

load("seeddata.RData")
#m <-readRDS("m.rds")