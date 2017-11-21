setwd('/Users/luxinyi/Desktop/Research/HarborSeal/Script/EDA')
library(geosphere)
library(leaflet)
library(shiny)
load('dfc.Rdata')
source('functions.R')

shinyApp(ui = ui, server = server)