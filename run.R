library(htmlwidgets)
library(shiny)
library(leaflet)
library(RJSONIO)
library(rgdal)
library(maptools)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
