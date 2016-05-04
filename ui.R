#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(htmlwidgets)
library(shiny)
library(leaflet)
library(RJSONIO)
library(rgdal)
library(maptools)

ui <- fluidPage(
  h1(paste("DC 2016 Crime Report")),# (Data as of ", as.character(lMod), ")")),
  #p(),
  # Copy the line below to make a select box
  #selectInput("offense", label = h3("Select Offense"), choices=attributes(dcMap2$OFFENSE),selected = "ARSON"),
  selectInput("offense", label = h3("Select Offense"), 
              choices = list("Arson" = "ARSON", "Assault with Dangerous Weapon" = "ASSAULT W/DANGEROUS WEAPON", 
                             "Burglary" = "BURGLARY", "Homicide" = "HOMICIDE", "Motor Vehicle Theft" = "MOTOR VEHICLE THEFT", 
                             "Robbery" = "ROBBERY", "Sex Abuse" = "SEX ABUSE", "Theft from Motor Vehicle" = "THEFT F/AUTO", 
                             "General Theft" = "THEFT/OTHER", "Show All" = "Show All"), 
              selected = "ASSAULT W/DANGEROUS WEAPON"),
  leafletOutput("m"), #, width = "1600px", height = "900px")
  dataTableOutput("t")
)
