#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

grabInfoA<-function(val,tmp){
  l<- length(tmp[[1]][[val]])
  tmp<-lapply(1:l, function(y) 
    sapply(tmp, function(x){
      if(!is.null(x[[val]][[y]])){
        return(x[[val]][[y]])
      }else{
        return(NA)
      }}))}
grabInfoB<-function(val,val2,tmp){
  l<- length(tmp[[1]][[val]][[val2]])
  tmp<-lapply(1:l, function(y) 
    sapply(tmp, function(x){
      if(!is.null(x[[val]][[val2]][[y]])){
        return(x[[val]][[val2]][[y]])
      }else{
        return(NA)
      }}))}
grabJson<-function(val){
  addy1<-"http://opendata.dc.gov/datasets/bda20763840448b58f8383bae800a843_26.geojson?where=OFFENSE%20like%20'%25"
  addy2<-"%25'&geometry={\"xmin\":-8678721.84390722,\"ymin\":4682202.251605163,\"xmax\":-8472494.741593735,\"ymax\":4728064.468576287,\"spatialReference\":{\"wkid\":102100,\"latestWkid\":3857}}"
  addy <- paste(addy1,val,addy2,sep="", collapse=NULL)
  
  if (val=="Show All") {
    addy <- "http://opendata.dc.gov/datasets/bda20763840448b58f8383bae800a843_26.geojson"
  }
  
  crime.json <- fromJSON(addy, simplify=TRUE)
  
  #Post extraction from JSON do summary() on list (crime.json) to identify the node for extraction
  tmp <-crime.json[['features']]
  
  #Pulling data out of nested list and combining into a data frame
  a<-data.frame(grabInfoA(2, tmp))
  b<-data.frame(grabInfoB(3,2, tmp))
  names(a)<-names(tmp[[1]]$properties)
  names(b)<-c("coords.x1","coords.x2")
  dcMap2 <- cbind(a,b)
  return(dcMap2)
}

#Criminal Offense list
off <- c("ARSON", "ASSAULT W/DANGEROUS WEAPON", "BURGLARY", "HOMICIDE", "MOTOR VEHICLE THEFT", "ROBBERY", "SEX ABUSE","THEFT F/AUTO", "THEFT/OTHER")

# Marker color list
cMarker <- c("pink", "blue", "green","orange", "black","yellow", "gray", "purple", "red")

server <- function(input, output, session) {

  off.df <- reactive({grabJson(input$offense)})
  
  #extrating the last modified date
  #lMod <- reactive({format(as.Date(as.POSIXlt(off.df()$LASTMODIFIEDDATE[1])), format="%B %d, %Y")}) 
  
  date <- reactive({format(as.Date(as.POSIXlt(off.df()$REPORTDATETIME)), format="%B %d, %Y")}) #extrating the date from the date time format
  date.df <- reactive({paste("<b>Date: </b>",as.character(date()),"<br><b>Offense: </b>",off.df()$OFFENSE)}) #concatenating 2 text columns in a data.frame
  
  #Associates icon color with criminal offense
  leafIcons <- reactive({icons(iconUrl = ifelse(off.df()$OFFENSE == "ARSON", "pink.png",ifelse(off.df()$OFFENSE == "ASSAULT W/DANGEROUS WEAPON", 
             "blue.png",ifelse(off.df()$OFFENSE == "BURGLARY", "green.png",ifelse(off.df()$OFFENSE == "HOMICIDE", "orange.png",
             ifelse(off.df()$OFFENSE == "MOTOR VEHICLE THEFT", "black.png",ifelse(off.df()$OFFENSE == "ROBBERY","yellow.png",
             ifelse(off.df()$OFFENSE == "SEX ABUSE", "gray.png",ifelse(off.df()$OFFENSE == "THEFT F/AUTO", "violet.png",
             "red.png")))))))), iconWidth = 20, iconHeight = 40)})
  
  output$m <- renderLeaflet({
    withProgress(message = 'Making plot', value = 0, {
    leaflet() %>%
      setView(lng = -77.015734, lat = 38.877077, zoom = 14) %>% 
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=off.df()$coords.x1, lat=off.df()$coords.x2,popup = date.df(), icon = leafIcons()) %>% #add markers and colors
      addLegend(position = 'topright', colors = cMarker, labels = off, opacity = 0.6, title = "Legend")
    })
      })
  output$t <- renderDataTable({off.df()})
  
}