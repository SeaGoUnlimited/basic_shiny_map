library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(maptools)
library(sf)
library(sp)
#library(gstat)
library(rgdal)
library(rgeos)
library(tmap)
library(raster)
library(dplyr)
library(shiny)
library(shinydashboard)

#import raw whale data
sp_wa_obis <- read_csv("sp_wa_obis.csv")

#add date column to simplify datetime to date
sp_wa_obis$date<- as.Date(sp_wa_obis$EventDate, format="%Y/%M/%D")

#add time column
sp_wa_obis$time <- format(as.POSIXct(sp_wa_obis$EventDate) ,format = "%H:%M:%S") 

#add year column
sp_wa_obis$year<-format(as.Date(sp_wa_obis$EventDate, format="%Y/%m/%d"),"%Y")

#add month column
sp_wa_obis$month<-format(as.Date(sp_wa_obis$EventDate, format="%Y/%m/%d"),"%m")

################################

#BEGIN APP
##########################




ui <- dashboardPage(
  
#setting up shiny dashboard layout  
  dashboardHeader(title = "SEAtizen Science",titleWidth = 450,
                  #a lil bit of html styling
                  tags$li(a(href ="http://www.seatizenscience.org/",
                            img(src='whale-fi.png',
                                title = "SEAtizen Science", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  #setup sidebar layout
  dashboardSidebar(
    sidebarMenu(
    #menuitem 'whale map' which will have the map. 
      menuItem("Whale Map", tabName = "cinms", icon=icon("map"),startExpanded = FALSE),
      #year dropdown which uses year column
      selectInput(inputId = "year",                                   label="Year:",
                  selected = "2018",
                  choices = sort(unique(sp_wa_obis$year))),
      #sliderInput(inputId = "month",
                 # label="Month:",
                  #min = 1, max=12,
                  #value = c(1,12)),
      #makes species dropdown 
      selectInput(inputId = "species",
                  label="Species:",
                  selected = "Blue Whale",
                  choices = sort(unique(sp_wa_obis$vernacularName))),
      
      #setup an about menuitem to explain what the data is about
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      #setup a Team menuitem to explain who's involved with the project
      menuItem("The Team", tabName = "team", icon = icon("dashboard"))
    
      
      
      )),
  
  #connects sidebar items with 'dashboard body' 
  #uses a lil html stylin to make the layout nice.
  body<-dashboardBody(
    tags$style(type = "text/css", "#cinms {height: calc(100vh - 80px) !important;}"),
    #tabitems need to match ones made above. i.e. "menuItem("Whale Map", tabName = "cinms" needs to match "tabItem(tabName="cinms"
    tabItems(
      
      tabItem(tabName="cinms",
              fluidRow(
                box(align="center",
                  title = "Whale Conservation Map",
                  collapsible = TRUE,
                  background = "blue",
                  width = "100%",
                  height = "2000px",tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  leafletOutput("cinms")
                ))),
      tabItem(tabName="about",
              fluidRow(
                box(align="center",
                  h2("Data Collection Summary"),
                    p("This app presents data collected by volunteer citizen scientists using Spotter Pro, a smart phone app designed for observing marine species. The volunteers are part of the Channel Islands Naturalist Corps, a program started by Channel Island National Marine Sanctuary (CINMS) in the 1990s. The Naturalist Corps is comprised of over 160 volunteers who record data on marine life nearly every day aboard whale watching boats in the Santa Barbara Channel (SBC). When the program first started, sightings were recorded on paper logs. Since 2013, volunteers input data directly into the Spotter Pro mobile application while aboard marine vessels, typically the Condor Express whale watching boat out of Santa Barbara, CA. Only trained Naturalists can access the Spotter Pro app, but another CINMS mobile application called Whale Alert allows the general public to record sightings. This citizen-collected information has been used to create on of the largest datasets on marine mammals in the SBC, and was even used by CINMS to move shipping lanes in 2013 by one nautical mile to prevent whale ship strikes. THIS DATA IS RAW AND HAS NOT UNDERGONE AND FILTERING."),
                  p(div(img(src='whale.jpeg', height=400, width = 600)), a(br(em("Source: Condor Express")), href = "https://condorexpress.com/")), h6("Citizen scientists aboard the condor express take photos of Humpbacks that surfaced near the boat. These photos are used to identify individual whales as a part of the dataset created by Channel Islands Naturalist Corps volunteers."),
                  collapsible = TRUE,
                  background = "blue",
                  width = "100%",
                  height = "1000px"
                  
                ))),
      
      tabItem(tabName="team",
              fluidRow(
                box(align="center",
                  h3("Meet the Team!"),
                  h4("A radical group of Master's and PhD students from the Bren School of Environmental Science and Management, UCSB."),
                  p(div(img(src='team.png', height=600, width = 800)), a(br(em("")), href = "https://seatizenscience.org/")), h4("Left to right: Niklas Greissbaum, Sean Goral, Rae Fuhrman, Jasmine Vazin, Molly Williams, Charlene Kormondy & Dr. James Frew"),
                  collapsible = TRUE,
                  background = "blue",
                  width = "100%",
                  height = "1000px"
                  
                  )))
      
    )
  )
)

#Server is where reactivity occurs. connects widgets to data

server <- function(input, output, session) {
  
  
 whaleicon=icons("whale_icon.png")
  
 #render leaflet map
  output$cinms <- renderLeaflet({ 
    #Creates 'dynamic df' for the leaflet map, reference this 'reactive df in all code below
    cinms_map<-sp_wa_obis %>% 
      filter(vernacularName==input$species) %>% 
      filter(year==input$year) #%>%  
     # filter(month==input$month)
    
    #set up labels for map datapoints. There's a lot of crazy wayss to do this so...
    labs <- lapply(seq(nrow(cinms_map)), function(i) {
      paste0( " Species: ",cinms_map[i,"vernacularName"], '</p>', 
              " Scientific Name: ",cinms_map[i,"scientificName"], '</p>', 
              "Certainty: ",cinms_map[i, "occurenceStatus"], '</p>', 
              "Total Sighted: ",cinms_map[i, "individualCount"],'</p>', 
              "App Used: ",cinms_map[i, "app_used"], '</p>',
              " Trip ID:: ",cinms_map[i,"collectionID"], '</p>',
              " Month: ",cinms_map[i,"month"], '</p>')
      
    })
    
    #create leaflet map
    leaflet(cinms_map) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addAwesomeMarkers(lat=cinms_map$DecimalLatitude,lng=cinms_map$DecimalLongitude, label = lapply(labs, HTML)) 
  })
  
}
#seal the deal
shinyApp(ui, server)
