if (!require("tidyverse")) install.packages('tidyverse')


library(tidyverse)
library(shiny)
library(shinydashboard)
library(naniar)
library(janitor)
library(lubridate)
library(here)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)



turtles<-read.csv(here("Final Project","Official Final Project Documents/Turtle_data.csv"))
turtles_tidy<-turtles%>%
  na_if("")%>%
  na_if("0")%>%
  na_if("0.0")%>%
  mutate(dead_alive_new=if_else(Dead_Alive=="alive","Alive",Dead_Alive))
turtles3<-turtles_tidy%>%
  mutate(DateCapture_new=dmy(DateCapture))%>%
  filter(!is.na(DateCapture_new))%>%
  mutate(Capture_month=month(DateCapture_new))%>%
  mutate(Capture_day=day(DateCapture_new))%>%
  mutate(capture_week_day=wday(DateCapture_new))
turtles3<-clean_names(turtles3)


turtles_map<-turtles3%>%
  select(species, cap_longitude, cap_latitude)
green_turtles <- turtles_map %>%
  filter(cap_longitude != "NA" & cap_longitude != "NA") %>% 
  filter(species == "Green")

loggerhead_turtles <- turtles_map %>%
  filter(species == "Loggerhead")

kemps_ridley_turtles<- turtles_map %>%
  filter(species == "Kemps_Ridley")

unknown_turtles<- turtles_map %>%
  filter(species == "UN")

hawksbill_turtles <- turtles_map %>%
  filter(species == "Hawksbill")

leatherback_turtles<- turtles_map %>%
  filter(species == "Leatherback")


ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "Turtle Capture Locations"),
                    dashboardSidebar(disable = T),
                    dashboardBody(
                      fluidPage(
                        titlePanel("Turtle Capture Locations"),
                        leafletOutput(outputId = "mymap"),
                        absolutePanel(bottom = 25, left = 20, 
                                      checkboxInput("green_point", "Green", FALSE),
                                      checkboxInput("leatherback_point", "Leatherback", FALSE),
                                      checkboxInput("loggerhead_point", "Loggerhead", FALSE),
                                      checkboxInput("kemps_ridley_point", "Kemps Ridley", FALSE),
                                      checkboxInput("unknown_point", "Unknown", FALSE),
                                      checkboxInput("hawksbill_point", "Hawksbill", FALSE)
                        )
                      )
                    )
)
server <- function(input, output, session) {
  
  factpal <- colorFactor(topo.colors(6), turtles3$species)
  
  output$mymap <- renderLeaflet({
    
    basemap= leaflet()  %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    basemap %>%
      addCircles(data = green_turtles, lat = ~ green_turtles$cap_latitude, lng = ~ green_turtles$cap_longitude, weight = 1, color = "green", radius = 5, fillOpacity = 0.5, group = "Green") %>% 
      addCircles(data = leatherback_turtles, lat = ~ leatherback_turtles$cap_latitude, lng = ~ leatherback_turtles$cap_longitude, weight = 1, radius = 10, fillOpacity = 0.5,  group = "Leatherback") %>% 
      addCircles(data = loggerhead_turtles, lat = ~ loggerhead_turtles$cap_latitude, lng = ~ loggerhead_turtles$cap_longitude, weight = 1, radius = 5, fillOpacity = 0.5,  group = "Loggerhead") %>% 
      addCircles(data = kemps_ridley_turtles, lat = ~ kemps_ridley_turtles$cap_latitude, lng = ~ kemps_ridley_turtles$cap_longitude, weight = 1, radius = 10, fillOpacity = 0.5, group = "Kemps Ridley") %>% 
      addCircles(data = unknown_turtles, lat = ~ unknown_turtles$cap_latitude, lng = ~ unknown_turtles$cap_longitude, weight = 1, radius = 10, fillOpacity = 0.5, group = "Unknown") %>% 
      addCircles(data = hawksbill_turtles, lat = ~ hawksbill_turtles$cap_latitude, lng = ~ hawksbill_turtles$cap_longitude, weight = 1, radius = 10, fillOpacity = 0.5, group = "Hawksbill")
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = green_turtles)
    proxy %>% clearMarkers()
    if (input$green_point) {
      proxy %>%  addCircles(data = green_turtles, lat = ~ green_turtles$cap_latitude, lng = ~ green_turtles$cap_longitude, weight = 3, radius = 5, fillOpacity = 0.5, group = "Green", color = ~factpal(species)) 
    }
    else{
      proxy %>% clearShapes()
    }
    
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = loggerhead_turtles)
    proxy %>% clearMarkers()
    if (input$loggerhead_point) {
      proxy %>%  addCircles(data = loggerhead_turtles, lat = ~ loggerhead_turtles$cap_latitude, lng = ~ loggerhead_turtles$cap_longitude, weight = 3, radius = 10, fillOpacity = 0.5, group = "Loggerhead", color = ~factpal(species)) 
    }
    else{
      proxy %>% clearShapes()
    }
    
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = kemps_ridley_turtles)
    proxy %>% clearMarkers()
    if (input$kemps_ridley_point) {
      proxy %>%  addCircles(data = kemps_ridley_turtles, lat = ~ kemps_ridley_turtles$cap_latitude, lng = ~ kemps_ridley_turtles$cap_longitude, weight = 3, radius = 10, fillOpacity = 0.5, group = "Kemps Ridley", color = ~factpal(species)) 
    }
    else{
      proxy %>% clearShapes()
    }
    
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = unknown_turtles)
    proxy %>% clearMarkers()
    if (input$unknown_point) {
      proxy %>%  addCircles(data = unknown_turtles, lat = ~ unknown_turtles$cap_latitude, lng = ~ unknown_turtles$cap_longitude, weight = 10, radius = 10, fillOpacity = 0.5, group = "Unknown", color = ~factpal(species)) 
    }
    else{
      proxy %>% clearShapes()
    }
    
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = hawksbill_turtles)
    proxy %>% clearMarkers()
    if (input$hawksbill_point) {
      proxy %>%  addCircles(data = hawksbill_turtles, lat = ~ hawksbill_turtles$cap_latitude, lng = ~ hawksbill_turtles$cap_longitude, weight = 10, radius = 10, fillOpacity = 0.5, group = "Hawksbill", color = ~factpal(species)) 
    }
    else{
      proxy %>% clearShapes()
    }
    
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = leatherback_turtles)
    proxy %>% clearMarkers()
    if (input$leatherback_point) {
      proxy %>%  addCircles(data = leatherback_turtles, lat = ~ leatherback_turtles$cap_latitude, lng = ~ leatherback_turtles$cap_longitude, weight = 10, radius = 10, fillOpacity = 0.5, group = "Leatherback", color = ~factpal(species)) 
    }
    else{
      proxy %>% clearShapes()
    }
    
    
  })
  session$onSessionEnded(stopApp)
}
shinyApp(ui, server)