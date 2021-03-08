



if (!require("tidyverse")) install.packages('tidyverse')


library(tidyverse)
library(shiny)
library(shinydashboard)
library(naniar)
library(janitor)
library(lubridate)
library(here)

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


turtles3$year<-as.character(turtles3$year)
ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "Turtle Captures"),
                    dashboardSidebar(disable = F),
                    dashboardBody(selectInput("species", "Select Species:", 
                                              choices=unique(turtles3$species)),
                                  fluidRow(
                                    box(title = "Plot Options", width = 4,
                                        selectInput("x", "Catch Details", choices = c("research_type","dead_alive_new", "body_area_pit","body_area","record_type","cap_region","rel_region"), 
                                                    selected = "record_type"),
                                        hr(),
                                        helpText("Source: (https://www.fisheries.noaa.gov/inport/item/35875). Capture efforts were conducted to evaluate the growth rates, sex ratios, size distribution, species composition, genetic composition, relative survival rates and foraging ecology of sea turtle populations in NC.")
                                    ), 
                                    box(title = "Turtle Information", width = 8,
                                        plotOutput("plot", width = "800px", height = "500px")
                                    ) 
                                  ) 
                    ) 
)
server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    turtles3 %>%
      filter(species == input$species) %>%
      ggplot(aes_string(x ="year",fill = input$x)) +
      geom_bar(position = "dodge")+
      scale_fill_brewer(palette = "Set1")+
      theme_light(base_size = 18)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      labs(title = "Turtle Catch Characteristics",x=NULL,y="Number of Turtles")
  })
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)