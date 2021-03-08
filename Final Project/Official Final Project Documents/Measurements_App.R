

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

ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "Turtle Measurements"),
                    dashboardSidebar(disable = T),
                    dashboardBody(selectInput("species", "Select Species:", 
                                              choices=unique(turtles3$species)),
                      fluidRow(
                        box(title="Measurement Options",width=3,
                            selectInput("x", "Select X Variable", choices = c("scl_notch ", "scl_tip", "scw", "ccl_notch", "ccl_tip", "ccw", "circumference", "tail", "girth", "depth_mid", "weight"), selected = "scl_notch"),
                            selectInput("y", "Select Y Variable", choices = c("scl_notch ", "scl_tip", "scw", "ccl_notch", "ccl_tip", "ccw", "circumference", "tail", "girth", "depth_mid", "weight"), selected = "scl_notch"),
                            hr(),
                            helpText("Source: (https://www.fisheries.noaa.gov/inport/item/35875). Capture efforts were conducted to evaluate the growth rates, sex ratios, size distribution, species composition, genetic composition, relative survival rates and foraging ecology of sea turtle populations in NC.")
                        ),
                        box(
                          plotOutput("plot", width = "700px", height = "400px"),
                          h5("SCL_notch-straight carapace length measured from notch to tip in cm"),
                          h5("SCL_tip-straight carapace length measured from tip to tip in cm"),h5("SCW-straight carapace width in cm"),
                          h5("CCL_notch-curved carapace length measured from notch to tip in cm"),
                          h5("CCL_tip-curved carapace length measured from tip to tip in cm"),
                          h5("CCW-curved carapace width in cm"),h5("Circumference-body depth of turtle measured with tape measure in cm"),
                          h5("Girth-body depth of turtle measured with calipers, at highest point of carapace in cm"),
                          h5("Depth_ mid-body depth of turtle measured with calipers, at mid-point of carapace in cm"),h5("Tail-total length of tail in cm"),
                          h5("Weight-weight of turtle in kilograms"),
                        ) 
                      ) 
                    ) 
) 

server <- function(input, output, session) { 
  output$plot <- renderPlot({
    turtles3 %>% 
    filter(species == input$species) %>%
    ggplot(aes_string(x = input$x, y = input$y)) + geom_point(alpha=0.8) +scale_fill_brewer(palette = "Set1")+ theme_light(base_size = 18)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title="Turtle Measurement Comparisons")
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)