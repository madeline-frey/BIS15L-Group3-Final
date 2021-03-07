---
title: "Surveying Sea Turtles"
author: "Eric Coyle, Isaiah Bluestein, and Madeline Frey"
output: 
  html_document: 
    keep_md: yes
---







# Turtle Dataset
We are using data collected by NOAA Fisheries' Southeast Fisheries Science Center. They characterized North Carolina populations of sea turtles between 1995 and 2015.
[NOAA Download link](https://www.nodc.noaa.gov/archive/arc0108/0162846/1.1/data/0-data/26466/pop_survey_tagging_data.csv)

# Cleaning Data


```r
turtles_tidy<-turtles%>%
  na_if("")%>%
  na_if("0")%>%
  na_if("0.0")%>%
  mutate(dead_alive_new=if_else(Dead_Alive=="alive","Alive",Dead_Alive))
```



```r
turtles3<-turtles_tidy%>%
  mutate(DateCapture_new=dmy(DateCapture))%>%
  filter(!is.na(DateCapture_new))%>%
  mutate(Capture_month=month(DateCapture_new))%>%
  mutate(Capture_day=day(DateCapture_new))%>%
  mutate(capture_week_day=wday(DateCapture_new))
turtles3<-clean_names(turtles3)
```

# Where were turtles captured? 

```
##   cap_latitude   cap_longitude   
##  Min.   :33.49   Min.   :-79.08  
##  1st Qu.:34.84   1st Qu.:-76.38  
##  Median :34.93   Median :-76.27  
##  Mean   :34.96   Mean   :-76.24  
##  3rd Qu.:35.05   3rd Qu.:-76.13  
##  Max.   :38.41   Max.   :-75.47
```


```r
cap_lat <- c(33.49, 38.41)
cap_long <- c(-79.08, -75.47)
bbox <- make_bbox(cap_long, cap_lat, f = 0.1)
```


```r
cap_map_base <- get_map(bbox, maptype = "toner-lite", source = "stamen")
```

```
## Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
```



![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

# Where were turtles released?


```
##   rel_latitude   rel_longitude   
##  Min.   :27.82   Min.   :-81.24  
##  1st Qu.:34.83   1st Qu.:-76.38  
##  Median :34.89   Median :-76.33  
##  Mean   :34.92   Mean   :-76.26  
##  3rd Qu.:35.04   3rd Qu.:-76.12  
##  Max.   :39.00   Max.   :-36.03  
##  NA's   :351     NA's   :353
```


```r
rel_lat <- c(27.82, 39)
rel_long <- c(-81.24, -36.03)
bbox2 <- make_bbox(rel_long, rel_lat, f = 0.05)
```


```r
rel_map_base <- get_map(bbox2, maptype = "toner-lite", source = "stamen")
```

```
## Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
```


![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
There is one outlier Loggerhead turtle, so we decided to remove it:


```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-13-1.png)<!-- -->




#Qualitative Data of Turtle Captures over Time

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}


#What Tests were performed on Turtles over time


```r
turtles3$year<-as.character(turtles3$year)
ui <- dashboardPage(skin="green",
  dashboardHeader(title = "Turtle Capture Tests"),
  dashboardSidebar(disable = F),
  dashboardBody(selectInput("species", "Select Species:", 
                  choices=unique(turtles3$species)),
  fluidRow(
  box(title = "Plot Options", width = 4,
  selectInput("x", "Type of Test", choices = c("oxtetracyclene","health_blood","satellite_tag","holding_facility","sex_laparoscopy",     "sex_necropsy","sex_testosterone_level_1","sex_testosterone_level_2","hematology","oc_ob","fh","metals","sia_skin","sia_bone",     "sia_blood","sia_barnacles","sia_scutes","sia_analyzed","photos","scute","cloacal","lesion","fat","fecal","pathogens",     "entangled","imaging","organ_biopsy"), 
              selected = "entangled"),
      hr(),
      helpText("Source: (https://www.fisheries.noaa.gov/inport/item/35875). Capture efforts were conducted to evaluate the growth rates, sex ratios, size distribution, species composition, genetic composition, relative survival rates and foraging ecology of sea turtle populations in NC.")
  ), 
  box(title = "Test Performed T/F", width = 8,
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
      labs(title = "Tests Performed On Turtles",x=NULL,y="Number of Turtles Tested for Selected Test")
  })
  
  session$onSessionEnded(stopApp)
  }

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}

#Quantitative Data Apps (rought first idea, will adjust aes on Thursday-remember change color and add descriptions for measurements and source data like in the qualitative apps)


```r
ui <- dashboardPage(skin="green",
  dashboardHeader(title = "Turtle Measurements"),
  dashboardSidebar(disable = T),
  dashboardBody(
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
  ggplot(turtles3, aes_string(x = input$x, y = input$y)) + geom_point(alpha=0.8) +scale_fill_brewer(palette = "Set1")+ theme_light(base_size = 18)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title="Turtle Measurement Comparisons")
  })
  session$onSessionEnded(stopApp)
  }

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}

#this is where (if done) the interactive map will go



#after those we can include any additional graphs or charts that we want to include not covered in the apps






```r
green_turtles <- turtles3 %>%
  filter(species == "Green")
```

```r
loggerhead_turtles <- turtles3 %>%
  filter(species == "Loggerhead")
```

```r
kemps_ridley_turtles<- turtles3 %>%
  filter(species == "Kemps_Ridley")
```

```r
unknown_turtles<- turtles3 %>%
  filter(species == "UN")
```

```r
hawksbill_turtles <- turtles3 %>%
  filter(species == "Hawksbill")
```

```r
leatherback_turtles<- turtles3 %>%
  filter(species == "Leatherback")
```





```r
ui <- dashboardPage(skin="green",
  dashboardHeader(title = "Turtle Capture Locations"),
  dashboardSidebar(disable = T),
  dashboardBody(

fluidPage(
 
 titlePanel("Turtle Capture Locations"),
  
  

leafletOutput(outputId = "mymap"),
absolutePanel(top = 60, left = 20, 
      checkboxInput("green_point", "Green", FALSE),
      checkboxInput("leatherback_point", "Leatherback", FALSE),
      checkboxInput("loggerhead_point", "Loggerhead", FALSE),
      checkboxInput("kemps_ridley_point", "Kemps Ridley", FALSE),
      checkboxInput("unknown_point", "Unknown", FALSE),
      checkboxInput("hawksbillk_point", "Hawksbill", FALSE)
    )
)
)

)



server <- function(input, output, session) {
  
 pal1 <- colorFactor(topo.colors(7), turtles3$species)

   #create the map
  output$mymap <- renderLeaflet({
    addTiles() %>%
    leaflet(turtles3) %>% 
      setView(lng = -99, lat = 45, zoom = 3)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      
      addCircles(data = green_turtles, lat = ~ cap_latitude, lng = ~ cap_longitude, weight = 1, radius = 2, fillOpacity = 0.5, color = ~pal(turtles3$species), group = "Green") %>% 
    addCircles(data = leatherback_turtles, lat = ~ cap_latitude, lng = ~ cap_longitude, weight = 1, radius = 2, fillOpacity = 0.5, color = ~pal(turtles3$species), group = "Leatherback") %>% 
    addCircles(data = loggerhead_turtles, lat = ~ cap_latitude, lng = ~ cap_longitude, weight = 1, radius = 2, fillOpacity = 0.5, color = ~pal(turtles3$species), group = "Loggerhead") %>% 
    addCircles(data = kemps_ridley_turtles, lat = ~ cap_latitude, lng = ~ cap_longitude, weight = 1, radius = 2, fillOpacity = 0.5, color = ~pal(turtles3$species), group = "Kemps Ridley") %>% 
    addCircles(data = unknown_turtles, lat = ~ cap_latitude, lng = ~ cap_longitude, weight = 1, radius = 2, fillOpacity = 0.5, color = ~pal(turtles3$species), group = "Unknown")
    addCircles(data = hawksbill_turtles, lat = ~ cap_latitude, lng = ~ cap_longitude, weight = 1, radius = 2, fillOpacity = 0.5, color = ~pal(turtles3$species), group = "Hawksbill")   
      
        
  }) 

  

  
#next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
 
  
  observe({
    proxy <- leafletProxy("mymap", data = turtles3)
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal1(turtles3$species), fillOpacity = 0.2,
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
}
```










```r
turtles3%>%
  ggplot(aes(x=year,fill=year))+
  geom_bar(position = "dodge")+
  labs(title="Total Turtle Captures Across the Years",x="Year",y="Turtle Captures")+
  theme_light(base_size = 12)+
     theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-25-1.png)<!-- -->



```r
turtles3%>%
  ggplot(aes(month(x=date_capture_new,label=TRUE), fill=month(x=date_capture_new,label=TRUE)))+
  geom_bar(position = "dodge")+
  labs(title="Turtle Captures by Month",x = NULL,
         y = "Number of Captures",fill="Month")+
  scale_fill_brewer(palette="Set3")+
  theme_light(base_size = 12)+
     theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
turtles3%>%
  ggplot(aes(month(x=date_capture_new,label=TRUE),fill=month(x=date_capture_new,label=TRUE)))+
  geom_bar()+
  labs(title="Turtle Captures by Month",x = NULL,
         y = "Number of Captures",fill="Month")+
  scale_fill_brewer(palette="Set3")+
  theme_light(base_size = 12)+
     theme(axis.text.x = element_text(angle = 60, hjust = 1,size=6))+
  facet_wrap(~year)
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
turtles3$year<-as.factor(turtles3$year)
turtles3%>%
  group_by(species,year)%>%
  summarise(Total=n())%>%
  ggplot(aes(x=year,y=Total,group=species,color=species))+
  geom_line()+
  geom_point(shape=2)+
  scale_fill_brewer(palette = "Set3")+
  theme_light(base_size = 12)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "Turtle Captures By Species Across the Years",x="Year",y="Turtle Captures")
```

```
## `summarise()` has grouped output by 'species'. You can override using the `.groups` argument.
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-28-1.png)<!-- -->



```r
turtles3%>%
  ggplot(aes(x=day(date_capture_new)))+
  geom_density(color="black",fill="green",alpha=.5)+
  labs(title="Turtle Captures Within a Given Month",x = "Day of the Month",
         y = "Number of Captures")+
  theme_light(base_size = 12)+
     theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-29-1.png)<!-- -->


```r
turtles3%>%
  ggplot(aes(wday(x=date_capture_new,label=TRUE),fill=wday(x=date_capture_new,label=TRUE)))+
  geom_bar()+
  labs(title="Turtle Captures Across the Week Days",x = NULL,
         y = "Number of Captures",fill="Week Day")+
  scale_fill_brewer(palette = "Set3")+
  theme_light(base_size = 12)+
     theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](Final-Final-Presentation-Test_files/figure-html/unnamed-chunk-30-1.png)<!-- -->


