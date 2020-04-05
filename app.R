# app based on processing json files from google

#check if required packages are there and suggest install if not
packages <- c("shiny", "shinyjs", "tidyverse", "lubridate", "RJSONIO", "devtools", "leaflet.extras")
lapply(packages, function(x) if(!require(x, character.only = TRUE)) install.packages(x))

# leaflet has to be devel version
if (packageVersion("leaflet") < "2.0.0.9000") {
  devtools::install_github("rstudio/leaflet")
}

library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet.extras)
library(lubridate)

# this file contains the function needed to process json from google 
source("bin/process-google-json.R") # this reads the json file and makes a tibble


ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 10,
                width = "20%",
                fileInput("file", "",
                          multiple = FALSE,
                          accept = ".json")),
  
  absolutePanel(top = 10, right = 10,
                #draggable = TRUE,
                width = "15%",
                actionButton("showInfo", label = "Show data info"),
                dateRangeInput("daterange", "", 
                               start = today() - years(1),
                               end = today()
                               
                ),
          checkboxInput("showoptions", "Show map options", value = F),
              conditionalPanel(condition = "input.showoptions",
                sliderInput("opacity", "Map opacity", min = 0.1, max = 1, value = 0.5
                ),
                sliderInput("radius", "Radius", min = 1, max = 10, step = 1, value = 3
                ),
                sliderInput("blur", "Blur", min = 1, max = 20, value = 4
                ),
                
                checkboxInput("animatehistory", label = "Animate", value = F),
                  conditionalPanel(condition = "input.animatehistory",
                          #radioButtons("step", "Animation step", choices = c("month", "day"), selected = "day", inline = T),
                          
                    sliderInput("animate", "", min = today() - years(1), max = today(), value = today() - years(1), 
                           timeFormat = "%F",
                           #step = 7,
                           animate = animationOptions(interval = 200)
                    )
                  )
              )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=1000*1024^2,
          shiny.launch.browser = TRUE) 
  # this stops the app when user closes browser window
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ### read and transform json
  df <- reactive({
    
    inFile <- input$file 
     if(is.null(inFile))
     {return(NULL)}  
    
    
    validate(
      need(expr = !is.null(input$file), "Please upload a json file first")
    )
    
    readjson(inFile$datapath)
    
  })

### Reactive expression for the data subsetted to the dates the user selected
   filteredData <- reactive({
     validate(
       need(expr = !is.null(input$file), "Please upload a json file first")
     )
     df() %>% 
       dplyr::filter(time >= input$daterange[1] & 
                       time <= input$daterange[2]) 
   })
   
   # reactives for animation, 7 days per frame
   points_new <- reactive({
     
       filteredData() %>%                             
         dplyr::filter(as_date(time) == input$animate)
   })
  
  output$map <- renderLeaflet({
    
    filteredData() %>% leaflet() %>% 
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
  })
  
  
  # Incremental changes to the map are performed in
  # an observer. In a later version, the tiles may be changed by user input, so that's why in separate observer
  observe({
    
   leafletProxy("map", data = filteredData()) %>%
      clearTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,options = tileOptions(opacity = input$opacity))
   
 })
  
  
# This observer updates the animation slider, and draws the heatmap.
# It uses points_new() if animation is selected and filteredData() otherwise 
  observe({
    updateSliderInput(session, "animate", min = input$daterange[1], max = input$daterange[2])
    ifelse(input$animatehistory,
    #yes
     leafletProxy("map") %>%
      #clearGroup(group = "newpoints") %>%
      clearGroup(group = "filteredData") %>%
      # addCircleMarkers(group = "newpoints", lng = ~lng, lat = ~lat,
      #                  stroke = FALSE,
      #                  opacity = 0.8,
      #                  fillOpacity = 0.8,
      #                  color = "red",
      #                  radius = 5,
      #                  data = points_new() %>% head(1)) %>%
      addHeatmap(lng = ~lng, lat = ~lat,
                 radius = input$radius,
                 blur = input$blur,
                 data = points_new()),
    #   
    #no
    leafletProxy("map", data = filteredData()) %>%
      clearHeatmap() %>%
      addHeatmap(group = "filteredData",
                 lng = ~lng, lat = ~lat,
                 blur = input$blur,
                 max = 0.5,
                 radius = input$radius)
    )
    })
    
  ### observer for notifications
  observe({
    validate(
      need(expr = !is.null(input$file), "Please upload a json file first")
    )
    
    # check if data contains something
    if(nrow(df()) >= 1) {
      # hide upload button
        shinyjs::hide("file")
      
      # show notifications
        showNotification("The data was read successfully. Initially, only the last year is shown.", 
                     type = "message",
                     duration = 6, 
                     closeButton = TRUE)
        showNotification(paste0("Your location data has ",nrow(df()) , " entries collected between ",
                            as_date(min(df()$time)), " and ", as_date(max(df()$time))),
                     type = "message",
                     duration = 8,
                     closeButton = TRUE)
    } else {
    showNotification("Something went wrong with your data", 
                     type = "error",
                     duration = NULL, # has to be closed by user
                     closeButton = TRUE)
    }
    
    # update DateRangeInput with the min and max values from the uploaded file
    updateDateRangeInput(session, "daterange", 
                         min = (min(df()$time) - days()) %>% floor_date(unit = "days"),
                         max = today() + days(30))
  })
  
  ### observer and modal for showInfo
  observeEvent(input$showInfo, {
    if(!is.null(input$file)) {
    showModal(modalDialog(title = "Your data in numbers",
                            HTML(
                            paste0("The location data has ", 
                                 "<b>", nrow(df()) , "</b>",
                                 " entries <br> collected between ",
                                 "<b>", as_date(min(df()$time)), "</b>",
                                 " and ", 
                                 "<b>", as_date(max(df()$time)), "<hr/>"
                                 )
                                ),
                          renderPlot(
                            df() %>% sample_frac(0.2) %>% ggplot() + 
                                  geom_freqpoly(aes(time), color = "red", bins = 50, size = 1) + 
                                  labs(title = "Frequency of location data points over time") + 
                                  xlab("") +
                                  theme_bw(),
                              
                          res = 100),
                          easyClose = TRUE
                          ))
    } else {
      showModal(modalDialog(title = "Your data in numbers",
                            HTML("Still no data loaded...")
                            )
                )
    }
    
  })

}

shinyApp(ui, server)
