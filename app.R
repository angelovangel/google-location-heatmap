# app based on processing json files from google
library(shiny)
library(shinyjs)
#library(data.table)
library(tidyverse)
library(leaflet.extras)
library(lubridate)

source("R/process-google-json.R") #this reads the json file and makes a tibble


ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 10,
                fileInput("file", "",
                          multiple = FALSE,
                          accept = ".json")),
  absolutePanel(top = 10, right = 10,
                #draggable = TRUE,
                width = "15%",
                dateRangeInput("daterange", "Date range", 
                               start = today() - years(1),
                               end = today()
                               
                ),
          checkboxInput("showoptions", "Show map options", value = F),
              conditionalPanel(condition = "input.showoptions",
                sliderInput("opacity", "Map opacity", min = 0.1, max = 1, value = 0.5
                ),
                sliderInput("radius", "Radius", min = 1, max = 10, step = 1, value = 5
                ),
                sliderInput("blur", "Blur", min = 1, max = 20, value = 5
                ),
                
                checkboxInput("animatehistory", label = "Animate (one week per frame)", value = F),
                  conditionalPanel(condition = "input.animatehistory",
                          #radioButtons("step", "Animation step", choices = c("month", "day"), selected = "day", inline = T),
                          
                    sliderInput("animate", "", min = today() - years(1), max = today(), value = today() - years(1), 
                           timeFormat = "%F",
                           step = 7,
                           animate = animationOptions(interval = 1000)
                    )
                  )
              )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=1000*1024^2) 
  
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
   points <- reactive({
     
       filteredData() %>%                             
         dplyr::filter(as_date(time) <= input$animate & as_date(time) >= input$animate - 7)
   })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
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
# It uses points() if animation is selected and filteredData() otherwise 
  observe({
    updateSliderInput(session, "animate", min = input$daterange[1], max = input$daterange[2])
    ifelse(input$animatehistory,
    #yes
     leafletProxy("map", data = points()) %>%
       clearHeatmap() %>%
       addHeatmap(lng = ~lng, lat = ~lat,
                  blur = input$blur,
                  max = 0.5,
                  radius = input$radius),
    #   
    #no
    leafletProxy("map", data = filteredData()) %>%
      clearHeatmap() %>%
      addHeatmap(lng = ~lng, lat = ~lat,
                 blur = input$blur,
                 max = 0.5,
                 radius = input$radius)
    )
    })
    
  ### observer to update daterange
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

}

shinyApp(ui, server)
