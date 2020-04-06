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
library(htmlwidgets)
library(lubridate)
library(rChartsCalmap) # install_github("ramnathv/rChartsCalmap")

# this file contains the function needed to process json from google 
source("bin/process-google-json.R") # this reads the json file and makes a tibble


ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 15, left = 15,
                width = "20%",
                fileInput("file", "",
                          multiple = FALSE,
                          accept = ".json")),
  
  absolutePanel(top = 15, right = 15,
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
  
  observe({
    validate(
      need(expr = !is.null(input$file), "Please upload a json file first")
    )
    mindate <- date( min(df()$time) )
    maxdate <- date( max(df()$time) )
    # update DateRangeInput with the min and max values from the uploaded file
    updateDateRangeInput(session, "daterange", 
                         start = mindate,
                         end = maxdate, 
                         min = mindate - 30,
                         max = maxdate + 30
                         )
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
   
   
  
   output$map <- renderLeaflet({
     
     filteredData() %>% leaflet() 
     })
  
  # Incremental changes to the map are performed in
  # an observer. In a later version, the tiles may be changed by user input, so that's why in separate observer
  observe({
    
   leafletProxy("map", data = filteredData()) %>%
      clearTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,options = tileOptions(opacity = input$opacity))
   
 })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
    fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
  })
  
  observe({
    leafletProxy("map", data = filteredData() ) %>%
    clearHeatmap() %>%
      addHeatmap(group = "filteredData",
                 lng = ~lng, lat = ~lat,
                 blur = input$blur,
                 max = 0.5,
                 radius = input$radius)
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
    
  })
  
  ### observer and modal for showInfo
  observeEvent(input$showInfo, {
    if(!is.null(input$file)) {
      # observations per day
      modaldf <- df() %>% mutate(date = date(time)) %>% 
        group_by(date) %>% 
        summarise(n = n()) %>% 
        mutate(year = year(date))
      
      modaldflist <- split(modaldf, modaldf$year)
      
      showModal(modalDialog(title = "Your data in numbers",
                            size = "l",
                            footer = tagList(actionButton("prev", "<<<"), 
                                             actionButton("next", ">>>"), 
                                             modalButton("Dismiss")),
                            HTML(
                              paste0("The location data has ", 
                                 "<b>", prettyNum( nrow(df()), big.mark = "," ) , "</b>",
                                 " entries <br> collected between ",
                                 "<b>", date(min(df()$time)), "</b>",
                                 " and ", 
                                 "<b>", date(max(df()$time)), "<hr/>")
                            
                            
                            ),
                            renderCalheatmap(
                              calheatmap(
                                x = 'date',
                                y = 'n',
                                data = modaldf,
                                domain = 'month',
                                start = min(modaldf$date),
                                itemName = 'datapoint',
                                previousSelector = '#prev',
                                nextSelector = '#next',
                                range = 12, 
                                tooltip = 'true', 
                                cellSize = 11, 
                                domainLabelFormat = "%b-%Y"
                              )
                            ),
                          easyClose = TRUE
                          )
                )
    } else {
      showModal(modalDialog(title = "Your data in numbers",
                            HTML("Still no data loaded..."), 
                            footer =tagList(actionButton("next", ">>"), actionButton("prev", "<<"))
                            
                            )
                )
    }
    
  })

}

shinyApp(ui, server)
