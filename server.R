createTextRow <- function(label, count, total){
  sprintf("&emsp; <small><b>%s:</b> %s (%s%%)</small>",
          label, count, round(count/total *100, 2))
}

createMapLabels <- function(data) {
  
  lapply(sprintf(includeHTML('./tooltipContents.html'),
      data$Precinct,
      data$Population,
      data$TotalA,
      data$White/data$Population*100, # TODO: repeated arguments!
      data$Black/data$Population*100,
      data$White/data$Population*100,
      data$Black/data$Population*100,
      data$WhiteA/data$TotalA*100, 
      data$BlackA/data$TotalA*100,
      data$WhiteA/data$TotalA*100,
      data$BlackA/data$TotalA*100,
      data$BlackA/data$Black*data$White/data$WhiteA
      
    ), HTML)
}


shinyServer(function(input, output, session) {
  
  #    Render base map ----
  output$nycMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-74.004, 40.705, zoom = 10) %>%
      setMaxBounds(-73.000, 40.200, -75.000, 41.100)
  })
  
  #    Create instance of base map where polygons can be added
  MapProxy <- leafletProxy('nycMap')
  
  # Filter shapes to only show visible precincts
  filteredPrecincts <- reactive({
    precincts1[!(precincts1$Precinct %in% input$removePrecincts), ]
  })
  
  # Same as above, but for data
  filteredData <- reactive({
    arrestData[!(arrestData$Precinct %in% input$removePrecincts), ]
  })
  
  #    Define color vector, according to user input
  countryVar <- reactive({
    data <- switch(input$colorby,
                   'population' = switch(as.character(input$race),
                                         "O" = filteredData()$Population,
                                         "W" = filteredData()$White,
                                         "B" = filteredData()$Black,
                                         "H" = filteredData()$Hisp,
                                         "A" = filteredData()$AsPac,
                                         "N" = filteredData()$Native),
                   'number' = switch(as.character(input$race),
                                     "O" = filteredData()$TotalA,
                                     "W" = filteredData()$WhiteA,
                                     "B" = filteredData()$BlackA,
                                     "H" = filteredData()$HispA,
                                     "A" = filteredData()$AsPacA,
                                     "N" = filteredData()$NativeA),
                   'proportion' = switch(as.character(input$race),
                                         "O" = filteredData()$TotalA/filteredData()$Population*1000,
                                         "W" = filteredData()$WhiteA/filteredData()$White*1000,
                                         "B" = filteredData()$BlackA/filteredData()$Black*1000,
                                         "H" = filteredData()$HispA/filteredData()$Hisp*1000,
                                         "A" = filteredData()$AsPacA/filteredData()$AsPac*1000,
                                         "N" = filteredData()$NativeA/filteredData()$Native*1000),
                   stop('Invalid input value: input$colorby =', input$colorby) # default: if no match is found
    )
    
    if (input$scale == 'Logarithmic') { 
      data <- log(data+1)
    }
    
    return(data)
  })
  
  # TODO: remove global assignment
  getColor <- function(values){
    lower <- min(values)
    upper <- max(values)
    mapPalette <<- colorNumeric(rev(heat.colors(10)), c(lower, upper), 10)
    mapPalette(values)
  }
  
  #    Rerender the map whenever an input changes
  observe({
    MapProxy %>%
      clearControls() %>%
      clearShapes() %>% 
      addPolygons(data = filteredPrecincts(),
                  layerId = ~Precinct,
                  color = getColor(countryVar()),
                  weight = 2, fillOpacity = .6, 
                  label = createMapLabels(filteredData()),
                  labelOptions = labelOptions(clickable = FALSE, 
                                              className = "label-box", 
                                              textsize = 16,
                                              textOnly = TRUE,
                                              style=list(
                                                'background'='rgba(255,255,255,0.95)',
                                                'border-color' = 'rgba(0,0,0,1)',
                                                'border-radius' = '4px',
                                                'border-style' = 'solid',
                                                'border-width' = '2px',
                                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)")),
                  highlight = highlightOptions(weight = 5,
                                               color = "#FFF",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE)) %>%
      addLegend('bottomright',
                title = switch(input$colorby,
                               'number' = 'Arrests',
                               'proportion' = 'Arrests per 1000 people',
                               'population' = 'Population',
                               stop("Invalid option in legend: input$colorby=", input$colorby)),
                pal = mapPalette, 
                values = countryVar(), 
                opacity = 0.7,
                labFormat = labelFormat(transform = if(input$scale == 'Logarithmic') 
                  exp_minus_one 
                  else identity))
    
  })
  
  
})
