ALL_PRECINCTS <- precincts1$Precinct

createTextRow <- function(label, count, total){
  sprintf("&emsp; <small><b>%s:</b> %s (%s%%)</small>",
          label, count, round(count/total *100, 2))
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
  
  #    Initialize information box to display default text
  output$precinctOverInfo <- renderText({"<center><h4>Hover over a precinct for more information.</h4></center>"})
  
  # Determines the visible precincts based on both selectize inputs
  allowedPrecincts <- reactive({
    show <- if (input$filterPrecincts[1] == "Show all") 
              ALL_PRECINCTS 
            else input$filterPrecincts
    hide <- input$removePrecincts
    return(setdiff(show, hide))
  })
  
  # Filter shapes to only show visible precincts
  filteredPrecincts <- reactive({
    precincts1[(precincts1$Precinct %in% allowedPrecincts()), ]
  })
  
  # Same as above, but for data
  filteredData <- reactive({
    arrestData[(arrestData$Precinct %in% allowedPrecincts()), ]
  })
  
  #    Define color vector, according to user input
  countryVar <- reactive({
    data <- switch(input$colorby,
             'race_dist' = switch(as.character(input$race),
                                  "W" = filteredData()$White,
                                  "B" = filteredData()$Black,
                                  "H" = filteredData()$Hisp,
                                  "A" = filteredData()$AsPac,
                                  "N" = filteredData()$Native),
             'race_arr' = switch(as.character(input$race),
                                 "W" = filteredData()$WhiteA,
                                 "B" = filteredData()$BlackA,
                                 "H" = filteredData()$HispA,
                                 "A" = filteredData()$AsPacA,
                                 "N" = filteredData()$NativeA),
             'race_weighted' = switch(as.character(input$race),
                                      "W" = filteredData()$WhiteA/filteredData()$White*1000,
                                      "B" = filteredData()$BlackA/filteredData()$Black*1000,
                                      "H" = filteredData()$HispA/filteredData()$Hisp*1000,
                                      "A" = filteredData()$AsPacA/filteredData()$AsPac*1000,
                                      "N" = filteredData()$NativeA/filteredData()$Native*1000),
             'arrests_weighted' = filteredData()$TotalA/filteredData()$Population*1000,
             'arrests_raw' = filteredData()$TotalA,
             stop('invalid colorby option') # default: if no match is found
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

  
  
  #      #    Updates "Filter Precincts" so that removed precincts do not show up in the
  #      #    select menu and crash the app - NOT WORKING PROPERLY
  #      observeEvent(input$removePrecincts, {
  #           print(allowedPrecincts())
  #
  #           updateSelectizeInput(session, 'filterPrecincts',
  #                                choices = c("Show all", arrestData$Precinct) %>%
  #                                     setdiff(input$removePrecincts))
  #      })
  #      observeEvent(input$filterPrecincts, {
  #           updateSelectizeInput(session, 'removePrecincts',
  #                                choices = c(arrestData$Precinct) %>%
  #                                     setdiff(input$filterPrecincts))
  #      })
  
  #    Prevents users from picking "Show all" with other precincts
  observe({
    if ( "Show all" %in% input$filterPrecincts && length(input$filterPrecincts) > 1) {
      selected <- if( input$filterPrecincts[1] == "Show all" ) 
                    input$filterPrecincts %>% setdiff( "Show all" )
                  else "Show all"
      updateSelectizeInput(session, inputId = "filterPrecincts", selected = selected)
    }
  })
  
  #    Mouseover events: highlights precinct and prints information
  observe({
    eventOver <- input$nycMap_shape_mouseover
    
    if(!is.numeric(eventOver$id)) {
      return()
    }
    
    #         Precinct information:
    precinctOver <- precincts1[precincts1$Precinct==eventOver$id,]
    precinctOverData <- arrestData[arrestData$Precinct==eventOver$id,]
    
    #         Highlights precinct:
    MapProxy %>% addPolygons(data = precinctOver, group = 'highlighted', 
                             color="white", fill = FALSE)
    
    #         Prints precinct information ----
    output$precinctOverInfo <- renderText({
      
      paste(
        sprintf(
          "<b><center><h2>Precinct %s</h2></center></b></br>
                        <b>Area:</b> %s square miles</br>
                        <table style='width:100%%'><tr><td><b>Population:</b> %s (2010 census)",
          precinctOverData$Precinct, round(precinctOverData$Area, 2), precinctOverData$Population),
        createTextRow("White", precinctOverData$White, precinctOverData$Population),
        createTextRow("Afr.-American", precinctOverData$Black, precinctOverData$Population),
        createTextRow("Hispanic", precinctOverData$Hisp, precinctOverData$Population),
        createTextRow("Asian/Pac. Islndr", precinctOverData$AsPac, precinctOverData$Population),
        createTextRow("Native American", precinctOverData$Native, precinctOverData$Population),
        
        sprintf("<td><b>Total number of arrests:</b> %s", precinctOverData$TotalA),
        createTextRow("White", precinctOverData$WhiteA, precinctOverData$TotalA),
        createTextRow("Afr.-American", precinctOverData$BlackA, precinctOverData$TotalA),
        createTextRow("Hispanic", precinctOverData$HispA, precinctOverData$TotalA),
        createTextRow("Asian/Pac. Islndr", precinctOverData$AsPacA, precinctOverData$TotalA),
        createTextRow("Native American", precinctOverData$NativeA, precinctOverData$TotalA),
        sep="<br/>")
    })
    
    output$graph_perc <- renderPlot({
      
      precinct_pop <- as.numeric(precinctOverData[5:9])
      precinct_arr <- as.numeric(precinctOverData[10:14])
      graph_max <- max(precinct_arr/precinct_pop, na.rm=TRUE)+0.01
      
      barplot(
        rbind(
          precinct_arr/precinct_pop, rep(graph_max,5)-precinct_arr/precinct_pop
        ), # Arrested in precinct/Living in precinct
        
        main = "Proportion of Population Arrested, by Race",
        names.arg = c("White", "Afrn Am.", "Natv Am.", "Asian", "Hispanic"),
        ylim = c(0,graph_max),
        xlab = "Race", ylab = "Percent in Precinct")
      mtext("(Number of arrests divided by number living in precinct)", padj = -0.6)
    })
    
  })
  
  #    Mouseout events: stop displaying information
  observeEvent(input$nycMap_shape_mouseout$id, {
    if( input$nycMap_shape_mouseout$id  %>% is.null %>% not) {
      MapProxy %>% clearGroup( 'highlighted' )
      output$precinctOverInfo <- renderText("<center><h4>Hover over a precinct for more information.</h4></center>")
      output$graph_perc <- renderPlot(NULL)
    }
  })
  
  #    Rerender the map whenever an input changes
  observeEvent({input$colorby; input$race; input$removePrecincts; input$scale; input$filterPrecincts}, {
    MapProxy %>%
      clearControls() %>%
      clearShapes() %>% 
      addPolygons(data = filteredPrecincts(),
                  layerId = ~Precinct,
                  color = getColor(countryVar()),
                  weight = 2, fillOpacity = .6) %>%
      addLegend('bottomleft',
                title = switch(input$colorby,
                               'arrests_weighted' = ,
                               'race_weighted' = 'Arrests per 1000 people',
                               'race_dist' = 'Population',
                               'arrests_weighted' = ,
                               'arrests_raw' = 'Arrests'),
                pal = mapPalette, 
                values = countryVar(), 
                opacity = 0.7,
                labFormat = labelFormat(transform = if(input$scale == 'Logarithmic') 
                                                      exp_minus_one 
                                                    else identity))
    
    # Stroke focused precincts
    if ( input$filterPrecincts[1]  != "Show all" ) {
      MapProxy %>% addPolygons(data = filteredPrecincts(),
                               color = 'red', weight = 2,
                               fill = FALSE, opacity = 1)
    }
  })
  
  
})
