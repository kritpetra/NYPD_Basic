shinyUI(dashboardPage(
  dashboardHeader(title = "A Look at Stop-and-Frisk in New York City, 2005"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    tags$head(
      tags$title("Stop-and-Frisk in New York City"),
      tags$style(includeCSS('./style.css'))
      ),
    
    bootstrapPage(
      div(class='leaflet-container',
          leafletOutput('nycMap', height = '100%', width = '100%'),
          fixedPanel(id = "options", class = "panel panel-default",
                     draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                     width = 330, height = "auto",
                     
                     h3("Map Options"), 
                     selectInput("colorby","Color Precincts by:",
                                 choices = c("Number of arrests" = "number",
                                             "Proportion of arrests, compared to residents" = "proportion",
                                             "Number of residents" = "population")),
                     selectInput("race", "Race", 
                                 choices = c("Overall" = "O",
                                             "White" = "W",
                                             "African-American" = "B",
                                             "Hispanic" = "H",
                                             "Asian/Pacific islander" = "A",
                                             "Native American" = "N")),
                     selectizeInput('removePrecincts', "Filter out precincts", multiple = TRUE,
                                    choices=arrestData$Precinct, selected = 22),
                     radioButtons('scale', "Scale", choices = c('Linear', 'Logarithmic'), inline = TRUE,
                                  selected = "Logarithmic")
          )
      )
    )
  )
))
