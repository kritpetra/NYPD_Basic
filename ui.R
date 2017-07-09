shinyUI(dashboardPage(
  dashboardHeader(title = "Stop-and-Frisk in New York City"),
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
                     draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                     width = 330, height = "auto",
                     
                     h3("Map Options"), 
                     selectInput("colorby","Color Precincts by:",
                                 choices = c("Number of arrests" = "number",
                                             "Proportion of arrests" = "proportion",
                                             "Overall population" = "population")),
                     selectInput("race", "Race", 
                                 choices = c("Overall" = "O",
                                             "White" = "W",
                                             "African-American" = "B",
                                             "Hispanic" = "H",
                                             "Asian/Pacific islander" = "A",
                                             "Native American" = "N")),
                     selectizeInput('removePrecincts', "Remove precincts", multiple = TRUE,
                                    choices=arrestData$Precinct, selected = 22),
                     selectizeInput('filterPrecincts', "Filter precincts", multiple = TRUE,
                                    choices = c("Show all", arrestData$Precinct),
                                    selected = "Show all",
                                    options = list(maxItems = 5)),
                     radioButtons('scale', "Scale", choices = c('Linear', 'Logarithmic'), inline = TRUE,
                                  selected = "Logarithmic")
          ),
          
          fixedPanel(id = "charts", class = "panel panel-default", draggable = FALSE, 
                     top = "auto", left = "auto", right = 20, bottom = 10, 
                     width = 550, height = "auto",
                     
                     # htmlOutput("precinctOverInfo"),
                     div(align = "center",
                       # plotlyOutput("populationGraph", height = 200),
                       htmlOutput("htmlGraph", height = 200, width = '80%')
                       # plotOutput("arrestsGraph"),
                       # plotOutput("graph_perc", width = "80%", height = 300)
                     )
          )
      )
    )
  )
))
