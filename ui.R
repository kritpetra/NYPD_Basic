shinyUI(dashboardPage(
     dashboardHeader(title = "Stop-and-Frisk in New York City"),
     dashboardSidebar(disable = TRUE),
     dashboardBody(

               tags$head(tags$style(HTML('

                         /* Sidebar font size */
                              .sidebar-menu>li>a {
                                   font-size:16px;
                              }
                         /* Box title font size */
                              .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
                                     font-size: 20px;
                              }
                         /* Overall font size */
                              body {
                              font-size: 16px;
                              }
                              small {
                              font-size: 14px;
                              }
                         /* Table properties */
                              td {
                                  padding-left: 15px;
                                  padding-right: 15px;
                                   vertical-align: middle;
                              }
                         /* Expand and center title */
                              .main-header .logo {
                                   float:inherit;
                                   width:inherit;
                              }
                              .main-header .navbar {
                                   display: none;
                              }

                             '))),

          title = "NYPD Precincts", skin="blue",

                                fluidPage(fluidRow(
                                     column(7,
                                            box(title = "Map of Police Precincts", solidHeader=T, status="primary", width = '100%',
                                                div(leafletOutput("nycMap", height = 450)),
                                                div(htmlOutput("footer"), align = "right")
                                            ),

                                            box(title="Map Options", status = "primary", solidHeader=T, collapsible=T, width = '100%',

                                                div(style = 'display: flex',
                                                    div(style = 'flex: 2',
                                                        selectInput("colorby","Color Precincts by:",choices=c(
                                                             "Total number of arrests, weighted by population" = "arrests_weighted",
                                                             "Total number of arrests" = "arrests_raw",
                                                             "Number of arrests by race" = "race_arr",
                                                             "Number of arrests by race, weighted by population" = "race_weighted",
                                                             "Racial distribution of city" = "race_dist"))),

                                                    div(style = 'flex: 1',
                                                        conditionalPanel(condition = "input.colorby == 'race_dist' ||
                                                                         input.colorby == 'race_arr' || input.colorby == 'race_weighted'",
                                                                         selectInput("race", "Race", choices = c(
                                                                              "White" = "W",
                                                                              "African-American" = "B",
                                                                              "Hispanic" = "H",
                                                                              "Asian/Pacific islander" = "A",
                                                                              "Native American" = "N"))))
                                                ),

                                                div(style = 'display: flex',
                                                    div(style = 'flex: 1',
                                                         selectizeInput('removePrecincts', "Remove precincts", multiple = TRUE,
                                                                   choices=arrestData$Precinct, selected = c(22, 50))),
                                                    div(style = 'flex: 1',
                                                        selectizeInput('filterPrecincts', "Filter precincts", multiple = TRUE,
                                                                       choices = c("Show all", arrestData$Precinct),
                                                                       selected = "Show all",
                                                                       options = list(maxItems = 5)))),

                                                        radioButtons('scale', "Scale", choices = c('Linear', 'Logarithmic'), inline = TRUE,
                                                                     selected = "Logarithmic")

                                     )),
                                     column(5,

                                            box(title="Precinct Information", status = "primary", solidHeader=T, width = '100%',
                                                htmlOutput("precinctOverInfo"),
                                                div(plotOutput("graph_perc", width = "80%", height = 300), align = "center")
                                            )

                                     ))
                        )
     )
))
