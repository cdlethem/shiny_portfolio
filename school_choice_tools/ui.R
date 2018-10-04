require(shiny)
require(shinythemes)
require(ggthemes)
require(dplyr)
require(ggplot2)
require(DT)

# The UI needs access to schools because it is used as options for dropdowns
apps <-
  read.csv(file.path(getwd(), "4.14 scatdat.csv"),
           stringsAsFactors = FALSE,
           sep = ",")
schools <- unique(apps$University.Name)
o <- order(schools)
schools <- schools[o]
high_schools <- unique(apps$High.School)

shinyUI(
  navbarPage(
    "ESC School Choice Tools",
    theme = shinytheme("flatly"),
    selected = "School List Explorer",
    tags$head(tags$style(
      HTML("hr {border-top: 3px solid #000000;}")
    ),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")),
    tabPanel(
      "School List Explorer",
      sidebarLayout(
        sidebarPanel(
          width = 5,
          fluidRow(
            column(
              width = 6,
              helpText(
                "Use the controls to filter student ID's that match your criteria. Select a student ID to display that student's school list"
              ),
              
              checkboxGroupInput(
                "app.region.s",
                label = h4("Applicant Pools"),
                choices = list(
                  "Beijing/Tianjin" = 1,
                  "Other China" = 2,
                  "International Boarding School" = 3
                ),
                selected = c(1, 2, 3)
              ),
              
              checkboxGroupInput(
                "app.year.s",
                label = h4("Application Years"),
                choices = list(
                  "2013" = 1,
                  "2014" = 2,
                  "2015" = 3,
                  "2016" = 4,
                  "2017" = 5,
                  "2018" = 6
                ),
                selected = c(4:6)
              ),
              
              radioButtons(
                "includeED",
                h4("Include Students Accepted in ED/ED2/REA"),
                choices = list(
                  "All Students" = 1,
                  "RD Students Only" = 2,
                  "ED/ED2/REA Students Only" = 3
                ),
                selected = 2
              ),
              
              selectizeInput(
                "EDSchoolFilter",
                h4("Filter by ED Choice"),
                choices = schools,
                multiple = TRUE
              )
            ),
            
            column(
              width = 6,
              
              uiOutput("idselector"),
              
              hr(),
              h4("Toggle Test Filters"),
              helpText("Choose whether or not to apply test score filters to the ID choices being displayed. Turning this off will show more results when filtering by ED Choice"),
              radioButtons(
                "testToggle",
                label = NULL,
                choices = list(
                  "Enable Test Score Filters" = 1,
                  "Disable Test Score Filters" = 2),
                selected = 1
                ),
              hr(),
              
              radioButtons(
                "testSelect",
                h4("ACT or RSAT"),
                choices = list("ACT" = "ACT",
                               "RSAT" = "RSAT"),
                selected = "RSAT"
              ),
              
              conditionalPanel(
                condition = "input.testSelect =='RSAT'",
                sliderInput(
                  "RSAT.range.s",
                  h4("RSAT Filter:"),
                  min = 1200,
                  max = 1600,
                  value = c(1490,1520),
                  step = 10
                )
              ),
              
              conditionalPanel(
                condition = "input.testSelect =='ACT'",
                sliderInput(
                  "ACT.range.s",
                  h4("ACT Filter:"),
                  min = 27,
                  max = 36,
                  value = c(30,31),
                  step = 1
                )
              ),
              
              sliderInput(
                "TOEFL.range.s",
                h4("TOEFL Filter:"),
                min = 80,
                max = 120,
                value = c(107,108)
              )
            )
          ),
          fluidRow(
            hr(),
            h3("Applicant Information"),
            DT::dataTableOutput("Info")
          )
        ),
        mainPanel(width = 7,
                  fluidRow(
                    column(
                      width = 12,
                      h3("Score Profile"),
                      DT::dataTableOutput("Score.Profile"),
                      h3("Application List"),
                      DT::dataTableOutput("Applist")
                    )),
                  fluidRow(
                    column(
                      width = 12,
                      HTML('<div data-iframe-height></div>')
                    )
                  )
        )		
      )
    ),
    tabPanel(
      "You May Also Like...",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput(
            "correlationschool",
            "Select a college to compare",
            choices = schools,
            multiple = FALSE,
            selected = "Bates College"
          ),
          numericInput(
            "schoolsnum",
            "Select the number of colleges to display",
            value = 20,
            min = 10,
            max = 40
          ),
          radioButtons(
            "normalizelogical",
            label = h4("Normalize?"),
            list("Total Overlap" = 1, "Normalized Overlap" = 2),
            selected = 2
          ),
          helpText(
            "Selecting Total Overlap will show the raw number of applicants in common, which gives results that are biased toward the schools that ESC students apply to most often.
            Selecting Normalized Overlap will adjust for this bias but may be skewed by small sample sizes and introduce bias toward schools with relatively few applicants."
          )
          
          ),
        mainPanel(width = 9,
                  fluidRow(
                    uiOutput("overlappui")),
                  fluidRow(
                    HTML('<div data-iframe-height></div>')))
      )
      ),
    tabPanel("Risk Evaluator",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 sliderInput(
                   "yourRSAT.re",
                   "Your RSAT",
                   min = 1300,
                   max = 1600,
                   value = 1480,
                   step = 10
                 ),
                 
                 sliderInput(
                   "yourTOEFL.re",
                   "Your TOEFL",
                   min = 90,
                   max = 120,
                   value = 108,
                   step = 1
                 ),
                 
                 checkboxGroupInput(
                   "risk.selector",
                   label = h2("Choose Risk Levels to Display"),
                   choices = list(
                     "Low Risk" = 1,
                     "Medium-Low Risk" = 2,
                     "Medium Risk" = 3,
                     "Medium-High Risk" = 4,
                     "High Risk" = 5,
                     "Fringe" = 6,
                     "Out of Range" = 7
                   ),
                   selected = c(1, 2, 3)
                 )
               ),
               
               mainPanel(width = 9,
                         fluidRow(
                           column(
                             width = 12,
                             h3("Matching Schools"),
                             DT::dataTableOutput("riskevaltbl")
                           )
                         ),
                         fluidRow(
                           column(width = 12,
                                  HTML('<div data-iframe-height></div>')
                           )
                         )
               )
             )
    )
)
)
