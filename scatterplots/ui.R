require(shiny)
require(dplyr)
require(ggplot2)
require(DT)
require(shinythemes)
require(ggthemes)

# The UI needs access to schools because it is used as options for dropdowns
apps <-
  read.csv(file.path(getwd(), "4.14 scatdat.csv"),
           stringsAsFactors = FALSE,
           sep = ",")
schools <- unique(apps$University.Name)
o <- order(schools)
schools <- schools[o]
high_schools <- unique(apps$High.School)

# Start defining the UI

shinyUI(
  navbarPage(
    "ESC College Application Navigator",
    selected = "Scatterplots",
    tags$head(tags$style(
      HTML("hr {border-top: 1.5px solid #000000;}")
			),
	tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript")
			),
    theme = shinytheme('flatly'),
    tabPanel("Scatterplots",
             sidebarLayout(
               sidebarPanel(width = 4,
                            fluidRow(
                              column(
                                width = 6,
                                selectizeInput(
                                  "schoolfilter",
                                  "Primary College to Display",
                                  choices = schools,
                                  multiple = FALSE,
                                  selected = "American University" #This default could be set to any school, or turned off
                                ),
                                
                                uiOutput("compareSelector"),
                                
                                hr(),
                                
                                checkboxGroupInput(
                                  "app.region",
                                  label = "Applicant Pools",
                                  choices = list(
                                    "Beijing/Tianjin" = 1,
                                    "Other China" = 2,
                                    "International Boarding School" = 3
                                  ),
                                  selected = c(1, 2)
                                ),
                                
                                hr(),
                                
                                checkboxGroupInput(
                                  "app.year",
                                  label = "Application Years",
                                  choices = list(
                                    "2013" = 1,
                                    "2014" = 2,
                                    "2015" = 3,
                                    "2016" = 4,
                                    "2017" = 5,
                                    "2018" = 6
                                  ),
                                  selected = c(5, 6)
                                ),
                                
                                hr(),
                                
                                selectizeInput(
                                  "hsfilter",
                                  "High School Filter",
                                  choices = high_schools,
                                  multiple = TRUE,
                                  selected = NULL
                                ),
                                
                                hr(),
                                
                                checkboxInput("independent",
                                              label = "Include Independent Apps?",
                                              value = TRUE)
                              ),
                              
                              column(
                                width = 6,
                                
                                radioButtons(
                                  "testSelect",
                                  "ACT or RSAT",
                                  choices = list("ACT" = "ACT",
                                                 "RSAT" = "RSAT"),
                                  selected = "RSAT"
                                ),
                                
                                hr(),
                                
                                conditionalPanel(
                                  condition = "input.testSelect =='RSAT'",
                                  sliderInput(
                                    "RSAT.range",
                                    "RSAT Filter:",
                                    min = 1300,
                                    max = 1600,
                                    value = c(1300, 1600),
                                    step = 10
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.testSelect =='ACT'",
                                  sliderInput(
                                    "ACT.range",
                                    "ACT Filter:",
                                    min = 27,
                                    max = 36,
                                    value = c(27, 36),
                                    step = 1
                                  )
                                ),
                                
                                sliderInput(
                                  "TOEFL.range",
                                  "TOEFL Filter:",
                                  min = 90,
                                  max = 120,
                                  value = c(90, 120)
                                ),
                                
                                hr(),
                                
                                sliderInput(
                                  "yourRSAT",
                                  "Your RSAT",
                                  min = 1300,
                                  max = 1600,
                                  value = 1450,
                                  step = 10
                                ),
                                
                                sliderInput(
                                  "yourTOEFL",
                                  "Your TOEFL",
                                  min = 90,
                                  max = 120,
                                  value = 100,
                                  step = 1
                                )
                              )
                            )),
               
               mainPanel(
                 width = 8,
                 tabsetPanel(
                   type = "tabs",
                   id ="scatterplotTab",
                   tabPanel(
                     "Selectivity Viewer",
                     fluidRow(
                       plotOutput(
                         "plot2",
                         height = 600,
                         hover = hoverOpts(id = "plot_hover"),
                         brush = brushOpts(id = "plot_brush_2",
                                           resetOnNew = TRUE)
                       )
                     ),
                     fluidRow(
                       column(6,
                              h3("Filtered Admit Rate"),
                              DT::dataTableOutput("rawhistory")),
                       column(6,
                              h3("Selected Admit Rate"),
                              DT::dataTableOutput("brush_info"))
                     ),
					 fluidRow(
							  HTML('<div data-iframe-height></div>')	
                   )),
                   tabPanel(
                     "Side by Side",
                     value = "sideBySide",
                     fluidRow(plotOutput("plot3",
                                         height = 350)),
                     fluidRow(plotOutput("plot4",
                                         height = 350)),
                     fluidRow(
                       column(width = 6,
                              h3(textOutput("rawhistoryComp1Label")),
                              DT::dataTableOutput("rawhistoryComp1")
                              ),
                       column(width = 6,
                              h3(textOutput("rawhistoryComp2Label")),
                              DT::dataTableOutput("rawhistoryComp2")
                              )),
					 fluidRow(
							  HTML('<div data-iframe-height></div>')	
                   )
                   ),
                   tabPanel(
                     "Closer Look",
                     fluidRow(
                       plotOutput(
                         "plot1",
                         height = 600,
                         click = clickOpts(id = "plot_click"),
                         dblclick = dblclickOpts(id = "plot_dblclick"),
                         brush = brushOpts(id = "plot_brush",
                                           resetOnNew = TRUE)
                       )
                     ),
                     fluidRow(DT::dataTableOutput("click_info")),
					 fluidRow(
							  HTML('<div data-iframe-height></div>')	
                   )
                   ),
                   tabPanel("Year by Year",
					 fluidRow(
                        plotOutput("plot5",
                                   height = 1600)),
					 fluidRow(
							  HTML('<div data-iframe-height></div>')	
                   )
                 )
               )
             ))),
    tabPanel(
      "School Profiles and Data Tables",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          helpText(
            "Select a university and set the filters to examine ESC's applicant data and school profiles"
          ),
          selectizeInput(
            "schoolfilter1",
            "Select College to Display",
            choices = schools,
            multiple = FALSE,
            selected = "University of Chicago" #This default could be set to any school, or turned off
          ),
          
          sliderInput(
            "RSAT.range1",
            "RSAT Filter:",
            min = 1300,
            max = 1600,
            value = c(1300, 1600),
            step = 10
          ),
          
          sliderInput(
            "TOEFL.range1",
            "TOEFL Filter:",
            min = 90,
            max = 120,
            value = c(90, 120)
          ),
          
          selectizeInput(
            "hsfilter1",
            "High School Filter",
            choices = high_schools,
            multiple = TRUE,
            selected = NULL
          ),
          
          checkboxGroupInput(
            "app.region1",
            label = "Applicant Pools",
            choices = list(
              "Beijing/Tianjin" = 1,
              "Other China" = 2,
              "International Boarding School" = 3
            ),
            selected = c(1, 2, 3)
          ),
          
          checkboxGroupInput(
            "app.year1",
            label = "Application Years",
            choices = list(
              "2013" = 1,
              "2014" = 2,
              "2015" = 3,
              "2016" = 4,
              "2017" = 5,
              "2018" = 6
            ),
            selected = c(1, 2, 3, 4, 5, 6)
          ),
          checkboxInput("independent1",
                        label = "Include Independent Apps?",
                        value = TRUE)
        ),
        mainPanel(width = 9,
                  tabsetPanel(
                    tabPanel("School Profiles",
                             fluidRow(
                               column(
                                 width = 6,
                                 offset = 1,
                                 h4("Historical Admissions Rate"),
                                 DT::dataTableOutput("rawhistory1")
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 6,
                                 offset = 1,
                                 h4("Admitted Student Profile"),
                                 DT::dataTableOutput("admitprofile1"),
                                 verbatimTextOutput("hsfilter1")
                               )
                             ),
							 fluidRow(
								HTML('<div data-iframe-height></div>')	
                   )),
                    tabPanel("Applicant Data Tables",
							 fluidRow(
                              DT::dataTableOutput("tbl")
							 ),
							 fluidRow(
							  HTML('<div data-iframe-height></div>')	
							 )
							)							 
							 
                  ))
      )
    )
  )
)