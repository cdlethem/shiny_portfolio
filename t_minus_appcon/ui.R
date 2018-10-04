library(shiny)
library(shinythemes)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)

shinyUI(navbarPage(
  "T Minus Appcon",
  theme = shinytheme("flatly"),
  selected = "View by Score Range",
  tags$head(
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript")
      ),
  tabPanel("View by Score Range",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               div(
                 radioButtons(
                   "testSelect",
                   "Select Test to Display",
                   choices = list(
                     "ACT" = "ACT",
                     "ACT R" = "ACT R",
                     "RSAT" = "RSAT",
                     "RSAT CR" = "RSAT CR",
                     "TOEFL" = "TOEFL",
                     "TOEFL S" = "TOEFL S"
                   ),
                   selected = "TOEFL"
                 )
               ),
               
               div(h3("Filters")),
               
               div(
                 sliderInput(
                   "tValue",
                   "T Value (Number of Days Before Appcon)",
                   min = 0,
                   max = 1095,
                   step = 5,
                   value = 180
                 ),
                 
                 conditionalPanel(
                   condition = "input.testSelect == 'ACT'",
                   sliderInput(
                     "ACTRange",
                     "ACT Filter",
                     min = 20,
                     max = 36,
                     value = c(29, 31),
                     step = 1
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.testSelect == 'ACT R'",
                   sliderInput(
                     "ACTRRange",
                     "ACT R Filter",
                     min = 12,
                     max = 36,
                     value = c(26, 28),
                     step = 1
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.testSelect =='RSAT'",
                   sliderInput(
                     "RSATRange",
                     "RSAT Filter",
                     min = 1200,
                     max = 1600,
                     value = c(1380, 1420),
                     step = 10
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.testSelect =='RSAT CR'",
                   sliderInput(
                     "RSATCRRange",
                     "RSAT CR Filter",
                     min = 400,
                     max = 800,
                     value = c(580, 620),
                     step = 10
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.testSelect == 'TOEFL'",
                   sliderInput(
                     "TOEFLRange",
                     "TOEFL Filter:",
                     min = 80,
                     max = 120,
                     value = c(97, 100)
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.testSelect == 'TOEFL S'",
                   sliderInput(
                     "TOEFLSRange",
                     "TOEFL Filter:",
                     min = 12,
                     max = 30,
                     value = c(21, 23)
                   )
                 )
               ),
               
               div(
                 checkboxGroupInput(
                   "includeType",
                   "Select Score Types to Include",
                   choices = list(
                     "Official" = 1,
                     "Final" = 2,
                     "Diagnostic" = 3
                   ),
                   selected = c(1, 2)
                 ),
                 checkboxGroupInput(
                   "appRegion",
                   "Student Pool(s)",
                   choices = list(
                     "Beijing/Tianjin" = 1,
                     "Other China" = 2,
                     "International Boarding School" = 3
                   ),
                   selected = c(1, 2)
                 ),
                 
                 checkboxGroupInput(
                   "appYear",
                   "Appcon Year(s)",
                   choices = list(
                     "2017" = 1,
                     "2018" = 2,
                     "2019" = 3
                   ),
                   selected = c(1, 2)
                 )
               )
               
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 selected = "Spaghetti Plot",
                 tabPanel("Spaghetti Plot",
					fluidRow(
						plotOutput("aggregatePlot",
							 height = 800)),
					fluidRow(
						HTML('<div data-iframe-height></div>'))
						),
                 tabPanel(
                   "Histogram",
                   fluidRow(plotOutput("histPlot",
                                       height = 400)),
				   fluidRow(hr()),
                   fluidRow(
                            column(3,
                                   h4(textOutput("finalDistTableLabel")),
                                   DT::dataTableOutput("finalDistTable"),
								   HTML('<div data-iframe-height></div>')),
                            column(9,
                                   plotOutput(
                                     "densityPlot",
                                     height = 400
                                   )))
                 ),
                 tabPanel(
					"Table",
					fluidRow(
							DT::dataTableOutput("test")),
					fluidRow(
							HTML('<div data-iframe-height></div>'))
               )
             )
           ))
	),
  tabPanel("By Student",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("selectedStudents",
                              "Select Student to Display",
                              choices = NULL)
             ),
             mainPanel(
               "TODO: Render score improvement plot for each score type for selected student"
             )
           ))
  
))
