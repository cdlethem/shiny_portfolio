require(jsonlite)
require(dplyr)
require(reshape)
require(ggplot2)
require(ggthemes)
require(shinythemes)
require(shiny)
require(DT)
require(lubridate)
require(stringr)

pipeline_statuses <- c(
  "Signed",
  "ANS",
  "ANS - Expired",
  "Declined",
  "Decision Pending",
  "Waitlist",
  "Reject",
  "Interview Pending",
  "Warm lead",
  "Dead lead"
)

grad_years <- c("2017",
                "2018",
                "2019",
                "2020",
                "2021",
                "2022",
                "2023")

markets <- c(
  "Beijing/Tianjin",
  "Other China",
  "International Boarding School",
  "Graduate School Applicant",
  "Transfer Student"
)

shinyUI(
  navbarPage(
    "SRT Dashboard",
    selected = "Call Lists",
    tags$head(tags$style(
      HTML(
        ".multicol{font-size:12px;
        height:auto;
        -webkit-column-count: 2;
        -moz-column-count: 2;
        column-count: 2;
        }
        
        div.checkbox {margin-top: 0px;}
        div.radio {margin-top: 0px}
        hr {margin-top: 5px;
        margin-bottom: 5px}"
        
      )
    )),
    theme = shinytheme("flatly"),
    tabPanel("Call Lists",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 tags$div(
                   align = "center",
                   tags$strong(textOutput("timestamp")),
                   hr(),
                   tags$a(
                     href = "https://escwebtools.xyz/podio/update/PFP",
                     target = "_blank",
                     class = 'btn btn-danger',
                     "Refresh from Podio"
                   ) 
                 ),
                 hr(),
                 
                 h3("Filters"),
                 helpText("These filters control which prospies appear on the call lists"),
                 
                 hr(),
                 
                 h4("Lead Acquisition Date"),
                 
                 dateRangeInput(
                   inputId = "lead_acquisition_range",
                   label = NULL,
                   start = "2017-07-01",
                   end = NULL,
                   min = "2017-07-01",
                   max = NULL
                 ),
                 
                 radioButtons(
                   "include_missing_dates",
                   label = "Missing dates",
                   choices = c("Include", "Exclude"),
                   selected = "Exclude",
                   inline = TRUE
                              ),
                 
                 h4("SRT Liaison"),
                 
                 tags$div(
                   align = "left",
                   class = "multicol",
                   radioButtons(
                     inputId = "srt_liaison",
                     label = NULL,
                     choices = list("All",
                                    "rita",
                                    "Amanda Guo",
                                    "Louis Von",
                                    "Melinda Yang"),
                     selected = "All"
                   )
                 ),
                 
                 hr(),
                 
                 h4("Graduation Year"),
                 
                 tags$div(
                   align = "left",
                   class = "multicol",
                   checkboxGroupInput(
                     label = NULL,
                     inputId =  "grad_year",
                     choices = grad_years,
                     selected = grad_years
                   )
                 ),
                 
                 hr(),
                 
                 h4("Pipeline Status"),
                 
                 tags$div(
                   align = "left",
                   class = "multicol",
                   checkboxGroupInput(
                     inputId = "pipeline_status",
                     label = NULL,
                     choices = pipeline_statuses,
                     selected = pipeline_statuses
                   )
                 ),
                 
                 uiOutput("events_attended_ui"),
                 
                 hr(),
                 
                 h4("Market"),
                 
                 tags$div(
                   align = "left",
                   class = "multicol",
                   checkboxGroupInput(
                     inputId = 'market',
                     label = NULL,
                     choices = markets,
                     selected = markets
                   )
                 ),
                 
                 uiOutput("region_ui")
                 
               ),
               
               mainPanel(width = 9,
                         tabsetPanel(
                           tabPanel("General Info",
                                    DTOutput("call_list_general")),
                           tabPanel("Contact Info",
                                    DTOutput("call_list_contact")),
                           tabPanel("Pipeline Info",
                                    DTOutput("call_list_pipeline"))
                         ))
             ))
    )
    )