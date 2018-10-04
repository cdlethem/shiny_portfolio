require(jsonlite)
require(dplyr)
require(shinythemes)
require(shiny)
require(DT)
require(stringr)
require(lubridate)
require(tidyr)

shinyUI(
  navbarPage(
    "Student Portal Admin Dashboard",
    selected = "2019 Student Portal",
    theme = shinytheme("flatly"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript"),
    tabPanel(
      "2019 Student Portal",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$div(
            align = "center",
            tags$strong(textOutput("ss_timestamp")),
            hr(),
            tags$a(
              href = "https://escwebtools.xyz/podio/update/ssportal",
              target = "_blank",
              class = 'btn btn-danger',
              "Refresh from Podio"
            )
          ),
          hr(),
          conditionalPanel(condition = "input.ss_tabSelect == 'Responses By Field'",
                           uiOutput("ss_field_selector")),
          conditionalPanel(condition = "input.ss_tabSelect == 'Responses By Student'",
                           uiOutput("ss_student_selector")),
          hr(),
          tags$div(
            align = "center",
            tags$strong("Export current view as CSV"),
            hr(),
            downloadButton("ss_downloadData", "Download"),
            hr()
          )
        ),
        mainPanel(width = 9,
                  tabsetPanel(
                    id = "ss_tabSelect",
                    tabPanel("Responses By Field",
                             DTOutput("ss_by_field_table")),
                    tabPanel("Responses By Student",
                             DTOutput("ss_by_student_table"))
                  ))
      )
    ),
    tabPanel(
      "JS Student Portal",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$div(
            align = "center",
            tags$strong(textOutput("js_timestamp")),
            hr(),
            tags$a(
              href = "https://escwebtools.xyz/podio/update/jsportal",
              target = "_blank",
              class = 'btn btn-danger',
              "Refresh from Podio"
            )
          ),
          hr(),
          conditionalPanel(condition = "input.js_tabSelect == 'Responses By Field'",
                           uiOutput("js_field_selector")),
          conditionalPanel(condition = "input.js_tabSelect == 'Responses By Student'",
                           uiOutput("js_student_selector")),
          hr(),
          tags$div(
            align = "center",
            tags$strong("Export current view as CSV"),
            hr(),
            downloadButton("js_downloadData", "Download"),
            hr()
          )
        ),
        mainPanel(width = 9,
                  tabsetPanel(
                    id = "js_tabSelect",
                    tabPanel("Responses By Field",
                             DTOutput("js_by_field_table")),
                    tabPanel("Responses By Student",
                             DTOutput("js_by_student_table"))
                  ))
      )
    )
  )
)