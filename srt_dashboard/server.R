require(jsonlite)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(shinythemes)
require(shiny)
require(DT)
require(lubridate)
require(stringr)

filter_by_checkbox_input <-
  function(df, df_field, selectedOptions) {
    stopifnot(class(df_field) == "character")
    i <- match(df_field, colnames(df))
    l <- lapply(selectedOptions, function(option) {
      filter(df, df[i] == option)
    })
    return(bind_rows(l))
  }


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

pipeline_colors <-
  c(
    "#60992D",
    "#B1E285",
    "#C5F29D",
    "#9F4A54",
    "#418AD8",
    "#F2E94E",
    "#E3170A",
    "#A3D9FF",
    "#98E0BC",
    "#F34213"
  )

names(pipeline_colors) <- pipeline_statuses

shinyServer(function(input, output) {

  cleaned_dat <- reactive({
    response <-
      bind_rows(fromJSON("https://escwebtools.xyz/podio/fetch/PFP"))
    
    col_names_list <- str_split(colnames(response), pattern = "\\$_:_\\$")
    colnames(response) <- sapply(col_names_list, function(col) { col[1] })
    
    df <- select(
      response,
      "Edit PFP" = "link",
      "Pinyin Name" = "job-title",
      "English Name" = name,
      "Chinese Name" = 'chinese-name-characters',
      "Gender" = gender,
      "Graduation Year" = 'graduation-year-2',
      "Market" = pool,
      "Region" = "pool-2",
      "SRT Liaison" = "srt-liaison",
      "SRT Interviewer" = "srt-liaison-2",
      "Pipeline Status" = "pipeline-status",
      "Events Attended" = "events-attended",
      "Number of Events Attended" = "number-of-events-attended",
      "Source" = source,
      "Student Wechat" = "wechat-id",
      "Student Phone" = "phone-number",
      "Student Email" = "email-address",
      "Mothers Name" = "mothers-name-chinese",
      "Mothers Phone" = "mothers-phone",
      "Mothers Wechat" = "mothers-wechat",
      "Mothers Email" = "mothers-email",
      "Fathers Name" = "fathers-name-chinese",
      "Fathers Phone" = "fathers-phone",
      "Fathers Wechat" = "fathers-wechat",
      "Fathers Email" = "fathers-email",
      "Has Parent Contact?" = "parent-contact-captured",
      "Number of Outreach" = "of-times-contacted",
      "POT Call List" = "call-list-2",
      "Nominator(s)" = "nominators",
      "High Test?" = "hi-test",
      "Lead Acquisition Date" = "lead-acquisition-date",
      "Interviewer" = "interviewer",
      "Interview Date" = "interview-date"
    )
    
    # df$SRT_Liaison <- factor(df$SRT_Liaison)
    #
    # df$Pipeline_Status <- factor(df$Pipeline_Status,
    #                              levels = pipeline_statuses)
    # df$Market <- factor(df$Market)
    # df$Region <- factor(df$Region)
    # df$Source <- factor(df$Source)
    df$`Lead Acquisition Date` <-
      as.Date(df$`Lead Acquisition Date`, "%Y-%m-%d")
    df$`Number of Outreach` <-
      as.integer(df$`Number of Outreach`)
    df$`Number of Events Attended` <-
      as.integer(df$`Number of Events Attended`)
    df$`Events Attended` <-
      gsub(", (?=,|$)",
           "",
           as.character(df$`Events Attended`),
           perl = TRUE)
    df  <-
      df %>% mutate(
        `Edit PFP` = paste0(
          "<a href = '",
          `Edit PFP`,
          "' target='_blank' class= 'btn btn-primary'>Edit PFP</a>"
        )
      )
    
    df
    
  })
  
  filtered_dat <- reactive({
    df_0 <- cleaned_dat()
    
    df_1 <-
      filter_by_checkbox_input(df_0, "Graduation Year", input$grad_year)
    
    if (input$srt_liaison == "All") {
      df_2 <- df_1
    } else
      df_2 <-
      filter_by_checkbox_input(df_1, "SRT Liaison", input$srt_liaison)
    
    df_3 <- filter_by_checkbox_input(df_2, "Market", input$market)
    
    df_4 <-
      filter_by_checkbox_input(df_3, "Pipeline Status", input$pipeline_status)
    
    if (is.null(input$events_attended)) {
      df_5 <- df_4
    } else {
      l <- lapply(input$events_attended, function(event) {
        filter(df_4, str_detect(`Events Attended`, event))
      })
      df_5 <- unique(bind_rows(l))
    }
    
    if (is.null(input$region)) {
      df_6 <- df_5
    } else {
      l <- lapply(input$region, function(region) {
        filter(df_5, str_detect(Region, region))
      })
      df_6 <- unique(bind_rows(l))
    }
    
    date_range <- interval(input$lead_acquisition_range[[1]],input$lead_acquisition_range[[2]])
    
    if (input$include_missing_dates == "Include") {
    df_7 <- unique(bind_rows(filter(df_6, is.na(`Lead Acquisition Date`)), filter(df_6, `Lead Acquisition Date` %within% date_range)))
    } else {
      df_7 <- filter(df_6, `Lead Acquisition Date` %within% date_range)
    }
    df_7
  })
  
  pivot <- reactive({
    df <-
      cast(melt(
        filtered_dat(),
        `SRT Liaison` ~ `Pipeline Status`,
        fun.aggregate = length
      ))
    
    df <- mutate(
      filtered_dat(),
      Interviewed = ANS +
        `ANS - Expired` +
        `Decision Pending` +
        Declined +
        `Interview Pending` +
        Reject +
        Signed +
        Waitlist
    )
    
    df
    
  })
  
  output$events_attended_ui <- renderUI({
    selectizeInput(
      inputId = "events_attended",
      label = "Events Attended",
      multiple = TRUE,
      choices = unique(unlist(
        sapply(cleaned_dat()$`Events Attended`,
               function(x) {
                 strsplit(x, split = ", ", fixed = TRUE)
               })
      ))
    )
  })
  
  output$region_ui <- renderUI({
    selectizeInput(
      inputId = "region",
      label = h4("Region"),
      multiple = TRUE,
      choices = unique(cleaned_dat()$Region)
    )
  })
  
  output$call_list_general <-
    renderDT(filtered_dat()[c(
      "Edit PFP",
      "Pinyin Name",
      "Chinese Name",
      "Graduation Year",
      "SRT Liaison",
      "SRT Interviewer",
      "Pipeline Status",
      "Lead Acquisition Date"
    )],
    options = list(order = list(8, 'desc'),
                   scrollX = TRUE),
    escape = FALSE)
  output$call_list_contact <- renderDT(filtered_dat()[c(
    "Edit PFP",
    "Pinyin Name",
    "Chinese Name",
    "Student Wechat",
    "Student Phone",
    "Mothers Name",
    "Mothers Phone",
    "Mothers Wechat",
    "Fathers Name",
    "Fathers Phone",
    "Fathers Wechat",
    "Lead Acquisition Date"
  )], options = list(order = list(12, 'desc'),
                     scrollX = TRUE),
  escape = FALSE)
  output$call_list_pipeline <-
    renderDT(filtered_dat()[c(
      "Edit PFP",
      "Pinyin Name",
      "Chinese Name",
      "Pipeline Status",
      "Number of Events Attended",
      "Events Attended",
      "Source",
      "Nominator(s)",
      "Interview Date",
      "Number of Outreach",
      "Lead Acquisition Date"
    )], options = list(order = list(11, 'desc'),
                       scrollX = TRUE),
    escape = FALSE)

    output$timestamp <- renderText({
      update_time <- ymd_hms(fromJSON("https://escwebtools.xyz/podio/fetchCreatedAt/PFP"), tz = "Asia/Taipei")
      paste0("Last updated on: ", toString(update_time)," CST")
    })
})

