require(jsonlite)
require(dplyr)
require(shinythemes)
require(shiny)
require(DT)
require(stringr)
require(lubridate)
require(tidyr)

shinyServer(function(input, output) {
  
  cleaned_ss_dat <- reactive({
    dat <-
      bind_rows(fromJSON("https://escwebtools.xyz/podio/fetch/ssportal"))
    
    # Split JSON keys into unique external ID and field label. Field labels are easier to understand, but can be duplicate
    
    col_names_df <-
      as_data_frame(str_split(colnames(dat), pattern = "\\$_:_\\$", simplify = TRUE))
    
    # Use prettier field labels instead of unique identifiers, unless the field label is a duplicate
    col_name_dupes <-
      as_data_frame(table(col_names_df$V2)) %>% filter(n > 1) %>% arrange(desc(n)) %>% rename(dupes = Var1)
    colnames(dat) <-
      apply(col_names_df, 1, function(x) {
        if (x[2] == "" ||
            x[2] %in% col_name_dupes$dupes) {
          return(x[1])
        } else
          return(x[2])
      })
    
    dat <- dat %>% mutate(
      link = paste0(
        "<a href = '",
        link,
        "' target='_blank' class= 'btn btn-primary'>Edit in Podio</a>"
      )
    )
    
    # Specify columns to keep and remove rows with NA names
    identity_cols <-
      c("link", "Student Name", "Email", "Counselor")
    trash_cols <-
      c(
        'Students',
        '[*Password Hash*]',
        'RSAT Webform',
        'Password Created?',
        "Student Item ID",
        'SAT Input',
        'ACT Webform',
        'ACT Input',
        'TOEFL Webform',
        'TOEFL Input',
        'AP Webform',
        'AP Input',
        'SAT II Webform',
        'SAT II Input',
        'Event Attendance',
        'Event RSVP',
        'Assignment History',
        'Upcoming SAT Date',
        'SAT',
        'Diagnostic SAT ',
        'ACT',
        'Diagnostic ACT',
        'TOEFL',
        'Diagnostic TOEFL',
        'SATII',
        'AP',
        'Highest RSAT',
        'Add New Test Score',
        'Last 3 Parent Notes',
        'Nodes Relationship',
        'Highest ACT',
        'Highest TOEFL',
        'Send Notes to this email:',
        'CC this email',
        'Meeting Notes',
        'Upcoming ACT Date',
        'Wau Portal Hook',
        'Date of Most Recent Meeting (MDIF)',
        'Assignments and Deadlines'
      )
    response_cols <-
      setdiff(colnames(dat), c(identity_cols, trash_cols))
    response_dat <-
      bind_cols(dat[identity_cols], dat[response_cols]) %>% filter(!is.na(`Student Name`))
    response_col_names <- colnames(response_dat[response_cols])
    
    # ======== Tables by Student ===========
    
    # Transpose each row of the data by making key-value pairs from colnames and portal responses (1 x 65 ===> 65 x 2)
    
    by_student_tables <-
      lapply(response_dat$`Student Name`, function(studentname) {
        l <-
          tidyr::gather(
            filter(response_dat, `Student Name` == studentname),
            key = "Field",
            value = "Response",
            na.rm = TRUE
          )
        l
      })
    
    names(by_student_tables) <- response_dat$`Student Name`
    
    # ========= Tables by Field ==========
    
    by_field_tables <-
      lapply(response_col_names, function(col_name) {
        n <- length(identity_cols) + 1
        l <- bind_cols(dat[identity_cols], dat[c(col_name)])
        # use dummy column name b/c 'arrange' expects literal, unquoted column name
        colnames(l)[n] <- c("dummy_var")
        l <- arrange(l, dummy_var)
        colnames(l)[n] <- c(col_name)
        l
      })
    
    
    names(by_field_tables) <- response_col_names
    
    list(by_field_tables,
         by_student_tables,
         response_dat)
    
  })
  
  cleaned_js_dat <- reactive({
    
    # Fetch processed data from webtools API
    
    dat <-
      bind_rows(fromJSON("https://escwebtools.xyz/podio/fetch/jsportal"))
    
    # Split JSON keys into unique external ID and field label. Field labels are easier to understand, but can be duplicate
    
    col_names_df <-
      as_data_frame(str_split(colnames(dat), pattern = "\\$_:_\\$", simplify = TRUE))
    
    # Use prettier field labels instead of unique identifiers, unless the field label is a duplicate
    col_name_dupes <-
      as_data_frame(table(col_names_df$V2)) %>% filter(n > 1) %>% arrange(desc(n)) %>% rename(dupes = Var1)
    colnames(dat) <-
      apply(col_names_df, 1, function(x) {
        if (x[2] == "" ||
            x[2] %in% col_name_dupes$dupes) {
          return(x[1])
        } else
          return(x[2])
      })
    
    dat <- dat %>% mutate(
      link = paste0(
        "<a href = '",
        link,
        "' target='_blank' class= 'btn btn-primary'>Edit in Podio</a>"
      )
    )
    
    # Specify columns to keep and remove rows with NA names
    identity_cols <-
      c("link","Student Name","Email", "Appcon Year", "Counselor" )
    trash_cols <-
      c(
        'Students',
        '[*Password Hash*]',
        'Password Created?',
        'RSAT Webform',
        "Student Item ID",
        'SAT Input',
        'ACT Webform',
        'ACT Input',
        'TOEFL Webform',
        'TOEFL Input',
        'AP Webform',
        'AP Input',
        'SAT II Webform',
        'SAT II Input',
        'Event Attendance',
        'Event RSVP',
        'Assignment History',
        'Upcoming SAT Date',
        'SAT',
        'Diagnostic SAT ',
        'ACT',
        'Diagnostic ACT',
        'TOEFL',
        'Diagnostic TOEFL',
        'SATII',
        'AP',
        'Highest RSAT',
        'Add New Test Score',
        'Last 3 Parent Notes',
        'Nodes Relationship',
        'Highest ACT',
        'Highest TOEFL',
        'Send Notes to this email:',
        'CC this email',
        'Meeting Notes',
        'Upcoming ACT Date',
        'Wau Portal Hook',
        'Date of Most Recent Meeting (MDIF)',
        'Assignments and Deadlines'
      )
    response_cols <- setdiff(colnames(dat),c(identity_cols,trash_cols))
    response_dat <-
      filter(dat[c(identity_cols, response_cols)],!is.na(`Student Name`))
    response_col_names <- colnames(response_dat[response_cols])
    
    # ======== Tables by Student ===========
    
    # Transpose each row of the data by making key-value pairs from colnames and portal responses i.e. (1 x 65 ===> 65 x 2)
    
    by_student_tables <-
      lapply(response_dat$`Student Name`, function(studentname) {
        l <-
          gather(
            filter(response_dat, `Student Name` == studentname),
            key = "Field",
            value = "Response",
            na.rm = TRUE
          )
        l
      })
    
    names(by_student_tables) <- response_dat$`Student Name`
    
    # ========= Tables by Field ==========
    
    by_field_tables <- lapply(response_col_names, function(col_name) {
      n <- length(identity_cols) + 1
      l <- bind_cols(dat[identity_cols], dat[c(col_name)])
      # use dummy column name b/c 'arrange' expects literal, unquoted column name
      colnames(l)[n] <- c("dummy_var")
      l <- arrange(l, dummy_var)
      colnames(l)[n] <- c(col_name)
      l
    })
    
    
    names(by_field_tables) <- response_col_names
    
    list(by_field_tables,
         by_student_tables,
         response_dat)
  })
  
  filtered_ss_dat <- reactive({
    if (is.null(input$ss_field_select)) {
      by_field <- cleaned_ss_dat()[[1]][[1]]
    } else {
      by_field <- cleaned_ss_dat()[[1]][[input$ss_field_select]]
    }
    if (is.null(input$ss_student_select)) {
      by_student <- cleaned_ss_dat()[[2]][[1]]
    } else {
      by_student <- cleaned_ss_dat()[[2]][[input$ss_student_select]]
    }
    
    list(by_field,
         by_student)
    
  })
  
  filtered_js_dat <- reactive({
    if (is.null(input$js_field_select)) {
      by_field <- cleaned_js_dat()[[1]][[1]]
    } else {
      by_field <- cleaned_js_dat()[[1]][[input$js_field_select]]
    }
    if (is.null(input$js_student_select)) {
      by_student <- cleaned_js_dat()[[2]][[1]]
    } else {
      by_student <- cleaned_js_dat()[[2]][[input$js_student_select]]
    }
    
    list(by_field,
         by_student)
    
  })
  
  output$ss_field_selector <- renderUI({
    selectizeInput(
      inputId = "ss_field_select",
      label = h4("Select Field"),
      multiple = FALSE,
      choices = names(cleaned_ss_dat()[[1]])
    )
  })
  
  output$ss_student_selector <- renderUI({
    selectizeInput(
      inputId = "ss_student_select",
      label = h4("Select Student"),
      multiple = FALSE,
      choices = names(cleaned_ss_dat()[[2]])
    )
  })
  
  output$js_field_selector <- renderUI({
    selectizeInput(
      inputId = "js_field_select",
      label = h4("Select Field"),
      multiple = FALSE,
      choices = names(cleaned_js_dat()[[1]])
    )
  })
  
  output$js_student_selector <- renderUI({
    selectizeInput(
      inputId = "js_student_select",
      label = h4("Select Student"),
      multiple = FALSE,
      choices = names(cleaned_js_dat()[[2]])
    )
  })
  
  output$ss_by_field_table <-
    renderDT(filtered_ss_dat()[[1]],
             options = list(
               order = list(5, "desc"),
               paging = FALSE
             ),
             escape = FALSE)
  
  output$ss_by_student_table <-
    renderDT(filtered_ss_dat()[[2]],
             options = list(paging = FALSE),
             escape = FALSE)
  
  output$js_by_field_table <-
    renderDT(filtered_js_dat()[[1]],
             options = list(
               order = list(6, "desc"),
               paging = FALSE
             ),
             escape = FALSE)
  
  output$js_by_student_table <-
    renderDT(filtered_js_dat()[[2]],
             options = list(paging = FALSE),
             escape = FALSE)
  
  output$ss_timestamp <- renderText({
    update_time <-
      ymd_hms(fromJSON("https://escwebtools.xyz/podio/fetchCreatedAt/ssportal"),
              tz = "Asia/Taipei")
    paste0("Last updated on: ", toString(update_time), " CST")
  })
  
  output$js_timestamp <- renderText({
    update_time <-
      ymd_hms(fromJSON("https://escwebtools.xyz/podio/fetchCreatedAt/jsportal"),
              tz = "Asia/Taipei")
    paste0("Last updated on: ", toString(update_time), " CST")
  })
  
  output$ss_downloadData <- downloadHandler(
    filename = function() {
      if (input$ss_tabSelect == 'Responses By Field') {
        a <- "ss_by_field"
        b <- input$ss_field_select
      } else {
        a <- "ss_by_student"
        b <- input$ss_student_select
      }
      paste(a,"_",b,".csv", sep = "")
    },
    content = function(file) {
      if (input$ss_tabSelect == 'Responses By Field') {
        write.csv(mutate(filtered_ss_dat()[[1]],link = NULL), file, row.names = FALSE)
      } else {
        write.csv(mutate(filtered_ss_dat()[[2]],link = NULL), file, row.names = FALSE)
      }
    }
  )
  
  output$js_downloadData <- downloadHandler(
    filename = function() {
      if (input$js_tabSelect == 'Responses By Field') {
        a <- "by_field"
        b <- input$js_field_select
      } else {
        a <- "by_student"
        b <- input$js_student_select
      }
      paste(a,"_",b,".csv", sep = "")
    },
    content = function(file) {
      if (input$js_tabSelect == 'Responses By Field') {
        write.csv(mutate(filtered_js_dat()[[1]],link = NULL), file, row.names = FALSE)
      } else {
        write.csv(mutate(filtered_js_dat()[[2]],link = NULL), file, row.names = FALSE)
      }
    }
  )
  
})
