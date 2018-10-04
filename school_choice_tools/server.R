#Load dependencies

require(shiny)
require(dplyr)
require(ggplot2)
require(DT)
require(shinythemes)
require(ggthemes)

#This code chunk imports our datasets, reads in premade R objects and creates some key variables.

import_custom <-
  function(filename) {
    #this is just a wrapper for read.csv
    apps <-
      read.csv(filename,
               stringsAsFactors = FALSE,
               sep = ",")
    return(apps)
  }
apps <- import_custom("9.13 scatdat.csv")
tbl.re <-  import_custom("11.19 risk_params.csv")
dat_list <- readRDS("dat_list.RData")
overlap_matrix <- readRDS("overlap_matrix.RData")

colleges <- tbl.re$Institution
schools <- row.names(overlap_matrix)
o <- order(schools)
schools <- schools[o]
high_schools <- unique(apps$High.School)
N <- length(schools)

shinyServer(function(input, output) {
  # Helper function to make correlation plot
  correlation_plotter <- function(collegename, schoolsnum) {
    l <-
      select(overlap.df(), College_Name, match(collegename, names(overlap.df())))
    colnames(l) <- c("Comparison_School", "Overlap_Coefficient")
    o <- order(l$Overlap_Coefficient, decreasing = TRUE)
    l <-
      transmute(l,
                Comparison_School = Comparison_School[o],
                Overlap_Coefficient = Overlap_Coefficient[o]) %>%
      filter(Comparison_School != collegename) %>%
      filter(between(row_number(), 1, schoolsnum))
    smartMedian <- median(l$Overlap_Coefficient)
    p <-
      ggplot(l, aes(
        Overlap_Coefficient,
        reorder(Comparison_School, Overlap_Coefficient)
      )) +
      geom_point(aes(fill = Overlap_Coefficient),
                 size = 5,
                 shape = 21) +
      scale_fill_gradient2(
        low = "#a6cee3",
        mid = "#1f78b4",
        high = "#b2df8a",
        midpoint = smartMedian
      ) +
      theme_fivethirtyeight() +
      labs(
        title = paste(collegename, "App Overlap"),
        subtitle = paste(
          "Applicants to these schools were most likely to have also applied to ",
          collegename
        ),
        x = "Overlap Coefficient",
        y = "School Name"
      )
    p
  }
  
  # Create dataset for correlation/overlap plot
  
  overlap.df <- reactive({
    if (input$normalizelogical == "2") {
      overlap_matrix <- overlap_matrix / diag(overlap_matrix)
    } else if (input$normalizelogical == "1") {
      overlap_matrix <- overlap_matrix
    }
    
    overlap  <- as_data_frame(overlap_matrix)
    overlap <- overlap %>% mutate(College_Name = schools)
    overlap
  })
  
  
  
  RSATlookup <- data_frame(
    ACT = c(27,
            28,
            29,
            30,
            31,
            32,
            33,
            34,
            35,
            36),
    RSAT =
      c(1300,
        1330,
        1370,
        1410,
        1430,
        1470,
        1510,
        1550,
        1580,
        1600)
  )
  
  # Create datasets for school list explorer.
  
  filtered_dat_list <- reactive({
    dl_0 <- dat_list
    
    rsat_input <- switch(input$testSelect,
                         ACT = c(RSATlookup$RSAT[[match(input$ACT.range.s[1], RSATlookup$ACT)]],RSATlookup$RSAT[[match(input$ACT.range.s[2], RSATlookup$ACT)]]),
                         RSAT = input$RSAT.range.s)
    
    # Filter test scores
    if(1 %in% input$testToggle) {
      dl_1 <- Filter(function(x) {
        (as.integer(x[["RSAT"]]) >= rsat_input[1] && rsat_input[2] >= (as.integer(x[["RSAT"]]))) &&
        (as.integer(x[["TOEFL"]]) >= input$TOEFL.range.s[1] && input$TOEFL.range.s[2] >= (as.integer(x[["TOEFL"]])))
    }, dl_0)
    } else 
      dl_1 <- dl_0
    
    # Check International/China-based/both
    
    if (!(1 %in% input$app.region.s)) {
      dl_2 <- Filter(function(x) {
        x[["Market"]] != "Beijing/Tianjin"
      }, dl_1)
    } else
      dl_2 <- dl_1
    if (!(2 %in% input$app.region.s)) {
      dl_3 <- Filter(function(x) {
        x[["Market"]] != "Other China"
      }, dl_2)
    } else
      dl_3 <- dl_2
    if (!(3 %in% input$app.region.s)) {
      dl_4 <- Filter(function(x) {
        x[["Market"]] != "International Boarding School"
      }, dl_3)
    } else
      dl_4 <- dl_3
    
    # Check Application year
    if (!(1 %in% input$app.year.s)) {
      dl_5 <- Filter(function(x) {
        x[["Year"]] != "2013"
      }, dl_4)
    } else
      dl_5 <- dl_4
    
    if (!(2 %in% input$app.year.s)) {
      dl_6 <- Filter(function(x) {
        x[["Year"]] != "2014"
      }, dl_5)
    } else
      dl_6 <- dl_5
    
    if (!(3 %in% input$app.year.s)) {
      dl_7 <- Filter(function(x) {
        x[["Year"]] != "2015"
      }, dl_6)
    } else
      dl_7 <- dl_6
    
    if (!(4 %in% input$app.year.s)) {
      dl_8 <- Filter(function(x) {
        x[["Year"]] != "2016"
      }, dl_7)
    } else
      dl_8 <- dl_7
    if (!(5 %in% input$app.year.s)) {
      dl_9 <- Filter(function(x) {
        x[["Year"]] != "2017"
      }, dl_8)
    } else
      dl_9 <- dl_8
    if (!(6 %in% input$app.year.s)) {
      dl_10 <- Filter(function(x) {
        x[["Year"]] != "2018"
      }, dl_9)
    } else
      dl_10 <- dl_9
    
    # Check ED/ED2/REA Accepted
    
    if (input$includeED == 2) {
      dl_11 <- Filter(function(x) {
        if ("ED" %in% x$Applist$Plan) {
          EDi <- match("ED", x$Applist$Plan)
          ED_test <- x$Applist$Result[EDi] == "Accepted"
        } else
          ED_test <- FALSE
        
        if ("ED2" %in% x$Applist$Plan) {
          ED2i <- match("ED2", x$Applist$Plan)
          ED2_test <- x$Applist$Result[ED2i] == "Accepted"
        } else
          ED2_test <- FALSE
        
        if ("REA" %in% x$Applist$Plan) {
          REAi <- match("REA", x$Applist$Plan)
          REA_test <- x$Applist$Result[REAi] == "Accepted"
        } else
          REA_test <- FALSE
        
        return(!ED_test && !ED2_test && !REA_test)
        
      }, dl_10)
    } else if (input$includeED == 3) {
      dl_11 <- Filter(function(x) {
        if ("ED" %in% x$Applist$Plan) {
          EDi <- match("ED", x$Applist$Plan)
          ED_test <- x$Applist$Result[EDi] == "Accepted"
        } else
          ED_test <- FALSE
        
        if ("ED2" %in% x$Applist$Plan) {
          ED2i <- match("ED2", x$Applist$Plan)
          ED2_test <- x$Applist$Result[ED2i] == "Accepted"
        } else
          ED2_test <- FALSE
        
        if ("REA" %in% x$Applist$Plan) {
          REAi <- match("REA", x$Applist$Plan)
          REA_test <- x$Applist$Result[REAi] == "Accepted"
        } else
          REA_test <- FALSE
        
        return(ED_test || ED2_test || REA_test)
        
      }, dl_10)
    } else
      dl_11 <- dl_10
    
    #Check ED/REA Choice filter
    
    if (!is.null(input$EDSchoolFilter)) {
      dl_12 <- Filter(function(x) {
        tests <- sapply(input$EDSchoolFilter,
                        function(college) {
                          if ("ED" %in% x$Applist$Plan) {
                            i <- match("ED", x$Applist$Plan)
                            return(college == x$Applist$University[i])
                          } else if ("REA" %in% x$Applist$Plan) {
                            i <- match("REA", x$Applist$Plan)
                            return(college == x$Applist$University[i])
                          } else
                            return(FALSE)
                        })
        any(tests)
      }, dl_11)
    } else
      dl_12 <- dl_11
    
    dl_12
  })
  
  # Final dat list is the data that corresponds to the selected ID
  final_dat_list <- reactive({
    l <- Filter(function(x) {
      x[["ID"]] == input$displayid
    }, dat_list)
    
    l[[1]]$Applist <- arrange(l[[1]]$Applist,Plan, Result)
    
    l
  })
  
  # Create dataset for risk evaluator
  risk_TOEFL <- function(TOEFL, college) {
    i <- match(college, colleges)
    
    if (TOEFL <= tbl.re$Out.of.Range.TOEFL[i]) {
      return("Out of Range")
    }
    else if (tbl.re$Fringe.TOEFL.Range.Min[i] <= TOEFL &&
             TOEFL <= tbl.re$Fringe.TOEFL.Range.Max[i]) {
      return("Fringe")
    }
    
    else if (tbl.re$Low.Risk.TOEFL.Range.Min[i] <= TOEFL &&
             TOEFL <= tbl.re$Low.Risk.TOEFL.Range.Max[i]) {
      return("Low")
    }
    
    else if (tbl.re$Medium.Risk.TOEFL.Range.Min[i] <= TOEFL &&
             TOEFL <= tbl.re$Medium.Risk.TOEFL.Range.Max[i]) {
      return("Medium")
    }
    
    else if (tbl.re$High.Risk.TOEFL.Range.Min[i] <= TOEFL &&
             TOEFL <= tbl.re$High.Risk.TOEFL.Range.Max[i]) {
      return("High")
    }
    
    else
      
      return("No Match")
  }
  
  risk_RSAT <- function(RSAT, college) {
    i <- match(college, colleges)
    
    
    if (RSAT <= tbl.re$Out.of.Range.RSAT[i]) {
      return("Out of Range")
    }
    
    else if (tbl.re$Fringe.RSAT.Min[i] <= RSAT &&
             RSAT <= tbl.re$Fringe.RSAT.Max[i]) {
      return("Fringe")
    }
    
    else if (tbl.re$Low.Risk.RSAT.Range.Min[i] <= RSAT &&
             RSAT <= tbl.re$Low.Risk.RSAT.Range.Max[i]) {
      return("Low")
    }
    
    else if (tbl.re$Medium.Risk.RSAT.Range.Min[i] <= RSAT &&
             RSAT <= tbl.re$Medium.Risk.RSAT.Range.Max[i]) {
      return("Medium")
    }
    
    else if (tbl.re$High.Risk.RSAT.Range.Min[i] <= RSAT &&
             RSAT <= tbl.re$High.Risk.RSAT.Range.Max[i]) {
      return("High")
    }
    
    else
      
      return("No Match")
  }
  
  riskevaltbl <- reactive({
    TOEFL <- input$yourTOEFL.re
    RSAT <- input$yourRSAT.re
    
    l <- sapply(colleges, risk_TOEFL, TOEFL = TOEFL)
    
    df_TOEFL <- data_frame(College = names(l),
                           TOEFL.Risk = l)
    
    s <- sapply(colleges, risk_RSAT, RSAT = RSAT)
    
    df_RSAT <- data_frame(College = names(s),
                          RSAT.Risk = s)
    
    df_combined <- data_frame(
      College = df_TOEFL$College,
      TOEFL_risk = df_TOEFL$TOEFL.Risk,
      RSAT_risk = df_RSAT$RSAT.Risk
    )
    
    OoR_TOEFL <-
      df_combined %>% filter(TOEFL_risk == "Out of Range")
    OoR_RSAT <- df_combined %>% filter(RSAT_risk == "Out of Range")
    fringe_TOEFL <-
      df_combined %>% filter(TOEFL_risk == "Fringe") %>% filter(RSAT_risk != "Out of Range")
    fringe_RSAT <-
      df_combined %>% filter(TOEFL_risk != "Out of Range") %>% filter(RSAT_risk == "Fringe")
    low_low <-
      df_combined %>% filter(TOEFL_risk == "Low") %>% filter(RSAT_risk == "Low")
    low_med <-
      df_combined %>% filter(TOEFL_risk == "Low") %>% filter(RSAT_risk == "Medium")
    low_high <-
      df_combined %>% filter(TOEFL_risk == "Low") %>% filter(RSAT_risk == "High")
    med_low <-
      df_combined %>% filter(TOEFL_risk == "Medium") %>% filter(RSAT_risk == "Low")
    med_med <-
      df_combined %>% filter(TOEFL_risk == "Medium") %>% filter(RSAT_risk == "Medium")
    med_high <-
      df_combined %>% filter(TOEFL_risk == "Medium") %>% filter(RSAT_risk == "High")
    high_low <-
      df_combined %>% filter(TOEFL_risk == "High") %>% filter(RSAT_risk == "Low")
    high_med <-
      df_combined %>% filter(TOEFL_risk == "High") %>% filter(RSAT_risk == "Medium")
    high_high <-
      df_combined %>% filter(TOEFL_risk == "High") %>% filter(RSAT_risk == "High")
    
    OoR <- unique(rbind(OoR_RSAT, OoR_TOEFL))
    OoR <- mutate(OoR, Overall.Risk = rep("Out of Range", nrow(OoR)))
    fringe <- unique(rbind(fringe_RSAT, fringe_TOEFL))
    fringe <- mutate(fringe, Overall.Risk = rep("Fringe", nrow(fringe)))
    low <- low_low
    low <- mutate(low, Overall.Risk = rep("Low", nrow(low)))
    medium.low <- unique(rbind(low_med, med_low))
    medium.low <- mutate(medium.low, Overall.Risk = rep("Medium-Low", nrow(medium.low)))
    medium <- unique(rbind(med_med, low_high, high_low))
    medium <- mutate(medium, Overall.Risk = rep("Medium", nrow(medium)))
    medium.high <- unique(rbind(med_high, high_med))
    medium.high <- mutate(medium.high, Overall.Risk = rep("Medium-High", nrow(medium.high)))
    high <- high_high
    high <- mutate(high, Overall.Risk = rep("High", nrow(high)))
    
    final <- data_frame()
    
    if (1 %in% input$risk.selector) {
      final <- rbind(final, low)
    }
    
    if (2 %in% input$risk.selector) {
      final <- rbind(final, medium.low)
    }
    
    if (3 %in% input$risk.selector) {
      final <- rbind(final, medium)
    }
    
    if (4 %in% input$risk.selector) {
      final <- rbind(final, medium.high)
    }
    
    if (5 %in% input$risk.selector) {
      final <- rbind(final, high)
    }
    
    if (6 %in% input$risk.selector) {
      final <- rbind(final, fringe)
    }
    
    if (7 %in% input$risk.selector) {
      final <- rbind(final, OoR)
    }
    
    final <- final %>% select("College or University" = College,
                              "Overall Risk" = Overall.Risk,
                              "TOEFL Risk" = TOEFL_risk,
                              "RSAT Risk" = RSAT_risk)
    
    final  
  })
  
  # The following functions render the plots and tables contained in the reactive objects defined above.
  # Each are attached to the output object and to be accessed by the UI
  
  # render the filtered ID choices for the school list explorer
  
  output$idselector <- renderUI({
    dat <- filtered_dat_list()
    N <- length(dat)
    if (N != 0) {
      ID_list <- lapply(c(1:N), function(i) {
        l <- dat[[i]]$ID
        l
      })
      m <- do.call(rbind, ID_list)
    } else
      m <- NULL
    
    selectizeInput(
      "displayid",
      "Select student ID to display",
      choices = m,
      multiple = FALSE,
      selected = NULL
    )
  })
  
  
  # Render display info for applist explorer
  output$Applist <-
    DT::renderDataTable(datatable(final_dat_list()[[1]]$Applist,
                                  options = list(paging = FALSE,
                                                 style = "bootstrap",
                                                 dom = "t"),
                                  rownames = FALSE) %>% formatStyle(
                                    "Result",
                                    target = "row",
                                    backgroundColor = styleEqual(c("Accepted","Rejected","Waitlisted"),
                                                                 c(	"#bcfaad", "#faadbc","#faebad")))
    )
  
  output$Score.Profile <- DT::renderDataTable(
    final_dat_list()[[1]]$Score.Profile,
    options = list(
      ordering = FALSE,
      dom = "t",
      style = "bootstrap"
    ),
    rownames = FALSE
  )
  
  output$Info <- DT::renderDataTable(
    datatable(final_dat_list()[[1]]$Info,
              options = list(
                ordering = FALSE,
                dom = "t",
                style = "bootstrap"
              ),
              rownames = FALSE
    ))
  
  # Render the table for the risk evaluator
  
  output$riskevaltbl <- DT::renderDataTable(riskevaltbl(),
                                            options = list(
                                              paging = FALSE,
                                              dom = "t",
                                              style = 'bootstrap'
                                            ),
                                            rownames = FALSE
  )
  # Render the overlap plot
  
  output$overlapplot <- renderPlot({
    correlation_plotter(input$correlationschool, input$schoolsnum)
  })
  
  output$overlappui <- renderUI({
    n <- input$schoolsnum
    
    
    plotOutput("overlapplot",
               width = "75%",
               height = n * 40)
  })
  
  # Close server function
})
