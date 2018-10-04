library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(ggthemes)
library(DT)
library(scales)

shinyServer(function(input, output) {
  ylimits <- reactive({
    switch(
      input$testSelect,
      "ACT" = c(24, 36),
      "ACT R" = c(18, 36),
      "RSAT" = c(1300, 1600),
      "RSAT CR" = c(400, 800),
      "TOEFL" = c(85, 120),
      "TOEFL S" = c(85, 120)
    )
  })
  
  plot_dat <- readRDS("plot_dat.RData")
  plot_dat$Student <- as.factor(plot_dat$Student)
  
  input_values <- reactive({
    testSelected <- input$testSelect
    
    lowerLimit <- switch(
      testSelected,
      "ACT" = input$ACTRange[[1]],
      "ACT R" = input$ACTRRange[[1]],
      "RSAT" = input$RSATRange[[1]],
      "RSAT CR" = input$RSATCRRange[[1]],
      "TOEFL" = input$TOEFLRange[[1]],
      "TOEFL S" = input$TOEFLSRange[[1]]
    )
    
    upperLimit <- switch(
      testSelected,
      "ACT" = input$ACTRange[[2]],
      "ACT R" = input$ACTRRange[[2]],
      "RSAT" = input$RSATRange[[2]],
      "RSAT CR" = input$RSATCRRange[[2]],
      "TOEFL" = input$TOEFLRange[[2]],
      "TOEFL S" = input$TOEFLSRange[[2]]
    )
    list(
      testSelected = testSelected,
      lowerLimit = lowerLimit,
      upperLimit = upperLimit
    )
  })
  filtered_plot_dat <- reactive({
    # Check score range
    
    # All the scores before t, that fit within the range
    # All the students who scored in that range, including the ones that beat that score before t
    plot_dat %>%
      filter(Test == input_values()$testSelected,
             Type != "Diagnostic",
             input_values()$lowerLimit <= y,
             input_values()$upperLimit >= y,
             abs(x) >= abs(input$tValue)) -> filterMatch1
    
    plot_dat %>%
      filter(Test == input_values()$testSelected,
             Type != "Diagnostic",
             y > input_values()$upperLimit,
             abs(x) >= abs(input$tValue)) -> filterMatch2
    
    filterResult <- setdiff(filterMatch1$Student,filterMatch2$Student)
    
    df_1 <-
      plot_dat %>% filter(Student %in% filterResult, Test == input_values()$testSelected)
    
    if (!(1 %in% input$appRegion)) {
      df_2 <- df_1 %>% filter(Market != "Beijing/Tianjin")
    } else
      df_2 <- df_1
    if (!(2 %in% input$appRegion)) {
      df_3 <- df_2 %>% filter(Market != "Other China")
    } else
      df_3 <- df_2
    if (!(3 %in% input$appRegion)) {
      df_4 <- df_3 %>% filter(Market != "International Boarding School")
    } else
      df_4 <- df_3
    # Check Application year
    if (!(1 %in% input$appYear)) {
      df_5 <- df_4 %>% filter(Year != 2017)
    } else
      df_5 <- df_4
    
    if (!(2 %in% input$appYear)) {
      df_6 <- df_5 %>% filter(Year != 2018)
    } else
      df_6 <- df_5
    
    if (!(3 %in% input$appYear)) {
      df_7 <- df_6 %>% filter(Year != 2019)
    } else
      df_7 <- df_6
    
    if (!(1 %in% input$includeType)) {
      df_8 <- df_7 %>% filter(Type != "Official")
    } else
      df_8 <- df_7
    
    if (!(2 %in% input$includeType)) {
      df_9 <- df_8 %>% filter(Type != "Final")
    } else
      df_9 <- df_8
    
    if (!(3 %in% input$includeType)) {
      df_10 <- df_9 %>% filter(Type != "Diagnostic")
    } else
      df_10 <- df_9
    
    filterDisplayTable <- filter(df_10, Type == "Final")
    
    list(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10,filterDisplayTable)
    
  })
  
  distTable <- reactive({
    l <- filtered_plot_dat()[[11]] %>%
      count(y, sort = TRUE) %>%
      rename(Score = y, Frequency = n)
    l
  })
  
  smartMedian <- reactive({
    dat <- filter(filtered_plot_dat()[[10]],
                  Test == input$testSelect)
    median(dat$y)
  })
  
  output$aggregatePlot <- renderPlot({
    dat <- filtered_plot_dat()[[10]]
    
    p <- ggplot(dat, aes(x = x, y = y, fill = Category)) +
      geom_count(aes(size = ..n..),
                 shape = 21) +
      geom_line(aes(group = Student, color = y),
                size = 1) +
      geom_vline(xintercept = -input$tValue) +
      coord_cartesian(xlim = c(-600, 5), ylim = ylimits()) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(breaks = pretty_breaks(),
                         sec.axis = dup_axis()) +
      theme_fivethirtyeight() +
      guides(size = guide_legend(title = "Frequency"),
             fill = guide_legend(title = "Score Type", 
                                 override.aes = list(shape = 21,
                                                     size = 10))) +
      scale_color_gradient2(low = "#fc8d62",
                            mid = "#8da0cb",
                            high = "#66c2a5",
                            midpoint = smartMedian(),
                            guide = "none") +
      scale_fill_manual(values = c("First" = "#fc8d62",
                                   "Middle" = "#8da0cb",
                                   "Highest" = "#66c2a5",
                                   "Final" = "lightgrey")) +
      scale_shape_manual(values = c("First" = 21,
                           "Middle" = 23,
                           "Highest" =24,
                           "Final" = 22)) +
      
      scale_size_area(max_size = 15) +
      theme(#legend.position = "none",
            plot.title = element_text(size = 28, face = "bold"),
            plot.subtitle = element_text(size = 18)) +
      ggtitle(
        paste(input$testSelect, "Score Improvement"),
        subtitle = paste(
          "Showing students that had a highest",
          input$testSelect,
          "score between",
          input_values()$lowerLimit,
          "and",
          input_values()$upperLimit,
          ",",
          input$tValue,
          "days before Appcon"
        )
      )
    
    p
  })
  
  output$histPlot <- renderPlot({

    ggplot(data = filtered_plot_dat()[[11]],
           aes(y, fill = y)) +
      geom_bar() +
      theme_fivethirtyeight() +
      scale_fill_continuous_tableau() +
      ggtitle(paste(
        "Distribution of Final",
        input$testSelect,
        "Scores"
      )) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(breaks = pretty_breaks())
  })
  
  output$densityPlot <- renderPlot({
    
    ggplot(data = filtered_plot_dat()[[11]],
           aes(y)) +
      geom_density() +
      theme_fivethirtyeight() +
      ggtitle(paste(
        "Estimated Probability of Final",
        input$testSelect,
        "Score"
      )) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(breaks = pretty_breaks())
  })
  
  output$finalDistTableLabel <- renderText(paste("Distribution of Final",input$testSelect, "Scores"))
  
  output$finalDistTable <- DT::renderDataTable(distTable(),
                                              options = list(paging = FALSE,
                                                             dom = "t",
                                                             ordering = FALSE,
                                                             style = "bootstrap"),
                                              rownames = FALSE)
  
  output$test <- DT::renderDataTable(filtered_plot_dat()[[7]],
                                     options = list(paging = FALSE,
                                                    dom = "t",
                                                    style = 'bootstrap'))
})
