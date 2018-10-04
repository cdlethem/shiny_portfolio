#Load dependencies

require(shiny)
require(dplyr)
require(ggplot2)
require(DT)
require(shinythemes)
require(ggthemes)

#This code chunk imports our datasets, reads in premade R objects and creates some key variables.

import_custom <- function(filename) {
  apps <-
    read.csv(filename,
             stringsAsFactors = FALSE,
             sep = ",")
  return(apps)
}
apps <- import_custom("9.13 scatdat.csv")

schools <- unique(apps$University.Name)
o <- order(schools)
schools <- schools[o]
high_schools <- unique(apps$High.School)
N <- length(schools)

# Define static graphing parameters as global variables to use in the plot

xlimits <-
  as.vector(c(1300, 1603), "numeric") #add some "cushion" to the right endpoint to prevent points from being jittered off
ylimits <- as.vector(c(95, 120), "numeric")
ptsx <- as.numeric(c(1575,
                     1540,
                     1505))
ptsy <- as.numeric(c(100, 100, 100))
ptslabel <- as.character(c("A", "B", "C"))
pts <- data_frame(Point = ptslabel, X = ptsx, Y = ptsy)
concordancexmins <-
  as.vector(c(1310,
              1390,
              1450,
              1520,
              1590),
            "numeric")
concordancexmaxs <- vector("numeric", length = 9)
concordancexmaxs <- c(1350,
                      1420,
                      1490,
                      1560,
                      1601)
concordanceymins <-
  as.vector(rep(ylimits[1], 5), "numeric")
concordanceymaxs <-
  as.vector(rep(ylimits[2], 5), "numeric")
concordancemidpts <-
  as.vector(c(1330,
              1370,
              1405,
              1435,
              1470,
              1505,
              1540,
              1575,
              1595),
            "numeric")
concordanceys <- c(rep(97, 9), rep(118, 9))
concordancelabels <-
  as.vector(c("28", "29", "30", "31", "32", "33", "34", "35", "36"),
            "character")
concordance <-
  data_frame(
    xmin = concordancexmins,
    xmax = concordancexmaxs,
    ymin = concordanceymins,
    ymax = concordanceymaxs
  )
labeller <-
  data_frame(ACT = c(rep(concordancelabels, 2)),
             xmid = c(rep(concordancemidpts, 2)),
             ymid = concordanceys)
ybreaks <- as.vector(c(seq(100, 120, 5)), "numeric")
shapes <- as.vector(c(19, 17))
names(shapes) <- c("circle", "triangle")
plot.list <- vector("list", N)
legenddat <- data_frame(
  RSAT = rep(c(1400), 5),
  TOEFL = rep(c(110), 5),
  Plan = factor(
    x = c("RD", "EA", "ED", "ED2", "REA"),
    ordered = TRUE
  ),
  Result = c("Accepted", "Rejected", "Waitlisted", "Accepted", "Rejected")
)
legenddat <- tbl_df(legenddat)

# Now use the dataframe legenddat to define our base graph p_0. The ggplot2 package allows you to build
# graphics step by step. This first step is a very basic plot that only has a dummy geom, geom_blank.
# geom_blank insures that each factor shows up on the legend, but it doesn't put anything on the plot itself
# Later, the filtered plot data and parameters that react to user input
# will be added to this base plotter. This way, the whole plot is not generated every time an input changes,
# only the points are replotted

p_0 <- ggplot() +
  geom_blank(data = legenddat,
             # this puts nothing on the graph, but makes the legend consistent, even if there are no points
             aes(
               x = RSAT,
               y = TOEFL,
               color = Result,
               fill = Result,
               shape = Plan
             )) +
  geom_rect(
    # this puts static, grey rectangles on the plot to indicate ACT ranges
    data = concordance,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.2,
    show.legend = FALSE,
    inherit.aes = FALSE,
    color = 'grey60'
  )  +
  geom_text(
    data = labeller,
    aes(x = xmid, y = ymid, label = ACT),
    size = 4.5,
    vjust = 0,
    hjust = 0,
    nudge_x = -3,
    colour = "#535353",
    fontface = "bold"
  ) +
  labs(x = "RSAT by ACT & SAT Concordance", y = "TOEFL") +
  coord_cartesian(xlim = xlimits,
                  ylim = ylimits,
                  expand = FALSE) +
  scale_x_continuous(
    limits = xlimits,
    breaks = c(seq(1300, 1600, 20)),
    minor_breaks = c(seq(1300, 1600, 10))
  ) +
  scale_y_continuous(limits = ylimits,
                     breaks = ybreaks)  +
  scale_shape_manual(values = c(22, 24, 25, 21, 23)) +
  scale_color_manual(values = c("limegreen", "red2", "royalblue3")) +
  scale_fill_manual(values = c("limegreen", "red2", "royalblue3"))  +
  theme_fivethirtyeight(base_size = 18)

# This function creates and formats the plot data for a given college
# It is used later to dynamically generate plot data in response to input

make_plot_data <- function(collegename) {
  plot_data <-
    apps %>%
    filter(University.Name == collegename) %>%
    select(
      Applicant = Full.Name,
      High_School = High.School,
      Matriculation,
      Region = Market,
      Year = Appcon.Year,
      Type,
      Independent,
      Plan,
      Result,
      TOEFL,
      RSAT,
      ACT = ACT.Con.,
      CR
    )
  for (i in 4:9) {
    plot_data[[i]] <- as.factor(plot_data[[i]])
  }
  
  plot_data[4] <-
    factor(
      plot_data[[4]],
      levels = c(
        "Beijing/Tianjin",
        "Other China",
        "International Boarding School"
      ),
      ordered = TRUE
    )
  plot_data[5] <-
    factor(
      plot_data[[5]],
      levels = c("2018",
                 "2017",
                 "2016",
                 "2015",
                 "2014",
                 "2013",
                 "2012"),
      ordered = TRUE
    )
  plot_data[6] <-
    factor(plot_data[[6]],
           levels = c("Non-Binding",
                      "Binding"),
           ordered = TRUE)
  plot_data[7] <-
    factor(
      plot_data[[7]],
      levels = c("Official",
                 "Independent",
                 "Independent - Unsupported"),
      ordered = TRUE
    )
  plot_data[8] <-
    factor(plot_data[[8]],
           levels = c("RD",
                      "EA",
                      "ED",
                      "ED2",
                      "REA"),
           ordered = TRUE)
  plot_data[9] <-
    factor(
      plot_data[[9]],
      levels = c("Accepted",
                 "Waitlisted",
                 "Rejected"),
      ordered = TRUE
    )
  return(plot_data)
}

# lookup table to be used later

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

# Start the server

shinyServer(function(input, output) {
  # The reactive() function tells the server to reload a chunk of code when
  # inputs are changed by monitoring the input values of the variables defined in the ui
  # The following are reactive datasets w/ logic to respond to inputs
  
  rsat_input <- reactive({
    switch(
      input$testSelect,
      ACT = c(RSATlookup$RSAT[[match(input$ACT.range[[1]], RSATlookup$ACT)]],
              RSATlookup$RSAT[[match(input$ACT.range[[2]], RSATlookup$ACT)]]),
      RSAT = c(input$RSAT.range[[1]], input$RSAT.range[[2]])
    )
  })
  
  # Create reactive datasets for graphing and generating datatables
  
  dataInput <- lapply(c(1:3), function(nr) {
    reactive({
      df_0 <- switch(
        nr,
        tbl_df(make_plot_data(input$schoolfilter)),
        tbl_df(make_plot_data(input$schoolfilter1)),
        tbl_df(make_plot_data(input$schoolfilterComp))
      )
	  
      if(nr==2) {
	  RSAT_bounds = input$RSAT.range1
	  TOEFL_bounds = input$TOEFL.range1
	  appregion = input$app.region1
	  appyear = input$app.year1
	  appind = input$independent1
	  apphs = input$hsfilter1
	  } else {
	  RSAT_bounds = rsat_input()
	  TOEFL_bounds = input$TOEFL.range
      appregion = input$app.region
	  appyear = input$app.year
	  appind = input$independent
	  apphs = input$hsfilter
	  }
	  
      # the following code tells the server what data to use in response to user inputs
      # The strategy here is to keep defining new data frames after every step
      # If a condition is met, the data is filtered, if not, it is copied as is
      
      # Check score range
      
      df_0 %>%
        filter(RSAT_bounds[[1]] <= RSAT &
                 RSAT <= RSAT_bounds[[2]]) %>%
        filter(TOEFL_bounds[[1]] <= TOEFL &
                 TOEFL <= TOEFL_bounds[[2]]) -> df_1
      # Check International/China-based/both
      
      if (!(1 %in% appregion)) {
        df_2 <- df_1 %>% filter(Region != "Beijing/Tianjin")
      } else
        df_2 <- df_1
      if (!(2 %in% appregion)) {
        df_3 <- df_2 %>% filter(Region != "Other China")
      } else
        df_3 <- df_2
      if (!(3 %in% appregion)) {
        df_4 <- df_3 %>% filter(Region != "International Boarding School")
      } else
        df_4 <- df_3
      # Check Application year
      if (!(1 %in% appyear)) {
        df_5 <- df_4 %>% filter(Year != "2013")
      } else
        df_5 <- df_4
      if (!(2 %in% appyear)) {
        df_6 <- df_5 %>% filter(Year != "2014")
      } else
        df_6 <- df_5
      if (!(3 %in% appyear)) {
        df_7 <- df_6 %>% filter(Year != "2015")
      } else
        df_7 <- df_6
      if (!(4 %in% appyear)) {
        df_8 <- df_7 %>% filter(Year != "2016")
      } else
        df_8 <- df_7
      if (!(5 %in% appyear)) {
        df_9 <- df_8 %>% filter(Year != "2017")
      } else
        df_9 <- df_8
      if (!(6 %in% appyear)) {
        df_10 <- df_9 %>% filter(Year != "2018")
      } else
        df_10 <- df_9
      if (appind == FALSE) {
        df_11 <- df_10 %>% filter(Independent == "Official")
      } else
        df_11 <- df_10
      
      #Check High School
      
      if (is.null(apphs)) {
        df_12 <- df_11
      } else
        df_12 <- df_11 %>% filter(High_School %in% apphs)
      
      df_12
      
    })
  })
  
  # Create reactive tables to show filtered data in the summary tables
  tables <- lapply(c(1:3), function(nr) {
    reactive({
      if (nr == 1) {
        n <- match(input$schoolfilter, schools)
        dat <- dataInput[[1]]()
      } else if (nr == 2) {
        n <- match(input$schoolfilter1, schools)
        dat <- dataInput[[2]]()
      } else {
        n <- match(input$schoolfilterComp, schools)
        dat <- dataInput[[3]]()
      }
      apply.tot <- nrow(dat)
      adat <- filter(dat, Result == "Accepted")
      accept.tot <- nrow(adat)
      abinddat <- filter(adat, Type == "Binding")
      accept.ed <- nrow(abinddat)
      anonbinddat <- filter(adat, Type == "Non-Binding")
      accept.rd <- nrow(anonbinddat)
      binddat <- filter(dat, Type == "Binding")
      apply.ed <- nrow(binddat)
      nonbinddat <- filter(dat, Type == "Non-Binding")
      apply.rd <- nrow(nonbinddat)
      totals <-
        c(apply.tot, accept.tot, round(accept.tot / apply.tot, 2))
      binding <-
        c(apply.ed, accept.ed, round(accept.ed / apply.ed, 2))
      nonbinding <-
        c(apply.rd, accept.rd, round(accept.rd / apply.rd, 2))
      collabels1 <-
        as.vector(c("Applicants", "Accepted", "Admit Rate"), "character")
      rowlabels1 <-
        as.vector(c("Total", "Binding (ED/ED2/REA)", "Non-Binding (RD/EA)"),
                  "character")
      rawhistory <-
        matrix(
          rbind(totals, binding, nonbinding),
          ncol = 3,
          nrow = 3,
          dimnames = list(rowlabels1, collabels1)
        )
      rsat.quartile <-
        quantile(adat$RSAT, na.rm = TRUE, names = FALSE)
      rsat.average <- mean(adat$RSAT, na.rm = TRUE)
      rsat.sd <- sd(adat$RSAT, na.rm = TRUE)
      cr.quartile <-
        quantile(adat$CR, na.rm = TRUE, names = FALSE)
      cr.average <- mean(adat$CR, na.rm = TRUE)
      cr.sd <- sd(adat$CR, na.rm = TRUE)
      act.quartile <-
        quantile(adat$ACT, na.rm = TRUE, names = FALSE)
      act.average <- mean(adat$ACT, na.rm = TRUE)
      act.sd <- sd(adat$ACT, na.rm = TRUE)
      toefl.quartile <-
        quantile(adat$TOEFL, na.rm = TRUE, names = FALSE)
      toefl.average <- mean(adat$TOEFL, na.rm = TRUE)
      toefl.sd <- sd(adat$TOEFL, na.rm = TRUE)
      #combine stats into a table
      rsat.all <-
        as.vector(
          c(
            rsat.quartile[[1]],
            rsat.quartile[[2]],
            rsat.quartile[[3]],
            rsat.average,
            rsat.quartile[[4]],
            rsat.quartile[[5]],
            rsat.sd
          ),
          "numeric"
        )
      cr.all <-
        as.vector(
          c(
            cr.quartile[[1]],
            cr.quartile[[2]],
            cr.quartile[[3]],
            cr.average,
            cr.quartile[[4]],
            cr.quartile[[5]],
            cr.sd
          ),
          "numeric"
        )
      act.all <-
        as.vector(
          c(
            act.quartile[[1]],
            act.quartile[[2]],
            act.quartile[[3]],
            act.average,
            act.quartile[[4]],
            act.quartile[[5]],
            act.sd
          ),
          "numeric"
        )
      toefl.all <-
        as.vector(
          c(
            toefl.quartile[[1]],
            toefl.quartile[[2]],
            toefl.quartile[[3]],
            toefl.average,
            toefl.quartile[[4]],
            toefl.quartile[[5]],
            toefl.sd
          ),
          "numeric"
        )
      collabels2 <-
        as.vector(c("TOEFL", "CR", "RSAT", "ACT"), mode = "character")
      rowlabels2 <-
        as.vector(c(
          "Low",
          "25th",
          "Median",
          "Average",
          "75th",
          "High",
          "S.Dev"
        ),
        "character")
      
      # Now that the data is separated into easy to work with vectors, we re-combine
      # them into matrices
      
      admitprofile <-
        matrix(
          data = rbind(
            round(toefl.all, 1),
            round(cr.all),
            round(rsat.all),
            round(act.all, 1)
          ),
          nrow = 7,
          ncol = 4,
          byrow = TRUE,
          dimnames = list(rowlabels2, collabels2)
        )
      list(rawhistory, admitprofile)
    })
    
  })
  
  # create reactive datasets based on click and brush (highlight) inputs
  
  clickInput <- reactive({
    clickdat <- nearPoints(dataInput[[1]](),
                           input$plot_click,
                           threshold = 10)
    clickdat <-
      select(clickdat,
             Result,
             Plan,
             Year,
             Region,
             Matriculation,
             TOEFL,
             RSAT,
             ACT)
    clickdat
  })
  
  brushInput <- reactive({
    brushdat <- brushedPoints(dataInput[[1]](),
                              input$plot_brush_2)
    apply.tot <- nrow(brushdat)
    abrushdat <- filter(brushdat, Result == "Accepted")
    accept.tot <- nrow(abrushdat)
    abinddat <- filter(abrushdat, Type == "Binding")
    accept.ed <- nrow(abinddat)
    anonbinddat <- filter(abrushdat, Type == "Non-Binding")
    accept.rd <- nrow(anonbinddat)
    binddat <- filter(brushdat, Type == "Binding")
    apply.ed <- nrow(binddat)
    nonbinddat <- filter(brushdat, Type == "Non-Binding")
    apply.rd <- nrow(nonbinddat)
    totals <-
      c(apply.tot, accept.tot, round(accept.tot / apply.tot, 2))
    binding <-
      c(apply.ed, accept.ed, round(accept.ed / apply.ed, 2))
    nonbinding <-
      c(apply.rd, accept.rd, round(accept.rd / apply.rd, 2))
    collabels1 <-
      as.vector(c("Applicants", "Accepted", "Admit Rate"), "character")
    rowlabels1 <-
      as.vector(c("Total", "Binding (ED/ED2/REA)", "Non-Binding (RD/EA)"),
                "character")
    brushtable <-
      matrix(
        rbind(totals, binding, nonbinding),
        ncol = 3,
        nrow = 3,
        dimnames = list(rowlabels1, collabels1)
      )
    brushtable
  })
  
  # Add a listener so that when a double-click happens, check if there's a brush (highlight) on the plot.
  # If so, zoom to the brush (highlight) bounds; if not, reset the zoom.
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Add listener to check if the side by side view is selected
  
  sideBysideSelected <- reactive({
    input$scatterplotTab == "sideBySide"
  })
  
  # Render plots and tables to be displayed using the reactive objects
  
  output$compareSelector <- renderUI({
    if(sideBysideSelected() == TRUE) {
      selectizeInput(
        "schoolfilterComp",
        "Secondary College to Compare",
        choices = schools,
        multiple = FALSE,
        selected = "American University"
      )
    }
  })
  # Render the summary data tables
  
  output$rawhistory <-
    DT::renderDataTable(tables[[1]]()[[1]],
                        options = list(
                          dom = "t",
                          style = 'bootstrap',
                          ordering = FALSE
                        ))
  output$admitprofile <-
    DT::renderDataTable(tables[[1]]()[[2]],
                        options = list(
                          dom = "t",
                          style = 'bootstrap',
                          ordering = FALSE
                        ))
  output$rawhistory1 <-
    DT::renderDataTable(tables[[2]]()[[1]],
                        options = list(
                          dom = "t",
                          style = 'bootstrap',
                          ordering = FALSE
                        ))
  output$admitprofile1 <-
    DT::renderDataTable(tables[[2]]()[[2]],
                        options = list(
                          dom = "t",
                          style = 'bootstrap',
                          ordering = FALSE
                        ))
  output$tbl = DT::renderDataTable(dataInput[[2]](),
                                   options = list(paging = FALSE,
                                                  style = 'bootstrap'))
  output$rawhistoryComp1 <-
    DT::renderDataTable(tables[[1]]()[[1]],
                        options = list(
                          dom = "t",
                          ordering = FALSE
                        ))
  output$rawhistoryComp1Label <- renderText(input$schoolfilter)
  output$rawhistoryComp2 <-
    DT::renderDataTable(tables[[3]]()[[1]],
                        options = list(
                          dom = "t",
                          ordering = FALSE
                        ))
  output$rawhistoryComp2Label <- renderText(input$schoolfilterComp)
  
  # Render data tables for click response
  
  output$brush_info <- DT::renderDataTable(brushInput(),
                                           options = list(dom = "t",
                                                          style = 'bootstrap'))
  
  output$click_info <- DT::renderDataTable(clickInput(),
                                           options = list(
                                             dom = "tf",
                                             paging = FALSE,
                                             style = 'bootstrap'
                                           ))
  
  # Render the scatterplots and add points based on the status of the reactive datasets
  
  # Render five different scatterplots by looping a plotting function
  # Conditional logic is used to make minor differences to each plot
  
  lapply(1:5, function(nr) {
    output[[paste0("plot", nr)]] <- renderPlot({
      # plot 4 uses data from the comparison school
      if (nr == 4) {
        dat <- dataInput[[3]]()
        n <- match(input$schoolfilterComp, schools)
      } else {
        dat <- dataInput[[1]]()
        n <- match(input$schoolfilter, schools)
      }
      
      title <- schools[[n]]
      yourdot <- data_frame(RSAT = input$yourRSAT,
                            TOEFL = input$yourTOEFL)
      
      p <- p_0 + geom_point(
        data = dat,
        aes(
          x = RSAT,
          y = TOEFL,
          color = Result,
          fill = Result,
          shape = Plan
        ),
        size = 4,
        alpha = .6,
        position = position_jitter(width = 2, height = .2)
      ) +
        geom_point(
          data = yourdot,
          aes(x = RSAT, y = TOEFL),
          fill = "yellow1",
          color = "mediumorchid3",
          shape = 21,
          size = 5
        ) +
        ggtitle(title) +
        coord_cartesian(xlim = ranges$x,
                        ylim = ranges$y,
                        expand = FALSE)
      
      # remove legend from plots 3 and 4 (comparison views) to improve viewability
      if (nr %in% c(3, 4)) {
        p <- p + theme(legend.position = "none")
      }
      
      # add facet grid to plot 5 (year by year) and add a secondary axis
      if (nr == 5) {
        p <- p + facet_grid(Year ~ .) +
          scale_x_continuous(
            limits = xlimits,
            breaks = c(seq(1300, 1600, 20)),
            minor_breaks = c(seq(1300, 1600, 10)),
            sec.axis = dup_axis()
          ) +
          theme(strip.text.y = element_text(size = 28,
                                            colour = "#535353"))
      }
      
      p
      
    })
  })
  
  #Close the server function
})
