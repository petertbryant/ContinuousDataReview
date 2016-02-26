library(shiny)
library(reshape2)
library(ggplot2)
library(googleVis)

#runApp("T:/TempTrends/Check_shinyapp/",host='0.0.0.0',port=3169)

slu <- read.csv('data/R_lookup.csv', stringsAsFactors = FALSE)
slu$TimeD <- as.POSIXct(strptime(slu$TimeD, format = '%Y-%m-%d %H:%M:%S'))
slu$TimeR <- as.POSIXct(strptime(slu$TimeR, format = '%Y-%m-%d %H:%M:%S'))
slu$SampledD <- as.POSIXct(strptime(slu$SampledD, format = '%Y-%m-%d %H:%M:%S'))
slu$SampledR <- as.POSIXct(strptime(slu$SampledR, format = '%Y-%m-%d %H:%M:%S'))

all_cols <- c(rep('red',4), 
              "black", "#FF9B4C", "#7277C1", "#998B6B", 
              "black", "#FF9B4C", "#7277C1", "#998B6B")
names(all_cols) <- as.vector(outer(c('A','B','C','E'), c('TRUE','FALSE',"NA"), 
                                   paste, sep=" "))
all_flls <- c("black", "#FF9B4C", "#7277C1", "#998B6B", 
              "black", "#FF9B4C", "#7277C1", "#998B6B",
              "black", "#FF9B4C", "#7277C1", "#998B6B")
names(all_flls) <- as.vector(outer(c('A','B','C','E'), c('FALSE','TRUE',"NA"), 
                                   paste, sep=" "))

shinyServer(function(input, output, session) {
  output$selectStation <- renderUI({
    selectInput("selectStation", label = h3("Select Station"),
                choices = sort(slu$myfiles))  
  })
  
  #   output$selectYear <- renderUI({
  #     selectInput("selectYear", "Select Year",
  #                 choices = unique(slu[which(slu$LasarID == input$selectStation),'Year']))
  #   })
  
  #   output$selectRange <- renderUI({
  #     new_data <- DataUse()
  #     
  #     sliderInput("selectRange", "Select Date Range", 
  #                 min = min(new_data$Sampled),
  #                 max = max(new_data$Sampled),
  #                 value = c(min(new_data$Sampled),
  #                           max(new_data$Sampled)),
  #                 timeFormat = "%F %T")
  #   })
  #   
  #   output$selectRange2 <- renderUI({
  #     sliderInput('selectRange2', "Select Date Range to plot",
  #                 min = input$selectRange[1],
  #                 max = input$selectRange[2],
  #                 value = c(input$selectRange[1],
  #                           input$selectRange[2]))
  #   })
  
  output$displayAudit <- renderUI({
    #     df <- slu[which(slu$LasarID == input$selectStation 
    #         & slu$Year == input$selectYear),]
    df <- slu[which(slu$myfiles == input$selectStation),]
    disp <- data.frame("Source" = c('Audit','Audit','Obs','Obs'),
                       "Datetime" = c(df$TimeD, df$TimeR, df$SampledD, df$SampledR),
                       "Result" = c(df$DTemp, df$RTemp, df$ResultD, df$ResultR),
                       "Grade_result" = c("", "", df$mydgradeD, df$myrgradeR))
    disp <- disp[order(disp$Datetime),]
    
    output$intermediate <- renderDataTable(disp, 
                                           options = list(paging = FALSE,
                                                          searching = FALSE))
    dataTableOutput("intermediate")
  })
  
  DataUse <- reactive({
    #     fname <- paste0('data/',slu[which(slu$LasarID == input$selectStation & 
    #                    slu$Year == input$selectYear), 'myfiles'])
    fname <- paste0('data/',input$selectStation)
    data <- read.csv(fname, stringsAsFactors = FALSE)
    data$Sampled <- as.POSIXct(strptime(data$Sampled, format = '%Y-%m-%d %H:%M:%S'))
    data
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"), 
                    as.POSIXct(brush$xmax, origin = "1970-01-01"))
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$plot <- renderPlot({
    new_data <- DataUse()
    new_data$comb_fac <- as.factor(paste(new_data$field_audit_grade, 
                                         new_data$anomaly))
    labs <- levels(new_data$comb_fac)
    cols = all_cols[names(all_cols) %in% labs]
    flls = all_flls[names(all_flls) %in% labs]
    ltitle <- "Field Audit\nGrade and\nAnomaly Check"
    
    p <- ggplot(data = new_data) + 
      geom_point(aes( x= Sampled, y = Result, fill = comb_fac,
                      col = comb_fac), shape = 21, size = 3) +
      scale_fill_manual(values = flls,
                        name = ltitle,
                        labels = labs) +
      scale_color_manual(name = ltitle, 
                         values = cols,
                         labels = labs) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) 
    #+ xlim(input$selectRange2[1], input$selectRange2[2])
    
    #         dr_data <- slu[which(slu$LasarID == input$selectStation & 
    #                                slu$Year == input$selectYear),]
    dr_data <- slu[which(slu$myfiles == input$selectStation),]
    
    p <- p + 
      geom_vline(xintercept = as.numeric(dr_data$TimeD)) +
      geom_vline(xintercept = as.numeric(dr_data$TimeR)) +
      geom_point(data = dr_data, aes(x = TimeR, y = RTemp), 
                 color = 'green', size = 3) +
      geom_point(data = dr_data, aes(x = TimeD, y = DTemp),
                 color = 'green', size = 3) +
      ggtitle(unique(dr_data$myfiles))
    
    p
  })
})