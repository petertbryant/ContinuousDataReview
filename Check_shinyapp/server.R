library(shiny)
library(reshape2)
library(ggplot2)
#library(googleVis)

#runApp("T:/TempTrends/Check_shinyapp/",host='0.0.0.0',port=3169)

all_cols <- c(rep('red',5), 
              "black", "#FF9B4C", "#7277C1", "#998B6B", "#C0C0C0",
              "black", "#FF9B4C", "#7277C1", "#998B6B", "#C0C0C0")
names(all_cols) <- as.vector(outer(c('A','B','C','E', "NA"), c('TRUE','FALSE',"NA"), 
                                   paste, sep=" "))
all_flls <- c("black", "#FF9B4C", "#7277C1", "#998B6B", "#C0C0C0", 
              "black", "#FF9B4C", "#7277C1", "#998B6B", "#C0C0C0",
              "black", "#FF9B4C", "#7277C1", "#998B6B", "#C0C0C0")
names(all_flls) <- as.vector(outer(c('A','B','C','E', "NA"), c('FALSE','TRUE',"NA"), 
                                   paste, sep=" "))

shinyServer(function(input, output, session) {
  output$selectStation <- renderUI({
    selectInput("selectStation", label = h3("Select Station"),
                choices = list.files("./data", pattern = "_.Rdata"))  
  })
  
  audit_data_reactive <- reactive({
    fname_audit <- gsub("[0-9]*[a-z]*[A-Z]*_.Rdata",
                        "AUDIT_INFO.Rdata", 
                        paste0('data/', input$selectStation))
    load(fname_audit)
    dr_info
    #ad <- read.csv(fname_audit, stringsAsFactors = FALSE)
    #ad$AUDIT_DATETIME <- as.POSIXct(strptime(ad$AUDIT_DATETIME, format = '%Y-%m-%d %H:%M:%S'))
    #ad
  })
  
  output$displayAudit <- renderUI({
    df <- audit_data_reactive()
    output$intermediate <- renderDataTable(df, 
                                           options = list(paging = FALSE,
                                                          searching = FALSE))
    dataTableOutput("intermediate")
  })
  
  DataUse <- reactive({
    fname <- paste0('data/',input$selectStation)
    load(fname)
    tmp_data
#     data <- read.csv(fname, stringsAsFactors = FALSE)
#     data$DATETIME <- as.POSIXct(strptime(data$DATETIME, format = '%Y-%m-%d %H:%M:%S'))
#     data
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
    new_data$comb_fac <- as.factor(paste(new_data$rDQL, 
                                         new_data$anomaly))
    labs <- levels(new_data$comb_fac)
    cols = all_cols[names(all_cols) %in% labs]
    flls = all_flls[names(all_flls) %in% labs]
    ltitle <- "Field Audit\nGrade and\nAnomaly Check"
    
    p <- ggplot(data = new_data) + 
      geom_point(aes( x= DATETIME, y = r, fill = comb_fac,
                      col = comb_fac), shape = 21, size = 3) +
      scale_fill_manual(values = flls,
                        name = ltitle,
                        labels = labs) +
      scale_color_manual(name = ltitle, 
                         values = cols,
                         labels = labs) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) 
    #+ xlim(input$selectRange2[1], input$selectRange2[2])
    
    dr_data <- audit_data_reactive()
    p <- p + 
      geom_vline(xintercept = as.numeric(dr_data[,'AUDIT_DATETIME'])) +
      geom_point(data = dr_data, aes(x = AUDIT_DATETIME, y = AUDIT_RESULT), 
                 color = 'green', size = 3) +
#       geom_point(data = dr_data, aes(x = TimeD, y = DTemp),
#                  color = 'green', size = 3) +
      ggtitle(unique(dr_data$myfiles))
    
    p
  })
})