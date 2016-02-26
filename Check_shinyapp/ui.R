library(shiny)

shinyUI(fluidPage(
  titlePanel("Continuous data check"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("selectStation"),
      # uiOutput("selectYear"),
      uiOutput("selectRange"),
      uiOutput("selectRange2")
    ),
    mainPanel(
      plotOutput('plot', dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 )
                 ),
      uiOutput("displayAudit")
    )
  )
))