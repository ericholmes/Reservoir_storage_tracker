library(shiny)

shinyUI(fluidPage(
  headerPanel("Reservoir Storage Tracker"),
  verticalLayout(
        wellPanel(selectInput(inputId = "selectRes", label = "Choose Reservoir", c("FMD","HHL","SLS","DNN", "FOL")),
            dateRangeInput("range", "Date Range:", start = "2000-01-01", end = Sys.Date(),
                           min = "2000-01-01",max = Sys.Date(), startview = "year"),
            checkboxInput(inputId = "check", label = "Display vertical line", value = FALSE),
            
            conditionalPanel("input.check == true", 
                             checkboxInput(inputId = "level", label = "Display horizontal line", value = FALSE),
                             dateInput(inputId = "vline", label = "Vertical line date",value = Sys.Date() - 11)
                             )
            ),
  
  mainPanel(plotOutput("Res"), width=12),
  textOutput("Current"),
  conditionalPanel("input.check == true", textOutput("vlinetext"))
  
)))
