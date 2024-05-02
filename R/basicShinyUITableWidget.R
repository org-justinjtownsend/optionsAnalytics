
library(shiny)
library(DT)
library(shinyWidgets)
library(dplyr)
# source("R/getOptionsChainData.R")

# joined <- getOC("AMZN")

ui <- fluidPage(
  titlePanel("title"),
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("picker"),
      actionButton("view", "View selection")),
  
  mainPanel(ui <-
              tableOutput("mytable"),
  ))
)

server <- function(input, output, session) {
  
  data <- reactive({
    joined
  })  
  
  output$picker <- renderUI({
    pickerInput(inputId = 'pick',
                label = '3. Choose variables',
                choices = colnames(data()),
                options = list(`actions-box` = TRUE), multiple = TRUE)
  })
  
  datasetInput <- eventReactive(input$view,{
    
    datasetInput <- data() %>% 
      dplyr::select(input$pick)
    
    return(datasetInput)
  })  
  
  output$mytable <- renderTable({
    datasetInput()
  })
  
}

shinyApp(ui, server)