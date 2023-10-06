library(shiny)
library(shinipsum)
library(DT)

ui <- fluidPage(
  h2("A Random DT"),
  DTOutput("data_table"),
  h2("A Random Plot"),
  plotOutput("plot"),
  h2("A Random Text"),
  tableOutput("text")
)

server <- function(input, output, session) {
  output$data_table <- DT::renderDT({random_DT(5,5)})
  output$plot <- renderPlot({random_ggplot()})
  output$text <- renderText({random_text(nwords = 50)})
}

shinyApp(ui, server)