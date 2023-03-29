#' getDatafromSource UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_getDatafromSource_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_6(
    tags$div(
      align = "center",
      tags$img(
        src = "www/favicon.ico", width = "50%", align = "center"
      )
    )
  )
 
  )
}
    
#' getDatafromSource Server Functions
#'
#' @noRd 
mod_getDatafromSource_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_getDatafromSource_ui("getDatafromSource_1")
    
## To be copied in the server
# mod_getDatafromSource_server("getDatafromSource_1")
