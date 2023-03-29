#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # List the first level callModules here
  print(get_golem_options("time"))
  
  # Your application server logic
  callModule(mod_getDatafromSource_server, "getDatafromSource_ui")
}
