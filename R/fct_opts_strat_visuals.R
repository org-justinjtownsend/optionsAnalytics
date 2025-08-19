#' Options Strategy Visualization Functions
#'
#' @description Functions for creating interactive tables of options strategies
#' to help determine the best strategies to use.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Load required packages
if (!requireNamespace("reactable", quietly = TRUE)) {
  install.packages("reactable")
}

library(reactable)

#' Create Interactive Options Strategy Table
#'
#' @description Creates an interactive, extensible table using the reactable() package
#' for displaying options strategies data.
#'
#' @param data A data frame containing options strategies data (e.g., output of create_vertical_spreads()$vsData)
#' @param columns Optional list of columns to display. If not provided, uses all column names from data
#' @param searchable Logical. Whether to enable search functionality (default: TRUE)
#' @param sortable Logical. Whether to enable sorting (default: TRUE, uses reactable default behavior)
#' @param striped Logical. Whether to use striped rows for better visibility (default: TRUE)
#' @param compact Logical. Whether to use compact display for better visibility with many rows (default: TRUE)
#' @param showDetail Logical. Whether to show expandable detail rows (default: FALSE)
#' @return A reactable widget
#' @export
#' @examples
#' \dontrun{
#' # Create table with default settings
#' table <- opts_strat_tbl(data = vs_result$vsData)
#' 
#' # Create table with custom columns
#' table <- opts_strat_tbl(
#'   data = vs_result$vsData,
#'   columns = list(dte = "Days to Expiry", vsType = "Strategy Type"),
#'   searchable = TRUE,
#'   striped = TRUE,
#'   compact = TRUE
#' )
#' }
opts_strat_tbl <- function(data, columns = NULL, searchable = TRUE, 
                          sortable = TRUE, striped = TRUE, compact = TRUE, 
                          showDetail = FALSE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (nrow(data) == 0) {
    warning("data has no rows")
    return(reactable(data.frame(Message = "No data available")))
  }
  
  # Use all columns if none specified
  if (is.null(columns)) {
    columns <- names(data)
  }
  
  # Validate that specified columns exist in data
  if (!all(columns %in% names(data))) {
    missing_cols <- setdiff(columns, names(data))
    stop("The following columns are not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Subset data to only include specified columns
  display_data <- data[, columns, drop = FALSE]
  
  # Create the reactable
  reactable(
    display_data,
    searchable = searchable,
    sortable = sortable,
    striped = striped,
    compact = compact,
    defaultColDef = colDef(
      headerStyle = list(backgroundColor = "#f8f9fa", fontWeight = "bold"),
      align = "left"
    ),
    theme = reactableTheme(
      headerStyle = list(
        backgroundColor = "#f8f9fa",
        borderBottom = "2px solid #dee2e6",
        fontWeight = "bold"
      )
    )
  )
}
