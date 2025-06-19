#' Options Data Extraction Helper Functions
#'
#' @description A set of helpers for loading and processing options data from the database.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#' Extract Standardized Options Data
#'
#' @description Extracts options data from the PostgreSQL database and returns it in a standardized format.
#' This function connects to the database, queries the opts_hist table, and returns a data frame with
#' standardized column names.
#'
#' @param expirDate Optional. The expiration date to filter by in 'YYYY-MM-DD' format as a character string.
#' Example: expirDate <- as.character('2024-09-20'). If NULL, returns all expiration dates.
#'
#' @return A data frame containing the standardized options data with the following columns:
#' \itemize{
#'   \item ticker - The stock ticker symbol
#'   \item tradeDate - The date of the trade
#'   \item expirDate - The expiration date
#'   \item dte - Days to expiration
#'   \item strike - The strike price
#'   \item stockPrice - The current stock price
#'   \item callValue - Call option value
#'   \item callVolume - Call option volume
#'   \item callOpenInterest - Call option open interest
#'   \item callBidSize - Call option bid size
#'   \item callAskSize - Call option ask size
#'   \item callBidPrice - Call option bid price
#'   \item callAskPrice - Call option ask price
#'   \item putValue - Put option value
#'   \item putVolume - Put option volume
#'   \item putOpenInterest - Put option open interest
#'   \item putBidSize - Put option bid size
#'   \item putAskSize - Put option ask size
#'   \item putBidPrice - Put option bid price
#'   \item putAskPrice - Put option ask price
#'   \item smvVol - Stock market volatility
#'   \item callMidIv - Call option mid implied volatility
#'   \item putMidIv - Put option mid implied volatility
#'   \item delta - Option delta
#'   \item gamma - Option gamma
#'   \item theta - Option theta
#'   \item vega - Option vega
#'   \item rho - Option rho
#'   \item phi - Option phi
#'   \item spotPrice - The spot price
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all options data
#' options_data <- opt_extract_std()
#' 
#' # Get options data for a specific expiration date
#' options_data <- opt_extract_std(expirDate = as.character('2024-09-20'))
#' }
opt_extract_std <- function(expirDate = NULL) {
  # Load required packages
  if (!requireNamespace("DBI", quietly = TRUE)) {
    install.packages("DBI")
  }
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    install.packages("RPostgres")
  }
  
  library(DBI)
  library(RPostgres)
  
  # Validate expirDate format if provided
  if (!is.null(expirDate)) {
    # Check if expirDate is a character
    if (!is.character(expirDate)) {
      stop("expirDate must be a character string in 'YYYY-MM-DD' format")
    }
    
    # Validate character format (YYYY-MM-DD)
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", expirDate)) {
      stop("expirDate must be in 'YYYY-MM-DD' format as a character string")
    }
  }
  
  # Connect to database
  con <- connect_to_db(schema = "opts_hist", table = "opts_hist")
  
  if (is.null(con)) {
    stop("Failed to connect to database")
  }
  
  # Define the base query with explicit column mapping
  base_query <- "
    SELECT 
      ticker,
      \"tradeDate\",
      \"expirDate\",
      dte,
      strike,
      \"stockPrice\",
      \"callValue\",
      \"callVolume\",
      \"callOpenInterest\",
      \"callBidSize\",
      \"callAskSize\",
      \"callBidPrice\",
      \"callAskPrice\",
      \"putValue\",
      \"putVolume\",
      \"putOpenInterest\",
      \"putBidSize\",
      \"putAskSize\",
      \"putBidPrice\",
      \"putAskPrice\",
      \"smvVol\",
      \"callMidIv\",
      \"putMidIv\",
      delta,
      gamma,
      theta,
      vega,
      rho,
      phi,
      \"spotPrice\"
    FROM opts_hist.opts_hist
  "
  
  # Add expiration date filter if provided
  if (!is.null(expirDate)) {
    query <- paste0(base_query, " WHERE \"expirDate\" = '", expirDate, "'")
  } else {
    query <- base_query
  }
  
  # Execute query and handle errors
  tryCatch({
    result <- dbGetQuery(con, query)
    
    # Explicitly rename columns to ensure consistent naming
    names(result) <- c(
      "ticker", "tradeDate", "expirDate", "dte", "strike", "stockPrice",
      "callValue", "callVolume", "callOpenInterest", "callBidSize", "callAskSize",
      "callBidPrice", "callAskPrice", "putValue", "putVolume", "putOpenInterest",
      "putBidSize", "putAskSize", "putBidPrice", "putAskPrice", "smvVol",
      "callMidIv", "putMidIv", "delta", "gamma", "theta", "vega", "rho", "phi",
      "spotPrice"
    )
    
    # Verify we got all expected columns
    expected_columns <- c(
      "ticker", "tradeDate", "expirDate", "dte", "strike", "stockPrice",
      "callValue", "callVolume", "callOpenInterest", "callBidSize", "callAskSize",
      "callBidPrice", "callAskPrice", "putValue", "putVolume", "putOpenInterest",
      "putBidSize", "putAskSize", "putBidPrice", "putAskPrice", "smvVol",
      "callMidIv", "putMidIv", "delta", "gamma", "theta", "vega", "rho", "phi",
      "spotPrice"
    )
    
    missing_columns <- setdiff(expected_columns, names(result))
    if (length(missing_columns) > 0) {
      stop("Missing columns in result: ", paste(missing_columns, collapse = ", "))
    }
    
    # Close connection
    dbDisconnect(con)
    
    return(result)
    
  }, error = function(e) {
    # Ensure connection is closed in case of error
    if (dbIsValid(con)) {
      dbDisconnect(con)
    }
    stop("Error extracting options data: ", conditionMessage(e))
  })
}

#' Get Option Details from IG API
#'
#' @description Retrieves detailed information for a specific option instrument using the IG API.
#' This is an internal helper function used by \code{opts_instrument_ig()} to fetch option details
#' for individual epic codes. It can also be used independently to get details for a single option.
#'
#' @param epic Character string specifying the epic code of the option instrument.
#' Epic codes follow the IG naming convention (e.g., "OP.D.SPX1.5500P.IP").
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item strike_price - The strike price of the option (numeric)
#'   \item option_type - The type of option ("C" for call, "P" for put)
#'   \item expiry_datetime - The expiration date and time in "YYYY-MM-DD HH:MM:SS.mmm" format
#'   \item underlyer - The underlying instrument symbol
#'   \item message - Status message ("success" if successful, error message if failed)
#' }
#'
#' @details This function wraps the \code{OptionsAnalytics::get_option_details()} function
#' and standardizes the output format. It handles the conversion of the expiry datetime
#' to a consistent string format and provides a standardized return structure.
#'
#' @note This function requires the OptionsAnalytics package to be installed and an active
#' IG API session to be established using \code{OptionsAnalytics::initiate_ig_session()}.
#'
#' @examples
#' \dontrun{
#' # Initialize IG session first
#' OptionsAnalytics::initiate_ig_session(env = "LIVE")
#' 
#' # Get details for a specific SPX option
#' details <- get_option_dtls("OP.D.SPX1.5500P.IP")
#' print(details)
#' 
#' # Get details for a call option
#' call_details <- get_option_dtls("OP.D.SPX2.5000C.IP")
#' print(call_details)
#' }
#'
#' @seealso \code{\link{opts_instrument_ig}} for batch processing of multiple options
#' @seealso \code{\link[OptionsAnalytics]{get_option_details}} for the underlying API call
#'
#' @noRd
get_option_dtls <- function(epic) {
  e <- OptionsAnalytics::get_option_details(epic)
  list(strike_price = e$strike_price,
       option_type = e$option_type,
       expiry_datetime = format(e$expiry_datetime, "%Y-%m-%d %H:%M:%OS3"),
       underlyer = e$underlyer,
       message = "success")
}

#' Create IG Option Instrument Names
#'
#' @description Creates and tests option instrument names following the IG naming convention.
#' This function generates epic names for options instruments and retrieves their details
#' using the OptionsAnalytics package.
#'
#' @param instrument Character string specifying the instrument (e.g., "SPX")
#' @param expiry_range Numeric vector or range of expiry values (e.g., seq(1, 5))
#' @param strike_price Numeric value for the strike price (default: 5500)
#' @param option_type Character specifying option type (default: "P" for put)
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item strike_price - The strike price of the option
#'   \item option_type - The type of option (call/put)
#'   \item expiry - The expiry datetime
#'   \item underlyer - The underlying instrument
#'   \item updated - Timestamp of when the data was retrieved
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get details for SPX options with expiries 1-5
#' spx_options <- opts_instrument_ig("SPX", seq(1, 5))
#' 
#' # Get details for SPX options with specific strike and call option
#' spx_calls <- opts_instrument_ig("SPX", seq(1, 3), strike_price = 5000, option_type = "C")
#' }
opts_instrument_ig <- function(instrument, expiry_range, strike_price = 5500, option_type = "P") {
  # Load required packages
  if (!requireNamespace("OptionsAnalytics", quietly = TRUE)) {
    stop("OptionsAnalytics package is required. Please install it using: devtools::install_github('zumthor86/OptionsAnalytics')")
  }
  
  # Loop through expiry range
  epics <- vector("character", length(expiry_range))
  
  for (i in seq_along(expiry_range)) {
    # Create epic name following IG convention
    epics[i] <- sprintf(
      "OP.D.%s%d.%d%s.IP",
      instrument,
      expiry_range[i],
      strike_price,
      option_type)
  }
  
  # Initialize dataframe with epics
  results <- data.frame(
    epic = epics,
    strike_price = NA,
    option_type = NA,
    expiry_datetime = NA,
    underlyer = NA,
    message = NA,
    updated = NA,
    stringsAsFactors = FALSE
  )
  
  # Try to get epic details
  for (i in 1:length(epics)) {
    result <- tryCatch({get_option_dtls(epics[i])}, error = function(e) {
      list(message = e$message)
    })
    results[i, names(result)] <- result
  }
  return(results)
}

