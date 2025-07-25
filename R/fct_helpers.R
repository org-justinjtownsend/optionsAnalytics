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
#' standardized column names. The function is vectorized to efficiently handle multiple expiration dates
#' using a single SQL query with an IN clause.
#'
#' @param expirDate Optional. A vector of expiration dates to filter by in 'YYYY-MM-DD' format as character strings.
#' Example: expirDate <- c(as.character('2024-09-20'), as.character('2024-10-18')).
#' If NULL, returns all expiration dates. The function is vectorized and uses an efficient SQL IN clause.
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
#' options_data <- opts_extract_std()
#' 
#' # Get options data for a specific expiration date
#' options_data <- opts_extract_std(expirDate = as.character('2024-09-20'))
#' 
#' # Get options data for multiple expiration dates (vectorized)
#' options_data <- opts_extract_std(expirDate = c(as.character('2024-09-20'), as.character('2024-10-18')))
#' }
opts_extract_std <- function(expirDate = NULL) {
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
    # Check if expirDate is a character vector
    if (!is.character(expirDate)) {
      stop("expirDate must be a character vector in 'YYYY-MM-DD' format")
    }
    
    # Validate character format (YYYY-MM-DD) for each element
    for (date in expirDate) {
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
        stop("All expirDate elements must be in 'YYYY-MM-DD' format as character strings")
      }
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
    # Create IN clause for efficient vectorized querying
    # Properly escape and quote each date for SQL safety
    quoted_dates <- paste0("'", expirDate, "'")
    in_clause <- paste(quoted_dates, collapse = ", ")
    query <- paste0(base_query, " WHERE \"expirDate\" IN (", in_clause, ")")
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

#' Extract Historical Options Data
#'
#' @description Retrieves historical options data for specified expiration dates with user-friendly limits.
#' This function wraps around \code{opts_extract_std()} to provide historical analysis capabilities.
#' It sorts results by expiration date and trade date, calculates available history, and returns
#' the requested number of days of historical data.
#'
#' @param expirDate A vector of expiration dates to filter by in 'YYYY-MM-DD' format as character strings.
#' Example: expirDate <- c(as.character('2024-09-20'), as.character('2024-10-18')).
#' @param hist_days An integer specifying the number of days of history to retrieve.
#' If the available history is less than hist_days, all available rows will be returned.
#'
#' @return A data frame containing historical options data with the same columns as \code{opts_extract_std()},
#' sorted by expirDate and tradeDate in ascending order. The data is limited to the requested number
#' of historical days.
#'
#' @details This function:
#' \itemize{
#'   \item Extracts data using \code{opts_extract_std()}
#'   \item Sorts results by expirDate and tradeDate (ascending)
#'   \item Calculates and reports the number of days of history available
#'   \item Returns the requested number of days (or all available if less than requested)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get 30 days of history for a specific expiration date
#' hist_data <- opts_extract_hist(
#'   expirDate = as.character('2024-09-20'),
#'   hist_days = 30
#' )
#' 
#' # Get 60 days of history for multiple expiration dates
#' hist_data <- opts_extract_hist(
#'   expirDate = c(as.character('2024-09-20'), as.character('2024-10-18')),
#'   hist_days = 60
#' )
#' }
opts_extract_hist <- function(expirDate, hist_days) {
  # Validate parameters
  if (missing(expirDate) || is.null(expirDate)) {
    stop("expirDate parameter is required")
  }
  
  if (missing(hist_days) || is.null(hist_days)) {
    stop("hist_days parameter is required")
  }
  
  if (!is.numeric(hist_days) || length(hist_days) != 1 || hist_days <= 0) {
    stop("hist_days must be a positive integer")
  }
  
  if (!is.character(expirDate)) {
    stop("expirDate must be a character vector in 'YYYY-MM-DD' format")
  }
  
  # Validate character format (YYYY-MM-DD) for each element
  for (date in expirDate) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      stop("All expirDate elements must be in 'YYYY-MM-DD' format as character strings")
    }
  }
  
  # Extract data using opts_extract_std()
  options_data <- opts_extract_std(expirDate = expirDate)
  
  # Check if we got any data
  if (nrow(options_data) == 0) {
    message("No data found for the specified expiration dates")
    return(options_data)
  }
  
  # Sort by expirDate and tradeDate ascending
  options_data <- options_data[order(options_data$expirDate, options_data$tradeDate), ]
  
  # Calculate available history for each expiration date
  for (exp_date in unique(options_data$expirDate)) {
    exp_data <- options_data[options_data$expirDate == exp_date, ]
    available_days <- length(unique(exp_data$tradeDate))
    
    message(sprintf(
      "Expiration date %s: %d days of history available",
      exp_date, available_days
    ))
  }
  
  # Get unique trade dates for each expiration date and limit to hist_days
  limited_data <- data.frame()
  
  for (exp_date in unique(options_data$expirDate)) {
    exp_data <- options_data[options_data$expirDate == exp_date, ]
    
    # Get unique trade dates for this expiration date
    unique_trade_dates <- unique(exp_data$tradeDate)
    unique_trade_dates <- sort(unique_trade_dates)
    
    # Limit to hist_days (or all available if less)
    if (length(unique_trade_dates) > hist_days) {
      # Take the most recent hist_days
      selected_dates <- tail(unique_trade_dates, hist_days)
    } else {
      # Take all available dates
      selected_dates <- unique_trade_dates
    }
    
    # Filter data to selected dates
    exp_limited <- exp_data[exp_data$tradeDate %in% selected_dates, ]
    limited_data <- rbind(limited_data, exp_limited)
  }
  
  # Sort the final result by expirDate and tradeDate
  limited_data <- limited_data[order(limited_data$expirDate, limited_data$tradeDate), ]
  
  # Report final result
  total_days_returned <- length(unique(limited_data$tradeDate))
  message(sprintf(
    "Returning %d days of historical data across %d expiration date(s)",
    total_days_returned, length(unique(limited_data$expirDate))
  ))
  
  return(limited_data)
}

