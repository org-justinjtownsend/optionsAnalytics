#' Get Historical Price Data for a Symbol
#'
#' @param symbol Character string representing the symbol to download (e.g., "^GSPC" for S&P 500)
#' @param from Start date for historical data (default: 1000 days ago)
#' @param to End date for historical data (default: today)
#' @param src Data source (default: "yahoo")
#' @return An xts object containing the historical price data
#' @export
#'
#' @examples
#' # Get S&P 500 data for the last 1000 days
#' spx_data <- get_historical_prices("^GSPC")
#'
#' # Get specific date range
#' spx_data <- get_historical_prices("^GSPC", 
#'                                  from = "2020-01-01", 
#'                                  to = "2024-03-18")
get_historical_prices <- function(symbol,
                                from = Sys.Date() - 1000,
                                to = Sys.Date(),
                                src = "yahoo") {
  
  # Input validation
  if (!is.character(symbol)) {
    stop("Symbol must be a character string")
  }
  
  # Convert dates to Date objects if they're strings
  if (is.character(from)) from <- as.Date(from)
  if (is.character(to)) to <- as.Date(to)
  
  # Download the data
  tryCatch({
    data <- quantmod::getSymbols(symbol,
                                src = src,
                                from = from,
                                to = to,
                                auto.assign = FALSE)
    
    # Check if we got any data
    if (nrow(data) == 0) {
      stop(paste("No data returned for symbol:", symbol))
    }
    
    return(data)
    
  }, error = function(e) {
    stop(paste("Error downloading data for", symbol, ":", e$message))
  })
}

#' Calculate Rolling Volatility
#'
#' @param data An xts object containing price data
#' @param period Number of periods for volatility calculation (default: 20)
#' @param annualize Whether to annualize the volatility (default: TRUE)
#' @param trading_days Number of trading days in a year (default: 252)
#' @return The original xts object with an additional column containing the rolling volatility
#' @export
#'
#' @examples
#' # Get data and calculate 20-day rolling volatility
#' spx_data <- get_historical_prices("^GSPC")
#' spx_data <- add_rolling_volatility(spx_data, period = 20)
#'
#' # Calculate 60-day rolling volatility without annualization
#' spx_data <- add_rolling_volatility(spx_data, period = 60, annualize = FALSE)
add_rolling_volatility <- function(data,
                                 period = 20,
                                 annualize = TRUE,
                                 trading_days = 252) {
  
  # Input validation
  if (!xts::is.xts(data)) {
    stop("Input must be an xts object")
  }
  
  if (period <= 0) {
    stop("Period must be positive")
  }
  
  # Calculate log returns
  returns <- log(data[, "Close"] / lag(data[, "Close"]))
  
  # Calculate rolling standard deviation
  vol <- rollapply(returns, 
                   width = period,
                   FUN = sd,
                   fill = NA,
                   align = "right")
  
  # Annualize if requested
  if (annualize) {
    vol <- vol * sqrt(trading_days)
  }
  
  # Add to original data
  col_name <- paste0("Volatility_", period, "d")
  data[[col_name]] <- vol
  
  return(data)
}

#' Calculate Rolling Standard Deviation
#'
#' @param data An xts object containing price data
#' @param period Number of periods for standard deviation calculation (default: 20)
#' @param annualize Whether to annualize the standard deviation (default: TRUE)
#' @param trading_days Number of trading days in a year (default: 252)
#' @return The original xts object with an additional column containing the rolling standard deviation
#' @export
#'
#' @examples
#' # Get data and calculate 20-day rolling standard deviation
#' spx_data <- get_historical_prices("^GSPC")
#' spx_data <- add_rolling_sd(spx_data, period = 20)
#'
#' # Calculate 60-day rolling standard deviation without annualization
#' spx_data <- add_rolling_sd(spx_data, period = 60, annualize = FALSE)
add_rolling_sd <- function(data,
                          period = 20,
                          annualize = TRUE,
                          trading_days = 252) {
  
  # Input validation
  if (!xts::is.xts(data)) {
    stop("Input must be an xts object")
  }
  
  if (period <= 0) {
    stop("Period must be positive")
  }
  
  # Calculate rolling standard deviation of returns
  returns <- log(data[, "Close"] / lag(data[, "Close"]))
  sd_returns <- rollapply(returns,
                         width = period,
                         FUN = sd,
                         fill = NA,
                         align = "right")
  
  # Annualize if requested
  if (annualize) {
    sd_returns <- sd_returns * sqrt(trading_days)
  }
  
  # Add to original data
  col_name <- paste0("SD_", period, "d")
  data[[col_name]] <- sd_returns
  
  return(data)
} 