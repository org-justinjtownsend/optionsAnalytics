#' Get Historical Price Data for a Symbol
#'
#' @param symbol Character string representing the symbol to download (e.g., "^GSPC" for S&P 500)
#' @param from Start date for historical data (default: 1000 days ago)
#' @param to End date for historical data (default: today)
#' @param src Data source (default: "yahoo")
#' @param write_to_db Whether to write the data to the database (default: FALSE)
#' @param get_all_history Whether to get all available history when writing to database for the first time (default: TRUE)
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
#'
#' # Get data and write to database
#' spx_data <- get_historical_prices("^GSPC", write_to_db = TRUE)
get_historical_prices <- function(symbol,
                                from = Sys.Date() - 1000,
                                to = Sys.Date(),
                                src = "yahoo",
                                write_to_db = FALSE,
                                get_all_history = TRUE) {
  
  # Input validation
  if (!is.character(symbol)) {
    stop("Symbol must be a character string")
  }
  
  # Convert dates to Date objects if they're strings
  if (is.character(from)) from <- as.Date(from)
  if (is.character(to)) to <- as.Date(to)
  
  # If writing to database and getting all history, set from date to a very early date
  if (write_to_db && get_all_history) {
    from <- as.Date("1900-01-01")
  }
  
  # Download the data
  tryCatch({
    message(paste("Downloading data for", symbol, "from", from, "to", to))
    data <- quantmod::getSymbols(symbol,
                                src = src,
                                from = from,
                                to = to,
                                auto.assign = FALSE)
    
    # Check if we got any data
    if (nrow(data) == 0) {
      stop(paste("No data returned for symbol:", symbol))
    }
    
    message(paste("Successfully downloaded", nrow(data), "rows of data"))
    message("Column names in data:")
    message(paste(colnames(data), collapse = ", "))
    
    # If writing to database, create table if it doesn't exist and write data
    if (write_to_db) {
      message("Connecting to database...")
      # Connect to database
      drv <- DBI::dbDriver("PostgreSQL")
      con <- DBI::dbConnect(drv,
                           dbname = "oa_indexes",
                           host = "localhost",
                           port = 5432,
                           user = "justinjtownsend",
                           password = Sys.getenv("PGSQL_PW"),
                           options = "-c search_path=opts_hist")
      
      on.exit(DBI::dbDisconnect(con))
      
      # Create schema if it doesn't exist
      DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS opts_hist")
      
      message("Converting data for database storage...")
      # Convert xts to data frame and prepare for database
      df <- data.frame(
        symbol = symbol,
        date = format(index(data), "%Y-%m-%d"),
        open = as.numeric(data[, paste0(gsub("\\^", "", symbol), ".Open")]),
        high = as.numeric(data[, paste0(gsub("\\^", "", symbol), ".High")]),
        low = as.numeric(data[, paste0(gsub("\\^", "", symbol), ".Low")]),
        close = as.numeric(data[, paste0(gsub("\\^", "", symbol), ".Close")]),
        volume = as.numeric(data[, paste0(gsub("\\^", "", symbol), ".Volume")]),
        adjusted = as.numeric(data[, paste0(gsub("\\^", "", symbol), ".Adjusted")])
      )
      
      message("Writing data to database...")
      # Write data to database using dbWriteTable
      DBI::dbWriteTable(con, 
                       "price_hist", 
                       df, 
                       row.names = FALSE,
                       append = TRUE)
      
      message("Successfully wrote data to database")
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

#' Calculate Potential Price Moves Based on Standard Deviation
#'
#' @param data An xts object containing price data
#' @param period Number of periods for standard deviation calculation (default: 20)
#' @param annualize Whether to annualize the standard deviation (default: TRUE)
#' @param trading_days Number of trading days in a year (default: 252)
#' @param sd_multipliers Vector of standard deviation multipliers to calculate (default: c(1))
#' @return The original xts object with additional columns containing potential price moves
#' @export
#'
#' @examples
#' # Get data and calculate potential price moves (1 SD)
#' spx_data <- get_historical_prices("^GSPC")
#' spx_data <- add_potential_moves(spx_data, period = 20)
#'
#' # Calculate potential moves for 1, 2, and 3 standard deviations
#' spx_data <- add_potential_moves(spx_data, period = 20, sd_multipliers = c(1, 2, 3))
#'
#' # Calculate potential moves using 60-day standard deviation
#' spx_data <- add_potential_moves(spx_data, period = 60, sd_multipliers = c(1, 2, 3))
add_potential_moves <- function(data,
                              period = 20,
                              annualize = TRUE,
                              trading_days = 252,
                              sd_multipliers = c(1)) {
  
  # Input validation
  if (!xts::is.xts(data)) {
    stop("Input must be an xts object")
  }
  
  if (period <= 0) {
    stop("Period must be positive")
  }
  
  if (!is.numeric(sd_multipliers) || any(sd_multipliers <= 0)) {
    stop("sd_multipliers must be a vector of positive numbers")
  }
  
  # First ensure we have the standard deviation data
  sd_col <- paste0("SD_", period, "d")
  if (!sd_col %in% colnames(data)) {
    data <- add_rolling_sd(data, period = period, annualize = annualize, trading_days = trading_days)
  }
  
  # Calculate potential moves for each standard deviation multiplier
  for (mult in sd_multipliers) {
    # Calculate positive potential move using the formula: Current Price * (1 + SD * multiplier)
    potential_move_up <- data[, "Close"] * (1 + data[[sd_col]] * mult)
    col_name_up <- paste0("Potential_Move_", period, "d_", mult, "SD_Up")
    data[[col_name_up]] <- potential_move_up
    
    # Calculate negative potential move using the formula: Current Price * (1 - SD * multiplier)
    potential_move_down <- data[, "Close"] * (1 - data[[sd_col]] * mult)
    col_name_down <- paste0("Potential_Move_", period, "d_", mult, "SD_Down")
    data[[col_name_down]] <- potential_move_down
    
    # Calculate the range (difference between up and down moves)
    range <- potential_move_up - potential_move_down
    col_name_range <- paste0("Potential_Move_", period, "d_", mult, "SD_Range")
    data[[col_name_range]] <- range
  }
  
  return(data)
}

#' Check if Daily Returns Exceed Standard Deviation Levels
#'
#' @param data An xts object containing price data
#' @param period Number of periods for standard deviation calculation (default: 20)
#' @param annualize Whether to annualize the standard deviation (default: TRUE)
#' @param trading_days Number of trading days in a year (default: 252)
#' @param sd_multipliers Vector of standard deviation multipliers to check (default: c(1, 2, 3))
#' @return The original xts object with additional binary columns indicating if returns exceed SD levels
#' @export
#'
#' @examples
#' # Get data and check if returns exceed standard deviations
#' spx_data <- get_historical_prices("^GSPC")
#' spx_data <- check_sd_exceedance(spx_data, period = 20)
#'
#' # Check exceedance for specific standard deviation levels
#' spx_data <- check_sd_exceedance(spx_data, period = 20, sd_multipliers = c(1.5, 2.5))
check_sd_exceedance <- function(data,
                              period = 20,
                              annualize = TRUE,
                              trading_days = 252,
                              sd_multipliers = c(1, 2, 3)) {
  
  # Input validation
  if (!xts::is.xts(data)) {
    stop("Input must be an xts object")
  }
  
  if (period <= 0) {
    stop("Period must be positive")
  }
  
  if (!is.numeric(sd_multipliers) || any(sd_multipliers <= 0)) {
    stop("sd_multipliers must be a vector of positive numbers")
  }
  
  # First ensure we have the standard deviation data
  sd_col <- paste0("SD_", period, "d")
  if (!sd_col %in% colnames(data)) {
    data <- add_rolling_sd(data, period = period, annualize = annualize, trading_days = trading_days)
  }
  
  # Calculate daily returns if not already present
  if (!"Returns" %in% colnames(data)) {
    data$Returns <- log(data[, "Close"] / lag(data[, "Close"]))
  }
  
  # Check exceedance for each standard deviation multiplier
  for (mult in sd_multipliers) {
    # Check positive exceedance
    exceed_up <- abs(data$Returns) > (data[[sd_col]] * mult)
    col_name_up <- paste0("Exceed_", period, "d_", mult, "SD_Up")
    data[[col_name_up]] <- exceed_up
    
    # Check negative exceedance
    exceed_down <- data$Returns < -(data[[sd_col]] * mult)
    col_name_down <- paste0("Exceed_", period, "d_", mult, "SD_Down")
    data[[col_name_down]] <- exceed_down
    
    # Check either direction exceedance
    exceed_any <- exceed_up | exceed_down
    col_name_any <- paste0("Exceed_", period, "d_", mult, "SD_Any")
    data[[col_name_any]] <- exceed_any
  }
  
  return(data)
}

#' Calculate Running Counts of FALSE Occurrences
#'
#' @param data An xts object containing price data with exceedance columns
#' @param period Number of periods for standard deviation calculation (default: 20)
#' @param sd_multipliers Vector of standard deviation multipliers to check (default: c(1, 2, 3))
#' @return The original xts object with additional columns containing running counts
#' @export
#'
#' @examples
#' # Get data, check exceedance, and calculate running counts
#' spx_data <- get_historical_prices("^GSPC")
#' spx_data <- check_sd_exceedance(spx_data, period = 20)
#' spx_data <- add_running_counts(spx_data, period = 20)
#'
#' # Calculate running counts for specific standard deviation levels
#' spx_data <- add_running_counts(spx_data, period = 20, sd_multipliers = c(1.5, 2.5))
add_running_counts <- function(data,
                             period = 20,
                             sd_multipliers = c(1, 2, 3)) {
  
  # Input validation
  if (!xts::is.xts(data)) {
    stop("Input must be an xts object")
  }
  
  if (period <= 0) {
    stop("Period must be positive")
  }
  
  if (!is.numeric(sd_multipliers) || any(sd_multipliers <= 0)) {
    stop("sd_multipliers must be a vector of positive numbers")
  }
  
  # Function to calculate running count that resets on TRUE
  calc_running_count <- function(x) {
    count <- 0
    result <- numeric(length(x))
    for (i in seq_along(x)) {
      if (is.na(x[i])) {
        result[i] <- NA
      } else if (x[i]) {
        count <- 0
        result[i] <- count
      } else {
        count <- count + 1
        result[i] <- count
      }
    }
    return(result)
  }
  
  # Calculate running counts for each standard deviation multiplier
  for (mult in sd_multipliers) {
    # Get the exceedance columns
    exceed_up_col <- paste0("Exceed_", period, "d_", mult, "SD_Up")
    exceed_down_col <- paste0("Exceed_", period, "d_", mult, "SD_Down")
    exceed_any_col <- paste0("Exceed_", period, "d_", mult, "SD_Any")
    
    # Calculate running counts for each direction
    if (exceed_up_col %in% colnames(data)) {
      count_up_col <- paste0("Count_", period, "d_", mult, "SD_Up")
      data[[count_up_col]] <- calc_running_count(data[[exceed_up_col]])
    }
    
    if (exceed_down_col %in% colnames(data)) {
      count_down_col <- paste0("Count_", period, "d_", mult, "SD_Down")
      data[[count_down_col]] <- calc_running_count(data[[exceed_down_col]])
    }
    
    if (exceed_any_col %in% colnames(data)) {
      count_any_col <- paste0("Count_", period, "d_", mult, "SD_Any")
      data[[count_any_col]] <- calc_running_count(data[[exceed_any_col]])
    }
  }
  
  return(data)
}

#' Create Statistical Summary of Counts and Final Potential Moves
#'
#' @param data An xts object containing price data with count and potential move columns
#' @param period Number of periods for standard deviation calculation (default: 20)
#' @param sd_multipliers Vector of standard deviation multipliers to check (default: c(1, 2, 3))
#' @return A single-row xts object containing statistical summaries and final potential moves
#' @export
#'
#' @examples
#' # Get data and calculate all metrics
#' spx_data <- get_historical_prices("^GSPC")
#' spx_data <- check_sd_exceedance(spx_data, period = 20)
#' spx_data <- add_running_counts(spx_data, period = 20)
#' spx_data <- add_potential_moves(spx_data, period = 20, sd_multipliers = c(1, 2, 3))
#'
#' # Create statistical summary
#' summary_data <- create_count_summary(spx_data, period = 20)
create_count_summary <- function(data,
                               period = 20,
                               sd_multipliers = c(1, 2, 3)) {
  
  # Input validation
  if (!xts::is.xts(data)) {
    stop("Input must be an xts object")
  }
  
  if (period <= 0) {
    stop("Period must be positive")
  }
  
  if (!is.numeric(sd_multipliers) || any(sd_multipliers <= 0)) {
    stop("sd_multipliers must be a vector of positive numbers")
  }
  
  # Create empty list to store results
  results <- list()
  
  # Function to calculate mode
  calc_mode <- function(x) {
    x <- x[!is.na(x)]  # Remove NA values
    if (length(x) == 0) return(NA)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculate statistics for each count column
  for (mult in sd_multipliers) {
    # Get count columns
    count_up_col <- paste0("Count_", period, "d_", mult, "SD_Up")
    count_down_col <- paste0("Count_", period, "d_", mult, "SD_Down")
    count_any_col <- paste0("Count_", period, "d_", mult, "SD_Any")
    
    # Calculate statistics for each direction if column exists
    if (count_up_col %in% colnames(data)) {
      results[[paste0("Mean_", count_up_col)]] <- mean(data[[count_up_col]], na.rm = TRUE)
      results[[paste0("Median_", count_up_col)]] <- median(data[[count_up_col]], na.rm = TRUE)
      results[[paste0("Mode_", count_up_col)]] <- calc_mode(data[[count_up_col]])
    }
    
    if (count_down_col %in% colnames(data)) {
      results[[paste0("Mean_", count_down_col)]] <- mean(data[[count_down_col]], na.rm = TRUE)
      results[[paste0("Median_", count_down_col)]] <- median(data[[count_down_col]], na.rm = TRUE)
      results[[paste0("Mode_", count_down_col)]] <- calc_mode(data[[count_down_col]])
    }
    
    if (count_any_col %in% colnames(data)) {
      results[[paste0("Mean_", count_any_col)]] <- mean(data[[count_any_col]], na.rm = TRUE)
      results[[paste0("Median_", count_any_col)]] <- median(data[[count_any_col]], na.rm = TRUE)
      results[[paste0("Mode_", count_any_col)]] <- calc_mode(data[[count_any_col]])
    }
  }
  
  # Get final potential moves for 3SD
  final_moves <- data[nrow(data), grep(paste0("Potential_Move_", period, "d_3SD_"), colnames(data))]
  for (col in colnames(final_moves)) {
    results[[paste0("Final_", col)]] <- as.numeric(final_moves[1, col])
  }
  
  # Create single-row xts object with results
  summary_data <- xts::xts(
    matrix(unlist(results), nrow = 1),
    order.by = tail(index(data), 1)
  )
  colnames(summary_data) <- names(results)
  
  return(summary_data)
} 