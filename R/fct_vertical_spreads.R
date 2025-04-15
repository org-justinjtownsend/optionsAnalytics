#' Create Vertical Spreads from Options Data
#'
#' This function creates vertical spreads from options data based on specified parameters.
#' It supports multiple types of vertical spreads (long call, short call, long put, short put)
#' and can handle multiple days to expiry and strike price ranges.
#'
#' @param optionsData A data frame containing options data with required columns:
#'   - ticker: character
#'   - tradeDate: character (YYYY-MM-DD format)
#'   - expirDate: character (YYYY-MM-DD format)
#'   - dte: integer
#'   - strike: integer
#'   - stockPrice: numeric
#'   - spotPrice: numeric
#'   - callBidPrice: numeric
#'   - callAskPrice: numeric
#'   - putBidPrice: numeric
#'   - putAskPrice: numeric
#' @param dte Integer or vector of integers specifying days to expiry
#' @param dist Integer specifying the exact range between strike prices
#' @param callPos Integer or character specifying the column position/name for call prices
#' @param putPos Integer or character specifying the column position/name for put prices
#' @param vsType Character vector specifying vertical spread type(s): "lcs", "scs", "lps", "sps"
#' @param output_file Optional path to save results as CSV
#' @return A data frame containing the vertical spread information
#' @export
create_vertical_spreads <- function(optionsData, dte, dist, callPos, putPos, vsType, output_file = NULL) {
  # Input validation
  if (!is.data.frame(optionsData)) {
    stop("optionsData must be a data frame")
  }
  
  # Validate required columns
  required_cols <- c("ticker", "tradeDate", "expirDate", "dte", "strike", 
                    "stockPrice", "spotPrice", "callBidPrice", "callAskPrice",
                    "putBidPrice", "putAskPrice")
  if (!all(required_cols %in% names(optionsData))) {
    stop("Missing required columns in optionsData")
  }
  
  # Validate data types
  if (!is.character(optionsData$ticker)) stop("ticker must be character")
  if (!is.character(optionsData$tradeDate)) stop("tradeDate must be character")
  if (!is.character(optionsData$expirDate)) stop("expirDate must be character")
  if (!is.integer(optionsData$dte)) stop("dte must be integer")
  if (!is.numeric(optionsData$strike)) stop("strike must be numeric")
  if (!is.numeric(optionsData$stockPrice)) stop("stockPrice must be numeric")
  if (!is.numeric(optionsData$spotPrice)) stop("spotPrice must be numeric")
  
  # Validate numeric fields are non-negative
  numeric_cols <- c("strike", "stockPrice", "spotPrice", "callBidPrice", 
                   "callAskPrice", "putBidPrice", "putAskPrice")
  for (col in numeric_cols) {
    if (any(optionsData[[col]] < 0, na.rm = TRUE)) {
      stop(paste(col, "must be non-negative"))
    }
  }
  
  # Validate bid-ask relationships
  if (any(optionsData$callBidPrice > optionsData$callAskPrice, na.rm = TRUE)) {
    stop("callBidPrice must be less than or equal to callAskPrice")
  }
  if (any(optionsData$putBidPrice > optionsData$putAskPrice, na.rm = TRUE)) {
    stop("putBidPrice must be less than or equal to putAskPrice")
  }
  
  # Validate vsType
  valid_types <- c("lcs", "scs", "lps", "sps")
  if (!all(vsType %in% valid_types)) {
    stop("vsType must be one of: 'lcs', 'scs', 'lps', 'sps'")
  }
  
  # Convert column positions to names if needed
  if (is.numeric(callPos)) callPos <- names(optionsData)[callPos]
  if (is.numeric(putPos)) putPos <- names(optionsData)[putPos]
  
  # Validate price columns
  if (!all(c(callPos, putPos) %in% names(optionsData))) {
    stop("Specified price columns not found in optionsData")
  }
  
  if (!all(sapply(optionsData[, c(callPos, putPos)], is.numeric))) {
    stop("Price columns must contain numeric values")
  }
  
  # Initialize empty result data frame
  result <- data.frame()
  
  # Process each dte value
  for (days in dte) {
    # Filter data for current dte
    dte_data <- optionsData[optionsData$dte == days, ]
    
    if (nrow(dte_data) < 2) {
      warning(sprintf("Not enough options for dte %d", days))
      next
    }
    
    # Find valid strike pairs
    for (i in 1:(nrow(dte_data)-1)) {
      for (j in (i+1):nrow(dte_data)) {
        strike_diff <- abs(dte_data$strike[j] - dte_data$strike[i])
        if (strike_diff == dist) {
          # Create new row for each spread type
          for (type in vsType) {
            new_row <- data.frame(
              dte = days,
              vsType = type,
              vsValue = 0,  # Will be calculated below
              callValL = dte_data[i, callPos],
              callValH = dte_data[j, callPos],
              putValL = dte_data[i, putPos],
              putValH = dte_data[j, putPos],
              strikeL = min(dte_data$strike[i], dte_data$strike[j]),
              strikeH = max(dte_data$strike[i], dte_data$strike[j]),
              spreadDist = dist
            )
            
            # Calculate vsValue based on spread type
            if (type == "lcs") {
              new_row$vsValue <- new_row$callValL - new_row$callValH
            } else if (type == "scs") {
              new_row$vsValue <- new_row$callValH - new_row$callValL
            } else if (type == "lps") {
              new_row$vsValue <- new_row$putValH - new_row$putValL
            } else if (type == "sps") {
              new_row$vsValue <- new_row$putValL - new_row$putValH
            }
            
            result <- rbind(result, new_row)
          }
        }
      }
    }
  }
  
  if (nrow(result) == 0) {
    warning("No valid vertical spreads found")
    return(NULL)
  }
  
  # Convert numeric columns
  numeric_cols <- c("vsValue", "callValL", "callValH", "putValL", "putValH", 
                   "strikeL", "strikeH", "spreadDist")
  for (col in numeric_cols) {
    result[[col]] <- as.numeric(result[[col]])
  }
  
  # Save to CSV if output_file is specified
  if (!is.null(output_file)) {
    tryCatch({
      write.csv(result, output_file, row.names = FALSE)
      message(sprintf("Results saved to %s", output_file))
    }, error = function(e) {
      warning(sprintf("Failed to save results to %s: %s", output_file, e$message))
    })
  }
  
  return(result)
} 