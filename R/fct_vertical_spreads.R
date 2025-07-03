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
#' @param dist Integer or vector of integers specifying the exact range between strike prices
#' @param callPos Integer or character specifying the column position/name for call prices
#' @param putPos Integer or character specifying the column position/name for put prices
#' @param vsType Character vector specifying vertical spread type(s): "lcs", "scs", "lps", "sps"
#' @param output_file Optional path to save results as CSV
#' @return A named list containing:
#'   - optionsData: Original input data frame
#'   - vsData: Data frame containing vertical spread information
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
    
    # Process each dist value for current dte
    for (dist_val in dist) {
      # Find valid strike pairs for current dist
      for (i in 1:(nrow(dte_data)-1)) {
        for (j in (i+1):nrow(dte_data)) {
          strike_diff <- abs(dte_data$strike[j] - dte_data$strike[i])
          if (strike_diff == dist_val) {
            # Mark strike types
            strikeL <- min(dte_data$strike[i], dte_data$strike[j])
            strikeH <- max(dte_data$strike[i], dte_data$strike[j])
            
            # Create new row for each spread type
            for (type in vsType) {
              # Determine instrument type and position type
              instrType <- ifelse(type %in% c("lcs", "scs"), "c", "p")
              posType <- ifelse(type %in% c("scs", "sps"), "cr_s", "dr_s")
              
              # Calculate vsValue based on spread type
              vsValue <- 0
              instrValL <- 0
              instrValH <- 0
              
              if (type %in% c("lcs", "scs")) {
                instrValL <- dte_data[i, callPos]
                instrValH <- dte_data[j, callPos]
                if (type == "lcs") {
                  vsValue <- instrValL - instrValH
                } else {  # scs
                  vsValue <- instrValH - instrValL
                }
              } else {  # lps or sps
                instrValL <- dte_data[i, putPos]
                instrValH <- dte_data[j, putPos]
                if (type == "lps") {
                  vsValue <- instrValH - instrValL
                } else {  # sps
                  vsValue <- instrValL - instrValH
                }
              }
              
              # Calculate p_max based on spread type
              p_max <- 0
              if (type %in% c("lcs", "lps")) {
                p_max <- dist_val - vsValue
              } else {  # scs or sps
                p_max <- vsValue
              }
              
              # Calculate bep based on spread type
              bep <- 0
              if (type %in% c("lcs", "scs")) {
                bep <- strikeL + vsValue
              } else {  # lps or sps
                bep <- strikeH - vsValue
              }
              
              new_row <- data.frame(
                dte = days,
                vsType = type,
                vsValue = vsValue,
                instrValL = instrValL,
                instrValH = instrValH,
                strikeL = strikeL,
                strikeH = strikeH,
                dist = dist_val,
                instrType = instrType,
                posType = posType,
                p_max = p_max,
                bep = bep,
                updated = Sys.time()
              )
              
              result <- rbind(result, new_row)
            }
          }
        }
      }
    }
  }
  
  if (nrow(result) == 0) {
    warning("No valid vertical spreads found")
    return(list(optionsData = optionsData, vsData = NULL))
  }
  
  # Convert numeric columns
  numeric_cols <- c("vsValue", "instrValL", "instrValH", 
                   "strikeL", "strikeH", "dist", "p_max", "bep")
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
  
  # Return named list with original data and results
  return(list(
    optionsData = optionsData,
    vsData = result
  ))
} 