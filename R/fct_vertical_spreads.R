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
#'   - callMidIv: numeric
#'   - putMidIv: numeric
#' @param dte Integer or vector of integers specifying days to expiry
#' @param dist Integer or vector of integers specifying the exact range between strike prices
#' @param vsType Character vector specifying vertical spread type(s): "lcs", "scs", "lps", "sps"
#' @param callPos Optional column position/name for call prices (required for lcs, scs)
#' @param putPos Optional column position/name for put prices (required for lps, sps)
#' @param output_file Optional path to save results as CSV
#' @return A named list containing:
#'   - optionsData: Original input data frame
#'   - vsData_h: Data frame containing detailed vertical spread information
#'   - vsData_s: Summary data frame with aggregated vertical spread information
#' @export
create_vertical_spreads <- function(optionsData, dte, dist, vsType, callPos = NULL, putPos = NULL, output_file = NULL) {
  # Input validation
  if (!is.data.frame(optionsData)) {
    stop("optionsData must be a data frame")
  }
  
  # Validate required columns
  required_cols <- c("ticker", "tradeDate", "expirDate", "dte", "strike", 
                    "stockPrice", "spotPrice", "callBidPrice", "callAskPrice",
                    "putBidPrice", "putAskPrice", "callMidIv", "putMidIv")
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
                   "callAskPrice", "putBidPrice", "putAskPrice", "callMidIv", "putMidIv")
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
  
  # Validate callPos and putPos based on vsType
  if (any(vsType %in% c("lcs", "scs")) && is.null(callPos)) {
    stop("callPos must be provided for vsType in (lcs, scs)")
  }
  if (any(vsType %in% c("lps", "sps")) && is.null(putPos)) {
    stop("putPos must be provided for vsType in (lps, sps)")
  }
  
  # Convert column positions to names if needed
  if (!is.null(callPos) && is.numeric(callPos)) callPos <- names(optionsData)[callPos]
  if (!is.null(putPos) && is.numeric(putPos)) putPos <- names(optionsData)[putPos]
  
  # Validate price columns exist and contain numeric values
  if (!is.null(callPos)) {
    if (!callPos %in% names(optionsData)) {
      stop("callPos column not found in optionsData")
    }
    if (!is.numeric(optionsData[[callPos]])) {
      stop("callPos column must contain numeric values")
    }
  }
  
  if (!is.null(putPos)) {
    if (!putPos %in% names(optionsData)) {
      stop("putPos column not found in optionsData")
    }
    if (!is.numeric(optionsData[[putPos]])) {
      stop("putPos column must contain numeric values")
    }
  }
  
  # Initialize container for result rows to avoid rbind name/type mismatches
  result_rows <- list()
  result_count <- 0
  
  # Process each dte value
  for (days in dte) {
    # Filter data for current dte
    dte_data <- optionsData[optionsData$dte == days, ]
    
    if (nrow(dte_data) < 2) {
      warning(sprintf("Not enough options for dte %d", days))
      next
    }
    
    # Ensure we only pair strikes within the same expiration date
    unique_expiries <- unique(dte_data$expirDate)
    for (exp_date in unique_expiries) {
      ed_data <- dte_data[dte_data$expirDate == exp_date, ]
      if (nrow(ed_data) < 2) next
      
      # Process each dist value for current dte and expiration
      for (dist_val in dist) {
        # Find valid strike pairs for current dist
        for (i in 1:(nrow(ed_data)-1)) {
          for (j in (i+1):nrow(ed_data)) {
            strike_diff <- abs(ed_data$strike[j] - ed_data$strike[i])
            if (strike_diff == dist_val) {
              # Mark strike types
              strikeL <- min(ed_data$strike[i], ed_data$strike[j])
              strikeH <- max(ed_data$strike[i], ed_data$strike[j])
              
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
                  instrValL <- as.numeric(ed_data[[callPos]][i])
                  instrValH <- as.numeric(ed_data[[callPos]][j])
                  if (type == "lcs") {
                    vsValue <- instrValL - instrValH
                  } else {  # scs
                    vsValue <- instrValH - instrValL
                  }
                } else {  # lps or sps
                  instrValL <- as.numeric(ed_data[[putPos]][i])
                  instrValH <- as.numeric(ed_data[[putPos]][j])
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
                
                # Calculate moneyness
                stockPrice <- ed_data$stockPrice[1]  # Use first stock price for this expirDate
                moneyness <- "otm"  # Default
                
                if (strikeL < stockPrice && strikeH > stockPrice) {
                  moneyness <- "atm"
                } else if (instrType == "c") {
                  if (strikeL < stockPrice && strikeH < stockPrice) {
                    moneyness <- "itm"
                  } else if (strikeL > stockPrice && strikeH > stockPrice) {
                    moneyness <- "otm"
                  }
                } else if (instrType == "p") {
                  if (strikeL > stockPrice && strikeH > stockPrice) {
                    moneyness <- "itm"
                  } else if (strikeL < stockPrice && strikeH < stockPrice) {
                    moneyness <- "otm"
                  }
                }
                
                # Calculate probability of profit using implied volatility from options data
                prob_profit <- 0.5  # Default value
                iv_pp <- 0.20  # Default IV value
                
                tryCatch({
                  # Get implied volatility from options data
                  if (type %in% c("lcs", "scs")) {
                    # Use callMidIv for call spreads
                    iv_l <- ed_data$callMidIv[i]
                    iv_h <- ed_data$callMidIv[j]
                  } else {
                    # Use putMidIv for put spreads
                    iv_l <- ed_data$putMidIv[i]
                    iv_h <- ed_data$putMidIv[j]
                  }
                  
                  # Check if IV values are valid
                  if (!is.na(iv_l) && !is.na(iv_h) && iv_l > 0 && iv_h > 0) {
                    # Average IV from both legs
                    iv_avg <- (iv_l + iv_h) / 2
                    iv_pp <- iv_avg
                  } else {
                    # Use default IV if not available
                    iv_pp <- 0.20
                  }
                  
                  # Calculate probability of profit using normal distribution
                  # Following the rules: pnorm(bep, stockPrice, iv * sqrt(dte/365), TRUE)
                  time_factor <- sqrt(days / 365)
                  if (type %in% c("lcs", "lps")) {
                    # Bull spreads: profit if stock > bep
                    prob_profit <- 1 - pnorm(bep, stockPrice, iv_pp * stockPrice * time_factor, TRUE)
                  } else {
                    # Bear spreads: profit if stock < bep
                    prob_profit <- pnorm(bep, stockPrice, iv_pp * stockPrice * time_factor, TRUE)
                  }
                  
                  # Ensure probability is between 0 and 1
                  prob_profit <- max(0, min(1, prob_profit))
                  
                }, error = function(e) {
                  # If calculation fails, use default probability
                  prob_profit <- 0.5
                })
                
                # Get ticker and tradeDate from the data
                ticker <- ed_data$ticker[1]
                tradeDate <- ed_data$tradeDate[1]
                
                # Create leg1_instrument and leg2_instrument based on vsType
                if (type %in% c("lcs", "scs")) {
                  # For call spreads, leg1 is always the lower strike
                  leg1_instrument <- paste0(ticker, strikeL, instrType, exp_date)
                  leg2_instrument <- paste0(ticker, strikeH, instrType, exp_date)
                } else {
                  # For put spreads, leg1 is always the higher strike
                  leg1_instrument <- paste0(ticker, strikeH, instrType, exp_date)
                  leg2_instrument <- paste0(ticker, strikeL, instrType, exp_date)
                }
                
                # Create spread_id
                spread_id <- paste0(leg1_instrument, "_", leg2_instrument)
                
                new_row <- data.frame(
                  expirDate = exp_date,
                  dte = days,
                  dist = dist_val,
                  vsType = type,
                  instrType = instrType,
                  posType = posType,
                  vsValue = vsValue,
                  strikeL = strikeL,
                  strikeH = strikeH,
                  instrValL = instrValL,
                  instrValH = instrValH,
                  leg1_instrument = leg1_instrument,
                  leg2_instrument = leg2_instrument,
                  spread_id = spread_id,
                  ticker = ticker,
                  p_max = p_max,
                  bep = bep,
                  moneyness = moneyness,
                  prob_profit = prob_profit,
                  iv_pp = iv_pp,
                  tradeDate = tradeDate,
                  updated = Sys.time(),
                  strat_type = "vs"
                )
                
                # Append to list
                result_count <- result_count + 1
                result_rows[[result_count]] <- new_row
              }
            }
          }
        }
      }
    }
  }
  
  # Bind all rows
  vsData_h <- dplyr::bind_rows(result_rows)
  
  if (is.null(vsData_h) || nrow(vsData_h) == 0) {
    warning("No valid vertical spreads found")
    return(list(optionsData = optionsData, vsData_h = NULL, vsData_s = NULL))
  }
  
  # Convert numeric columns
  numeric_cols <- c("vsValue", "instrValL", "instrValH", 
                   "strikeL", "strikeH", "dist", "p_max", "bep", "prob_profit")
  present_cols <- intersect(numeric_cols, names(vsData_h))
  for (col in present_cols) {
    vsData_h[[col]] <- as.numeric(vsData_h[[col]])
  }
  
  # Create summary data frame (vsData_s) as specified in Step 12
  vsData_s <- vsData_h %>%
    dplyr::group_by(spread_id) %>%
    dplyr::summarise(
      tradeDate_latest = max(tradeDate, na.rm = TRUE),
      dte = first(dte),
      vsType = first(vsType),
      instrType = first(instrType),
      posType = first(posType),
      moneyness = first(moneyness),
      vsValue = list(vsValue),
      iv_pp = list(iv_pp),
      .groups = "drop"
    )
  
  # Save to CSV if output_file is specified
  if (!is.null(output_file)) {
    tryCatch({
      write.csv(vsData_h, output_file, row.names = FALSE)
      message(sprintf("Results saved to %s", output_file))
    }, error = function(e) {
      warning(sprintf("Failed to save results to %s: %s", output_file, e$message))
    })
  }
  
  # Return named list with original data and results
  return(list(
    optionsData = optionsData,
    vsData_h = vsData_h,
    vsData_s = vsData_s
  ))
}
