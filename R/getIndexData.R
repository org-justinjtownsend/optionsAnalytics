# Index options analysis
# Load SPX data etc.

# Libraries
library("httr")
library("jsonlite")
library("quantmod")

# Set run date and save location
runDate <- format(Sys.time(), format = "%Y-%m-%d_%H:%M")
outDir <- paste0(getwd(), "/optionsAnalytics/data-raw/", runDate)

dir.create(outDir)

# Load the underlying index
EODHD_API_KEY <- Sys.getenv("EODHD_API_KEY")
SPX.URL <- paste0('https://eodhd.com/api/eod/GSPC.INDX?api_token=',
                  EODHD_API_KEY,
                  '&fmt=json')
response <- httr::GET(SPX.URL)

if (httr::http_type(response) == "application/json") {
  SPX <- httr::content(response, as = "text", encoding = "UTF-8")
  cat(SPX)
} else {
  cat("Error while receiving data\n")
}

SPX <- jsonlite::fromJSON(SPX)

# Quantmod  "tidy conversion"
rownames(SPX) <- SPX$date

SPX.KEEPS <- c("open",
               "high",
               "low",
               "close",
               "volume",
               "adjusted_close")

SPX.QMOD <- SPX[SPX.KEEPS]
# SQP.Q <- quantmod::as.quantmod.OHLC(SPX.QMOD, col.names = SPX.KEEPS)
SPX.QMOD <- xts::as.xts(SPX.QMOD)

# Load SPX options data
ORATS_API_KEY <- Sys.getenv("ORATS_API_KEY")
SPX.OPTS <- quantmod::getOptionChain("SPX",
                                     Exp = NULL,
                                     src = "orats",
                                     api.key = ORATS_API_KEY)

updatedAt <- unique(SPX.OPTS[[1]][["extra"]][["updatedAt"]])

# Get realized vols (from SPX)

# Volatilities
idx.vol.real <- function(vol.per, vol.method, idx.ohlc) {
  idx <- TTR::volatility(idx.ohlc, n = vol.per, calc = vol.method, N = 252)
  return(idx)
}
idx.vols.real <- function(vol.pers, vol.methods, idx.ohlc) {
  
  # vol.pers is a numeric vector
  if(!is.vector(vol.pers, mode = "numeric")) {
    stop("Volatility periods must be a vector (of numbers), e.g. 10, 20, 30")
  }
  
  # vol.methods is a character vector
  if(!is.vector(vol.methods, mode = "character")) {
    stop("Volatility methods must be a vector (of characters), e.g. 'close', 'garman.klass', 'parkinson'")
  }
  
  # idx.ohlc is of class('OHLC'), see quantmod.OHLC
  if(!quantmod::is.OHLC(idx.ohlc)) {
    stop("idx.ohlc must be of class OHLC, see quantmod.OHLC for further information.")
  }
  else {
    # Combination of all the input vectors' variables
    vols_combined <- base::expand.grid(vol.pers, vol.methods, stringsAsFactors = FALSE)
    
    vol.pers <- as.vector(vols_combined[,1], mode = "numeric")
    vol.methods <- as.vector(vols_combined[,2], mode = "character")
    
    # Volatility combinations e.g. 'vol: method : per'
    vol.names <- as.list(paste("vol:", vols_combined[,2], ":", vols_combined[,1]))
  }
  
  # Calculate all volatility combinations
  all_vols <- purrr::map2(vol.pers, vol.methods, idx.vol.real, idx.ohlc, .progress = FALSE)
  
  # Volatility parameters to column names
  n <- length(all_vols)
  for (i in seq_len(n)) {
    base::colnames(all_vols[[i]]) <- vol.names[[i]]
  }
  
  all_vols <- base::do.call("merge", all_vols)
  
  return(all_vols)
  
}

vols <- c(10, 20, 30)
vols.methods <- c("close", "garman.klass", "parkinson")

# Object type to be xts, JT, 2024-04-26: error check for xts.
SPX.VOLS.REAL <- idx.vols.real(vols, vols.methods, SPX.QMOD)

# Save objects to list, to RDS

SPX.L <- list(
  runDate,
  updatedAt,
  SPX.URL,
  SPX.QMOD,
  SPX.OPTS,
  SPX.VOLS.REAL)

names(SPX.L) <- c("SPX.OPTS.runDate",
                  "SPX.OPTS.updatedAt",
                  "SPX.URL",
                  "SPX.QMOD",
                  "SPX.OPTS",
                  "SPX.VOLS.REAL")

saveRDS(SPX.L, file = paste0(outDir, "/SPX.L"))
