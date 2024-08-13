#' volreal 
#'
#' @description Functions for realized volatility calculations.
#'
#' @noRd

#' Calculate the volatility of a series of prices (OHLC).
#'
#' @param vol.per A period (number) over which volatility is calculated.
#' @param vol.method A calculation method (string) for the volatility.
#' @param idx.ohlc An object which Open-High-Low-Close prices, convertible to xts.
#'
#' @return A object of the same class as OHLC or a vector (if try.xts fails) containing the chosen volatility estimator values.
#' @export
#'
#' @examples
#' idx.vol.real(20, "close", AAPL.OHLC)
#' idx.vol.real(30, "garman.klass", SPX.OHLC)
#' idx.vol.real(vol.per = 30, vol.method = "garman.klass", idx.ohlc = SPX.OHLC)
idx.vol.real <- function(vol.per, vol.method, idx.ohlc) {
  idx <- TTR::volatility(idx.ohlc, n = vol.per, calc = vol.method, N = 252)
  return(idx)
}

#' Calculate multiple volatilities, using multiple methods over a series of prices (OHLC).
#'
#' @param vol.pers A numeric vector representing volatility periods.
#' @param vol.methods A character vector indicating volatility calculation methods.
#' @param idx.ohlc An object which contains Open-High-Low-Close prices, convertible to xts.
#'
#' @return An xts object of volatilities (one column per combination).
#' @export
#'
#' @examples
#' vols <- c(10, 20, 30)
#' vols.methods <- c("close", "garman.klass", "parkinson")
#' vols.real.all <- idx.vols.real(vols, vols.methods, SPX.OHLC)
#' vols.real.all <- idx.vols.real(vol.pers = vols, vol.methods = vols.methods, idx.ohlc = SPX.OHLC)
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
  all_vols <- base::do.call("merge", all_vols)
  base::colnames(all_vols) <- vol.names
  
  return(all_vols)
  
}