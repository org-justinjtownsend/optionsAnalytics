# Index options analysis
# Load SPX data etc.

# 1. ORATS: Data extract function, for index ticker(s). ----
orats.opts.hist <- function(tradeDate, token, ticker, bow = orats.bow) {
  
  tradeDate <- as.Date(tradeDate, format = "%Y-%m-%d")
  ticker <- ticker
  token <- token
  
  print(paste0(tradeDate, ": Retrieving data for ticker: ", ticker))
  
  # 1. Agree modification of session to path with host. ----
  session <- polite::nod(
    bow = orats.bow,
    path = paste0("/datav2/hist/dailies?", "token=", token, "&ticker=", ticker,
                  "&tradeDate=", tradeDate)
  )
  
  # 2. Get data from the new path. ----
  res <- polite::scrape(session,
                        content="text/plain; charset=UTF-8")
  
  # 3. Render results to xts. ----
  if (!is.null(res)) {
    
    resXTS <- jsonlite::fromJSON(res)[["data"]]
    
    resXTS.keeps <- c(
      "tradeDate",
      "ticker",
      "unadjOpen",
      "unadjHiPx",
      "unadjLoPx",
      "unadjClsPx",
      "unadjStockVolume",
      "clsPx"
    )
    
    resXTS <- resXTS[resXTS.keeps]
    
    resXTS$tradeDate <- base::as.Date(resXTS$tradeDate, format = "%Y-%m-%d")
    row.names(resXTS) <- resXTS$tradeDate
    resXTS <- base::subset(resXTS, select = -c(tradeDate, ticker))
    resXTS <- xts::as.xts(resXTS)
    
  } else {
    
    # Error case.
    resXTS <- paste0("No information found for ", tradeDate)
    
  }
  
  # 4. Save the results. ----
  outDir <- paste0(getwd(), "/data-raw/", tradeDate)
  dir.create(outDir, showWarnings = FALSE)
  
  print(paste0(tradeDate, ": Saving data for ticker: ", ticker, " to ", outDir, "."))
  
  saveRDS(resXTS, file = paste0(outDir, "/spx_inx_df.rda"))
  
  # 4.1 Return the result as xts.
  return (resXTS)
}

# 2. ORATS: Job set-up. ----
runDate <- format(Sys.time(), format = "%Y-%m-%d_%H:%M")

ORATS_BASEURL <- "https://api.orats.io"
ORATS_API_KEY <- Sys.getenv("ORATS_API_KEY")

tradeDate_latest <- format((as.Date(Sys.time())-1), format = "%Y-%m-%d")
ticker <- "SPX"

# 3. ORATS: Politely ask for ticker data. ----
# * JT, 2024-07-15: If I need ALL history data, then I can remove the tradedate.
# * Is this coding even necessary, where a separate load job just gets it all done?
# * JT, 2024-07-05, 15:10. Got here, Do I use SPY, since it provides traded volume, or do I get the futures data?

orats.bow <- polite::bow(
  url = ORATS_BASEURL,
  user_agent = "justinjtownsend@gmail.com",
  force = TRUE
)

spx.hist <- orats.opts.hist(tradeDate = tradeDate_latest,
                            token = ORATS_API_KEY,
                            ticker = ticker,
                            bow = orats.bow
                            )

# 4. OpAnalytics: Historical data with newly retrieved data. ----

# 15-07-2024, 15:00: Merging the XTS sources, shall I do it in QMOD format, or with ALL COLs?
# How would I need the extra (e.g. updated date later?)

base::load(file = paste0(getwd(), "/data/", "orats_spx_raw_qmod.rda"))

tradeDate_last <- stats::end(orats_spx_raw_qmod)

if (tradeDate_latest > tradeDate_last) {
  
  # JT, 15-07-24: May need to confirm index uniqueness here.
  spx.hist <- quantmod::as.quantmod.OHLC(c(spx.hist, orats_spx_raw_qmod),
                                         name = ticker)
  } else {
  print(paste0(tradeDate_last,
               ": Later trade data is already available for ticker ",
               ticker,
               ", (",
               tradeDate_latest,
               "). This most recent retrieval will not be merged."))
  spx.hist <- orats_spx_raw_qmod
  }

# 5. OpAnalytics: Realized volatility. ----

# 5.1 OpAnalytics: Volatility
idx.vol.real <- function(vol.per, vol.method, idx.ohlc) {
  idx <- TTR::volatility(idx.ohlc, n = vol.per, calc = vol.method, N = 252)
  return(idx)
}

# 5.2 OpAnalytics: Volatility function factory
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

vols <- c(10, 20, 30)
vols.methods <- c('close', 'garman.klass', 'parkinson')

vols_tst <- idx.vols.real(vol.pers = vols, vol.methods = vols.methods,
                          idx.ohlc = spx.hist)

# 6. ORATS: Data extract function, for options data, related to the index ticker. ----
# Get most recent closing prices.
# Add most recent prices to options history table.
# Load expiries (and delta ranges) where you have a position
# AND, load expiries for DTE ranges (to be confirmed, but 30, 60, 90 range?)
# AND, stddev of prices / strikes of +1 / -1, or +2 . -2 range(s)

# # Load SPX options data
# ORATS_API_KEY <- Sys.getenv("ORATS_API_KEY")
# SPX.OPTS <- quantmod::getOptionChain("SPX",
#                                      Exp = NULL,
#                                      src = "orats",
#                                      api.key = ORATS_API_KEY)
# 
# updatedAt <- unique(SPX.OPTS[[1]][["extra"]][["updatedAt"]])
# 

# 
# # Object type to be xts, JT, 2024-04-26: error check for xts.
# 
# # Save objects to list, to RDS
# 
# SPX.L <- list(
#   runDate,
#   updatedAt,
#   SPX.URL,
#   SPX.QMOD,
#   SPX.OPTS,
#   SPX.VOLS.REAL)
# 
# names(SPX.L) <- c("SPX.OPTS.runDate",
#                   "SPX.OPTS.updatedAt",
#                   "SPX.URL",
#                   "SPX.QMOD",
#                   "SPX.OPTS",
#                   "SPX.VOLS.REAL")

# saveRDS(SPX.L, file = paste0(getwd(), "/data-raw/2024-05-11_01:30/SPX.L"))
