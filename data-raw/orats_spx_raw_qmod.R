## code to prepare `orats_spx_raw_qmod` dataset goes here

# 1. ORATS: Historical data extract function. ----
orats.opts.hist <- function(token, ticker, bow = orats.bow) {
  
  ticker <- ticker
  token <- token
  
  print(paste0("Retrieving data for ticker: ", ticker))
  
  # 1. Agree modification of session to path with host
  session <- polite::nod(
    bow = orats.bow,
    path = paste0("/datav2/hist/dailies?", "token=", token, "&ticker=", ticker)
  )
  
  # 2. Get data from the new path
  res <- polite::scrape(session,
                        content="text/plain; charset=UTF-8")
  
  # 3. Render result to JSON
  resDF <- jsonlite::fromJSON(res)[["data"]]
  
  resDF.keeps <- c(
    "tradeDate",
    "ticker",
    "unadjOpen",
    "unadjHiPx",
    "unadjLoPx",
    "unadjClsPx",
    "unadjStockVolume",
    "clsPx"
  )
  
  resDF <- resDF[resDF.keeps]
  
  # 4 Return the result
  return (resDF)
}

# 2. ORATS: Job set-up. ----
ORATS_BASEURL <- "https://api.orats.io"
ORATS_API_KEY <- Sys.getenv("ORATS_API_KEY")

ticker <- "SPX"

orats.bow <- polite::bow(
  url = ORATS_BASEURL,
  user_agent = "justinjtownsend@gmail.com",
  force = TRUE
)

# 3. Politely ask for ticker data. ----
orats_spx_raw_qmod <- orats.opts.hist(token = ORATS_API_KEY, ticker = ticker,
                                 bow = orats.bow
)

# 4. Remove NAs. ----
orats_spx_raw_qmod <- stats::na.omit(orats_spx_raw_qmod)

# 5. Convert date / time column to (unambiguous) date format. ----
orats_spx_raw_qmod$tradeDate <- base::as.Date(
  orats_spx_raw_qmod$tradeDate, format = "%Y-%m-%d")

# 6. Convert data-frame to an xts object. ----
row.names(orats_spx_raw_qmod) <- orats_spx_raw_qmod$tradeDate
orats_spx_raw_qmod <- base::subset(orats_spx_raw_qmod,
                                   select = -c(tradeDate, ticker))
orats_spx_raw_qmod <- quantmod::as.quantmod.OHLC(orats_spx_raw_qmod,
                                                 col.names = c("Open",
                                                               "High",
                                                               "Low",
                                                               "Close",
                                                               "Volume",
                                                               "Adjusted"),
                                                 name = ticker)

usethis::use_data(orats_spx_raw_qmod, overwrite = TRUE)
