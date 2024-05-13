# 1. Setup options ----
ORATS_BASEURL <- "https://api.orats.io"
orats.api.key <- Sys.getenv("ORATS_API_KEY")

# 1.1 Date ranges ----
orats.startDt <- as.Date("2018-04-01",
        format = "%Y-%m-%d")

orats.endDt <- as.Date("2019-03-31",
                         format = "%Y-%m-%d")

# 1.2 Business calendar ----
businessDts <- RQuantLib::getBusinessDayList(calendar = "UnitedStates/NYSE",
                                             from = orats.startDt,
                                             to = orats.endDt)

# 1.3 Ticker ----
orats.ticker <- "SPX"

# 2. Politely introduce yourself to the site. ----
orats.bow <- polite::bow(
  url = ORATS_BASEURL,
  user_agent = "justinjtownsend@gmail.com",
  force = TRUE
)

# 3. Agree modification to the path to hit the HISTORY endpoint. ----
orats.opts.hist <- function(tradeDate, token, ticker, bow = orats.bow) {
  
  tradeDate <- as.Date(tradeDate, format = "%Y-%m-%d")
  ticker <- ticker
  token <- token
  
  print(paste0(tradeDate, ": Starting call for ticker: ", ticker))
  
  # 1. Agree modification of session to path with host
  session <- polite::nod(
    bow = orats.bow,
    path = paste0("/datav2/hist/strikes?", "token=", token, "&ticker=", ticker,
                  "&tradeDate=", tradeDate)
  )
  
  # 2. Get data from the new path
  res <- polite::scrape(session,
                        content="text/plain; charset=UTF-8")
  
  # 3. Render result to JSON
  if (!grepl("Not Found.", res)) {
    
    resJSON <- jsonlite::fromJSON(res)[["data"]]
    
  } else {
    
    resJSON <- jsonlite::fromJSON(res)[["message"]]
    
  }
  
  # 4 Save the result
  outDir <- paste0(getwd(), "/data-raw/", tradeDate)
  dir.create(outDir)
  
  print(paste0(tradeDate, ": Saving data for ticker: ", ticker, " to ", outDir, "."))
  
  saveRDS(resJSON, file = paste0(outDir, "/spx.opts.df"))
  
  # 4.1 Return the result as JSON
  return (head(resJSON))
}

# 4. Map business dates over the history endpoint ----
orats.opt.hist.all <- purrr::map(businessDts,
                                 ~orats.opts.hist(.x, token = orats.api.key,
                                                  ticker = orats.ticker)
                                 )
