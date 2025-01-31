# 0. Setup options. ----
Sys.setenv("LOG_LEVEL" = "INFO")

rlog::log_info("Loading libraries")
library("RPostgreSQL")

# 0.1 OA_INDEXES: Local DB options
rlog::log_info("Setting local DB options...")

drv <- DBI::dbDriver("PostgreSQL")
db_pw <- Sys.getenv("PGSQL_PW")
db_user <- "justinjtownsend"

# 0.2 ORATS: Web data source options.
rlog::log_info("Setting remote data source options...")

ORATS_BASEURL <- "https://api.orats.io"
orats.api.key <- Sys.getenv("ORATS_API_KEY")

# 0.3 RQuantLib: Latest valid business date for ORATS extraction.
rlog::log_info("Getting most recent load date...")

startHour <- 2
currentHour <- as.numeric(format(Sys.time(), format = "%H"))
endDtTimeChk <- currentHour > startHour

# 1. Latest load dates (Local DB). ----
rlog::log_info("Fetching most recent load date from local DB...")

opts_latest_loadDates <- function(con) {
  latest_loadDates <- RPostgreSQL::dbGetQuery(
    con,
    statement = paste0(
      'SELECT max("tradeDate") as latestTradeDate, ',
      'max("quoteDate") as latestQuoteDate, ',
      'max("updatedAt") as latestUpdateDate ',
      "FROM opts_hist.opts_hist;"
    )
  )

  return(latest_loadDates)
}

rlog::log_info("Connecting to the local DB...")

latest_loadDates <- tryCatch(
  {
    con <- DBI::dbConnect(drv,
      dbname = "oa_indexes",
      host = "localhost", port = 5432,
      user = db_user, password = db_pw,
      options = "-c search_path=opts_hist"
    )

    opts_latest_loadDates(con)
  },
  error = function(err) {
    print(paste("MY_ERROR:  ", err))
  }
)

# 2. Business days (RQuantLib calendar). ----
rlog::log_info("Determing business date ranges...")

orats.startDt <- max(
  as.Date(latest_loadDates$latestquotedate),
  as.Date(latest_loadDates$latestupdatedate),
  as.Date(latest_loadDates$latesttradedate)
)
orats.startDt <- as.Date(orats.startDt,
  format = "%Y-%m-%d"
)

orats.endDt <- if (endDtTimeChk) Sys.Date() - 1 else Sys.Date()
orats.endDt <- as.Date(orats.endDt,
  format = "%Y-%m-%d"
)

businessDts <- RQuantLib::getBusinessDayList(
  calendar = "UnitedStates/NYSE",
  from = orats.startDt,
  to = orats.endDt
)

# 3. Most recent options (ORATS). ----
rlog::log_info("Fetching latest data from remote source...")

orats.ticker <- "SPX"

orats.bow <- polite::bow(
  url = ORATS_BASEURL,
  user_agent = "justinjtownsend@gmail.com",
  force = TRUE
)

orats.opts.hist <- function(tradeDate, token, ticker, bow = orats.bow) {
  tradeDate <- as.Date(tradeDate, format = "%Y-%m-%d")
  ticker <- ticker
  token <- token

  print(paste0(Sys.time(), ": loading ", ticker, ", for ", tradeDate, "."))

  session <- polite::nod(
    bow = orats.bow,
    path = paste0(
      "/datav2/hist/strikes?", "token=", token, "&ticker=", ticker,
      "&tradeDate=", tradeDate
    )
  )

  # 2. Get data from the new path
  res <- polite::scrape(session,
    content = "text/plain; charset=UTF-8"
  )

  # 3. Render result to JSON
  if (!grepl("Not Found.", res)) {
    resJSON <- jsonlite::fromJSON(res)[["data"]]
  } else {
    resJSON <- jsonlite::fromJSON(res)[["message"]]
  }

  # 4 Save the result

  # 4.1 Local file
  outDir <- paste0(getwd(), "/data-raw/", tradeDate)
  dir.create(outDir)

  print(paste0(tradeDate, ": Saving data for ticker: ", ticker, " to ", outDir, "."))

  saveRDS(resJSON, file = paste0(outDir, "/spx.opts.rda"))

  # 4.2 Return the result as JSON as confirmation (?)
  return(outDir)
}

orats.opt.hist.all <- purrr::map(
  businessDts,
  ~ orats.opts.hist(.x,
    token = orats.api.key,
    ticker = orats.ticker
  )
)

# Load to DB ONLY the files retrieved in the run.
path <- paste0(getwd(), "/data-raw/")
opts_files <- paste0(path, businessDts, "/spx.opts.rda")

rlog::log_info("Loading files from remote source to the DB...")
rlog::log_info("Inserting DB log entries...")

opts_hist_load <- function(fn) {
  f_name <- fn
  f_dir <- basename(dirname(f_name))

  fn <- readRDS(fn)
  fn <- as.data.frame(fn)

  fn_lastrow <- fn[nrow(fn), ]

  fn_tradeDate <- fn_lastrow$tradeDate
  fn_quoteDate <- fn_lastrow$quoteDate
  fn_updatedAt <- fn_lastrow$updatedAt

  fn_load_chk <- as.data.frame(
    cbind(
      load_time = as.character(Sys.time()),
      filename = f_name,
      directory = f_dir,
      tradeDate = fn_tradeDate,
      quoteDate = fn_quoteDate,
      updatedAt = fn_updatedAt
    )
  )

  con <- dbConnect(drv,
    dbname = "oa_indexes",
    host = "localhost", port = 5432,
    user = db_user, password = db_pw,
    options = "-c search_path=opts_hist"
  )
  on.exit(dbDisconnect(con))

  dbWriteTable(con, "opts_hist", fn, append = TRUE)
  dbWriteTable(con, "opts_load_hist", fn_load_chk, append = TRUE)
}

purrr::map(opts_files, opts_hist_load, .progress = "Loading options data")

# 4. Take most recent sample for loading in the package. ----
latestBusinessDt <- as.character(businessDts[length(businessDts)])

orats_spx_opts_raw <- RPostgreSQL::dbGetQuery(
  con,
  statement = paste0(
    "select",
    '"ticker", ',
    '"tradeDate", ',
    '"expirDate", ',
    '"dte", ',
    '"strike", ',
    '"stockPrice", ',
    '"callValue", ',
    '"callVolume", ',
    '"callOpenInterest", ',
    '"callBidSize", ',
    '"callAskSize", ',
    '"callBidPrice", ',
    '"callAskPrice", ',
    '"putValue", ',
    '"putVolume", ',
    '"putOpenInterest", ',
    '"putBidSize", ',
    '"putAskSize", ',
    '"putBidPrice", ',
    '"putAskPrice", ',
    '"smvVol", ',
    '"callMidIv", ',
    '"putMidIv", ',
    '"delta", ',
    '"gamma", ',
    '"theta", ',
    '"vega", ',
    '"rho", ',
    '"phi", ',
    '"spotPrice", ',
    '"updatedAt" as "orats_updatedAt"',
    "FROM opts_hist.opts_hist ",
    'WHERE "opts_hist"."tradeDate" = ',
    "'",
    latestBusinessDt,
    "'",
    ' ORDER BY "row.names" asc;'
  )
)

usethis::use_data(orats_spx_opts_raw, overwrite = TRUE)
