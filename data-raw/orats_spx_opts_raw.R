# Load required packages
if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
  stop("Package 'RPostgreSQL' needed for this script to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("DBI", quietly = TRUE)) {
  stop("Package 'DBI' needed for this script to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("polite", quietly = TRUE)) {
  stop("Package 'polite' needed for this script to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' needed for this script to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("RQuantLib", quietly = TRUE)) {
  stop("Package 'RQuantLib' needed for this script to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("purrr", quietly = TRUE)) {
  stop("Package 'purrr' needed for this script to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  stop("Package 'usethis' needed for this script to work. Please install it.",
       call. = FALSE)
}

# Setup logging
setup_logging <- function() {
  log_dir <- file.path(getwd(), "logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  log_file <- file.path(log_dir, paste0("orats_spx_opts_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  log_con <- file(log_file, "w")
  
  # Function to write to both console and log file
  log_message <- function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted_msg <- sprintf("[%s] %s: %s", timestamp, level, msg)
    writeLines(formatted_msg, log_con)
    print(formatted_msg)
  }
  
  return(list(
    log_message = log_message,
    log_con = log_con
  ))
}

# Initialize logging
logger <- setup_logging()
log <- logger$log_message

# 0.1 OA_INDEXES: Local DB options
drv <- DBI::dbDriver("PostgreSQL")
db_user <- "justinjtownsend"
db_pw <- "Welcome!"  # Hardcoded for testing
db_host <- "localhost"
db_port <- 5432
db_name <- "oa_indexes"
db_schema <- "opts_hist"

# 0.2 ORATS: Web data source options.
ORATS_BASEURL <- "https://api.orats.io"
orats.api.key <- Sys.getenv("ORATS_API_KEY")

# 0.3 RQuantLib: Latest valid business date for ORATS extraction.
startHour <- 2
currentHour <- as.numeric(format(Sys.time(), format = "%H"))
endDtTimeChk <- currentHour > startHour

# 1. Latest load dates (Local DB). ----
opts_latest_loadDates <- function(con) {
  tryCatch({
    latest_loadDates <- RPostgreSQL::dbGetQuery(
      con,
      statement = paste0(
        'SELECT max("tradeDate") as latestTradeDate, ',
        'max("quoteDate") as latestQuoteDate, ',
        'max("updatedAt") as latestUpdateDate ',
        'FROM opts_load_hist;'
      )
    )
    log("Successfully retrieved latest load dates from database")
    return(latest_loadDates)
  }, error = function(err) {
    log(paste("Error retrieving latest load dates:", err$message), "ERROR")
    stop(err)
  })
}

# Test database connection and get latest load dates
log("Attempting to connect to database...")
latest_loadDates <- tryCatch({
  con <- DBI::dbConnect(drv, 
                       dbname = db_name,
                       host = db_host, 
                       port = db_port,
                       user = db_user, 
                       password = db_pw,
                       options=paste0("-c search_path=", db_schema))
  
  on.exit(DBI::dbDisconnect(con))
  opts_latest_loadDates(con)
}, error = function(err) {
  log(paste("Database connection error:", err$message), "ERROR")
  stop(err)
})

log("Latest load dates retrieved:")
log(paste("Latest trade date:", latest_loadDates$latesttradedate))
log(paste("Latest quote date:", latest_loadDates$latestquotedate))
log(paste("Latest update date:", latest_loadDates$latestupdatedate))

# 2. Business days (RQuantLib calendar). ----
orats.startDt <- max(as.Date(latest_loadDates$latestquotedate),
                    as.Date(latest_loadDates$latestupdatedate),
                    as.Date(latest_loadDates$latesttradedate))
orats.startDt <- as.Date(orats.startDt,
                        format = "%Y-%m-%d")

orats.endDt <- if (endDtTimeChk) Sys.Date()-1 else Sys.Date()
orats.endDt <- as.Date(orats.endDt,
                      format = "%Y-%m-%d")

log(sprintf("Fetching business days from %s to %s", orats.startDt, orats.endDt))

businessDts <- tryCatch({
  RQuantLib::getBusinessDayList(calendar = "UnitedStates/NYSE",
                               from = orats.startDt,
                               to = orats.endDt)
}, error = function(err) {
  log(paste("Error getting business days:", err$message), "ERROR")
  stop(err)
})

log(sprintf("Found %d business days", length(businessDts)))
log("First few business days:")
log(paste(head(businessDts), collapse = ", "))
log("Last few business days:")
log(paste(tail(businessDts), collapse = ", "))

# 3. Most recent options (ORATS). ----
orats.ticker <- "SPX"

orats.bow <- polite::bow(
  url = ORATS_BASEURL,
  user_agent = "justinjtownsend@gmail.com",
  force = TRUE
)

orats.opts.hist <- function(tradeDate, token, ticker, bow = orats.bow) {
  tradeDate <- as.Date(tradeDate, format = "%Y-%m-%d")
  
  log(sprintf("Attempting to load %s data for %s", ticker, tradeDate))
  
  tryCatch({
    session <- polite::nod(
      bow = orats.bow,
      path = paste0("/datav2/hist/strikes?", "token=", token, "&ticker=", ticker,
                    "&tradeDate=", tradeDate)
    )
    
    # Get data from the new path
    res <- polite::scrape(session,
                         content="text/plain; charset=UTF-8")
    
    # Handle API response
    if (grepl("Not Found.", res)) {
      log(sprintf("No data found for %s on %s", ticker, tradeDate), "WARNING")
      return(NULL)
    }
    
    # Parse JSON response
    resJSON <- jsonlite::fromJSON(res)[["data"]]
    
    # Save the result
    outDir <- paste0(getwd(), "/data-raw/", tradeDate)
    dir.create(outDir, showWarnings = FALSE)
    
    saveRDS(resJSON, file = paste0(outDir, "/spx.opts.rda"))
    log(sprintf("Successfully saved data for %s on %s to %s", ticker, tradeDate, outDir))
    
    return(outDir)
    
  }, error = function(err) {
    # Check for 401 Unauthorized error
    if (grepl("401", err$message)) {
      log("API authentication failed (401 Unauthorized). Check your API key.", "ERROR")
      log("Stopping further API calls as they will also fail.", "ERROR")
      stop("API authentication failed. Please check your ORATS API key.")
    }
    
    log(sprintf("Error fetching data for %s on %s: %s", ticker, tradeDate, err$message), "ERROR")
    return(NULL)
  })
}

# Process business days with error handling
log("Starting data collection for all business days")

# Add error handling around the entire map operation
tryCatch({
  orats.opt.hist.all <- purrr::map(businessDts,
                                  ~orats.opts.hist(.x, token = orats.api.key,
                                                  ticker = orats.ticker),
                                  .progress = TRUE)
  
  # Log summary of results
  successful_days <- sum(!sapply(orats.opt.hist.all, is.null))
  log(sprintf("Completed data collection. Successfully processed %d out of %d days", 
              successful_days, length(businessDts)))
}, error = function(err) {
  # If we hit a 401, the error will propagate here
  log("Data collection stopped due to error", "ERROR")
  if (!grepl("401", err$message)) {
    # Only continue if it wasn't a 401 error
    stop(err)
  }
})

# Only continue with database operations if we have any successful results
if (exists("orats.opt.hist.all") && length(orats.opt.hist.all) > 0) {
  # Load to DB ONLY the files retrieved in the run.
  path = paste0(getwd(),"/data-raw/")
  opts_files <- paste0(path, businessDts, "/spx.opts.rda")
  opts_hist_load <- function(fn) {
    tryCatch({
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
      
      con <- DBI::dbConnect(drv, 
                           dbname = db_name,
                           host = db_host, 
                           port = db_port,
                           user = db_user, 
                           password = db_pw,
                           options=paste0("-c search_path=", db_schema))
      on.exit(DBI::dbDisconnect(con))
      
      DBI::dbWriteTable(con, "opts_hist", fn, append = TRUE)
      DBI::dbWriteTable(con, "opts_load_hist", fn_load_chk, append = TRUE)
      
      log(sprintf("Successfully loaded data from %s to database", f_name))
    }, error = function(err) {
      log(sprintf("Error loading data from %s to database: %s", f_name, err$message), "ERROR")
    })
  }

  log("Starting database load of retrieved files")
  purrr::map(opts_files, opts_hist_load, .progress = "Loading options data")

  # 4. Take most recent sample for loading in the package. ----
  latestBusinessDt <- as.character(businessDts[length(businessDts)])

  log(sprintf("Retrieving latest data for %s from database", latestBusinessDt))

  tryCatch({
    con <- DBI::dbConnect(drv, 
                         dbname = db_name,
                         host = db_host, 
                         port = db_port,
                         user = db_user, 
                         password = db_pw,
                         options=paste0("-c search_path=", db_schema))
    on.exit(DBI::dbDisconnect(con))
    
    orats_spx_opts_raw <- DBI::dbGetQuery(
      con,
      statement = paste0(
        'select',
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
        'FROM opts_hist.opts_hist ',
        'WHERE "opts_hist"."tradeDate" = ',
        '\'',
        latestBusinessDt,
        '\'',
        ' ORDER BY "row.names" asc;'
      )
    )
    
    log(sprintf("Successfully retrieved %d rows of data for package", nrow(orats_spx_opts_raw)))
    
    usethis::use_data(orats_spx_opts_raw, overwrite = TRUE)
    log("Successfully saved data to package")
  }, error = function(err) {
    log(sprintf("Error retrieving package data: %s", err$message), "ERROR")
    stop(err)
  })
}

# Close log file
close(logger$log_con)

