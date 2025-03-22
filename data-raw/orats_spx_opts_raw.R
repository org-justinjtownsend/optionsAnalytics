# 0.1 Load required packages. ----
# These packages are needed here because they support creating objects in the /data directory.
load_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' needed for this script to work. Please install it.", pkg),
         call. = FALSE)
  } else {
    message(sprintf("Package '%s' loaded successfully.", pkg))
  }
}

load_package("RPostgreSQL")
load_package("DBI")
load_package("polite")
load_package("jsonlite")
load_package("RQuantLib")
load_package("purrr")
load_package("usethis")

# 0.2 Setup logging. ----
# This function sets up logging to a file and the console.
# It creates a log directory if it doesn't exist, and writes log messages to both the console and the log file.
# It might be a good idea to have a silent mode (not log to the console) because the file may be called
# externally by RScript in a CRON job and the output will be sent to a log file only.
setup_logging <- function(silent = FALSE) {
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
    if (!silent) {
      print(formatted_msg)
    }
  }
  
  return(list(
    log_message = log_message,
    log_con = log_con
  ))
}

# Initialize logging with silent mode option
logger <- setup_logging(silent = FALSE)
log <- logger$log_message

# Example of using different log levels
log("This is a debug message", "DEBUG")
log("This is a warning message", "WARNING")

# 0.3 OA_INDEXES: Local DB options. ----
# This section is used to connect to the local database and retrieve the latest load dates.
# The database connection details will eventually go back into the .Renviron file in the package root.
drv <- DBI::dbDriver("PostgreSQL")
db_user <- "justinjtownsend"
db_pw <- "Welcome!"  # Hardcoded for testing
db_host <- "localhost"
db_port <- 5432
db_name <- "oa_indexes"
db_schema <- "opts_hist"

# 0.4 ORATS: Web data source options. ----
# This section is used to provide the ORATS API set-up details.
# It could maybe also be moved back into the .Renviron file in the package root.
ORATS_BASEURL <- "https://api.orats.io"
orats.api.key <- Sys.getenv("ORATS_API_KEY")

# 0.5 RQuantLib: Latest valid business date for ORATS extraction. ----
# This section is used to check the current time and set the start and end date for the ORATS API extraction.
# The function must check the time when it starts, given the time difference between the ORATS API and the local database.
# This affects which business dates are retrieved.
startHour <- 2
currentHour <- as.numeric(format(Sys.time(), format = "%H"))
endDtTimeChk <- currentHour > startHour

# 0.6 Setup Pushover notification using pushoverr. ----
# This function sends a notification to Pushover using the pushoverr library.
send_pushover_notification <- function(message, title = "Script Notification", priority = 0) {
  pushoverr::set_pushover_app(Sys.getenv("PUSHOVER_APP_TOKEN"))
  pushoverr::set_pushover_user(Sys.getenv("PUSHOVER_USER_KEY"))
  
  pushoverr::pushover(message = message, title = title, priority = priority)
}

# 1. Latest load dates (Local DB). ----
# This section is used to retrieve the latest load dates from the local database.
# It is a function that takes a database connection as an argument.
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

# 1.1 Test database connection and get latest load dates. ----
# This section is used to test the database connection and retrieve the latest load dates.
# Importantly, if the script cannot connect to the database, it will stop and not continue.
# It should report the error and stop.
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

# 1.2 Business days (RQuantLib calendar). ----
# Gathers ONLY the business days between the latest load dates and today.
# This is because the ORATS API only has data for the business days.
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

# 2.1 Most recent options from ORATS. ----
# This section is used to retrieve the most recent options from the ORATS API.
# It is a function that takes a trade date as an argument.
# It uses the ORATS API to retrieve the options data for the given trade date.
# It then saves the data to the data-raw directory.
# It also logs the data to the console.
# It also logs the data to the log file.
# It also logs the data to the database.
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

# 2.2 ORATS API retrieval successful, only THEN continue with database operations. ----# If we have any successful results, then continue with database operations.
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
# 2.3 Load the data to the database. ----
# It's important to log the progress of these files, since sometimes multiple days load may happen at once.
  log("Starting database load of retrieved files")
  purrr::map(opts_files, opts_hist_load, .progress = "Loading options data")

# 3.1 Take most recent sample for loading into the package. ----
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

# 3.2 At the end of the script, send a notification of success or failure. ----
# Make sure the title is informative and includes the latestBusinessDt : orats.ticker.
# Make sure the message is informative and includes the number of rows loaded to the package
# and the timestamp of the load.
tryCatch({
  # If the script completes successfully
  rows_loaded <- nrow(orats_spx_opts_raw)
  timestamp <- Sys.time()
  title <- sprintf("Success: %s - %s", latestBusinessDt, orats.ticker)
  message <- sprintf("Successfully loaded %d rows to the package at %s.", rows_loaded, timestamp)
  send_pushover_notification(message, title)
}, error = function(err) {
  # If an error occurs
  send_pushover_notification(paste("The script failed with error:", err$message), "Script Failure", priority = 1)
  stop(err)
})

# Close log file
close(logger$log_con)