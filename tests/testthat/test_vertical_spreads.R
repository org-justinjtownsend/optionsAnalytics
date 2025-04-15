library(testthat)
library(RPostgreSQL)

# Source the function directly
source("../../R/fct_vertical_spreads.R")

# Sample test data
create_test_data <- function() {
  data.frame(
    ticker = rep("SPX", 4),
    tradeDate = rep("2024-01-01", 4),
    expirDate = rep("2024-01-15", 4),
    dte = as.integer(rep(14, 4)),
    strike = c(3900, 3950, 4000, 4050),
    stockPrice = rep(4000, 4),
    spotPrice = rep(4000, 4),
    callBidPrice = c(100, 80, 60, 40),
    callAskPrice = c(105, 85, 65, 45),
    putBidPrice = c(40, 60, 80, 100),
    putAskPrice = c(45, 65, 85, 105)
  )
}

test_that("create_vertical_spreads works with valid data", {
  # Create test data
  test_data <- create_test_data()
  
  # Test with different spread types
  spreads <- create_vertical_spreads(
    optionsData = test_data,
    dte = as.integer(14),
    dist = 50,
    callPos = "callAskPrice",
    putPos = "putAskPrice",
    vsType = c("lcs", "scs", "lps", "sps")
  )
  
  # Basic validation
  expect_s3_class(spreads, "data.frame")
  expect_true(nrow(spreads) > 0)
  
  # Validate column presence
  required_cols <- c("dte", "vsType", "vsValue", "callValL", "callValH", 
                    "putValL", "putValH", "strikeL", "strikeH", "spreadDist")
  expect_true(all(required_cols %in% names(spreads)))
  
  # Validate spread calculations
  expect_true(all(spreads$spreadDist == 50))
  expect_true(all(spreads$dte == 14))
  
  # Save to CSV
  output_file <- "test_vertical_spreads.csv"
  write.csv(spreads, output_file, row.names = FALSE)
  
  # Verify CSV was created
  expect_true(file.exists(output_file))
  
  # Clean up
  unlink(output_file)
})

test_that("create_vertical_spreads handles multiple dte values", {
  # Create test data with multiple dte values
  test_data <- data.frame(
    ticker = rep("SPX", 4),
    tradeDate = rep("2024-01-01", 4),
    expirDate = c(rep("2024-01-15", 2), rep("2024-02-15", 2)),
    dte = as.integer(c(rep(14, 2), rep(45, 2))),
    strike = c(3900, 4000, 3900, 4000),  # One pair per dte
    stockPrice = rep(4000, 4),
    spotPrice = rep(4000, 4),
    callBidPrice = c(100, 60, 120, 80),
    callAskPrice = c(105, 65, 125, 85),
    putBidPrice = c(40, 80, 60, 100),
    putAskPrice = c(45, 85, 65, 105)
  )
  
  # Test with multiple dte values
  spreads <- create_vertical_spreads(
    optionsData = test_data,
    dte = as.integer(c(14, 45)),
    dist = 100,  # Distance between strikes is now 100
    callPos = "callAskPrice",
    putPos = "putAskPrice",
    vsType = c("lcs", "lps")
  )
  
  # Check number of rows (should be 4: 2 dte * 2 spread types)
  expect_equal(nrow(spreads), 4)
  
  # Check all spread types are present
  expect_setequal(unique(spreads$vsType), c("lcs", "lps"))
  
  # Check all dte values are present
  expect_setequal(unique(spreads$dte), c(14, 45))
})

test_that("create_vertical_spreads handles invalid data", {
  # Test with missing required columns
  test_data <- create_test_data()
  test_data$ticker <- NULL
  
  expect_error(
    create_vertical_spreads(
      optionsData = test_data,
      dte = as.integer(14),
      dist = 50,
      callPos = "callAskPrice",
      putPos = "putAskPrice",
      vsType = "lcs"
    ),
    "Missing required columns in optionsData"
  )
  
  # Test with invalid bid-ask relationship
  test_data <- create_test_data()
  test_data$callBidPrice[1] <- 110  # Higher than ask price
  
  expect_error(
    create_vertical_spreads(
      optionsData = test_data,
      dte = as.integer(14),
      dist = 50,
      callPos = "callAskPrice",
      putPos = "putAskPrice",
      vsType = "lcs"
    ),
    "callBidPrice must be less than or equal to callAskPrice"
  )
  
  # Test with invalid vsType
  test_data <- create_test_data()
  expect_error(
    create_vertical_spreads(
      optionsData = test_data,
      dte = as.integer(14),
      dist = 50,
      callPos = "callAskPrice",
      putPos = "putAskPrice",
      vsType = "invalid"
    ),
    "vsType must be one of: 'lcs', 'scs', 'lps', 'sps'"
  )
})

test_that("create_vertical_spreads handles no valid spreads", {
  # Test with no valid strike pairs
  test_data <- create_test_data()
  test_data$strike <- c(3900, 3901, 3902, 3903)  # No pairs with dist=50
  
  # Expect warning and NULL return
  expect_warning(
    result <- create_vertical_spreads(
      optionsData = test_data,
      dte = as.integer(14),
      dist = 50,
      callPos = "callAskPrice",
      putPos = "putAskPrice",
      vsType = "lcs"
    ),
    "No valid vertical spreads found"
  )
  
  expect_null(result)
})

test_that("create_vertical_spreads saves results to CSV when specified", {
  # Create test data
  test_data <- create_test_data()
  
  # Create temporary file path
  output_file <- tempfile(fileext = ".csv")
  
  # Test with CSV output
  spreads <- create_vertical_spreads(
    optionsData = test_data,
    dte = as.integer(14),
    dist = 50,
    callPos = "callAskPrice",
    putPos = "putAskPrice",
    vsType = c("lcs", "scs", "lps", "sps"),
    output_file = output_file
  )
  
  # Verify CSV was created
  expect_true(file.exists(output_file))
  
  # Verify CSV content
  csv_data <- read.csv(output_file)
  expect_equal(nrow(csv_data), nrow(spreads))
  expect_equal(names(csv_data), names(spreads))
  
  # Clean up
  unlink(output_file)
})

test_that("create_vertical_spreads works with database data", {
  # Connect to PostgreSQL database
  tryCatch({
    con <- dbConnect(
      PostgreSQL(),
      dbname = "oa_indexes",
      host = "localhost",
      port = 5432,
      user = "justinjtownsend",
      password = Sys.getenv("PGSQL_PW"),
      options = "-c search_path=opts_hist"
    )
  }, error = function(e) {
    skip(paste("Could not connect to database:", e$message))
  })
  
  # Query recent options data
  query <- "
    SELECT 
      symbol as ticker,
      date::text as tradeDate,
      date::text as expirDate,
      30 as dte,
      close as strike,
      close as stockPrice,
      close as spotPrice,
      close * 0.01 as callBidPrice,
      close * 0.02 as callAskPrice,
      close * 0.01 as putBidPrice,
      close * 0.02 as putAskPrice
    FROM price_hist
    WHERE date = (SELECT MAX(date) FROM price_hist)
    ORDER BY close
    LIMIT 100"
  
  tryCatch({
    db_data <- dbGetQuery(con, query)
    dbDisconnect(con)
  }, error = function(e) {
    dbDisconnect(con)
    skip(paste("Could not fetch data from database:", e$message))
  })
  
  # Skip if no data returned
  if (nrow(db_data) == 0) {
    skip("No data available from database")
  }
  
  # Create output file path
  output_file <- "db_vertical_spreads.csv"
  
  # Create vertical spreads
  spreads <- create_vertical_spreads(
    optionsData = db_data,
    dte = as.integer(unique(db_data$dte)),
    dist = 50,  # Using 50-point spreads for SPX
    callPos = "callAskPrice",
    putPos = "putAskPrice",
    vsType = c("lcs", "scs", "lps", "sps"),
    output_file = output_file
  )
  
  # Basic validation
  expect_s3_class(spreads, "data.frame")
  expect_true(nrow(spreads) > 0)
  
  # Validate column presence
  required_cols <- c("dte", "vsType", "vsValue", "callValL", "callValH", 
                    "putValL", "putValH", "strikeL", "strikeH", "spreadDist")
  expect_true(all(required_cols %in% names(spreads)))
  
  # Validate spread calculations
  expect_true(all(spreads$spreadDist == 50))
  expect_true(all(spreads$strikeH - spreads$strikeL == 50))
  
  # Verify CSV was created
  expect_true(file.exists(output_file))
  
  # Print summary of results
  cat("\nVertical Spreads Summary:\n")
  cat("Total spreads created:", nrow(spreads), "\n")
  cat("Unique DTEs:", paste(sort(unique(spreads$dte)), collapse=", "), "\n")
  cat("Spread types:", paste(sort(unique(spreads$vsType)), collapse=", "), "\n")
  cat("Strike range:", min(spreads$strikeL), "to", max(spreads$strikeH), "\n")
  
  # Clean up
  unlink(output_file)
}) 