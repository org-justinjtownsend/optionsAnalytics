#' Test opt_extract_std function
#'
#' @description Tests for the opt_extract_std function that extracts options data from the database.
#' These tests verify the function's behavior with various inputs and error conditions.

# Load required packages
library(testthat)
library(DBI)
library(RPostgres)

# Mock the database connection function
mock_connect_to_db <- function(schema, table) {
  # Create a mock connection object
  mock_con <- list(
    isValid = TRUE,
    disconnect = function() TRUE
  )
  class(mock_con) <- "DBIConnection"
  return(mock_con)
}

# Mock the dbGetQuery function
mock_dbGetQuery <- function(con, query) {
  # Create a sample data frame with the expected structure
  data.frame(
    ticker = c("AAPL", "MSFT"),
    tradeDate = c("2024-01-01", "2024-01-01"),
    expirDate = c("2024-09-20", "2024-09-20"),
    dte = c(100, 100),
    strike = c(150, 200),
    stockPrice = c(180, 250),
    callValue = c(30, 40),
    callVolume = c(1000, 2000),
    callOpenInterest = c(5000, 6000),
    callBidSize = c(10, 20),
    callAskSize = c(15, 25),
    callBidPrice = c(28, 38),
    callAskPrice = c(32, 42),
    putValue = c(20, 30),
    putVolume = c(800, 1800),
    putOpenInterest = c(4000, 5000),
    putBidSize = c(8, 18),
    putAskSize = c(12, 22),
    putBidPrice = c(18, 28),
    putAskPrice = c(22, 32),
    smvVol = c(0.25, 0.30),
    callMidIv = c(0.28, 0.33),
    putMidIv = c(0.27, 0.32),
    delta = c(0.6, 0.7),
    gamma = c(0.02, 0.03),
    theta = c(-0.5, -0.6),
    vega = c(0.4, 0.5),
    rho = c(0.1, 0.2),
    phi = c(-0.1, -0.2),
    spotPrice = c(180, 250)
  )
}

# Test suite
test_that("opt_extract_std works with valid inputs", {
  # Mock the required functions
  with_mock(
    connect_to_db = mock_connect_to_db,
    dbGetQuery = mock_dbGetQuery,
    {
      # Test with NULL expirDate
      result1 <- opt_extract_std()
      expect_s3_class(result1, "data.frame")
      expect_equal(nrow(result1), 2)
      expect_equal(ncol(result1), 30)
      
      # Test with valid expirDate
      result2 <- opt_extract_std(expirDate = as.character('2024-09-20'))
      expect_s3_class(result2, "data.frame")
      expect_equal(nrow(result2), 2)
      expect_equal(ncol(result2), 30)
    }
  )
})

test_that("opt_extract_std handles invalid expirDate format", {
  # Test with invalid date format
  expect_error(
    opt_extract_std(expirDate = "2024/09/20"),
    "expirDate must be in 'YYYY-MM-DD' format as a character string"
  )
  
  # Test with non-character input
  expect_error(
    opt_extract_std(expirDate = 20240920),
    "expirDate must be a character string in 'YYYY-MM-DD' format"
  )
})

test_that("opt_extract_std verifies all required columns", {
  # Mock dbGetQuery to return incomplete data
  mock_incomplete_data <- function(con, query) {
    data.frame(
      ticker = c("AAPL"),
      tradeDate = c("2024-01-01")
      # Missing other required columns
    )
  }
  
  with_mock(
    connect_to_db = mock_connect_to_db,
    dbGetQuery = mock_incomplete_data,
    {
      expect_error(
        opt_extract_std(),
        "Missing columns in result"
      )
    }
  )
})

test_that("opt_extract_std handles database connection errors", {
  # Mock connect_to_db to return NULL
  mock_failed_connection <- function(schema, table) NULL
  
  with_mock(
    connect_to_db = mock_failed_connection,
    {
      expect_error(
        opt_extract_std(),
        "Failed to connect to database"
      )
    }
  )
}) 