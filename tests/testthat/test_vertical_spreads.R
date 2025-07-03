library(testthat)
library(RPostgreSQL)
library(dplyr)

# Source the function directly
source("../../R/fct_vertical_spreads.R")

# Helper function to create synthetic test data
create_test_data <- function() {
  data.frame(
    ticker = rep("SPX", 6),
    tradeDate = rep("2024-03-15", 6),
    expirDate = c("2024-03-22", "2024-03-22", "2024-03-29", "2024-03-29", "2024-03-29", "2024-03-29"),
    dte = c(7L, 7L, 14L, 14L, 14L, 14L),
    strike = c(5000, 5100, 5000, 5100, 5200, 5300),
    stockPrice = rep(5050, 6),
    spotPrice = rep(5050, 6),
    callBidPrice = c(50, 30, 55, 35, 20, 10),
    callAskPrice = c(52, 32, 57, 37, 22, 12),
    putBidPrice = c(30, 50, 35, 55, 70, 90),
    putAskPrice = c(32, 52, 37, 57, 72, 92)
  )
}

# Test Case 1: Valid Data Tests
test_that("create_vertical_spreads handles valid data correctly", {
  test_data <- create_test_data()
  
  # Test basic functionality
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 7,
    dist = 100,
    callPos = "callBidPrice",
    putPos = "putBidPrice",
    vsType = c("lcs", "scs", "lps", "sps")
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_named(result, c("optionsData", "vsData"))
  expect_s3_class(result$vsData, "data.frame")
  
  # Check required columns
  required_cols <- c("dte", "vsType", "vsValue", "instrValL", "instrValH", 
                    "strikeL", "strikeH", "dist", "instrType", "posType", 
                    "p_max", "bep", "updated")
  expect_true(all(required_cols %in% names(result$vsData)))
  
  # Check spread calculations
  expect_true(all(result$vsData$dist == 100))
  expect_true(all(result$vsData$strikeH - result$vsData$strikeL == 100))
  
  # Check instrument and position types
  expect_true(all(result$vsData$instrType[result$vsData$vsType %in% c("lcs", "scs")] == "c"))
  expect_true(all(result$vsData$instrType[result$vsData$vsType %in% c("lps", "sps")] == "p"))
  expect_true(all(result$vsData$posType[result$vsData$vsType %in% c("scs", "sps")] == "cr_s"))
  expect_true(all(result$vsData$posType[result$vsData$vsType %in% c("lcs", "lps")] == "dr_s"))
})

# Test Case 2: Multiple DTE Tests
test_that("create_vertical_spreads handles multiple dte values correctly", {
  test_data <- create_test_data()
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = c(7, 14),
    dist = 100,
    callPos = "callBidPrice",
    putPos = "putBidPrice",
    vsType = c("lcs", "scs")
  )
  
  # Check multiple dte handling
  expect_equal(length(unique(result$vsData$dte)), 2)
  expect_true(all(c(7, 14) %in% result$vsData$dte))
  
  # Check spread types
  expect_equal(length(unique(result$vsData$vsType)), 2)
  expect_true(all(c("lcs", "scs") %in% result$vsData$vsType))
})

# Test Case 2b: Multiple DIST Tests
test_that("create_vertical_spreads handles multiple dist values correctly", {
  test_data <- create_test_data()
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 14,
    dist = c(100, 200),
    callPos = "callBidPrice",
    putPos = "putBidPrice",
    vsType = c("lcs", "scs")
  )
  
  # Check multiple dist handling
  expect_equal(length(unique(result$vsData$dist)), 2)
  expect_true(all(c(100, 200) %in% result$vsData$dist))
  
  # Check that we have spreads for both distances
  dist_100_spreads <- result$vsData[result$vsData$dist == 100, ]
  dist_200_spreads <- result$vsData[result$vsData$dist == 200, ]
  
  expect_gt(nrow(dist_100_spreads), 0)
  expect_gt(nrow(dist_200_spreads), 0)
  
  # Check strike differences match distances
  expect_true(all(dist_100_spreads$strikeH - dist_100_spreads$strikeL == 100))
  expect_true(all(dist_200_spreads$strikeH - dist_200_spreads$strikeL == 200))
})

# Test Case 2c: Multiple DTE and DIST Tests
test_that("create_vertical_spreads handles multiple dte and dist values correctly", {
  test_data <- create_test_data()
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = c(7, 14),
    dist = c(100, 200),
    callPos = "callBidPrice",
    putPos = "putBidPrice",
    vsType = c("lcs", "scs")
  )
  
  # Check multiple dte and dist handling
  expect_equal(length(unique(result$vsData$dte)), 2)
  expect_equal(length(unique(result$vsData$dist)), 2)
  expect_true(all(c(7, 14) %in% result$vsData$dte))
  expect_true(all(c(100, 200) %in% result$vsData$dist))
  
  # Check combinations
  combinations <- expand.grid(dte = c(7, 14), dist = c(100, 200))
  for (i in 1:nrow(combinations)) {
    subset_data <- result$vsData[
      result$vsData$dte == combinations$dte[i] & 
      result$vsData$dist == combinations$dist[i], 
    ]
    # Some combinations might not have valid strikes, so we just check structure
    if (nrow(subset_data) > 0) {
      expect_true(all(subset_data$dte == combinations$dte[i]))
      expect_true(all(subset_data$dist == combinations$dist[i]))
    }
  }
})

# Test Case 3: Invalid Data Tests
test_that("create_vertical_spreads handles invalid data correctly", {
  test_data <- create_test_data()
  
  # Test missing required columns
  invalid_data <- test_data[, -which(names(test_data) == "ticker")]
  expect_error(
    create_vertical_spreads(
      optionsData = invalid_data,
      dte = 7,
      dist = 100,
      callPos = "callBidPrice",
      putPos = "putBidPrice",
      vsType = "lcs"
    ),
    "Missing required columns in optionsData"
  )
  
  # Test invalid bid-ask relationships
  invalid_bid_ask <- test_data
  invalid_bid_ask$callBidPrice[1] <- 60  # Higher than ask price
  expect_error(
    create_vertical_spreads(
      optionsData = invalid_bid_ask,
      dte = 7,
      dist = 100,
      callPos = "callBidPrice",
      putPos = "putBidPrice",
      vsType = "lcs"
    ),
    "callBidPrice must be less than or equal to callAskPrice"
  )
  
  # Test invalid vsType
  expect_error(
    create_vertical_spreads(
      optionsData = test_data,
      dte = 7,
      dist = 100,
      callPos = "callBidPrice",
      putPos = "putBidPrice",
      vsType = "invalid"
    ),
    "vsType must be one of: 'lcs', 'scs', 'lps', 'sps'"
  )
})

# Test Case 4: Edge Cases
test_that("create_vertical_spreads handles edge cases correctly", {
  test_data <- create_test_data()
  
  # Test no valid strike pairs (has enough options but no valid pairs)
  expect_warning(
    result <- create_vertical_spreads(
      optionsData = test_data,
      dte = 7,
      dist = 200,  # No pairs with this distance
      callPos = "callBidPrice",
      putPos = "putBidPrice",
      vsType = "lcs"
    ),
    "No valid vertical spreads found"
  )
  expect_null(result$vsData)
  
  # Test single strike price (not enough options)
  single_strike <- test_data[1, ]
  expect_warning(
    result <- create_vertical_spreads(
      optionsData = single_strike,
      dte = 7,
      dist = 100,
      callPos = "callBidPrice",
      putPos = "putBidPrice",
      vsType = "lcs"
    ),
    "No valid vertical spreads found"
  )
  expect_null(result$vsData)
  
  # Test empty data frame (not enough options)
  empty_data <- test_data[0, ]
  expect_warning(
    result <- create_vertical_spreads(
      optionsData = empty_data,
      dte = 7,
      dist = 100,
      callPos = "callBidPrice",
      putPos = "putBidPrice",
      vsType = "lcs"
    ),
    "No valid vertical spreads found"
  )
  expect_null(result$vsData)
})

# Test Case 5: CSV Output
test_that("create_vertical_spreads handles CSV output correctly", {
  test_data <- create_test_data()
  temp_file <- tempfile(fileext = ".csv")
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 7,
    dist = 100,
    callPos = "callBidPrice",
    putPos = "putBidPrice",
    vsType = "lcs",
    output_file = temp_file
  )
  
  # Check if file was created
  expect_true(file.exists(temp_file))
  
  # Check if file can be read back
  read_back <- read.csv(temp_file)
  expect_s3_class(read_back, "data.frame")
  expect_equal(nrow(read_back), nrow(result$vsData))
  
  # Clean up
  unlink(temp_file)
})

# Test Case 6: Database Data (Skipped for now)
test_that("create_vertical_spreads works with database data", {
  skip("Skipping database test until database connection issues are resolved")
}) 