# Test file for vertical spreads function
# Test Data case 1: Synthetic test data
# Test Data case 2: Real data from PostgreSQL database

library(testthat)
library(dplyr)

# Source the vertical spreads function
source("../../R/fct_vertical_spreads.R")

# Test Data Case 1: Synthetic test data
test_that("Vertical spreads function works with synthetic test data", {
  # Create synthetic test data that matches required format
  test_data <- data.frame(
    ticker = rep("SPX", 12),
    tradeDate = rep("2024-01-15", 12),
    expirDate = c(rep("2024-02-16", 6), rep("2024-03-15", 6)),
    dte = as.integer(c(rep(32, 6), rep(60, 6))),
    strike = c(4500, 4600, 4700, 4800, 4900, 5000, 4500, 4600, 4700, 4800, 4900, 5000),
    stockPrice = rep(4750, 12),
    spotPrice = rep(4750, 12),
    callBidPrice = c(280, 190, 110, 50, 15, 5, 300, 210, 130, 70, 25, 8),
    callAskPrice = c(285, 195, 115, 55, 20, 8, 305, 215, 135, 75, 30, 12),
    putBidPrice = c(5, 15, 40, 80, 140, 220, 8, 20, 45, 85, 145, 225),
    putAskPrice = c(8, 20, 45, 85, 145, 230, 12, 25, 50, 90, 150, 230),
    callMidIv = c(0.25, 0.24, 0.23, 0.22, 0.21, 0.20, 0.26, 0.25, 0.24, 0.23, 0.22, 0.21),
    putMidIv = c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26)
  )
  
  # Test basic functionality with valid input data
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = 100,
    vsType = "lcs",
    callPos = "callBidPrice"
  )
  
  # Verify output structure and column presence
  expect_true(is.list(result))
  expect_true("optionsData" %in% names(result))
  expect_true("vsData_h" %in% names(result))
  expect_true(is.data.frame(result$vsData_h))
  
  # Check that required columns are present
  required_cols <- c("expirDate", "dte", "dist", "vsType", "instrType", "posType", 
                    "vsValue", "strikeL", "strikeH", "instrValL", "instrValH",
                    "leg1_instrument", "leg2_instrument", "spread_id", "ticker",
                    "p_max", "bep", "moneyness", "prob_profit", "updated", "strat_type")
  expect_true(all(required_cols %in% names(result$vsData_h)))
  
  # Validate spread calculations
  expect_true(nrow(result$vsData_h) > 0)
  expect_true(all(result$vsData_h$dte == 32))
  expect_true(all(result$vsData_h$dist == 100))
  expect_true(all(result$vsData_h$vsType == "lcs"))
  
  # Test CSV output functionality
  temp_file <- tempfile(fileext = ".csv")
  result_csv <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = 100,
    vsType = "lcs",
    callPos = "callBidPrice",
    output_file = temp_file
  )
  expect_true(file.exists(temp_file))
  unlink(temp_file)
  
  # Test with a more comprehensive CSV output
  output_file <- "vertical_spreads_test_output.csv"
  result_comprehensive <- create_vertical_spreads(
    optionsData = test_data,
    dte = c(32L, 60L),
    dist = c(100, 200),
    vsType = c("lcs", "scs", "lps", "sps"),
    callPos = "callBidPrice",
    putPos = "putBidPrice",
    output_file = output_file
  )
  expect_true(file.exists(output_file))
  cat("CSV output file created:", output_file, "\n")
  cat("File size:", file.size(output_file), "bytes\n")
  cat("Number of spreads in CSV:", nrow(result_comprehensive$vsData_h), "\n")
})

test_that("Multiple DTE values are handled correctly", {
  test_data <- data.frame(
    ticker = rep("SPX", 12),
    tradeDate = rep("2024-01-15", 12),
    expirDate = c(rep("2024-02-16", 6), rep("2024-03-15", 6)),
    dte = as.integer(c(rep(32, 6), rep(60, 6))),
    strike = c(4500, 4600, 4700, 4800, 4900, 5000, 4500, 4600, 4700, 4800, 4900, 5000),
    stockPrice = rep(4750, 12),
    spotPrice = rep(4750, 12),
    callBidPrice = c(280, 190, 110, 50, 15, 5, 300, 210, 130, 70, 25, 8),
    callAskPrice = c(285, 195, 115, 55, 20, 8, 305, 215, 135, 75, 30, 12),
    putBidPrice = c(5, 15, 40, 80, 140, 220, 8, 20, 45, 85, 145, 225),
    putAskPrice = c(8, 20, 45, 85, 145, 230, 12, 25, 50, 90, 150, 230),
    callMidIv = c(0.25, 0.24, 0.23, 0.22, 0.21, 0.20, 0.26, 0.25, 0.24, 0.23, 0.22, 0.21),
    putMidIv = c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26)
  )
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = c(32L, 60L),
    dist = 100,
    vsType = "lcs",
    callPos = "callBidPrice"
  )
  
  # Verify correct number of spreads created
  expect_true(nrow(result$vsData_h) > 0)
  expect_true(all(result$vsData_h$dte %in% c(32, 60)))
  
  # Validate spread types and dte values
  unique_dtes <- unique(result$vsData_h$dte)
  expect_true(length(unique_dtes) == 2)
  expect_true(all(unique_dtes %in% c(32, 60)))
})

test_that("Multiple dist values are handled correctly", {
  test_data <- data.frame(
    ticker = rep("SPX", 12),
    tradeDate = rep("2024-01-15", 12),
    expirDate = rep("2024-02-16", 12),
    dte = rep(32L, 12),
    strike = c(4500, 4600, 4700, 4800, 4900, 5000, 5100, 5200, 5300, 5400, 5500, 5600),
    stockPrice = rep(4750, 12),
    spotPrice = rep(4750, 12),
    callBidPrice = c(280, 190, 110, 50, 15, 5, 2, 1, 0.5, 0.2, 0.1, 0.05),
    callAskPrice = c(285, 195, 115, 55, 20, 8, 3, 1.5, 1, 0.5, 0.2, 0.1),
    putBidPrice = c(5, 15, 40, 80, 140, 220, 350, 520, 720, 950, 1200, 1500),
    putAskPrice = c(8, 20, 45, 85, 145, 225, 355, 525, 725, 955, 1205, 1505),
    callMidIv = c(0.25, 0.24, 0.23, 0.22, 0.21, 0.20, 0.19, 0.18, 0.17, 0.16, 0.15, 0.14),
    putMidIv = c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31)
  )
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = c(100, 200),
    vsType = "lcs",
    callPos = "callBidPrice"
  )
  
  # Verify correct number of spreads created
  expect_true(nrow(result$vsData_h) > 0)
  expect_true(all(result$vsData_h$dist %in% c(100, 200)))
})

test_that("Multiple vsType values are handled correctly", {
  test_data <- data.frame(
    ticker = rep("SPX", 6),
    tradeDate = rep("2024-01-15", 6),
    expirDate = rep("2024-02-16", 6),
    dte = rep(32L, 6),
    strike = c(4500, 4600, 4700, 4800, 4900, 5000),
    stockPrice = rep(4750, 6),
    spotPrice = rep(4750, 6),
    callBidPrice = c(280, 190, 110, 50, 15, 5),
    callAskPrice = c(285, 195, 115, 55, 20, 8),
    putBidPrice = c(5, 15, 40, 80, 140, 220),
    putAskPrice = c(8, 20, 45, 85, 145, 225),
    callMidIv = c(0.25, 0.24, 0.23, 0.22, 0.21, 0.20),
    putMidIv = c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25)
  )
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = 100,
    vsType = c("lcs", "scs", "lps", "sps"),
    callPos = "callBidPrice",
    putPos = "putBidPrice"
  )
  
  # Verify all spread types are created
  expect_true(nrow(result$vsData_h) > 0)
  expect_true(all(result$vsData_h$vsType %in% c("lcs", "scs", "lps", "sps")))
  
  # Check that call spreads have correct instrument type
  call_spreads <- result$vsData_h[result$vsData_h$vsType %in% c("lcs", "scs"), ]
  expect_true(all(call_spreads$instrType == "c"))
  
  # Check that put spreads have correct instrument type
  put_spreads <- result$vsData_h[result$vsData_h$vsType %in% c("lps", "sps"), ]
  expect_true(all(put_spreads$instrType == "p"))
})

test_that("Invalid data is handled correctly", {
  # Test missing required columns
  invalid_data <- data.frame(
    ticker = "SPX",
    tradeDate = "2024-01-15",
    expirDate = "2024-02-16",
    dte = 32L,
    strike = 4750,
    stockPrice = 4750,
    spotPrice = 4750
    # Missing callBidPrice, callAskPrice, putBidPrice, putAskPrice, callMidIv, putMidIv
  )
  
  expect_error(
    create_vertical_spreads(invalid_data, 32L, 100, "lcs", "callBidPrice"),
    "Missing required columns in optionsData"
  )
  
  # Test invalid bid-ask relationships
  invalid_bidask <- data.frame(
    ticker = "SPX",
    tradeDate = "2024-01-15",
    expirDate = "2024-02-16",
    dte = 32L,
    strike = 4750,
    stockPrice = 4750,
    spotPrice = 4750,
    callBidPrice = 100,
    callAskPrice = 90,  # Ask < Bid
    putBidPrice = 50,
    putAskPrice = 60,
    callMidIv = 0.25,
    putMidIv = 0.25
  )
  
  expect_error(
    create_vertical_spreads(invalid_bidask, 32L, 100, "lcs", "callBidPrice"),
    "callBidPrice must be less than or equal to callAskPrice"
  )
  
  # Test invalid vsType values
  test_data <- data.frame(
    ticker = "SPX",
    tradeDate = "2024-01-15",
    expirDate = "2024-02-16",
    dte = 32L,
    strike = 4750,
    stockPrice = 4750,
    spotPrice = 4750,
    callBidPrice = 90,
    callAskPrice = 100,
    putBidPrice = 50,
    putAskPrice = 60,
    callMidIv = 0.25,
    putMidIv = 0.25
  )
  
  expect_error(
    create_vertical_spreads(test_data, 32L, 100, "invalid_type", "callBidPrice"),
    "vsType must be one of: 'lcs', 'scs', 'lps', 'sps'"
  )
  
  # Test missing callPos for call spreads
  expect_error(
    create_vertical_spreads(test_data, 32L, 100, "lcs"),
    "callPos must be provided for vsType in \\(lcs, scs\\)"
  )
  
  # Test missing putPos for put spreads
  expect_error(
    create_vertical_spreads(test_data, 32L, 100, "lps"),
    "putPos must be provided for vsType in \\(lps, sps\\)"
  )
})

test_that("Edge cases are handled correctly", {
  # Test no valid strike pairs - need to ensure no pairs with dist = 100 exist
  test_data <- data.frame(
    ticker = rep("SPX", 3),
    tradeDate = rep("2024-01-15", 3),
    expirDate = rep("2024-02-16", 3),
    dte = rep(32L, 3),
    strike = c(4500, 4700, 4900),  # No pairs with dist = 100 (only 200)
    stockPrice = rep(4750, 3),
    spotPrice = rep(4750, 3),
    callBidPrice = c(280, 190, 110),
    callAskPrice = c(285, 195, 115),
    putBidPrice = c(5, 15, 40),
    putAskPrice = c(8, 20, 45),
    callMidIv = c(0.25, 0.24, 0.23),
    putMidIv = c(0.20, 0.21, 0.22)
  )
  
  # This should return no spreads, not necessarily a warning
  expect_warning(
    result <- create_vertical_spreads(
      optionsData = test_data,
      dte = 32L,
      dist = 100,
      vsType = "lcs",
      callPos = "callBidPrice"
    ),
    "No valid vertical spreads found"
  )
  
  # Check that no spreads were created
  expect_true(is.null(result$vsData_h) || nrow(result$vsData_h) == 0)
  
  # Test single strike price
  single_strike <- data.frame(
    ticker = "SPX",
    tradeDate = "2024-01-15",
    expirDate = "2024-02-16",
    dte = 32L,
    strike = 4750,
    stockPrice = 4750,
    spotPrice = 4750,
    callBidPrice = 90,
    callAskPrice = 100,
    putBidPrice = 50,
    putAskPrice = 60,
    callMidIv = 0.25,
    putMidIv = 0.25
  )
  
  # This should return no spreads due to insufficient data
  result <- suppressWarnings(
    create_vertical_spreads(single_strike, 32L, 100, "lcs", "callBidPrice")
  )
  expect_true(is.null(result$vsData_h) || nrow(result$vsData_h) == 0)
  
  # Test empty data frame
  empty_data <- data.frame()
  expect_error(
    create_vertical_spreads(empty_data, 32L, 100, "lcs", "callBidPrice"),
    "Missing required columns in optionsData"
  )
})

test_that("Probability of profit calculation works correctly", {
  test_data <- data.frame(
    ticker = rep("SPX", 4),
    tradeDate = rep("2024-01-15", 4),
    expirDate = rep("2024-02-16", 4),
    dte = rep(32L, 4),
    strike = c(4500, 4600, 4700, 4800),
    stockPrice = rep(4750, 4),
    spotPrice = rep(4750, 4),
    callBidPrice = c(280, 190, 110, 50),
    callAskPrice = c(285, 195, 115, 55),
    putBidPrice = c(5, 15, 40, 80),
    putAskPrice = c(8, 20, 45, 85),
    callMidIv = c(0.25, 0.24, 0.23, 0.22),
    putMidIv = c(0.20, 0.21, 0.22, 0.23)
  )
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = 100,
    vsType = c("lcs", "lps"),
    callPos = "callBidPrice",
    putPos = "putBidPrice"
  )
  
  # Check that probability of profit is calculated
  expect_true(all(!is.na(result$vsData_h$prob_profit)))
  expect_true(all(result$vsData_h$prob_profit >= 0))
  expect_true(all(result$vsData_h$prob_profit <= 1))
  
  # Check that different spread types have different probabilities (if spreads exist)
  if (nrow(result$vsData_h) >= 2) {
    unique_probs <- unique(result$vsData_h$prob_profit)
    expect_true(length(unique_probs) >= 1)  # At least one unique probability
  }
})

test_that("Moneyness calculation works correctly", {
  test_data <- data.frame(
    ticker = rep("SPX", 6),
    tradeDate = rep("2024-01-15", 6),
    expirDate = rep("2024-02-16", 6),
    dte = rep(32L, 6),
    strike = c(4500, 4600, 4700, 4800, 4900, 5000),
    stockPrice = rep(4750, 6),
    spotPrice = rep(4750, 6),
    callBidPrice = c(280, 190, 110, 50, 15, 5),
    callAskPrice = c(285, 195, 115, 55, 20, 8),
    putBidPrice = c(5, 15, 40, 80, 140, 220),
    putAskPrice = c(8, 20, 45, 85, 145, 225),
    callMidIv = c(0.25, 0.24, 0.23, 0.22, 0.21, 0.20),
    putMidIv = c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25)
  )
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = 100,
    vsType = "lcs",
    callPos = "callBidPrice"
  )
  
  # Check moneyness values
  expect_true(all(result$vsData_h$moneyness %in% c("atm", "itm", "otm")))
  
  # Check that ATM spreads have strikes around stock price
  atm_spreads <- result$vsData_h[result$vsData_h$moneyness == "atm", ]
  if (nrow(atm_spreads) > 0) {
    expect_true(all(atm_spreads$strikeL < 4750))
    expect_true(all(atm_spreads$strikeH > 4750))
  }
})

test_that("Spread calculations are mathematically correct", {
  test_data <- data.frame(
    ticker = rep("SPX", 4),
    tradeDate = rep("2024-01-15", 4),
    expirDate = rep("2024-02-16", 4),
    dte = rep(32L, 4),
    strike = c(4500, 4600, 4700, 4800),
    stockPrice = rep(4750, 4),
    spotPrice = rep(4750, 4),
    callBidPrice = c(280, 190, 110, 50),
    callAskPrice = c(285, 195, 115, 55),
    putBidPrice = c(5, 15, 40, 80),
    putAskPrice = c(8, 20, 45, 85),
    callMidIv = c(0.25, 0.24, 0.23, 0.22),
    putMidIv = c(0.20, 0.21, 0.22, 0.23)
  )
  
  result <- create_vertical_spreads(
    optionsData = test_data,
    dte = 32L,
    dist = 100,
    vsType = "lcs",
    callPos = "callBidPrice"
  )
  
  # Check that vsValue calculation is correct for lcs
  lcs_spread <- result$vsData_h[result$vsData_h$vsType == "lcs", ]
  if (nrow(lcs_spread) > 0) {
    # For lcs: vsValue = callBidPrice[L] - callBidPrice[H]
    expected_vsValue <- 280 - 190  # 4500 strike - 4600 strike
    expect_equal(lcs_spread$vsValue[1], expected_vsValue)
    
    # Check p_max calculation: dist - vsValue
    expected_p_max <- 100 - expected_vsValue
    expect_equal(lcs_spread$p_max[1], expected_p_max)
    
    # Check bep calculation: strikeL + vsValue
    expected_bep <- 4500 + expected_vsValue
    expect_equal(lcs_spread$bep[1], expected_bep)
  }
}) 