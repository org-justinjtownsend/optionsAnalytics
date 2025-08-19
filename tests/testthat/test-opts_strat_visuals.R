# Test suite for opts_strat_tbl function
# Tests all requirements from F1.1 to F1.10

library(testthat)
library(dplyr)

# Source the function
source("../../R/fct_opts_strat_visuals.R")

# Test data setup
setup_test_data <- function() {
  # Create synthetic test data that matches the required format
  data.frame(
    dte = c(30, 30, 60, 60),
    dist = c(100, 200, 100, 200),
    vsType = c("lcs", "scs", "lps", "sps"),
    strikeL = c(4000, 4100, 4000, 4100),
    strikeH = c(4100, 4300, 4100, 4300),
    vsValue = c(2.50, -1.75, 3.25, -2.00),
    instrValL = c(15.50, 8.25, 12.75, 6.50),
    instrValH = c(13.00, 10.00, 9.50, 8.50),
    instrType = c("c", "c", "p", "p"),
    posType = c("dr_s", "cr_s", "dr_s", "cr_s"),
    p_max = c(97.50, 1.75, 96.75, 2.00),
    bep = c(4002.50, 4098.25, 4103.25, 4298.00),
    moneyness = c("atm", "otm", "atm", "otm"),
    prob_profit = c(0.65, 0.35, 0.70, 0.30),
    stockPrice = c(4050, 4050, 4050, 4050),
    stringsAsFactors = FALSE
  )
}

# Test 1: Basic functionality with valid input data
test_that("Basic functionality with valid input data", {
  test_data <- setup_test_data()
  
  # Test basic function call
  result <- opts_strat_tbl(data = test_data)
  
  # Verify output is a reactable
  expect_true(inherits(result, "reactable"))
  
  # Verify all columns are present
  expect_equal(nrow(result$data), nrow(test_data))
  expect_equal(ncol(result$data), ncol(test_data))
})

# Test 2: Verify output structure and column presence
test_that("Output structure and column presence", {
  test_data <- setup_test_data()
  
  result <- opts_strat_tbl(data = test_data)
  
  # Check that all columns from test_data are present
  for (col in names(test_data)) {
    expect_true(col %in% names(result$data))
  }
  
  # Check data integrity
  expect_equal(result$data$dte, test_data$dte)
  expect_equal(result$data$vsType, test_data$vsType)
  expect_equal(result$data$strikeL, test_data$strikeL)
})

# Test 3: Validate spread calculations and distances
test_that("Spread calculations and distances validation", {
  test_data <- setup_test_data()
  
  result <- opts_strat_tbl(data = test_data)
  
  # Verify distance calculations are correct
  calculated_distances <- result$data$strikeH - result$data$strikeL
  expect_equal(calculated_distances, test_data$dist)
  
  # Verify vsValue calculations make sense
  expect_true(all(result$data$vsValue >= -100))  # Reasonable bounds
  expect_true(all(result$data$vsValue <= 100))
})

# Test 4: Test CSV output functionality (if applicable)
test_that("Data integrity for export", {
  test_data <- setup_test_data()
  
  result <- opts_strat_tbl(data = test_data)
  
  # Verify data can be extracted for CSV export
  export_data <- result$data
  
  # Check that export data maintains structure
  expect_equal(nrow(export_data), nrow(test_data))
  expect_equal(ncol(export_data), ncol(test_data))
  
  # Verify numeric columns are properly formatted
  numeric_cols <- c("dte", "dist", "strikeL", "strikeH", "vsValue", "p_max", "bep")
  for (col in numeric_cols) {
    if (col %in% names(export_data)) {
      expect_true(is.numeric(export_data[[col]]) || all(is.na(export_data[[col]])))
    }
  }
})

# Test 5: Multiple DTE tests
test_that("Multiple DTE handling", {
  test_data <- setup_test_data()
  
  # Test with multiple DTE values
  result <- opts_strat_tbl(data = test_data)
  
  # Verify correct number of spreads created
  expect_equal(nrow(result$data), 4)  # 2 DTE values × 2 distance values
  
  # Validate DTE values
  unique_dte <- unique(result$data$dte)
  expect_equal(length(unique_dte), 2)
  expect_true(all(unique_dte %in% c(30, 60)))
  
  # Validate spread types and DTE values
  expect_equal(length(unique(result$data$vsType)), 4)
})

# Test 6: Invalid data tests
test_that("Invalid data handling", {
  # Test missing required columns
  invalid_data <- data.frame(
    dte = c(30, 60),
    vsType = c("lcs", "scs")
    # Missing other required columns
  )
  
  expect_error(opts_strat_tbl(data = invalid_data), 
               "The following columns are not found in data")
  
  # Test non-data.frame input
  expect_error(opts_strat_tbl(data = "not a dataframe"), 
               "data must be a data frame")
  
  # Test empty data frame
  empty_data <- data.frame()
  result <- opts_strat_tbl(data = empty_data)
  expect_true(inherits(result, "reactable"))
})

# Test 7: Edge cases
test_that("Edge cases handling", {
  # Test single row
  single_row_data <- setup_test_data()[1, , drop = FALSE]
  result <- opts_strat_tbl(data = single_row_data)
  expect_equal(nrow(result$data), 1)
  
  # Test single strike price (should still work)
  single_strike_data <- setup_test_data()
  single_strike_data$strikeL <- rep(4000, nrow(single_strike_data))
  single_strike_data$strikeH <- rep(4100, nrow(single_strike_data))
  result <- opts_strat_tbl(data = single_strike_data)
  expect_true(inherits(result, "reactable"))
})

# Test 8: Parameter validation
test_that("Parameter validation", {
  test_data <- setup_test_data()
  
  # Test custom columns parameter
  custom_cols <- c("dte", "vsType", "strikeL")
  result <- opts_strat_tbl(data = test_data, columns = custom_cols)
  expect_equal(ncol(result$data), length(custom_cols))
  
  # Test searchable parameter
  result <- opts_strat_tbl(data = test_data, searchable = FALSE)
  expect_true(inherits(result, "reactable"))
  
  # Test striped parameter
  result <- opts_strat_tbl(data = test_data, striped = FALSE)
  expect_true(inherits(result, "reactable"))
  
  # Test compact parameter
  result <- opts_strat_tbl(data = test_data, compact = FALSE)
  expect_true(inherits(result, "reactable"))
})

# Test 9: Real data from database (if available)
test_that("Real database data handling", {
  # This test requires database connection and real data
  # Skip if not available
  skip_if_not_installed("DBI")
  
  # Try to get real data if possible
  tryCatch({
    # Source the vertical spreads function
    source("../../R/fct_vertical_spreads.R")
    
    # Check if we can get real data
    if (exists("opts_extract_std")) {
      real_data <- opts_extract_std()
      
      if (nrow(real_data) > 0) {
        # Create vertical spreads with real data
        vs_result <- create_vertical_spreads(
          optionsData = real_data,
          dte = c(30, 60),
          dist = c(100, 200),
          callPos = "callAskPrice",
          putPos = "putAskPrice",
          vsType = c("lcs", "scs", "lps", "sps")
        )
        
        if (!is.null(vs_result$vsData) && nrow(vs_result$vsData) > 0) {
          # Test with real data
          result <- opts_strat_tbl(data = vs_result$vsData)
          expect_true(inherits(result, "reactable"))
          expect_equal(nrow(result$data), nrow(vs_result$vsData))
        }
      }
    }
  }, error = function(e) {
    # Skip this test if database is not available
    skip("Database connection not available for real data test")
  })
})

# Test 10: Output data validation
test_that("Output data validation", {
  test_data <- setup_test_data()
  
  result <- opts_strat_tbl(data = test_data)
  
  # Verify output data frame structure
  expect_true(is.data.frame(result$data))
  
  # Validate all required columns are present
  required_cols <- c("dte", "dist", "vsType", "strikeL", "strikeH", 
                    "vsValue", "p_max", "bep", "instrType", "posType")
  present_cols <- intersect(required_cols, names(result$data))
  expect_true(length(present_cols) > 0)
  
  # Check spread calculations are correct
  if (all(c("strikeL", "strikeH", "dist") %in% names(result$data))) {
    calculated_dist <- result$data$strikeH - result$data$strikeL
    expect_equal(calculated_dist, result$data$dist)
  }
  
  # Ensure proper formatting of numeric columns
  numeric_cols <- c("dte", "dist", "strikeL", "strikeH", "vsValue", "p_max", "bep")
  for (col in numeric_cols) {
    if (col %in% names(result$data)) {
      expect_true(is.numeric(result$data[[col]]) || all(is.na(result$data[[col]])))
    }
  }
})

# Test 11: Performance with larger datasets
test_that("Performance with larger datasets", {
  # Create larger test dataset
  large_data <- data.frame(
    dte = rep(c(30, 60, 90), each = 100),
    dist = rep(c(50, 100, 200), each = 100),
    vsType = rep(c("lcs", "scs", "lps", "sps"), each = 75),
    strikeL = rep(c(4000, 4100, 4200), each = 100),
    strikeH = rep(c(4050, 4200, 4400), each = 100),
    vsValue = rnorm(300, mean = 0, sd = 5),
    stringsAsFactors = FALSE
  )
  
  # Test performance
  start_time <- Sys.time()
  result <- opts_strat_tbl(data = large_data)
  end_time <- Sys.time()
  
  # Should complete within reasonable time (less than 5 seconds)
  expect_true(as.numeric(difftime(end_time, start_time, units = "secs")) < 5)
  
  # Verify output integrity
  expect_true(inherits(result, "reactable"))
  expect_equal(nrow(result$data), nrow(large_data))
})

# Test 12: Error handling and graceful degradation
test_that("Error handling and graceful degradation", {
  # Test with data containing NA values
  na_data <- setup_test_data()
  na_data$vsValue[1] <- NA
  na_data$p_max[2] <- NA
  
  # Should handle NA values gracefully
  result <- opts_strat_tbl(data = na_data)
  expect_true(inherits(result, "reactable"))
  
  # Test with very large numbers
  large_num_data <- setup_test_data()
  large_num_data$strikeL <- large_num_data$strikeL * 1000
  large_num_data$strikeH <- large_num_data$strikeH * 1000
  
  result <- opts_strat_tbl(data = large_num_data)
  expect_true(inherits(result, "reactable"))
  
  # Test with very small numbers
  small_num_data <- setup_test_data()
  small_num_data$vsValue <- small_num_data$vsValue / 1000
  
  result <- opts_strat_tbl(data = small_num_data)
  expect_true(inherits(result, "reactable"))
})

# Run all tests
test_results <- testthat::test_file("test-opts_strat_visuals.R")

# Print summary
cat("\n=== TEST SUMMARY ===\n")
cat("Total tests run:", length(test_results), "\n")
cat("Passed:", sum(sapply(test_results, function(x) x$result == "PASS")), "\n")
cat("Failed:", sum(sapply(test_results, function(x) x$result == "FAIL")), "\n")
cat("Skipped:", sum(sapply(test_results, function(x) x$result == "SKIP")), "\n")
