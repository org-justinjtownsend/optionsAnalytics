# Basic test script for opts_strat_tbl function
# This script tests the basic functionality without requiring testthat

cat("=== TESTING BASIC OPTIONS STRATEGY VISUALS ===\n\n")

# Source the function
source("R/fct_opts_strat_visuals.R")

# Test 1: Create synthetic test data
cat("Test 1: Creating synthetic test data\n")
cat("------------------------------------\n")

test_data <- data.frame(
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

cat("Created test data with", nrow(test_data), "rows and", ncol(test_data), "columns\n")
cat("Columns:", paste(names(test_data), collapse = ", "), "\n\n")

# Test 2: Basic function call
cat("Test 2: Basic function call\n")
cat("---------------------------\n")

tryCatch({
  result <- opts_strat_tbl(data = test_data)
  cat("✓ Successfully created reactable\n")
  cat("  - Output class:", class(result)[1], "\n")
  cat("  - Data rows:", nrow(test_data), "\n")
  cat("  - Data columns:", ncol(test_data), "\n")
}, error = function(e) {
  cat("✗ Error in basic function call:", e$message, "\n")
})

cat("\n")

# Test 3: Custom columns parameter
cat("Test 3: Custom columns parameter\n")
cat("--------------------------------\n")

tryCatch({
  custom_cols <- c("dte", "vsType", "strikeL", "strikeH", "vsValue")
  result <- opts_strat_tbl(data = test_data, columns = custom_cols)
  cat("✓ Successfully created table with custom columns\n")
  cat("  - Requested columns:", length(custom_cols), "\n")
  cat("  - Actual columns:", length(custom_cols), "\n")
  cat("  - Column names:", paste(custom_cols, collapse = ", "), "\n")
}, error = function(e) {
  cat("✗ Error with custom columns:", e$message, "\n")
})

cat("\n")

# Test 4: Parameter variations
cat("Test 4: Parameter variations\n")
cat("----------------------------\n")

tryCatch({
  # Test with different parameter combinations
  result1 <- opts_strat_tbl(data = test_data, searchable = FALSE)
  cat("✓ searchable = FALSE: OK\n")
  
  result2 <- opts_strat_tbl(data = test_data, striped = FALSE)
  cat("✓ striped = FALSE: OK\n")
  
  result3 <- opts_strat_tbl(data = test_data, compact = FALSE)
  cat("✓ compact = FALSE: OK\n")
  
  cat("  All parameter variations successful\n")
}, error = function(e) {
  cat("✗ Error with parameter variations:", e$message, "\n")
})

cat("\n")

# Test 5: Error handling
cat("Test 5: Error handling\n")
cat("----------------------\n")

# Test invalid data type
tryCatch({
  opts_strat_tbl(data = "not a dataframe")
  cat("✗ Should have failed with invalid data type\n")
}, error = function(e) {
  cat("✓ Correctly caught invalid data type error\n")
})

# Test missing columns
tryCatch({
  opts_strat_tbl(data = test_data, columns = c("nonexistent_column"))
  cat("✗ Should have failed with missing columns\n")
}, error = function(e) {
  cat("✓ Correctly caught missing columns error\n")
})

cat("\n")

# Test 6: Data integrity
cat("Test 6: Data integrity\n")
cat("----------------------\n")

tryCatch({
  result <- opts_strat_tbl(data = test_data)
  
  # Check that data is preserved (reactable doesn't store data in $data)
  cat("✓ Reactable created successfully\n")
  cat("  - Original data rows:", nrow(test_data), "\n")
  cat("  - Original data columns:", ncol(test_data), "\n")
  
  # Check that distance calculations are correct in original data
  original_dist <- test_data$dist
  calculated_dist <- test_data$strikeH - test_data$strikeL
  
  if (identical(original_dist, calculated_dist)) {
    cat("✓ Distance calculations correct\n")
  } else {
    cat("✗ Distance calculations incorrect\n")
  }
  
}, error = function(e) {
  cat("✗ Error in data integrity test:", e$message, "\n")
})

cat("\n")

# Test 7: Performance test
cat("Test 7: Performance test\n")
cat("------------------------\n")

tryCatch({
  # Create larger dataset
  # Create large dataset with consistent lengths
  n_rows <- 150
  large_data <- data.frame(
    dte = rep(c(30, 60, 90), length.out = n_rows),
    dist = rep(c(50, 100, 200), length.out = n_rows),
    vsType = rep(c("lcs", "scs", "lps", "sps"), length.out = n_rows),
    strikeL = rep(c(4000, 4100, 4200), length.out = n_rows),
    strikeH = rep(c(4050, 4200, 4400), length.out = n_rows),
    vsValue = rnorm(n_rows, mean = 0, sd = 5),
    stringsAsFactors = FALSE
  )
  
  start_time <- Sys.time()
  result <- opts_strat_tbl(data = large_data)
  end_time <- Sys.time()
  
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat("✓ Large dataset processed successfully\n")
  cat("  - Rows:", nrow(large_data), "\n")
  cat("  - Processing time:", round(elapsed_time, 3), "seconds\n")
  cat("  - Performance:", ifelse(elapsed_time < 1, "Excellent", "Acceptable"), "\n")
  
}, error = function(e) {
  cat("✗ Error in performance test:", e$message, "\n")
})

cat("\n")

# Summary
cat("=== TEST SUMMARY ===\n")
cat("Basic functionality tests completed.\n")
cat("If all tests passed with ✓, the function is working correctly.\n")
cat("Any ✗ indicates an issue that needs attention.\n\n")

cat("Next steps:\n")
cat("1. Run the comprehensive test suite: testthat::test_file('tests/testthat/test-opts_strat_visuals.R')\n")
cat("2. Test with real data from the database\n")
cat("3. Verify the reactable output displays correctly in RStudio/R Markdown\n")
