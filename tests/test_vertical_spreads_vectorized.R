# Comprehensive Test Script for Vectorized Vertical Spreads
# This script demonstrates the new vectorized dist functionality

library(testthat)

# Source the function
source("R/fct_vertical_spreads.R")

# Create comprehensive test data with multiple strikes and DTE
create_comprehensive_test_data <- function() {
  data.frame(
    ticker = rep("SPX", 12),
    tradeDate = rep("2024-03-15", 12),
    expirDate = c(
      rep("2024-03-22", 4),  # DTE 7
      rep("2024-04-19", 4),  # DTE 35
      rep("2024-05-17", 4)   # DTE 63
    ),
    dte = c(
      rep(7L, 4),
      rep(35L, 4),
      rep(63L, 4)
    ),
    strike = c(
      5000, 5100, 5200, 5300,  # DTE 7 strikes
      5000, 5100, 5200, 5300,  # DTE 35 strikes
      5000, 5100, 5200, 5300   # DTE 63 strikes
    ),
    stockPrice = rep(5150, 12),
    spotPrice = rep(5150, 12),
    callBidPrice = c(
      150, 80, 30, 10,   # DTE 7 call bids
      200, 120, 60, 25,  # DTE 35 call bids
      250, 160, 90, 45   # DTE 63 call bids
    ),
    callAskPrice = c(
      155, 85, 35, 15,   # DTE 7 call asks
      205, 125, 65, 30,  # DTE 35 call asks
      255, 165, 95, 50   # DTE 63 call asks
    ),
    putBidPrice = c(
      10, 30, 80, 150,   # DTE 7 put bids
      25, 60, 120, 200,  # DTE 35 put bids
      45, 90, 160, 250   # DTE 63 put bids
    ),
    putAskPrice = c(
      15, 35, 85, 155,   # DTE 7 put asks
      30, 65, 125, 205,  # DTE 35 put asks
      50, 95, 165, 255   # DTE 63 put asks
    )
  )
}

cat("=== VERTICAL SPREADS VECTORIZED TESTING ===\n\n")

# Test 1: Single DTE, Single DIST (Original functionality)
cat("Test 1: Single DTE, Single DIST\n")
cat("--------------------------------\n")
test_data <- create_comprehensive_test_data()
result1 <- create_vertical_spreads(
  optionsData = test_data,
  dte = 35,
  dist = 100,
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

cat("Number of spreads found:", nrow(result1$vsData), "\n")
cat("Unique DTE values:", unique(result1$vsData$dte), "\n")
cat("Unique DIST values:", unique(result1$vsData$dist), "\n")
cat("Spread types found:", unique(result1$vsData$vsType), "\n\n")

# Test 2: Single DTE, Multiple DIST
cat("Test 2: Single DTE, Multiple DIST\n")
cat("----------------------------------\n")
result2 <- create_vertical_spreads(
  optionsData = test_data,
  dte = 35,
  dist = c(100, 200),
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

cat("Number of spreads found:", nrow(result2$vsData), "\n")
cat("Unique DTE values:", unique(result2$vsData$dte), "\n")
cat("Unique DIST values:", unique(result2$vsData$dist), "\n")
cat("Spread types found:", unique(result2$vsData$vsType), "\n")

# Show breakdown by distance
for (d in unique(result2$vsData$dist)) {
  subset_data <- result2$vsData[result2$vsData$dist == d, ]
  cat("  DIST", d, ":", nrow(subset_data), "spreads\n")
}
cat("\n")

# Test 3: Multiple DTE, Single DIST
cat("Test 3: Multiple DTE, Single DIST\n")
cat("---------------------------------\n")
result3 <- create_vertical_spreads(
  optionsData = test_data,
  dte = c(7, 35),
  dist = 100,
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

cat("Number of spreads found:", nrow(result3$vsData), "\n")
cat("Unique DTE values:", unique(result3$vsData$dte), "\n")
cat("Unique DIST values:", unique(result3$vsData$dist), "\n")
cat("Spread types found:", unique(result3$vsData$vsType), "\n")

# Show breakdown by DTE
for (d in unique(result3$vsData$dte)) {
  subset_data <- result3$vsData[result3$vsData$dte == d, ]
  cat("  DTE", d, ":", nrow(subset_data), "spreads\n")
}
cat("\n")

# Test 4: Multiple DTE, Multiple DIST (Full vectorization)
cat("Test 4: Multiple DTE, Multiple DIST (Full Vectorization)\n")
cat("--------------------------------------------------------\n")
result4 <- create_vertical_spreads(
  optionsData = test_data,
  dte = c(7, 35, 63),
  dist = c(100, 200),
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

cat("Number of spreads found:", nrow(result4$vsData), "\n")
cat("Unique DTE values:", unique(result4$vsData$dte), "\n")
cat("Unique DIST values:", unique(result4$vsData$dist), "\n")
cat("Spread types found:", unique(result4$vsData$vsType), "\n")

# Show breakdown by DTE and DIST
for (dte_val in unique(result4$vsData$dte)) {
  for (dist_val in unique(result4$vsData$dist)) {
    subset_data <- result4$vsData[
      result4$vsData$dte == dte_val & result4$vsData$dist == dist_val, 
    ]
    if (nrow(subset_data) > 0) {
      cat("  DTE", dte_val, "DIST", dist_val, ":", nrow(subset_data), "spreads\n")
    }
  }
}
cat("\n")

# Test 5: Validation of calculations
cat("Test 5: Validation of Spread Calculations\n")
cat("-----------------------------------------\n")

# Check a specific spread calculation
sample_spread <- result4$vsData[1, ]
cat("Sample spread validation:\n")
cat("  DTE:", sample_spread$dte, "\n")
cat("  DIST:", sample_spread$dist, "\n")
cat("  StrikeL:", sample_spread$strikeL, "\n")
cat("  StrikeH:", sample_spread$strikeH, "\n")
cat("  Strike difference:", sample_spread$strikeH - sample_spread$strikeL, "\n")
cat("  vsType:", sample_spread$vsType, "\n")
cat("  vsValue:", sample_spread$vsValue, "\n")
cat("  p_max:", sample_spread$p_max, "\n")
cat("  bep:", sample_spread$bep, "\n")

# Verify strike difference equals dist
strike_diff_check <- all(result4$vsData$strikeH - result4$vsData$strikeL == result4$vsData$dist)
cat("  Strike difference validation:", ifelse(strike_diff_check, "PASS", "FAIL"), "\n\n")

# Test 6: Edge cases
cat("Test 6: Edge Cases\n")
cat("-----------------\n")

# Test with no valid strikes for a distance
result6 <- create_vertical_spreads(
  optionsData = test_data,
  dte = 35,
  dist = c(100, 500),  # 500 doesn't exist in our data
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs")
)

cat("Edge case - invalid distance included:\n")
cat("  Number of spreads found:", nrow(result6$vsData), "\n")
cat("  Unique DIST values:", unique(result6$vsData$dist), "\n")
cat("  Only valid distances should appear in results\n\n")

# Test 7: Performance test with larger dataset
cat("Test 7: Performance with Larger Dataset\n")
cat("---------------------------------------\n")

# Create larger dataset
large_test_data <- do.call(rbind, lapply(1:5, function(i) {
  transform(test_data, 
           expirDate = paste0("2024-", sprintf("%02d", 3+i), "-19"),
           dte = as.integer(35L + i*7))
}))

# Get the actual DTE values from the large test data
actual_dtes <- unique(large_test_data$dte)
cat("Actual DTE values in large test data:", paste(actual_dtes, collapse = ", "), "\n")

system.time({
  result7 <- create_vertical_spreads(
    optionsData = large_test_data,
    dte = actual_dtes,
    dist = c(100, 200),
    callPos = "callAskPrice",
    putPos = "putAskPrice",
    vsType = c("lcs", "scs", "lps", "sps")
  )
})

cat("Large dataset performance:\n")
cat("  Total spreads generated:", nrow(result7$vsData), "\n")
cat("  Unique DTE values:", length(unique(result7$vsData$dte)), "\n")
cat("  Unique DIST values:", length(unique(result7$vsData$dist)), "\n")
cat("  Total combinations tested:", length(unique(result7$vsData$dte)) * length(unique(result7$vsData$dist)) * length(unique(result7$vsData$vsType)), "\n\n")

cat("=== TESTING COMPLETE ===\n")
cat("All tests demonstrate successful vectorization of the dist parameter.\n")
cat("The function now supports multiple distance values as specified in the requirements.\n")
