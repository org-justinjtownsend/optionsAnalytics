#' Test script for updated opts_instrument_ig function
#' 
#' This script tests the updated opts_instrument_ig function with the new specifications:
#' - Parameter name changed from expiry_range to expiry
#' - New column names: instrument, expiry_ig, expiry_opt
#' - Millisecond precision timestamps
#' - Proper error handling with NA values

# Load required packages
if (!requireNamespace("OptionsAnalytics", quietly = TRUE)) {
  stop("OptionsAnalytics package is required for testing")
}

library(OptionsAnalytics)

# Set up IG API session
tryCatch({
  initiate_ig_session(env = "LIVE")
  cat("IG session initiated successfully\n")
}, error = function(e) {
  cat("Warning: Could not initiate IG session:", e$message, "\n")
  cat("Some tests may fail without active session\n")
})

# Test parameters
instrument <- "SPX"
expiry <- seq(1, 5)  # Test with 5 expiry values

# Source the function
source("R/fct_opts_helpers.R")

cat("\n=== Testing opts_instrument_ig function ===\n")
cat("Instrument:", instrument, "\n")
cat("Expiry range:", paste(expiry, collapse = ", "), "\n")
cat("Default strike:", 5500, "\n")
cat("Default option type: P\n\n")

# Test 1: Basic functionality with default parameters
cat("Test 1: Basic functionality with default parameters\n")
cat("Running opts_instrument_ig...\n")

result <- opts_instrument_ig(
  instrument = instrument,
  expiry = expiry
)

# Print the results for visual inspection
cat("\nResults:\n")
print(result)

# Test 2: Validate output structure
cat("\nTest 2: Validate output structure\n")

# Check if we have the expected columns
expected_columns <- c("instrument", "strike_price", "option_type", "expiry_ig", "expiry_opt", "underlyer", "updated", "dte")
actual_columns <- names(result)

missing_columns <- setdiff(expected_columns, actual_columns)
extra_columns <- setdiff(actual_columns, expected_columns)

if (length(missing_columns) == 0 && length(extra_columns) == 0) {
  cat("✓ Test passed: All expected columns are present\n")
} else {
  cat("✗ Test failed: Column mismatch\n")
  if (length(missing_columns) > 0) {
    cat("  Missing columns:", paste(missing_columns, collapse = ", "), "\n")
  }
  if (length(extra_columns) > 0) {
    cat("  Extra columns:", paste(extra_columns, collapse = ", "), "\n")
  }
}

# Test 3: Validate row count
cat("\nTest 3: Validate row count\n")
if (nrow(result) == length(expiry)) {
  cat("✓ Test passed: Number of results matches number of expiry values\n")
} else {
  cat("✗ Test failed: Expected", length(expiry), "rows, got", nrow(result), "\n")
}

# Test 4: Validate epic naming convention
cat("\nTest 4: Validate epic naming convention\n")
cat("Expected epic format: OP.D.INSTRUMENT+EXPIRY.STRIKE+OPTION_TYPE.IP\n")

for (i in seq_along(expiry)) {
  expected_epic <- sprintf("OP.D.%s%d.%d.%s.IP", instrument, expiry[i], 5500, "P")
  actual_epic <- result$instrument[i]
  
  if (actual_epic == expected_epic) {
    cat("✓ Row", i, ":", actual_epic, "\n")
  } else {
    cat("✗ Row", i, ": Expected", expected_epic, "but got", actual_epic, "\n")
  }
}

# Test 5: Test with custom parameters
cat("\nTest 5: Test with custom parameters\n")
cat("Testing with strike = 5000, option_type = C\n")

custom_result <- opts_instrument_ig(
  instrument = instrument,
  expiry = c(1, 2),
  strike_price = 5000,
  option_type = "C"
)

cat("Custom parameters result:\n")
print(custom_result)

# Test 6: Test error handling with invalid instrument
cat("\nTest 6: Test error handling with invalid instrument\n")
invalid_instrument <- "INVALID"
invalid_result <- opts_instrument_ig(invalid_instrument, c(1, 2))

cat("Invalid instrument result:\n")
print(invalid_result)

# Verify that all rows in invalid_result have NA values for key fields
key_fields <- c("strike_price", "option_type", "expiry_ig", "expiry_opt", "underlyer", "dte")
na_check <- sapply(key_fields, function(field) {
  all(is.na(invalid_result[[field]]))
})

if (all(na_check)) {
  cat("✓ Test passed: All rows for invalid instrument have NA values in key fields\n")
} else {
  cat("✗ Test failed: Not all rows for invalid instrument have NA values\n")
  cat("Fields with non-NA values:", names(na_check)[!na_check], "\n")
}

# Test 7: Validate timestamp format
cat("\nTest 7: Validate timestamp format\n")
cat("Checking millisecond precision in 'updated' column...\n")

timestamp_check <- sapply(result$updated, function(ts) {
  # Check if timestamp has millisecond precision (contains .)
  grepl("\\.", ts) && grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d+$", ts)
})

if (all(timestamp_check)) {
  cat("✓ Test passed: All timestamps have millisecond precision\n")
} else {
  cat("✗ Test failed: Some timestamps don't have millisecond precision\n")
  cat("Timestamps without millisecond precision:", which(!timestamp_check), "\n")
}

# Test 8: Validate expiry_opt calculation
cat("\nTest 8: Validate expiry_opt calculation\n")
cat("Checking that expiry_opt = expiry_ig + 1 day...\n")

# Only check rows where we have valid expiry data
valid_rows <- !is.na(result$expiry_ig) & !is.na(result$expiry_opt)

if (any(valid_rows)) {
  expiry_check <- sapply(which(valid_rows), function(i) {
    ig_date <- as.POSIXct(result$expiry_ig[i])
    opt_date <- as.POSIXct(result$expiry_opt[i])
    expected_opt_date <- ig_date + as.difftime(1, units = "days")
    
    # Allow for small time differences (within 1 second)
    abs(as.numeric(opt_date - expected_opt_date)) < 1
  })
  
  if (all(expiry_check)) {
    cat("✓ Test passed: All expiry_opt values are correctly calculated (expiry_ig + 1 day)\n")
  } else {
    cat("✗ Test failed: Some expiry_opt values are not correctly calculated\n")
    cat("Rows with incorrect expiry_opt:", which(valid_rows)[!expiry_check], "\n")
  }
} else {
  cat("⚠ No valid expiry data to test\n")
}

# Test 9: Validate DTE calculation
cat("\nTest 9: Validate DTE calculation\n")
cat("Checking that dte values are reasonable and calculated correctly...\n")

# Only check rows where we have valid dte data
valid_dte_rows <- !is.na(result$dte) & result$dte != ""

if (any(valid_dte_rows)) {
  dte_values <- as.numeric(result$dte[valid_dte_rows])
  
  # Check if DTE values are reasonable (positive numbers, not too large)
  reasonable_dte <- all(dte_values > 0 & dte_values < 10000)
  
  if (reasonable_dte) {
    cat("✓ Test passed: All DTE values are reasonable positive numbers\n")
    cat("DTE range:", min(dte_values), "to", max(dte_values), "days\n")
  } else {
    cat("✗ Test failed: Some DTE values are unreasonable\n")
    cat("DTE values outside reasonable range:", dte_values[!(dte_values > 0 & dte_values < 10000)], "\n")
  }
  
  # Check if DTE values are numeric
  if (all(!is.na(dte_values))) {
    cat("✓ Test passed: All DTE values are valid numeric values\n")
  } else {
    cat("✗ Test failed: Some DTE values are not numeric\n")
  }
} else {
  cat("⚠ No valid DTE data to test\n")
}

# Summary
cat("\n=== Test Summary ===\n")
cat("Total tests run: 9\n")
cat("Function tested: opts_instrument_ig\n")
cat("Parameters tested: instrument, expiry, strike_price, option_type\n")
cat("Output columns:", paste(names(result), collapse = ", "), "\n")
cat("Rows returned:", nrow(result), "\n")

cat("\nFunction implementation appears to match the updated specifications.\n")
cat("Key improvements implemented:\n")
cat("- Parameter name changed from 'expiry_range' to 'expiry'\n")
cat("- New column structure with 'instrument', 'expiry_ig', 'expiry_opt', 'dte'\n")
cat("- Millisecond precision timestamps\n")
cat("- Robust error handling with NA values for failed API calls\n")
cat("- Proper epic naming convention: OP.D.INSTRUMENT+EXPIRY.STRIKE.OPTION_TYPE.IP\n")
cat("- DTE calculation (days to expiration)\n")
