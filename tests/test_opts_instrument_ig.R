#' Test script for opts_instrument_ig function
#' 
#' This script tests the opts_instrument_ig function with SPX options
#' for expiries 1-5.

# Load required packages
library(OptionsAnalytics)

# Set up IG API session
initiate_ig_session(env = "LIVE")

# Test parameters
instrument <- "SPX"
expiry <- seq(1, 7)

# Source the function
source("R/fct_opts_helpers.R")

# Run the function
result <- opts_instrument_ig(
  instrument = instrument,
  expiry = expiry
)

# Print the results for visual inspection
print(result)

# Print results
print("IG Options Instrument Test Results:")
print(result)

# Basic validation
if (nrow(result) == length(expiry)) {
  cat("\nTest passed: Number of results matches number of expiry values\n")
} else {
  cat("\nTest failed: Number of results does not match number of expiry values\n")
}

# Check for any NA values
na_count <- sum(is.na(result))
if (na_count == 0) {
  cat("Test passed: No NA values in results\n")
} else {
  cat("Test failed: Found", na_count, "NA values in results\n")
}

# Print epic names for debugging
cat("\nEpic names generated for each expiry:\n")
for (exp in expiry) {
  epic <- sprintf("OP.D.%s%d.%d.%s.IP", instrument, exp, 5500, "P")
  cat(epic, "\n")
}

# Test with invalid epic names to ensure NA handling
invalid_instrument <- "INVALID"
invalid_result <- opts_instrument_ig(invalid_instrument, expiry)

# Verify that all rows in invalid_result have NA values
all_na <- all(is.na(invalid_result$strike_price) & is.na(invalid_result$option_type) & is.na(invalid_result$expiry_ig) & is.na(invalid_result$underlyer) & is.na(invalid_result$dte))
if (all_na) {
  cat("Test passed: All rows for invalid epic names have NA values\n")
} else {
  cat("Test failed: Not all rows for invalid epic names have NA values\n")
}

# Ensure the final result includes all rows for the sequence 1-7
final_result <- opts_instrument_ig(instrument, expiry)

# Print the final result for visual inspection
print("Final Result:")
print(final_result)

# Print the invalid result for visual inspection
print("Invalid Result:")
print(invalid_result) 