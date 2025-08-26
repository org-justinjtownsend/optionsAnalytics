# Simple test suite for opts_strat_tbl function
# F1.10: Create a test suite to display the output from this function

library(testthat)

# Source the function
source("../../R/fct_opts_strat_visuals.R")

# Test data setup
setup_test_data <- function() {
  # Create test data that MUST re-create the output expected from create_vertical_spreads().vsData
  data.frame(
    ticker = c("SPX", "SPX", "SPX", "SPX"),
    expirDate = c("2024-02-16", "2024-02-16", "2024-03-15", "2024-03-15"),
    dte = c(30, 30, 60, 60),
    dist = c(100, 200, 100, 200),
    vsType = c("lcs", "scs", "lps", "sps"),
    instrType = c("c", "c", "p", "p"),
    posType = c("dr_s", "cr_s", "dr_s", "cr_s"),
    vsValue = c(2.50, -1.75, 3.25, -2.00),
    strikeL = c(4000, 4100, 4000, 4100),
    strikeH = c(4100, 4300, 4100, 4300),
    instrValL = c(15.50, 8.25, 12.75, 6.50),
    instrValH = c(13.00, 10.00, 9.50, 8.50),
    leg1_instrument = c("SPX4000c2024-02-16", "SPX4100c2024-02-16", "SPX4100p2024-03-15", "SPX4300p2024-03-15"),
    leg2_instrument = c("SPX4100c2024-02-16", "SPX4300c2024-02-16", "SPX4000p2024-03-15", "SPX4100p2024-03-15"),
    spread_id = c("SPX4000c2024-02-16_SPX4100c2024-02-16", "SPX4100c2024-02-16_SPX4300c2024-02-16", "SPX4100p2024-03-15_SPX4000p2024-03-15", "SPX4300p2024-03-15_SPX4100p2024-03-15"),
    p_max = c(97.50, 1.75, 96.75, 2.00),
    bep = c(4002.50, 4098.25, 4103.25, 4298.00),
    moneyness = c("atm", "otm", "atm", "otm"),
    prob_profit = c(0.65, 0.35, 0.70, 0.30),
    stockPrice = c(4050, 4050, 4050, 4050),
    updated = as.POSIXct(c("2024-01-17 10:00:00", "2024-01-17 10:00:00", "2024-01-17 10:00:00", "2024-01-17 10:00:00")),
    strat_type = c("vs", "vs", "vs", "vs"),
    stringsAsFactors = FALSE
  )
}

# Simple test to display the output
test_that("Display output from opts_strat_tbl function", {
  test_data <- setup_test_data()
  
  # Create the table
  result <- opts_strat_tbl(data = test_data)
  
  # Display the result
  print("=== OPTIONS STRATEGY TABLE OUTPUT ===")
  print(result)
  
  # Simple verification that it's a reactable
  expect_true(inherits(result, "reactable"))
  
  print("=== TEST COMPLETED SUCCESSFULLY ===")
})
