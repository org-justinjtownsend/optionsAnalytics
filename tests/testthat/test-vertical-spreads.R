# Test data creation
test_data <- data.frame(
  ticker = rep("SPX", 6),
  tradeDate = rep("2024-03-20", 6),
  expirDate = rep("2024-04-19", 6),
  dte = rep(30L, 6),
  strike = c(5000, 5100, 5200, 5300, 5400, 5500),
  stockPrice = rep(5250, 6),
  spotPrice = rep(5250, 6),
  callBidPrice = c(250, 180, 120, 70, 35, 15),
  callAskPrice = c(255, 185, 125, 75, 40, 20),
  putBidPrice = c(15, 35, 70, 120, 180, 250),
  putAskPrice = c(20, 40, 75, 125, 185, 255)
)

# Test the function
source("../../R/fct_vertical_spreads.R")

# Test case 1: Basic functionality with single dte and dist
result1 <- create_vertical_spreads(
  optionsData = test_data,
  dte = 30,
  dist = 100,
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

# Print results
print("Test Case 1 Results:")
print(result1$vsData)

# Verify call spreads only use call prices and put spreads only use put prices
print("\nVerifying price usage:")
call_spreads <- result1$vsData[result1$vsData$vsType %in% c("lcs", "scs"), ]
put_spreads <- result1$vsData[result1$vsData$vsType %in% c("lps", "sps"), ]

print("First call spread values:")
print(data.frame(
  strikeL = call_spreads$strikeL[1],
  strikeH = call_spreads$strikeH[1],
  callPriceL = test_data$callAskPrice[test_data$strike == call_spreads$strikeL[1]],
  callPriceH = test_data$callAskPrice[test_data$strike == call_spreads$strikeH[1]],
  spreadValL = call_spreads$instrValL[1],
  spreadValH = call_spreads$instrValH[1]
))

print("\nFirst put spread values:")
print(data.frame(
  strikeL = put_spreads$strikeL[1],
  strikeH = put_spreads$strikeH[1],
  putPriceL = test_data$putAskPrice[test_data$strike == put_spreads$strikeL[1]],
  putPriceH = test_data$putAskPrice[test_data$strike == put_spreads$strikeH[1]],
  spreadValL = put_spreads$instrValL[1],
  spreadValH = put_spreads$instrValH[1]
))

# Test case 2: Multiple dte values
test_data_multi_dte <- rbind(
  test_data,
  transform(test_data, 
           expirDate = "2024-05-17",
           dte = 58L)
)

result2 <- create_vertical_spreads(
  optionsData = test_data_multi_dte,
  dte = c(30, 58),
  dist = 100,
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

# Print results
print("\nTest Case 2 Results (Multiple DTE):")
print(result2$vsData)

# Test case 2b: Multiple dist values
result2b <- create_vertical_spreads(
  optionsData = test_data,
  dte = 30,
  dist = c(100, 200),
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs", "lps", "sps")
)

# Print results
print("\nTest Case 2b Results (Multiple DIST):")
print(result2b$vsData)

# Test case 2c: Multiple dte and dist values
result2c <- create_vertical_spreads(
  optionsData = test_data_multi_dte,
  dte = c(30, 58),
  dist = c(100, 200),
  callPos = "callAskPrice",
  putPos = "putAskPrice",
  vsType = c("lcs", "scs")
)

# Print results
print("\nTest Case 2c Results (Multiple DTE and DIST):")
print(result2c$vsData)

# Test case 3: Invalid data validation
test_data_invalid <- test_data
test_data_invalid$callBidPrice[1] <- 300  # Make bid > ask

tryCatch({
  result3 <- create_vertical_spreads(
    optionsData = test_data_invalid,
    dte = 30,
    dist = 100,
    callPos = "callAskPrice",
    putPos = "putAskPrice",
    vsType = c("lcs", "scs", "lps", "sps")
  )
}, error = function(e) {
  print("\nTest Case 3 Results (Invalid Data):")
  print(paste("Error caught as expected:", e$message))
}) 