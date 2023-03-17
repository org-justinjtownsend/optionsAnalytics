# Scrape options chains from the web

# Packages
library(jsonlite)
library(quantmod)

# Quantmod: Yahoo!
x <- "MA"
chain <- quantmod::getOptionChain(x, Exp=NULL)
calls <- as.data.frame(chain$Oct.20.2023$calls)
puts <- as.data.frame(chain$Oct.20.2023$puts)
