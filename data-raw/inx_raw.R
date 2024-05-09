## code to prepare `inx_raw` dataset goes here

inx_raw <- utils::read.csv("data-raw/inx_raw.csv",
                                      header = TRUE,
                                      sep = ",",
                                      dec = ".",
                                      stringsAsFactors = FALSE)

# Remove NAs
inx_raw <- stats::na.omit(inx_raw)

# Convert date / time column to (unambiguous) date format
inx_raw$Time <- base::as.Date(inx_raw$Time, format = "%m/%d/%Y")

# Convert data-frame to a time-series object
# JT, 2024-5-03: what about quantmod::OHLCV?
inx_raw <- xts::as.xts(inx_raw)

usethis::use_data(inx_raw, overwrite = TRUE)
