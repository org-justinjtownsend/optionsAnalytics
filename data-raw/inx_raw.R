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

# Convert data-frame to an xts object
row.names(inx_raw) <- inx_raw$Time
inx_raw <- base::subset(inx_raw, select = -c(Time))
inx_raw <- xts::as.xts(inx_raw)

usethis::use_data(inx_raw, overwrite = TRUE)