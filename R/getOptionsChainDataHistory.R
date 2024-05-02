library(RCurl)
res <- getURL("https://api.orats.io/datav2/hist/strikes?token=e4cd15f5-4dd0-41d0-bb9e-bf27d79df0ff&ticker=SPX&tradeDate=2022-08-08&dte=30,45", .opts=list(followlocation = TRUE))

url <- "https://api.orats.io/datav2/hist/strikes?token=e4cd15f5-4dd0-41d0-bb9e-bf27d79df0ff&ticker=SPX&tradeDate=2022-08-08&dte=30,45"

resJSON <- jsonlite::fromJSON(url)[["data"]]