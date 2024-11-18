devtools::install_github("zumthor86/OptionsAnalytics")
devtools::install_github("cran/fOptions")

library(OptionsAnalytics)

initiate_ig_session(env = "LIVE")

# A short term work around is to use the tickers inside the 3SD move, limiting the # of API calls
# 505694207 = name: 5501-6000

spx_mkts <- create_market_env(505694207, pause = 1)

# tbl <-
#   list.files(path=path, pattern=pattern, recursive=TRUE) %>% 
#   purrr::map_df(~data.frame(x=.x),.id="id")