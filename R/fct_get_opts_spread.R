devtools::install_github("zumthor86/OptionsAnalytics")
devtools::install_github("cran/fOptions")

library(OptionsAnalytics)

initiate_ig_session(env = "LIVE")

# A short term work around is to use the tickers inside the 3SD move for next
# 90 days, limiting the # of API calls.
# 505694207 = name: 5501-6000

spx_mkts <- create_market_env(505694207, pause = 1)

# tbl <-
#   list.files(path=path, pattern=pattern, recursive=TRUE) %>% 
#   purrr::map_df(~data.frame(x=.x),.id="id")

library(magrittr)
library(dplyr)

# get in all the options data



# 1. Filter to the instruments of interest, the ones where I have a bit of time and are matched in IG.
tst <- orats_spx_opts_raw %>%
  dplyr::filter(dte >= 46)

# What is happening to those options prices?

# What is happening to the interest / volume falling / rising?

# How should we display the altering interest