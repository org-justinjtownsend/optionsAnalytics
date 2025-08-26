devtools::install_github("zumthor86/OptionsAnalytics")
devtools::install_github("cran/fOptions")

library(OptionsAnalytics)
library(fOptions)

source("R/fct_opts_helpers.R")

initiate_ig_session(env = "LIVE")
epic <- "OP.D.SPX1.6400C.IP"
epic_dtls <- as.data.frame(get_option_details(epic))

instrument <- "SPX"
expiry <- seq(1, 7)

seq_res <- opts_instrument_ig(
  instrument = instrument,
  expiry = expiry
)

# A short term work around is to use the tickers inside the 3SD move for next
# 90 days, limiting the # of API calls.
# 346003 = top level US 500 options?
# 127897831 = JUN-25 does this change over-time?
# 
# 505694207 = 5501-6000?
# 494888912 = EOM markets?
# 431067622 = MAR-25

# Naming convention: "OP.D.SPXEOM.5305C.IP"
spx_eom_mkts <- create_market_env(494888912, pause = 1)

head(names(spx_eom_mkts))

# Naming convention: "OP.D.SPX3.5525C.IP"
spx_rng_mkts <- create_market_env(431067622, pause = 1)
spx_rng_mkts1 <- create_market_env(127897831, pause = 1)

spx_toplvl_mkts <- create_market_env(346003, pause = 1)

# tbl <-
#   list.files(path=path, pattern=pattern, recursive=TRUE) %>% 
#   purrr::map_df(~data.frame(x=.x),.id="id")

library(magrittr)
library(dplyr)

# get in all the options data

# 1. Filter to the instruments of interest, the ones where I have a bit of time and are matched in IG.
tst <- orats_spx_opts_raw %>%
  dplyr::filter(dte >= 46)

# I think you mark ITM, ATM, OTM and type this simplifies downstream filtering.
# 2. Each options position is an application of a sophisticated filter to the chain
# e.g. Long Call Spread
# Expiry sub-setting outside the function, but the df can include dte or expiry date as info
# 
# Leg 1 = Buy ITM Call (how much ITM = ?) Buy, Ask (Calc Mid)
leg1 <- orats_spx_opts_raw$strike
# Leg 2 = Sell OTM Call (how much OTM) Buy, Ask (Calc Mid)
# Spread between positions is an assumption of the distance the price will travel between position open
# and expiration (so spread is somewhat driven by the estimate for IV in the period)
# To build a vertical spread, construct a function to pick these legs based on:
# Vertical Spread Type = positions to choose, and the spread (likely move)
# Likely, or specified spread, along with the (min) +-3SD underlying price / strike

# At this point I could also include a function to set the naming convention for IG?

# For condors do I just add x2 vertical spreads or build from 1st principles?

# What is happening to those options prices?

# What is happening to the interest / volume falling / rising?

# How should we display the altering interest

epics <- c()
tst <- lapply(epics, get_epic_details)

