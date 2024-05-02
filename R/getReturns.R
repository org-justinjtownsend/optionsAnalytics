# Daily Log Returns Close-to-Close (quantmod::periodReturn)
idx.daily.rets <- function(idx) {
  quantmod::periodReturn(idx$Close, period = 'daily', type = 'log', leading = TRUE)
}

