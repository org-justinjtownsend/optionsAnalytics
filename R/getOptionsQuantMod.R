require("quantmod")

SPX.OPTS <- getOptionChain("^SPX", NULL)
data <- do.call(rbind, lapply(SPX.OPTS, function(x) do.call(rbind, x)))
