library(FinancialInstrument)
library(blotter)
library(quantstrat)

stock.str = "AAPL"  # what are we trying it on
currency("USD")
# > [1] 'USD'
stock(stock.str, currency = "USD", multiplier = 1)
# > [1] 'AAPL'

# >
startDate = "1999-12-31"
initEq = 1e+06
portfolio.st = "macross"
account.st = "macross"
initPortf(portfolio.st, symbols = stock.str)
# > [1] 'macross'
initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
# > [1] 'macross'
initOrders(portfolio = portfolio.st)
stratMACROSS <- strategy(portfolio.st)

stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA",
                              arguments = list(x = quote(Cl(mktdata)), n = 50), label = "ma50")
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA",
                              arguments = list(x = quote(Cl(mktdata)[, 1]), n = 200),
                              label = "ma200")

stratMACROSS <- add.signal(strategy = stratMACROSS, name = "sigCrossover",
                           arguments = list(columns = c("ma50", "ma200"), relationship = "gte"),
                           label = "ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS, name = "sigCrossover",
                           arguments = list(column = c("ma50", "ma200"), relationship = "lt"),
                           label = "ma50.lt.ma200")

stratMACROSS <- add.rule(strategy = stratMACROSS, name = "ruleSignal",
                         arguments = list(sigcol = "ma50.gt.ma200", sigval = TRUE,
                                          orderqty = 100, ordertype = "market", orderside = "long"),
                         type = "enter")
stratMACROSS <- add.rule(strategy = stratMACROSS, name = "ruleSignal",
                         arguments = list(sigcol = "ma50.lt.ma200", sigval = TRUE,
                                          orderqty = "all", ordertype = "market", orderside = "long"),
                         type = "exit")

# if you want a long/short Stops and Reverse MA cross
# strategy, you would add two more rules for the short
# side:

# stratMACROSS <- add.rule(strategy =
# stratMACROSS,name='ruleSignal', arguments =
# list(sigcol='ma50.lt.ma200',sigval=TRUE,
# orderqty=-100, ordertype='market',
# orderside='short'),type='enter') stratMACROSS <-
# add.rule(strategy = stratMACROSS,name='ruleSignal',
# arguments = list(sigcol='ma50.gt.ma200',sigval=TRUE,
# orderqty=100, ordertype='market',
# orderside='short'),type='exit')

getSymbols(stock.str, from = startDate)
# > [1] 'AAPL'
for (i in stock.str) assign(i, adjustOHLC(get(i), use.Adjusted = TRUE))

start_t <- Sys.time()
out <- applyStrategy(strategy = stratMACROSS, portfolios = portfolio.st)
# > [1] '2001-06-27 00:00:00 AAPL 100 @ 1.443241' > [1]
# '2001-09-07 00:00:00 AAPL -100 @ 1.068518' > [1]
# '2002-01-07 00:00:00 AAPL 100 @ 1.416034' > [1]
# '2002-07-10 00:00:00 AAPL -100 @ 1.070991' > [1]
# '2003-05-16 00:00:00 AAPL 100 @ 1.162508' > [1]
# '2006-06-22 00:00:00 AAPL -100 @ 7.368322' > [1]
# '2006-09-26 00:00:00 AAPL 100 @ 9.598111' > [1]
# '2008-03-07 00:00:00 AAPL -100 @ 15.118788' > [1]
# '2008-05-19 00:00:00 AAPL 100 @ 22.706005' > [1]
# '2008-09-24 00:00:00 AAPL -100 @ 15.917701' > [1]
# '2009-05-14 00:00:00 AAPL 100 @ 15.205353' > [1]
# '2012-12-11 00:00:00 AAPL -100 @ 67.548859' > [1]
# '2013-09-11 00:00:00 AAPL 100 @ 59.474586' > [1]
# '2015-08-31 00:00:00 AAPL -100 @ 104.390999' > [1]
# '2016-08-31 00:00:00 AAPL 100 @ 100.325439' > [1]
# '2018-12-24 00:00:00 AAPL -100 @ 143.924454' > [1]
# '2019-05-07 00:00:00 AAPL 100 @ 199.698502'
end_t <- Sys.time()
print(end_t - start_t)
# > Time difference of 0.1832633 secs

start_t <- Sys.time()
updatePortf(Portfolio = "macross", Dates = paste("::", as.Date(Sys.time()),
                                                 sep = ""))
# > [1] 'macross'
end_t <- Sys.time()
print("trade blotter portfolio update:")
# > [1] 'trade blotter portfolio update:'
print(end_t - start_t)
# > Time difference of 0.03264308 secs

chart.Posn(Portfolio = "macross", Symbol = stock.str, TA = c("add_SMA(n=50,col='red')",
                                                             "add_SMA(n=200,col='blue')"))

zoom_Chart("2014::2018")