#' helpers cool.
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

SPX.CRON <- cronR::cron_rscript(paste0(Sys.getenv("HOME"), "/optionsAnalytics/R/", "getIndexData.R"))

cronR::cron_add(command = SPX.CRON,
                frequency = 'daily',
                id = 'SPX1',
                at = '01:30',
                days_of_week = c(2, 3, 4, 5, 6))