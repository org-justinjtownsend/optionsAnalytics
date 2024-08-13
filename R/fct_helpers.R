#' helpers cool.
#'
#' @description A set of helpers for loading the options data to notification.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

SPX.CRON <- cronR::cron_rscript(paste0(Sys.getenv("HOME"), "/optionsAnalytics/data-raw/", "fct_getIndexData.R"))

cronR::cron_add(command = SPX.CRON,
                frequency = 'daily',
                id = 'SPX2',
                at = '01:30',
                days_of_week = c(2, 3, 4, 5, 6))