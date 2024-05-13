#' Combine calls and puts dataframes (with multiple expiries) to a single dataframe.
#'
# `opts.side_by_side.orats` returns the common side-by-side format for options chains from orats.
#'
#' This function is specific to orats data, sourced using quantmod::getOptionChain().
#' 
#' Example:
#' spx.opts <- getOptionChain("SPX", Exp = NULL, src = "orats", api.key = "<KEY HERE>")
#' spx.opts.single_df <- opts.side_by_side.orats(spx.opts, orats.extras = 0)

#' Credit / Inspiration: https://github.com/jgQuantScripts/get-Yahoo-Option-Chains
#'
#' @param l A list (of options expiries).
#' @param orats.extras A number (0, 1 - combine the extra dataframes).
#' @return A list (data frame).
#' @examples
#' opts.side_by_side.orats(list_of_expiries, orats.extras = 0)
#' opts.side_by_side.orats(list_of_expiries, orats.extras = 1)

opts.side_by_side.orats <- function(l, orats.extras = 0) {
  
  # Class should be a list
  if (!is.list(l)) {
    stop("Functions expects options expiries as a list. Confirm you are providing an object of list. :-)")
  }
  else {
    
    # Declare empty data frame
    opts_side_by_side = data.frame()
    
    for (opts in 1:length(l)) {
      
      # Expiry string to date
      expiry <- as.character(names(l[opts]))
      
      # Dynamic name for the expiry dataframe (list index based)
      side_by_side <- paste0("side_by_side_", opts, sep = "")
      
      if (orats.extras == 0) {
        
        # Extracting options dataframes
        calls <- as.data.frame(l[[opts]]["call"])
        puts <- as.data.frame(l[[opts]]["put"])
        
        # Side by side: calls / puts join
        by <- dplyr::join_by(call.Ticker == put.Ticker, call.Strike == put.Strike)
        side_by_side <- dplyr::full_join(calls, puts, by)
        side_by_side$exp <- expiry
        
      } else {
        
        # Extracting options dataframes
        calls <- as.data.frame(l[[opts]]["call_extra"])
        puts <- as.data.frame(l[[opts]]["put_extra"])
        
        # Side by side: calls / puts join
        by <- dplyr::join_by(call_extra.ticker == put_extra.ticker, call_extra.strike == put_extra.strike)
        side_by_side <- dplyr::full_join(calls, puts, by)
        side_by_side$exp <- expiry
        
      }
      
      # Append expiry to existing expiries
      opts_side_by_side <- dplyr::bind_rows(opts_side_by_side, side_by_side)
    }
    
    return(opts_side_by_side)
    
  }
}