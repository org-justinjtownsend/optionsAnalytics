#!/bin/bash
export R_PROFILE='/home/justinjtownsend/.Rprofile'
export R_ENVIRON='/home/justinjtownsend/optionsAnalytics/.Renviron'

/usr/lib/R/bin/Rscript '/home/justinjtownsend/optionsAnalytics/orats_spx_opts_raw.R'  >> '/home/justinjtownsend/optionsAnalytics/orats_spx_opts_raw.log' 2>&1