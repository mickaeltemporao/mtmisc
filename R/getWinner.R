#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getWinner.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:27:05
# Modified:     2016-11-17 10:28:55
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getWinner <- function(data, varname){
  df <- dplyr::select(data, starts_with(varname))
  df <- pbapply(df,1, getRanks)
  df <- as.data.frame(df)
  colnames(df) <- c(paste0('winner.',varname))
  df <- dplyr::bind_cols(data, df)
  return(df)
}
