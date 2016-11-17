#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getRci.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:27:36
# Modified:     2016-11-17 10:28:58
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getRci <- function (data, varname) {
  FUN <- function (data) {
    win <- getRanks(data)
    data <- data - data[win]
    data[win] <- data[win] - sort(data, decreasing = TRUE)[2]
    return(data)
  }
  df <- dplyr::select(data, starts_with(varname))
  df <- t(pbapply(df,1, FUN))
  df <- as.data.frame(df)
  colnames(df) <- c(paste0(varname, 'RciParty', 1:dim(df)[2]))
  #data <- dplyr::bind_cols(data, df) # remove auto append to initial DF
  return(df)
}
