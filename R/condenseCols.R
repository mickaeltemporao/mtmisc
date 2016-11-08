#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        condenseCols
# Filename:     condenseCols.R
# Description:  Function to combine low frequency cols
# Version:      0.0.0.001
# Created:      2016-11-08 12:19:58
# Modified:     2016-11-08 14:12:17
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
condenseCols <- function(x, threshold=0.02) {
  low_freq <- apply(x, 2, sum)<threshold*nrow(x)
  var_prefix <- substr(names(x)[1], 1, 4)
  var_name <- paste0(var_prefix, 'low_freq')
  new_var <- apply(x[,low_freq], 1, sum)
  new_var <- ifelse(new_var==0, 0, 1)
  x <- x[,!low_freq]
  x[,var_name] <- new_var
  return(x)
}
