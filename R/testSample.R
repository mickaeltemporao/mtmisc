#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     testSample.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:26:45
# Modified:     2016-11-17 10:28:51
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
testSample <- function (data,by) {
# Creates small samples to test functions
#
# Args:
#   data: A data frame from which the sample data will be created
#   by: A character containing the variable name to subset from
#
# Returns:
#   A test sample data frame
  data <- as.data.frame(data)
  Temp <- data[0,]
  for (i in unique(data[,by])){
    print(paste0('Sampling ', i,))
    Temp <- dplyr::union(Temp, head(data[data[,by] == i,]))
  }
  return(Temp)
}
