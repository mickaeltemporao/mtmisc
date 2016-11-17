#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getRatio.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:28:05
# Modified:     2016-11-17 10:29:00
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getRatio <- function(data, varname, ...){
  ratio <- function(values, ...){
    (values)/(max(values, na.rm=T))
  }
  data2 <- tbl_df(data %>% dplyr::select(starts_with(varname)))
  for (i in 1:nrow(data2)) {
    data2[i,] <- bind_rows(ratio(data2[i,], na.rm=T))
  }
  colnames(data2) <- c(paste0(varname, 'RatioParty', 1:dim(data2)[2]))
  data <- dplyr::bind_cols(data, data2)
  return(data)
}
