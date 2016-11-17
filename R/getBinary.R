#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getBinary.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:27:16
# Modified:     2016-11-17 10:28:56
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getBinary <- function (data, varname) {
  FUN <- function (data) {
    data <- rank(data, na.last='keep', ties.method = 'random' )
    data <- ifelse(data %in% max(data,na.rm=TRUE), 1, ifelse(is.na(data), NA, 0))
    return(data)
  }
  df <- dplyr::select(data, starts_with(varname))
  df <- t(pbapply(df,1, FUN))
  df <- as.data.frame(df)
  colnames(df) <- c(paste0(varname, 'BinaryParty', 1:dim(df)[2]))
  #data <- dplyr::bind_cols(data, df) # remove auto append to initial DF
  return(df)
}
