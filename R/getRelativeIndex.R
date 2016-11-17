#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getRelativeIndex.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:28:21
# Modified:     2016-11-17 10:29:01
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getRelativeIndex <- function (data, varname, ...) {
  FUN <- function (data) {
    df <- data
    df <- scales::rescale(df, c(-1,1))
    winner <- getRanks(df)
    df <- df - df[winner]
    df[winner] <- df[winner] - sort(df, decreasing = TRUE)[2]
    return(df)
  }
df <- dplyr::select(data, starts_with(varname))
df <- t(pbapply(df,1, FUN))
df <- as.data.frame(df)
colnames(df) <- c(paste0(varname, 'RelativeParty', 1:dim(df)[2]))
data <- dplyr::bind_cols(data, df)
return(data)
}
