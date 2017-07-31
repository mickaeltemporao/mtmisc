#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     GetRri.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:27:55
# Modified:     2016-11-17 10:28:59
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getRri <- function (data, varname) {
  winner <- NA
  winnerValue <- NA
  secondValue <- NA
  certainty <- NA
  n <- NA
  vars <- data %>% dplyr::select(starts_with(varname))
  for (i in 1:nrow(data)) {
    winner[i] <- ifelse(sum(vars[i,], na.rm=TRUE)==0, NA, getRanks(vars[i,]))
    n[i] <- ifelse(sum(vars[i,], na.rm=TRUE)==0, NA, length(sort(vars[i,])))
    winnerValue[i] <- ifelse(sum(vars[i,], na.rm=TRUE)==0, NA, sort(vars[i,])[[n[i]]])
    secondValue[i] <- ifelse(sum(vars[i,], na.rm=TRUE)==0, NA, sort(vars[i,], partial=n-1)[[n[i]-1]])
    certainty[i] <- ifelse(sum(vars[i,], na.rm=TRUE)==0, NA, winnerValue[i] - secondValue[i])
  }
  df <- data.frame(winner, certainty)
  colnames(df) <- c(paste0(varname,'Winner'), paste0(varname,'Certainty'))
  data <- dplyr::bind_cols(data, df)
  return(data)
}
