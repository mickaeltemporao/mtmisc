#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getRanks.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:26:54
# Modified:     2016-11-17 10:28:53
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getRanks <- function(x){
  if (all(is.na(x))) {
    ranks <- rank(c(rep(0, length(x))), ties.method = 'random')
  } else {
    ranks <- rank(x, na.last='keep', ties.method = 'random')
  }
  winner <- which(ranks == max(ranks, na.rm=TRUE))
  return(winner)
}
