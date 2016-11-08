#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getRMSE.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-08 15:23:27
# Modified:     2016-11-08 15:35:28
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getRMSE <- function (y, y_hat, decimals=2) {
  rmse <- sqrt(mean((y - y_hat)^2, na.rm=T))
  rmse <- round(rmse, decimals)
  return(rmse)
}
