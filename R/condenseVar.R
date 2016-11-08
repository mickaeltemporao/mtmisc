#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        condenseVar
# Filename:     condenseVar.R
# Description:  Function to combine low frequency counts
# Version:      0.0.0.000
# Created:      2016-11-08 12:19:35
# Modified:     2016-11-08 12:25:10
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
#
#http://stackoverflow.com/questions/34385340/combining-low-frequency-counts-in-r
condenseVar <- function(vector, threshold = 0.02, new_name = "low_freq") {
  to_condense <- names(which(prop.table(table(vector)) < threshold))
  vector[vector %in% to_condense] <- new_name
  vector
}
