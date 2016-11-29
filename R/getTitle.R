#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getTitle.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-29 13:55:27
# Modified:     2016-11-29 13:55:54
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getTitle <- function (var_name, q_descr) {
  qnames <- gsub("\\.", "_", q_descr)
  value <- q_descr[grep(var_name, qnames)]
  return(value)
}
