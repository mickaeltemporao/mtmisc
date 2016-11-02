#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Panel Resample
# Filename:     panel_reseample.R
# Description:  Generates weights to resample from a panel given a target
# Version:      0.0.0.000
# Created:      2016-11-02 12:12:37
# Modified:     2016-11-02 12:47:05
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
source('../settings')
data <- readWorksheet(wb, sheet = "Sheet1",startCol=1,endCol=7)
files <- lapply(path_files, read.csv)
targets <- XLConnect::loadWorkbook(path_targets, sheet = 1, startRow=3)

str(files[[1]])

d <- files[[1]]

str(files[[1]][-1,])
d <- files[[1]][-1,]
d$QID43 <- 2016 - as.numeric(d$QID43)

d$QID43[d$QID43 %in% 18:29] <- '18:29'
d$QID43[d$QID43 %in% 30:44] <- '30:44'
d$QID43[d$QID43 %in% 45:64] <- '45:64'
d$QID43[d$QID43 >= 65] <- '65+'

round(prop.table(table(gender=d$QID61, age=d$QID43)),2)
