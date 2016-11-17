#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     simplifyText.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-11-17 10:28:43
# Modified:     2016-11-17 10:29:02
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
simplifyText <- function(text) {
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  text <- tolower(gsub( "\\.|/|\\-|\"|\\s" , "" , text))
  return(text)
}
