#!/usr/bin/env Rscript
#-*- coding: utf-8 -*-

# ===============================================================
# File Name     : mtmisc.R
# Description   : Mickael Temporão's Miscellaneous Functions
# Created By    : Mickael Temporão
# Creation Date : 18-12-2015
# Last Modified : Thu Dec 24 13:51:01 2015
# Contact       : mickael dot temporao dot 1 at ulaval dot ca
# ===============================================================
# Copyright (C) 2015 Mickael Temporão
# GNU GENERAL PUBLIC LICENSE Version 2
# http://www.gnu.org/licenses/gpl-2.0.txt
# ===============================================================

testSample <- function (data,by) {
# Creates small samples to test functions
#
# Args:
#   data: A data frame from which the sample data will be created
#   by: A character containing the variable name to subset from
#
# Returns:
#   A test sample data frame
  data <- as.data.frame(data)
  Temp <- data[0,] 
  for (i in unique(data[,by])){
    print(paste0('Sampling ', i,))
    Temp <- dplyr::union(Temp, head(data[data[,by] == i,]))
  }
  return(Temp)
}

getRanks <- function(x){
  ranks <- rank(x, na.last=FALSE, ties.method = 'random' )
  winner <- which(ranks == max(ranks, na.rm=TRUE))
  return(winner)
}

# TODO: Rename function to something like relativeIndex
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

getRelativeIndex <- function (data) {
  percent <- function (data, ...) {
    (data)/(sum(data,na.rm=T))
  }
  ratio <- function(values, ...){
    (values)/(max(values, na.rm=T))
  }
  data <- percent(data)
  data <- ratio(data)
  nParties <- length(data)
  pWinner <- getRanks(data)
  relativeIndex <- data[1:nParties] - data[pWinner]
  relativeIndex[pWinner] <- data[pWinner] - sort(data, decreasing = TRUE)[2]
  return(relativeIndex)
}

simpleLowercase<- function(text) {
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  text <- tolower(gsub( "\\.|/|\\-|\"|\\s" , "" , text))
  return(text)
}

# ===============================================================
# Copyright (C) 2015 Mickael Temporão
# GNU GENERAL PUBLIC LICENSE Version 2
# ===============================================================
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
