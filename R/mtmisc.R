#!/usr/bin/env Rscript
#-*- coding: utf-8 -*-

# ===============================================================
# File Name     : mtmisc.R
# Description   : Mickael Temporão's Miscellaneous Functions
# Created By    : Mickael Temporão
# Creation Date : 18-12-2015
# Last Modified : Sun Dec 27 13:49:15 2015
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
  ranks <- rank(x, na.last='keep', ties.method = 'random' )
  winner <- which(ranks == max(ranks, na.rm=TRUE))
  return(winner)
}

#getRanks <- function(x){
#  ranks <- rank(x, na.last=FALSE, ties.method = 'random' )
#  winner <- which(ranks == max(ranks, na.rm=TRUE))
#  return(winner)
#}

#getBinary <- function (data, varname, ...) {
#  require(dplyr)
#  df <- data %>% dplyr::select(starts_with(varname))
#  for (i in 1:nrow(df)) {
#  df[i,] <- rank(df[i,], na.last='keep', ties.method = 'random' )
#  df[i,] <- ifelse(df[i,] %in% max(df[i,],na.rm=TRUE), 1, 0)
#  print(paste0(round(i/nrow(df)*100,2),' % Completed'))
#  }
#  colnames(df) <- c(paste0(varname, 'BinaryParty', 1:dim(df)[2]))
#  data <- dplyr::bind_cols(data, df)
#  return(data)
#}

apply_pb <- function(X, MARGIN, FUN, ...)
{
  env <- environment()
  pb_Total <- sum(dim(X)[MARGIN])
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total,
                       style = 3)
  wrapper <- function(...)
  {
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir= env)
    setTxtProgressBar(get("pb", envir= env),
                           curVal +1)
    FUN(...)
  }
  res <- apply(X, MARGIN, wrapper, ...)
  close(pb)
  res
}

getBinary <- function (data, varname) {
  FUN <- function (data) {
    data <- rank(data, na.last='keep', ties.method = 'random' )
    data <- ifelse(data %in% max(data,na.rm=TRUE), 1, ifelse(is.na(data), NA, 0))
    return(data)
  }
  df <- dplyr::select(data, starts_with(varname))
  df <- t(apply_pb(df,1, FUN))
  df <- as.data.frame(df)
  colnames(df) <- c(paste0(varname, 'BinaryParty', 1:dim(df)[2]))
  data <- dplyr::bind_cols(data, df)
  return(data)
}

getRci <- function (data, varname) {
  FUN <- function (data) {
    data <- c(43,5,3,2)
    win <- getRanks(data)
    data <- data - data[win]
    data[win] <- data[win] - sort(data, decreasing = TRUE)[2]
  }
  df <- dplyr::select(data, starts_with(varname))
  df <- t(apply_pb(df,1, FUN))
  df <- as.data.frame(df)
  colnames(df) <- c(paste0(varname, 'RciParty', 1:dim(df)[2]))
  data <- dplyr::bind_cols(data, df)
  return(data)
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

getRelativeIndex <- function (data, varname, ...) {
  percent <- function (data, ...) {
    (data)/(sum(data,na.rm=T))
  }
  ratio <- function(values, ...){
    (values)/(max(values, na.rm=T))
  }
  data2 <- data %>% dplyr::select(starts_with(varname))
  for (i in 1:nrow(data)) {
    data2[i,] <- ratio(percent(data2[i,]))
    nParties <- length(data2[i,])
    pWinner <- getRanks(data2[i,])
    data2[i,] <- data2[i,c(1:nParties)] - data2[[i,pWinner]]
    data2[i,pWinner] <- data2[i,pWinner] - sort(data2[i,], decreasing = TRUE)[2]
  }
  colnames(data2) <- c(paste0(varname, 'RelativeParty', 1:dim(data2)[2]))
  data <- dplyr::bind_cols(data, data2)
  return(data)
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
