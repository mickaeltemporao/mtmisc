#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     Test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2015-12-18 12:07:32
# Modified:     2016-10-07 10:17:32
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
#TODO: simplify outputs of functions
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
  if (all(is.na(x))) {
    ranks <- rank(c(rep(0, length(x))), ties.method = 'random')
  } else {
    ranks <- rank(x, na.last='keep', ties.method = 'random')
  }
  winner <- which(ranks == max(ranks, na.rm=TRUE))
  return(winner)
}

getWinner <- function(data, varname){
  df <- dplyr::select(data, starts_with(varname))
  df <- pbapply(df,1, getRanks)
  df <- as.data.frame(df)
  colnames(df) <- c(paste0('winner.',varname))
  df <- dplyr::bind_cols(data, df)
  return(df)
}

getBinary <- function (data, varname) {
  FUN <- function (data) {
    data <- rank(data, na.last='keep', ties.method = 'random' )
    data <- ifelse(data %in% max(data,na.rm=TRUE), 1, ifelse(is.na(data), NA, 0))
    return(data)
  }
  df <- dplyr::select(data, starts_with(varname))
  df <- t(pbapply(df,1, FUN))
  df <- as.data.frame(df)
  colnames(df) <- c(paste0(varname, 'BinaryParty', 1:dim(df)[2]))
  #data <- dplyr::bind_cols(data, df) # remove auto append to initial DF
  return(df)
}

getRci <- function (data, varname) {
  FUN <- function (data) {
    win <- getRanks(data)
    data <- data - data[win]
    data[win] <- data[win] - sort(data, decreasing = TRUE)[2]
    return(data)
  }
  df <- dplyr::select(data, starts_with(varname))
  df <- t(pbapply(df,1, FUN))
  df <- as.data.frame(df)
  colnames(df) <- c(paste0(varname, 'RciParty', 1:dim(df)[2]))
  #data <- dplyr::bind_cols(data, df) # remove auto append to initial DF
  return(df)
}

# TODO: Rename function to something more meaningful relativeIndex
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

simpleLowercase<- function(text) {
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  text <- tolower(gsub( "\\.|/|\\-|\"|\\s" , "" , text))
  return(text)
}
