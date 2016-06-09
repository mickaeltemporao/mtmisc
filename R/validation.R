#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     Test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-04-01 18:03:03
# Modified:     2016-06-09 15:06:17
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

# TODO: bug while loop
# TODO: add option to redo last
# TODO: add option to open internet page
# TODO: screen size max 80
# TODO: options when launching script for election
# TODO: add message when job completes

suppressMessages(
  require(dplyr)
)

#### Inputs Required
# Set the path where the debug file is
file_path <- "../../data/output/extraction_local/"

# Election information
year      <- 2015
country   <- 'can'
province  <- 'alberta'
file_type <- 'debug'
actors    <- 'politicians'

#### Start of script

# NEW Function not stable USE V1 for old version
V1 <- T
V2 <- F

prompt_user <- function() {
require(dplyr)
  cat("Enter 1=No, 2=Maybe, 3=Sure, or 'q'=Quit \n> ")
  input <- readLines(con="stdin", 1)
  cat(input, "\n")
  if(!(input %in% c(1:3, 'q'))) {
    return(prompt_user())
  } else if (input == 'q') {
        db1 <- db1 %>% bind_rows(db2)
        write.csv(db1,paste0(file_path, file_name, '_ids_validity.csv'), row.names=F)
        quit(save='no')
    }
  return(as.numeric(input))
}

# Creating global file name
file_name <- paste0 (year,'-',
                    country,'-',
                    province,'_',
                    file_type,'-',
                    actors
                    )
# Search for file to validate
input_file <- c(list.files(file_path))

ifelse (
  file.exists(paste0(file_path, file_name, '_ids_validity.csv')),
  db1 <- read.csv(paste0(file_path,file_name, '_ids_validity.csv'), stringsAsFactors=FALSE),
  db1 <- read.csv(paste0(file_path,file_name, '_ids.csv'), stringsAsFactors=FALSE)
  )

# Check & Create validity variable in data set
if ('validity' %in% names(db1)) {
  } else {
    db1[,'validity'] <- 2
  }

repeat {
# Overall Progress
  counter <- round(((sum(db1$validity==3)+1)/(dplyr::group_by(db1, Surname.and.name) %>%
    summarise() %>% nrow()))*100,0)

# Collect a list of non validated politicians
  politician <- unique(dplyr::filter(db1, validity == 2)) %>%
    dplyr::select(Surname.and.name) %>%
    collect %>% .[['Surname.and.name']] %>% sample(1)

# Separate Politician under review in 2 data bases
  db2 <- dplyr::filter(db1, Surname.and.name==politician)
  db1 <- dplyr::filter(db1, Surname.and.name!=politician) %>%
    dplyr::filter(validity %in% c(2,3))

# Prompt lines in console
if (V1 == TRUE) {
  for (i in 1:nrow(db2)) {
      user_name <- db2$Surname.and.name[i]
      followers <- db2$Followers_count[i]
      description <- db2$Description[i]
      tw_account <- db2$Twitter_account[i]


      cat(paste0('--------------------------------------------------------------------------------','\n'))
      cat(paste0('Percentage done : ',counter,' %','\n'))
      cat(paste0('--------------------------------------------------------------------------------','\n'))
      cat(paste0('Name: ',user_name,'\n'))
      cat(paste0('Account: ',tw_account,'\n'))
      cat(paste0('Folowers: ',followers,'\n'))
      cat(paste0('Description: ',description,'\n', '\n'))
      cat(paste0('--------------------------------------------------------------------------------','\n'))
      cat("Enter 1=No, 2=Maybe, 3=Sure, or 'q'=Quit \n")
      cat("> ")
      input <- readLines(file("stdin"),1)

      while(!(input %in% c(1:3, 'q'))) {
        cat(paste0('--------------------------------------------------------------------------------','\n'))
        cat("Please enter one of the following: 1=No, 2=Maybe, 3=Sure or 'q'=Quit\n")
        cat(paste0('--------------------------------------------------------------------------------','\n'))
      }
      if (input == 'q') {
          write.csv(db1,paste0(file_path, file_name, '_ids_validity.csv'))
          quit(save='no')
      }
      input <- as.numeric(input)
      db2[i,'validity'] <- input

  if (input == 3) {
    db1 <- dplyr::filter(db2,
    !(Surname.and.name==user_name & validity != 3)) %>%
      dplyr::bind_rows(db1)
    write.csv(db1,paste0(file_path, file_name, '_ids_validity.csv'))
    break
  } else {
      db1 <- db1 %>% bind_rows(db2[i,])
      write.csv(db1,paste0(file_path, file_name, '_ids_validity.csv'))
    }

  if (all(db1$validity==3)){
    cat(paste0('--------------------------------------------------------------------------------','\n'))
    cat(paste0('Validation ',counter,' %',' Complete ! \n'))
    cat(paste0('--------------------------------------------------------------------------------','\n'))
    break
  }
}
}

if (V2 == TRUE) {
  for (i in 1:nrow(db2)) {
        user_name <- db2$Surname.and.name[i]
        followers <- db2$Followers_count[i]
        description <- db2$Description[i]
        tw_account <- db2$Twitter_account[i]
        cat(paste0('--------------------------------------------------------------------------------','\n'))
        cat(paste0('Percentage done : ',counter,' %','\n'))
        cat(paste0('--------------------------------------------------------------------------------','\n'))
        cat(paste0('User: ',user_name,'\n'))
        cat(paste0('Account: ',tw_account,'\n'))
        cat(paste0('Folowers: ',followers,'\n'))
        cat(paste0('Description: ',description,'\n', '\n'))
        cat(paste0('--------------------------------------------------------------------------------','\n'))

        input <- prompt_user()
        db2[i,'validity'] <- input
  }
# Take action on input
  if (input == 3) {
    db1 <- dplyr::filter(db2,
    !(Surname.and.name==user_name & validity != 3)) %>%
      dplyr::bind_rows(db1)
    write.csv(db1,paste0(file_path, file_name, '_ids_validity.csv'))
    break
  } else {
      db1 <- db1 %>% bind_rows(db2[i,])
      write.csv(db1,paste0(file_path, file_name, '_ids_validity.csv'))
    }

  if (all(db1$validity==3)){
    cat(paste0('--------------------------------------------------------------------------------','\n'))
    cat(paste0('Validation ',counter,' %',' Complete ! \n'))
    cat(paste0('--------------------------------------------------------------------------------','\n'))
    break
  }
}


}
