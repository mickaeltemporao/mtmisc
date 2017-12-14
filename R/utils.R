# Text File
# ------------------------------------------------------------------------------
# Filename:     utils.R
# Created:      2017-12-07 09:56:49
# Modified:     2017-12-07 09:56:49
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the MIT < https://opensource.org/licenses/MIT >
# ------------------------------------------------------------------------------

#' @export
#' importFrom digest digest
anonymize <- function(x, algo = "crc32"){
  unique_hashes <- vapply(unique(x), 
                          function(object) digest(object, algo = algo), 
                          FUN.VALUE = "", 
                          USE.NAMES = TRUE)
  unname(unique_hashes[x])
}

#' @export
substr_right <- function (x, n) {
  substr(x, nchar(x)-n+1, nchar(x))
}

#' @export
impute_median <- function (x) {
  ind_na <- is.na(x)
  x[ind_na] <- median(x[!ind_na])
  as.numeric(x)
}

#' @export
range_01 <- function (x) {
    (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
extract_top <- function (input = NULL,
                         sort_var = "value",
                         top = 10) {

  input <- arrange(input, get(sort_var))
  input <- slice(input, 1:top)
  input
}

#' @export
to_array <- function (input) {
  len <- length(input)
  out <- vector("list", len)

  for (item in 1:len) {
    out[item] <- input[[item]]
  }
  out
}

#' @export
#' @importFrom dplyr anti_join
#' @importFrom dplyr slice
top_slice <- function (input, top_s, max_e = max_edges) {
  output <- anti_join(input, top_s)
  output <- slice(output, 1:max_e)
  output
}

#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr filter
#' @importFrom dplyr pull
extract_list <- function(x) {
  out <- vector("list", length(unique(x[['key']])))
  names(out) <- unique(x$key)

  for (item in names(out)) {
    tmp_val <- x %>% filter(key == item) %>% pull(value)
    out[[item]] <- vector("list", length(tmp_val))
    names(out[[item]]) <- tmp_val
    tmp_res <- x %>% filter(key == item) %>% pull(prop)
    for (res in seq_along(out[[item]])) {
      out[[item]][[res]] <- tmp_res[res]
    }
  }
  out
}
