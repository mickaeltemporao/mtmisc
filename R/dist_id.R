# Text File
# ------------------------------------------------------------------------------
# Filename:     dist_id.R
# Created:      2017-12-07 10:01:31
# Modified:     2017-12-07 10:01:31
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2017 Mickael Temporão
# Licensed under the MIT < https://opensource.org/licenses/MIT >
# ------------------------------------------------------------------------------

# Find pairs
dist_id <- function(dd, value) {
        if (length(value) == 1) {
                g <- grep(value, dd)
                N <- attr(dd, "Size")
                idx <- cumsum(seq(N-1, 1))
                ic <- sum(g > idx) + 1
                ir <- g - c(0,idx)[ic] + ic
                c(row = ir, col = ic)
        } else sapply(value, find_pair, dd = dd)
}

# x <- matrix(rnorm(100), nrow=5)
# dd <- dist(x)
# dd
# dd[7]
# find_pair(dd, dd[7])
# find_pair(dd, unlist(dd))
