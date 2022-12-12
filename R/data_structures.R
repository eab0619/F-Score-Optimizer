
#'
#' Comparison data should be in the same format as fastLink's tableCounts object, e.g.:
#'       gamma.1 gamma.2 gamma.3 gamma.4 counts
#' [1,]       0       0       0       0 113956
#' [2,]       1       0       0       0    187
#' [3,]       2       0       0       0   1455
#' [4,]       0       1       0       0     24
#' [5,]       0       2       0       0    802
#' [6,]       1       2       0       0      2
#' [7,]       2       2       0       0     10
#' [8,]       0      NA       0       0  55505
#' [9,]       1      NA       0       0     75
#' [10,]       2      NA       0       0    678
#' [11,]       0       0       1       0     39
#' [12,]       0      NA       1       0     11
#' [13,]       0       0       2       0     78
#' [14,]       1       0       2       0      1
#' [15,]       0       2       2       0      1
#' [16,]       0      NA       2       0     40
#' [17,]       0       0       0       2   1349
#' [18,]       1       0       0       2      6
#' [19,]       2       0       0       2     22
#' [20,]       0       2       0       2     14
#' [21,]       0      NA       0       2    688
#' [22,]       1      NA       0       2      1
#' [23,]       2      NA       0       2      5
#' [24,]       0       0       2       2      1
#' [25,]       2       2       2       2     43
#' [26,]       2      NA       2       2      7
#' attr(,"class")
#' [1] "fastLink"    "tableCounts"
#'
#'
#' @importFrom assert assert
#' @export
is.comparisonData <- function(obj, levels) {
  stop("Not implemented.")
}

#' @export
is.linkageChain <- function(obj, nA, nB) {
  stop("Not implemented.")
}

#' @export
is.probsMatrix <- function(obj, nA, nB) {
  stop("Not implemented.")
}

#' @export
is.linkage <- function(obj, nA, nB) {
  stop("Not implemented.")
}
