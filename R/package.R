#' @title MAT253 testing
#' @description This is a test
#' @name dpos
#' @docType package
#' @author Sebastian Kopf
#' @import dplyr ggplot2
#' @importFrom tidyr gather
#' @importFrom isoread isoread
#' @importFrom methods is
#' @importFrom stats setNames
#' @importFrom stringr str_match_all
#'
#' @include loading.R
#' @include plotting.R
NULL

#' @export
as.data.frame.RangeScanFile <- function(x, ...) {
  class(x) <- "data.frame"
  return(x)
}

#' @export
as_data_frame.RangeScanFile <- function(x, ...) {
  as.data.frame(x) %>% as_data_frame()
}

#' @export
print.RangeScanFile <- function(x, ...) {
  cat("IsodatRangeFile")
  cat("\nFile:", attr(x, "filename"))
  cat("\nLocation:", attr(x, "filepath"))
  cat("\nData:\n")
  print(as_data_frame(x), ...)
}
