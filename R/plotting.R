
#' Generate default ggplot
#' @param x data object
#' @export
make_ggplot <- function (x, ...) {
  UseMethod("make_ggplot", x)
}

#' @method make_ggplot default
#' @rdname make_ggplot
#' @export
make_ggplot.default <- function(x, ...) {
  stop("generating a default ggplot is not implemented for object of class '", class(x), "'", call. = FALSE)
}

#' @method make_ggplot IsodatScanFile
#' @rdname make_ggplot
#' @export
make_ggplot.IsodatScanFile <- function(x, ...){
  x$make_ggplot()
}


#' @method make_ggplot RangeScanFile
#' @rdname make_ggplot
#' @export
make_ggplot.RangeScanFile <- function(x, ...){
  get_mass_data(x, format = "long", quiet = T) %>%
    ggplot() +
    aes(scan_value, intensity, color = mass) +
    geom_line() +
    facet_grid(tune_value1 ~ tune_value2, scales = "free") +
    theme_bw()
}

