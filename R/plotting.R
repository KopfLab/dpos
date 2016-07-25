
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

#' @method make_ggplot IsodatDualInletFile
#' @rdname make_ggplot
#' @export
make_ggplot.IsodatDualInletFile <- function(x, ...){
  x$make_ggplot()
}


#' @method make_ggplot RangeScanFile
#' @rdname make_ggplot
#' @export
make_ggplot.RangeScanFile <- function(x, ...){
  plot.df <- get_mass_data(x, format = "long", quiet = T) %>%
    mutate(group = paste(mass, tune_value1, tune_value2))
    #mutate(panel1 = paste(tune_param1, "=", tune_value1),
    #       panel2 = paste(tune_param2, "=", tune_value2))
  plot.df %>%
    ggplot() +
    aes(scan_value, intensity,
        linetype = mass,
        group = group,
        color = tune_value1,
        alpha = tune_value2) +
    geom_line() +
    scale_alpha_continuous(plot.df$tune_param2[1]) +
    scale_color_gradient(plot.df$tune_param1[1], low = "red", high = "blue") +
    # facet_grid(panel1 ~ panel2, scales = "free") +
    theme_bw()
}



#' Generate default interactive plot
#' @param x data object
#' @export
make_iplot <- function (x, ...) {
  UseMethod("make_iplot", x)
}

#' @method make_iplot default
#' @rdname make_iplot
#' @export
make_iplot.default <- function(x, ...) {
  stop("generating a default interactive plot is not implemented for object of class '", class(x), "'", call. = FALSE)
}

#' @method make_iplot IsodatScanFile
#' @rdname make_iplot
#' @export
make_iplot.IsodatScanFile <- function(x, ...){
  p <- make_ggplot(x)
  class(p$mapping) <- c(class(p$mapping), "uneval")
  plotly::ggplotly(p)
}

#' @method make_iplot IsodatDualInletFile
#' @rdname make_iplot
#' @export
make_iplot.IsodatDualInletFile <- function(x, ...){
  p <- make_ggplot(x) +
    theme(legend.position = "none")
  class(p$mapping) <- c(class(p$mapping), "uneval")
  plotly::ggplotly(p)
}

#' @method make_iplot RangeScanFile
#' @rdname make_iplot
#' @export
make_iplot.RangeScanFile <- function(x, ...){
  p <- make_ggplot(x) +
    theme(legend.position = "none")
  class(p$mapping) <- c(class(p$mapping), "uneval")
  plotly::ggplotly(p)
}



