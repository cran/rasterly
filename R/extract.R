#' @title Extract or replace parts of a \code{rasterly} object
#' @name extract
#' @description The \code{extract} function provides functionality for updating existing \code{rasterly} objects.
#' @param x Object from which to extract element(s) or in which to replace element(s).
#' @param name Character. A literal string to be extracted from \code{x}. See details for more information.
#'
#' @details Available names:
#' \itemize{
#'  \item{Aggregation: }{"data", "mapping", "plot_width", "plot_height", "range", "x_range",
#' "y_range", "xlim", "ylim", "aesthetics", "reduction_func", "glyph",
#' "max_size", "group_by_data_table", "drop_data", "variable_check"}
#'  \item{Display: }{"background", "color", "alpha", "span",
#'  "show_raster", "layout"}
#' }
#'
#' @examples
#' library(rasterly)
#' r <- rasterly(
#'        data = data.frame(x = 1:1e4, y = runif(1e4), category = sample(1:4, 1e4, replace = TRUE)),
#'        mapping = aes(x = x, y = y)
#' ) %>%
#'   rasterly_points(xlim = c(1, 5000)) %>%
#'   rasterly_points(
#'     mapping = aes(x = x, y = y, color = category),
#'     xlim = c(5001, 1e4)
#'   )
#' r["mapping"]
#' r["xlim"]
#'
#' # reassign parent `rasterly()` mapping
#' r["mapping"] <- aes(x = x, y = y, color = category)
#' r["mapping"]
#'
#' # reassign all mapping systems
#' r["mapping", level = 1:length(r)] <- aes(x = x, y = y)
#' r["mapping"]
#' @export
`[.rasterly` <- function(x, name) {
  # x is executed
  if(is.rasterlyBuild(x)) {
    getElement(x, name)
  } else {
    # x is an unexecuted list of environments
    lapply(x,
           function(envir) {
             .get(name, envir = envir)
           })
  }
}

#' @param ... (missing) or NULL.
#' @param value values to replace; typically an array-like R object of a similar class as x.
#' @rdname extract
#' @details 
#' Set \code{level} in \code{...}. \code{level} is numeric used for specifing level of `rasterly` object to modify; 
#' default is 1 for the parent layer (\code{rasterly()}).
#' 
#' @export
`[<-.rasterly` <- function(x, name, ..., value) {
  
  args <- list(...)
  level <- args$level %||% 1

  # x is executed
  if(is.rasterlyBuild(x)) {
    warning("Replacing elements of existing `rasterlyBuild` objects is not supported.", call. = FALSE)
    do.call(`$<-`, list(x = x, name = name, value = value))
  } else {
    # x is an unexecuted list of environments
    for(l in level) {
      assign(name, value, envir = x[[l]])
    }
    invisible(return(x))
  }
}
