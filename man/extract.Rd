% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract}
\alias{extract}
\alias{[.rasterly}
\alias{[<-.rasterly}
\title{Extract or replace parts of a \code{rasterly} object}
\usage{
\method{[}{rasterly}(x, name)

\method{[}{rasterly}(x, name, ...) <- value
}
\arguments{
\item{x}{Object from which to extract element(s) or in which to replace element(s).}

\item{name}{Character. A literal string to be extracted from \code{x}. See details for more information.}

\item{...}{(missing) or NULL.}

\item{value}{values to replace; typically an array-like R object of a similar class as x.}
}
\description{
The \code{extract} function provides functionality for updating existing \code{rasterly} objects.
}
\details{
Available names:
\itemize{
 \item{Aggregation: }{"data", "mapping", "plot_width", "plot_height", "range", "x_range",
"y_range", "xlim", "ylim", "aesthetics", "reduction_func", "glyph",
"max_size", "group_by_data_table", "drop_data", "variable_check"}
 \item{Display: }{"background", "color", "alpha", "span",
 "show_raster", "layout"}
}

Set \code{level} in \code{...}. \code{level} is numeric used for specifing level of `rasterly` object to modify; 
default is 1 for the parent layer (\code{rasterly()}).
}
\examples{
library(rasterly)
r <- rasterly(
       data = data.frame(x = 1:1e4, y = runif(1e4), category = sample(1:4, 1e4, replace = TRUE)),
       mapping = aes(x = x, y = y)
) \%>\%
  rasterly_points(xlim = c(1, 5000)) \%>\%
  rasterly_points(
    mapping = aes(x = x, y = y, color = category),
    xlim = c(5001, 1e4)
  )
r["mapping"]
r["xlim"]

# reassign parent `rasterly()` mapping
r["mapping"] <- aes(x = x, y = y, color = category)
r["mapping"]

# reassign all mapping systems
r["mapping", level = 1:length(r)] <- aes(x = x, y = y)
r["mapping"]
}
