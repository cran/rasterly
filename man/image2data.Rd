% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image2data.R
\name{image2data}
\alias{image2data}
\title{Image raster to data frame.}
\usage{
image2data(x, background = "white", x_range = NULL, y_range = NULL)
}
\arguments{
\item{x}{It could be a rasterly object or a raster image.}

\item{background}{The background of image raster.}

\item{x_range}{The range represents image width.}

\item{y_range}{The range represents image height.}
}
\value{
a \code{data.table} object
}
\description{
Transform a image raster to a data frame.
}
\examples{
x <- rnorm(1000, mean = 10)
y <- rnorm(1000, mean = 20)
color <- sample(1:5, 1000, replace = TRUE)
rastObj <- data.frame(x = x, y = y, color = color) \%>\%
       rasterly(mapping = aes(x = x, y = y, color = color)) \%>\%
       rasterly_points()
p <- rasterly_build(rastObj)
dt <- image2data(p)
if(requireNamespace("ggplot2")) {
  # Note that each point represents a single pixel in the image
  ggplot2::ggplot(dt, mapping = aes(x = x, y = y)) + 
    ggplot2::geom_point(color = dt$color, size = 0.5)
}
}
\seealso{
\link{ggRasterly}
}
