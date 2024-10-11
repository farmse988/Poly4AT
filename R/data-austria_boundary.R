#' Austria Boundary Dataset
#'
#' A simple feature collection representing the boundary of Austria.
#'
#' This dataset contains geographical boundary information for Austria.
#' It is represented as a polygon with a single feature.
#'
#' @format A simple feature collection (sf) with 1 feature and 1 field:
#' \describe{
#'   \item{FID}{Feature ID, a unique identifier for the feature.}
#'   \item{geometry}{A POLYGON representing the boundary of Austria.}
#' }
#'
#' @details
#' The geometry is in WGS 84 coordinate reference system (CRS).
#' The bounding box of the polygon is:
#' - xmin: 9.521155
#' - ymin: 46.37864
#' - xmax: 17.14834
#' - ymax: 49.00977
#'
#'
#' @examples
#' library(sf)
#' data("austria_boundary", package = "Poly4AT")
#' plot(austria_boundary)
#'
"austria_boundary"
