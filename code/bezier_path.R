
#' Create path defining asymmetric bezier curve. Parameterisation follows
#' that published in Wood et al. 2011. doi: 10.3138/carto.46.4.239.
#'
#' @param data A df defining an OSGB OD pair (e.g. cartesian coordinates)
#' and weight.
#'
#' @return A df of coord pairs representing asymmetric curve.
#'
#' @export
# Convert degrees to radians.
get_radians <- function(degrees) {
  (degrees * pi) / (180)
}

#' @export
get_trajectory <- function(data) {
  o_x=data$o_x
  o_y=data$o_y
  d_x=data$d_x
  d_y=data$d_y
  od_pair=data$od_pair
  weight=data$weight

  curve_angle=get_radians(-90)
  x=(o_x-d_x)/6
  y=(o_y-d_y)/6
  c_x=d_x + x*cos(curve_angle) - y*sin(curve_angle)
  c_y=d_y + y*cos(curve_angle) + x*sin(curve_angle)
  d <- tibble(
    x=c(o_x,c_x,d_x),
    y=c(o_y,c_y,d_y),
    od_pair=od_pair,
    weight=weight
  )
  return(d)
}
