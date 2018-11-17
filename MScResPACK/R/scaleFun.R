
#' scale a vector 0 to 1
#'
#' This function allows you to scale a vector 0 to 1.
#' @param x vector of values to scale
#' @param a maximum value to be scaled to (i.e. 0 to a). Default value is a = 1.
#' @keywords cats
#' @export
#' @examples
#' scaleFun()
#'

scaleFun=function(x,a=1) {
  (x-min(x))/(max(x)-min(x))*a
}
