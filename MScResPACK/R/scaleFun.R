
#' scale a vector 0 to 1
#'
#' This function allows you to scale a vector 0 to 1.
#' @param x vector of values to scale
#' @keywords cats
#' @export
#' @examples
#' scaleFun()
#'

scaleFun=function(x) {
  (x-min(x))/(max(x)-min(x))
}
