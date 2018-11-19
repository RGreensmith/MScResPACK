
#' scale a vector 0 to 1
#'
#' This function allows you to scale a vector 0 to 1.
#' @param x vector of values to scale
#' @param a minimum value to be scaled to. Default value is a = 0.
#' @param b maximum value to be scaled to. Default value is a = 1.
#' @keywords cats
#' @export
#' @examples
#' scaleFun()
#'

scaleFun=function(x,a=0,b=1) {
  
  dif=b-a
  (x-min(x))/(max(x)-min(x))*dif+a
  
}
