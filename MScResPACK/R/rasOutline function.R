
#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param plot Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' rasOutline()
#'
rasOutline = function(x,plot = TRUE,installRgeos = FALSE) {

  if (installRgeos == TRUE) {
    install.packages("rgeos",quiet = TRUE)
    library(rgeos, quietly = TRUE)
  }

  x = x > -Inf

  oL =  raster::rasterToPolygons(x, dissolve=TRUE)

  if (isTRUE(plot)) {
    raster::plot(x)
    raster::plot(oL, add=TRUE)
  }

  return(oL)
}

############################################
                # End #
############################################
