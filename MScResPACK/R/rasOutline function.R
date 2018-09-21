
#' Create polygon of raster outline
#'
#' This function allows you to create a polygon of the outline of a raster object.
#' @param x raster object that outline will be based on
#' @param plot do you want the original raster and new outline plotting to check? (TRUE/FALSE), default is TRUE.
#' @param installRgeos should rgeos be installed? (TRUE/FALSE), default is FALSE.
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
