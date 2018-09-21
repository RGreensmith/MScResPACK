####################################################
                    # rastFun #
####################################################

# rastFun is a wrapper function for sp and raster

#########
# Usage #
#########

# rastFun(Val,Lon,Lat,data,overZero = TRUE, rasterNm)

#############
# Arguments #
#############

# x             = vector of Longitude
# y             = vector of Latitude
# z             = vector of values to be rasterised
# overZero      = (logical, TRUE/FALSE) should rastFun subset data over zero, default == FALSE

################# rastFun ##########################

#' wrapper function for sp and raster
#'
#' This function allows you to create a raster object.
#' @param x vector of Longitude
#' @param y vector of Latitude
#' @param z vector of values to be rasterised
#' @param overZero (logical, TRUE/FALSE) should rastFun subset data over zero, default == FALSE
#' @keywords cats
#' @export
#' @examples
#' rastFun()
#'
#'
rastFun = function(x,y,z,overZero=FALSE) {

  dSet=data.frame(x,y,z)

  if (isTRUE(overZero)) {
    dSet=subset(dSet,dSet$z >0)
  }

  sp::coordinates(dSet)=c("x","y")
  rast <- raster::rasterFromXYZ(dSet)
  return(rast)
  rm(dSet)


}


####################################################
                    # End #
####################################################
