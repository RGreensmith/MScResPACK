
#' Plot up to 2 rasters
#'
#' This function allows you to create string of colours for a numeric vector, based on user-specified bins.
#' @param topmapDF Dataframe object containing "Val", "Lon" and "Lat".
#'
#' @param colours Dataframe object containing "colLeg" (a vector of colours) and "t" (a vector of regular intervals between minimum and maximum of "topmapDF$Val")
#'
#' @keywords cats
#' @export
#' @examples
#' colourMapper()

colourMapper = function(topmapDF, colours) {
  j=1
  colmap = rep(NA, length(topmapDF$Val))
  
  for (i in 1:length(colours$colLeg)) {
    
    while(topmapDF$Val[j]<= colours$t[i] && j<=length(topmapDF$Val)){
      
      colmap[j] = colours$colLeg[i]
      j=j+1
      
    }
    
  }
  
  return(colmap)
  
}

