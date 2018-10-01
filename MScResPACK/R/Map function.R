#################################################
                  # mapFun #
#################################################

#' Plot up to 2 rasters
#'
#' This function allows you to plot with max=2 rasters: basemap and top map, with options for contours.
#' @param baseRefsDf (dataframe) dataframe of basemap refs; names to call in (df[,1] = "fileNm") and legend label (df[,2] = "leglab")
#'
#' @param legTOP (character) label of legend for top map
#'
#' @param mapsVis ("base","top","both") which map should be plotted; default = "both"
#'
#' @param basemapOutline (character or NULL) name of basemap outline to call in (optional)
#'
#' @param basemapDF raster object or null for basemap
#'
#' @param topmapDF (character) name of dataframe for top map, col names must be ("Val", "Lon", "Lat")
#'
#' @param wdExtension (character) location within the working directory to save the map (must end in "/")
#'
#' @param mapName (character) file name
#'
#' @param countOnly (logical, TRUE/FALSE) for maps where no zeros are desired, if TRUE, mapFun subsets values > 0 (for raster)
#'
#' @param bubble (logical, TRUE/FALSE) default = TRUE, should top map be bubble or normal raster
#'
#' @keywords cats
#' @export
#' @examples
#' mapFun()

################################

mapFun = function(baseRefsDf = NULL,legTOP = NULL,mapsVis = "both",basemapOutline = NULL,basemapDF=NULL,topmapDF = NULL,
                  wdExtension = NULL,mapName = NULL,countOnly = FALSE,bubble = TRUE) {


  #####################################
  # Create top map raster  or set cex #
  #####################################

  if (mapsVis == "top" || mapsVis == "both") {

    if (isTRUE(bubble)) {

      cex=scale(topmapDF$Val)

    } else {

      if (class(topmapDF)=="data.frame") {

        topRaster=rastFun(topmapDF$Lon,topmapDF$Lat,topmapDF$Val, overZero = countOnly)

      } else {

        topRaster = topmapDF

      }

    }



  }


  ###########################################

  # if (mapsVis != "top") {
  # 
  # }

  for (y in 1:length(baseRefsDf$baseNm)) {

    #################################
    # Load basemap #
    #################################

    if (is.null(basemapDF)) {

      baseRasName=load(file = paste(wd,baseRefsDf[y,1],".Rdata",sep = ""))
      b = paste("baseRaster = ", baseRasName,sep = "")
      eval(parse(text = b))

      b = paste("rm(", baseRasName,")",sep = "")
      eval(parse(text = b))

    } else {

      baseRaster = basemapDF

    }


    # if (baseRefsDf$baseNm[y]=="BAT") {
    #
    #   baseRaster=calc(baseRaster,function(x)x-x*2)
    #
    # }

    #################################
    # Load basemap outline #
    #################################

    if (is.null(basemapOutline)==FALSE) {
      
      load(paste(wd,basemapOutline,".Rdata",sep = ""))
      outline = pp
      rm(pp)
      
    }

    ##########
    # Colour #
    ##########

    if (mapsVis == "top") {

      colTOP = rainbow(length(unique(topRaster)),start = 0.16, end = 0.8,alpha = 0.8)

    } else if (mapsVis == "both") {

      colTOP = rainbow(length(unique(topRaster)),start = 0.75, end = 0.15)

      colScheme="GnBu"
      n1=RColorBrewer::brewer.pal.info[colScheme,1]
      colBASE = RColorBrewer::brewer.pal(n1,colScheme)

    } else if (mapsVis == "base") {

      colScheme="GnBu"
      n1=RColorBrewer::brewer.pal.info$maxcolors[colScheme]

      colBASE = RColorBrewer::brewer.pal(n1,colScheme)

    }

    ################
    # Legend label #
    ################

    legBASE = baseRefsDf$leglab[y]

    ###############
    # open device #
    ###############

    if (is.null(wdExtension)==FALSE) {

      png(filename=paste(wdExtension,mapName," ",baseRefsDf$baseNm[y],".png", sep = ""),width=1000,height=1000)

      op=par(mar=c(3,4,2.2,5) + 0.1) #  c(bottom, left, top, right)

    }

    ###########
    # Basemap #
    ###########

    if (mapsVis=="both") {

      plot(baseRaster,col = colBASE,
           legend = FALSE)


      plot(baseRaster, legend.only=TRUE, col = colBASE,
           legend.width=1, legend.shrink=0.75,
           smallplot=c(0.94, 0.95,     0.09, 0.49),
           legend.args=list(text=legBASE,
                            side=2, font=2, line=1.2, cex=1.1))

    } else if (mapsVis=="base") {

      plot(baseRaster,col = colBASE, legend = TRUE)

    }


    ############################
    # Contours and map outline #
    ############################

    if (mapsVis == "base" || mapsVis == "both") {

      contour(baseRaster,add = TRUE, drawlabels=TRUE,col="darkgrey",lwd=0.5)

    }

    if (is.null(basemapOutline)==FALSE) {

      plot(outline,lwd = 0.5,add = TRUE)

    }

    ###########
    # Top map #
    ###########

    if (mapsVis == "both") {

      if (isTRUE(bubble)) {

        points(x,y,cex = cex)

      } else {

        raster::plot(topRaster,col= colTOP,
                     add = TRUE,legend = FALSE)

        raster::plot(topRaster, legend.only=TRUE, col = colTOP ,
                     legend.width=1, legend.shrink=0.75,
                     smallplot=c(0.94, 0.95,     0.50, 0.9),
                     legend.args=list(text=legTOP,
                                      side=2, font=2, line=1.2, cex=1.1))
      }

    } else if (mapsVis == "top") {

      if (isTRUE(bubble)) {

        plot(x,y,cex = cex)

      } else {

        raster::plot(topRaster,col= colTOP,legend = TRUE)

        # raster::plot(topRaster, legend.only=TRUE, col = colTOP ,
                     # legend.width=1, legend.shrink=0.75,
                     # smallplot=c(0.94, 0.95,     0.50, 0.9),
                     # legend.args=list(text=legTOP,
                     #                  side=2, font=2, line=1.2, cex=1.1))
      }


    }

    ##################################
    # Close device #
    ##################################

    if (is.null(wdExtension)==FALSE) {

      par(op)
      dev.off()

    }

    ##################################

  }

}

##########################################################
                      # End #
##########################################################
