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
#' @param countOnly (logical, TRUE/FALSE) for maps where no zeros are desired, if TRUE, mapFun subsets values > 0 (for raster) and for bubble plot, plots zeros as light blue.
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
      
      if (isTRUE(countOnly)) {
        
        zeros=subset(topmapDF,topmapDF$Val==0)
        topmapDF=subset(topmapDF,topmapDF$Val>0)
        
      }
      
      topmapDF=topmapDF[order(topmapDF$Val),] 

      cex=scaleFun(topmapDF$Val,a=0.5,b=4)

    } else {

      if (class(topmapDF)=="data.frame") {

        topRaster=rastFun(topmapDF$Lon,topmapDF$Lat,topmapDF$Val, overZero = countOnly)

      } else {

        topRaster = topmapDF

      }

    }

  }
  
  #################################
  # create legend colour sequence #
  #################################
  
  t=seq(from = min(topmapDF$Val), to = max(topmapDF$Val), length.out = 15)
  lag=(max(topmapDF$Val)-min(topmapDF$Val))/15
  cexLeg=scaleFun(t,a=0.5,b=4)
  g=rep(1,times = length(t))
  colLeg = rainbow(length(t),start = 0.675, end = 0.175)
  
  colours=data.frame(colLeg,t)
  colours$colLeg=as.character(colours$colLeg)
  
  
  ###########################################

  for (y in 1:length(baseRefsDf$fileNm)) {

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
      
      if (isTRUE(bubble)) {
        
        colTOP = rainbow(length(unique(topmapDF$Val)),start = 0.675, end = 0.175)
        
      } else {
        
        colTOP = rainbow(length(raster::unique(topRaster)),start = 0.16, end = 0.8,alpha = 0.8)
        
      }

    } else if (mapsVis == "both") {
      
      if (isTRUE(bubble)) {
        
        # colTOP = rainbow(length(unique(topmapDF$Val)),start = 0.675, end = 0.175)
        colTOP=colourMapper(topmapDF, colours)
        
      } else {
        
        colTOP = rainbow(length(raster::unique(topRaster)),start = 0.75, end = 0.15)
        
      }


      colScheme="Greys"
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

      par(mar=c(3,4,2.2,5) + 0.1) #  c(bottom, left, top, right)
    }

    ###########
    # Basemap #
    ###########

    if (mapsVis=="both") {

      plot(baseRaster,col = colBASE,legend = FALSE)
      
      plot(baseRaster, legend.only=TRUE, col = colBASE,
           legend.width=1, legend.shrink=0.75,
           smallplot=c(0.94, 0.95, 0.09, 0.49),
           legend.args=list(text=legBASE,
                            side=2, font=2, line=1.2, cex=1.1))

    } else if (mapsVis=="base") {

      plot(baseRaster,col = colBASE, legend = TRUE)

    }

    ###########
    # Top map #
    ###########

    if (mapsVis == "both") {

      if (isTRUE(bubble)) {
        
        if (isTRUE(countOnly)) {
          points(zeros$Lon,zeros$Lat,cex = 0.2,col = "lightblue",pch = 19)
        }
        
        points(topmapDF$Lon,topmapDF$Lat,cex = cex,col = colTOP,pch = 19)
        
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
        
        if (isTRUE(countOnly)) {
          points(zeros$Lon,zeros$Lat,cex = 0.2,col = "lightblue",pch = 19)
        }
        
        plot(topmapDF$Lon,topmapDF$Lat,cex = cex,col = colTOP, pch = 19)

      } else {

        raster::plot(topRaster,col= colTOP,legend = TRUE)

        # raster::plot(topRaster, legend.only=TRUE, col = colTOP ,
                     # legend.width=1, legend.shrink=0.75,
                     # smallplot=c(0.94, 0.95,     0.50, 0.9),
                     # legend.args=list(text=legTOP,
                     #                  side=2, font=2, line=1.2, cex=1.1))
      }


    }

    ############################
    # Contours and map outline #
    ############################
    
    if (mapsVis == "base" || mapsVis == "both") {
      
      contour(baseRaster,add = TRUE, drawlabels=TRUE,col="black",lwd=0.01)      
    }
    
    if (is.null(basemapOutline)==FALSE) {
      
      plot(outline,lwd = 3,add = TRUE,border = "forestgreen")
      
    }
    
    
    ############################
    # Legends for bubble plots #
    ############################
    
    if (isTRUE(bubble)) {
      
      op=par(mar=c(35,60,2.2,0),new = TRUE) #  c(bottom, left, top, right) 
      
    
      
      plot(t~g,cex = cexLeg,col = colours$colLeg,pch = 19,xlim = c(0.9,1.1),ylim = c(min(t)-(lag*2),max(t)), 
           frame.plot=FALSE, axes = FALSE, xaxt='n',yaxt='n', ann=FALSE)
      
      for (legPos in 1:length(t)) {
        
        text(1.05,t[legPos],paste(round(t[legPos],digits = 2)))
        
      }
      
      mid=round(length(t)/2)
      text(c(1-0.05),c(round(t[mid],digits = 2)),paste(legTOP),srt = 90,font = 2)
      
      xline=c(0.975,1.025)
      yline=c(min(t)-(lag*2),min(t)-(lag*2))
      lines(xline,y=yline,col = "forestgreen",lwd = 3)
      text(1,min(t)-(lag),paste("Spatial Extent"),font = 2)
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
