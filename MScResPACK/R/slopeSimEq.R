#################
# Documentation #
#################
#' scale a vector 0 to 1
#'
#' This function allows you to scale a vector 0 to 1.
#' @param x1 value 1 of explanatory variable (for dy/dx)
#' @param x2 value 2 of explanatory variable (for dy/dx)
#' @param c Intercept
#' @param y response variable
#' @param m Vector of unscaled slope values
#' @param scaleMin minimum value to scale to
#' @param scaleMax maximum value to scale to
#' 
#' @keywords cats
#' @export
#' @examples
#' slopeSimEq()
#' 
#' ########
#' # data #
#' ########
#' 
#' condSummary1 = read.csv("C:/Users/Rose/Documents/Rose/Abundance models- whole study area/Species/Cetaceans/Bottlenose Dolphin/glmmTMB/Model objects/Coefficients/BTNDtmb5.1.1.1 coefficients table _ Conditional.csv",stringsAsFactors = FALSE)
#' condSummary2 = read.csv("C:/Users/Rose/Documents/Rose/Abundance models- whole study area/Species/Cetaceans/Bottlenose Dolphin/glmmTMB/Model objects/Coefficients/BTNDtmb5.2.1.1 coefficients table _ Conditional.csv",stringsAsFactors = FALSE)
#' 
#' sppColRef=formulas$dataset_name[m]
#' 
#' dataset = capXtreme(formulas,m,dataset,"X")
#' 
#' ##############
#' # Parameters #
#' ##############
#' 
#' c1 = condSummary1$Estimate[1]
#' c2 = condSummary2$Estimate[1]
#' 
#' y = dataset$BTND1
#' 
#' estLen = length(condSummary1$Estimate)
#' m1 = c(condSummary1$Estimate[2:estLen],0)
#' 
#' estLen = length(condSummary2$Estimate)
#' m2 = c(condSummary2$Estimate[2:estLen],0)
#' rm(estLen)
#' 
#' scaleMin = -1
#' scaleMax = 1
#' 
#' 
#' poiss = slopeSimEq(x1=0.2,x2=0.7,c=c1,y=y,m=m1,scaleMin = scaleMin,scaleMax = scaleMax)
#' 
#' negbin1 = slopeSimEq(x1=0.2,x2=0.7,c=c2,y=y,m=m2,scaleMin = scaleMin,scaleMax = scaleMax)
#' 
#' poiss$fam = "poiss"
#' poiss$cols = "deeppink"
#' 
#' negbin1$fam = "negbin1"
#' negbin1$cols = "blue"
#' 
#' 
#' bothModels = rbind(poiss,negbin1)
#' 
#' ##############
#' # plot #
#' ############
#' 
#' op = par(mfrow = c(3,2))
#' 
#' plot(bothModels$slopesScaled,col = bothModels$cols,pch = 20,cex = 2,main = "slope scaled")
#' abline(h=bothModels$slopesScaled[6],col = "deeppink")
#' abline(h=bothModels$slopesScaled[12],col = "blue",lty = "dashed")
#' 
#' boxplot(bothModels$slopesScaled~bothModels$fam,col=c("blue","deeppink"),main = "slope scaled")
#' 
#' 
#' plot(bothModels$m,col = bothModels$cols,pch = 20,cex = 2,main = "original slope value (m)")
#' abline(h=bothModels$m[6],col = "deeppink")
#' abline(h=bothModels$m[12],col = "blue",lty = "dashed")
#' 
#' 
#' boxplot(bothModels$m~bothModels$fam,col=c("blue","deeppink"),main = "original slope value (m)")
#' 
#' 
#' plot(bothModels$distFrm0,col = bothModels$cols,pch = 20,cex = 2,main = "distFrm0")
#' abline(h=bothModels$distFrm0[6],col = "deeppink")
#' abline(h=bothModels$distFrm0[12],col = "blue",lty = "dashed")
#' 
#' 
#' boxplot(bothModels$distFrm0~bothModels$fam,col=c("blue","deeppink"),main = "distFrm0")
#' 
#' 
#' par(op)
#' 
#' 
#' plot(bothModels$distFrm0~bothModels$m,col=c("deeppink","blue"),main = "distFrm0 ~ original slope value (m)")
#' abline(h=0,col = "darkorange",lty = "dashed")
#' abline(v=0,col = "darkgreen",lty = "dashed")
#' 
#'
#'


####################
# Function #
####################

slopeSimEq = function(x1=0.1,x2=0.9,c,y,m,scaleMin,scaleMax) {

  mByx1 = rep(NA, times = length(m))
  mByx2 = rep(NA, times = length(m))
  
  slopesScaled = rep(NA, times = length(m))
  
  for (i in c(1:length(m))) {
    
    ##################
    # find y1 and y2 #
    ##################
    
    mByx1[i] = m[i]*x1
    mByx2[i] = m[i]*x2
    
    y1 = mByx1[i] + c
    y2 = mByx2[i] + c
    yDiff = y2-y1
    
    #################################
    # Calculate scaled slope values #
    #################################
    
    ymin = min(y)
    ymax = max(y)
    
    slopesScaled[i] = ((y2-ymin)/(ymax-ymin))*(scaleMax-scaleMin)+scaleMin
    
    rm(y1,y2)
  }
  
  
  
  ###################
  # distance from 0 #
  ###################
  
  distFrm0 = rep(NA, times = (length(m)))
  
  mLen = length(m)
  rescaled0 = slopesScaled[mLen]
  rm(mLen)
  
  for (i in 1:length(distFrm0)) {
    
    distFrm0[i]=slopesScaled[i]-rescaled0
    
  }
  
  ##############
  # plot #
  ############
  
  op = par(mfrow = c(3,2))
  
  plot(slopesScaled,main = "slope scaled")
  boxplot(slopesScaled,main = "slope scaled")
  
  plot(m,main = "original slope value (m)")
  abline(h=0,col = "purple",lty = "dashed")
  
  boxplot(m,main = "original slope value (m)")
  
  plot(distFrm0,main = "distFrm0")
  abline(h=0,col = "darkorange",lty = "dashed")
  
  plot(distFrm0~m,main = "distFrm0 ~ original slope value (m)")
  
  abline(h=0,col = "darkorange",lty = "dashed")
  abline(v=0,col = "purple",lty = "dashed")
  
  
  par(op)

  ###############################
  # Return data #
  ###############################
  
  newSlopes = data.frame(slopesScaled,distFrm0,m)
  print(newSlopes)
  
  return(newSlopes)
}

#######################
# End #
#######################