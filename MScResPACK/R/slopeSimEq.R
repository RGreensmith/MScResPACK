
# slopeSimEq

# c = intercept
# y = response variable
# m = vector of unscaled slope values

########
# data #
########

condSummary = read.csv(file = "C:/Users/Rose/Documents/Rose/Abundance models- whole study area/Species/
                       Cetaceans/Bottlenose Dolphin/glmmTMB/Model objects/Coefficients/BTNDtmb5.2.1.1 coefficients table _ Conditional.csv",stringsAsFactors = FALSE)
sppColRef=formulas$dataset_name[m]

dataset = capXtreme(formulas,m,dataset,"X")

##############
# Parameters #
##############

c = condSummary$Estimate[1]

y = dataset$BTND1

estLen = length(condSummary$Estimate)
m = condSummary$Estimate[2:estLen]
rm(estLen)

scaleMin = 0
scaleMax = 1


slopeSimEq(x1=0.2,x2=0.7,c=c,y=y,m=m,scaleMin = scaleMin,scaleMax = scaleMax)

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
  
  ##############
  # plot #
  ############
  op = par(mfrow = c(2,2))
  
  plot(slopesScaled,main = "slope scaled")
  boxplot(slopesScaled,main = "slope scaled")
  
  plot(m,main = "m")
  boxplot(m,main = "m")
  
  par(op)
  
  ##############
  
  return(slopesScaled)
}

#######################
# End #
#######################