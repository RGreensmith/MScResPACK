####################################################################################################
# wrapper function for devtools::check() to check code has no errors or warnings before committing #
####################################################################################################

#' Wrapper function to check for warnings and errors before committing
#'
#' This function allows you to check for warnings and errors before committing changes. Depends on devtools.
#' @param wd path to find files that need checking.
#' @keywords cats
#' @export
#' @examples
#' 
#' checks()
#'

checks = function(wd="C:\\Users\\Rose\\MScResPACK\\MScResPACK") {
  devtools::check(pkg=wd)
}

############################################################
# creating .Rd files and creating help files for functions #
############################################################

#' creating or updating .Rd files for functions
#'
#' This function allows you to create or update .Rd files for functions.
#' @param wd working directory path from which the file to document exists and .Rd file will be created. Default is "M:/My Documents/MScResPACK/MScResPACK"
#' @keywords cats
#' @export
#' @examples
#' 
#' devROxWrap()
#'

devROxWrap = function(wd = "M:/My Documents/MScResPACK/MScResPACK") {

  setwd(wd)
  document()
  
}

###############################################
 # end and difference sys time function #
###############################################

#' end Sys.time and return difference
#'
#' This function allows you to end Sys.time and return the time difference.
#' @param start object of Sys.time set before running command to be timed.
#' @keywords cats
#' @export
#' @examples
#' 
#' # create start time object
#' start = Sys.time()
#' 
#' # run command to be timed
#' print("hello")
#' 
#' 
#' sysTimeDiff(start)
#'

sysTimeDiff = function (start) {
  
  end=Sys.time()
  diffFun=end-start
  print(diffFun)
  return(diffFun)
  
}

###############################################
      # find summary table length #
###############################################

#' find summary table length
#'
#' This function allows you to work out how long the summary table should be.
#' @param fams index for families.
#' @param effortVec index for effort.
#' @keywords cats
#' @export
#' @examples
#' tableLenFun()
#'

tableLenFun = function (fams,effortVec) {

  return((3*length(fams))*length(effortVec))

}

###############################################
      # create empty summary table #
###############################################

#' create empty summary table
#'
#' This function allows you to create empty summary table.
#' @param tableLength number of rows.
#' @keywords cats
#' @export
#' @examples
#' summaryTblFun()
#'

summaryTblFun = function(tableLength) {

  modelref=rep(NA, times = tableLength)
  loglikelihood=rep(NA, times = tableLength)
  deviance=rep(NA, times = tableLength)
  dfResid=rep(NA, times = tableLength)
  overdispersion=rep(NA, times = tableLength)
  family=rep(NA, times = tableLength)
  link=rep(NA, times = tableLength)
  obs=rep(NA, times = tableLength)
  formulaCond=rep(NA, times = tableLength)
  formulaZi=rep(NA, times = tableLength)
  formulaDisp=rep(NA, times = tableLength)
  aic=rep(NA, times = tableLength)
  bic=rep(NA, times = tableLength)

  fitIter=rep(NA, times = tableLength)
  fitEvalGrad=rep(NA, times = tableLength)
  fitEvalFunc=rep(NA, times = tableLength)
  fitObjective=rep(NA, times = tableLength)
  fitMessage=rep(NA, times = tableLength)
  fitConvergence=rep(NA, times = tableLength)
  sdrPDHess=rep(NA, times = tableLength)
  optimMethod=rep(NA, times = tableLength)
  mgc=rep(NA, times = tableLength)

  sqrErr1=rep(NA, times = tableLength)
  sqrErr2=rep(NA, times = tableLength)
  sqrErr3=rep(NA, times = tableLength)
  sqrErr4=rep(NA, times = tableLength)
  sqrErr5=rep(NA, times = tableLength)
  MeansqrErr=rep(NA, times = tableLength)
  metropEqual=rep(NA, times = tableLength)
  dwStat=rep(NA, times = tableLength)
  sumTbl=data.frame(modelref,loglikelihood,deviance,dfResid,overdispersion,family,link,obs,aic,bic,formulaCond,formulaZi,formulaDisp,
                          fitIter,fitEvalGrad,fitEvalFunc,fitObjective,fitMessage,fitConvergence,sdrPDHess,optimMethod,mgc,sqrErr1,sqrErr2,sqrErr3,sqrErr4,sqrErr5,MeansqrErr,metropEqual,dwStat)

  rm(modelref,loglikelihood,deviance,dfResid,overdispersion,family,link,obs,aic,bic,formulaCond,formulaZi,formulaDisp,
     fitIter,fitEvalGrad,fitEvalFunc,fitObjective,fitMessage,fitConvergence,sdrPDHess,optimMethod,mgc,tableLength,sqrErr1,sqrErr2,sqrErr3,sqrErr4,sqrErr5,MeansqrErr,metropEqual,dwStat)
  return(sumTbl)
}

###############################################
    # cap extreme values in dataset #
###############################################

#' cap extreme values in dataset
#'
#' This function allows you to cap extreme values in dataset.
#' @param formulas formulas dataframe.
#' @param m model iteration.
#' @param dataset dataset for model input.
#' @param orderCol name of column used to re-order the dataset (as.character) eg. orderCol = "X".
#' @keywords cats
#' @export
#' @examples
#' capXtreme()
#'


capXtreme = function (formulas,m,dataset,orderCol) {

  dataset=dataset[order(-dataset[formulas$dataset_name[m]]),]

  print(head(dataset[formulas$dataset_name[m]]))

  for (ext in 1:formulas$ExPts[m]) {
    dataset=dataset[-1,]
  }

  rm(ext)

  print(head(dataset[formulas$dataset_name[m]]))

  ds=dataset[order(dataset[,orderCol]),]

  return(ds)

}

########################################################
# create vector of effects and/or extensions for model #
########################################################

#' create vector of effects and/or extensions for model
#'
#' This function allows you to create vector of effects and/or extensions for model.
#' @param formulas formulas dataframe.
#' @param m model iteration.
#' @keywords cats
#' @export
#' @examples
#' effectExtensFun()
#'


effectExtensFun = function (formulas,m) {

  ev=c("")
  Y=0

  # KmOffsetZi?

  if (formulas$KmOffsetZi[m]=="Y") {

    ev=c(ev,as.character("+offset(Km_s)"))

    Y=Y+1

  }

  # HrOffsetZi?

  if (formulas$HrOffsetZi[m]=="Y") {

    ev=c(ev,as.character("+offset(Hr_s)"))

    Y=Y+1

  }

  # Spatial autocorrelation?

  if (formulas$SpatAC[m]=="Y") {
    for (ch in 1:length(covFunTable$seasonYearFun)) {
      if (covFunTable$Spp[ch]==formulas$Species[m]) {

        covVec=paste("+",covFunTable$seasonYearFun[ch],"(pos+0|dummy)",sep = "")

        ev=c(ev,as.character(covVec))

        print(ev)
      }
    }
    Y=Y+1
    rm(covVec,ch)
  }


  # Temporal Autocorrelation?

  if (formulas$TempAC[m]=="Y") {

    ev=c(ev,"+ar1(monthfrom0+0|dummy)")
    print(ev)
    Y=Y+1

  }


  # All?


  # if (Y > 1) {
  #
  #   all=paste(ev[2:length(ev)],collapse = "")
  #   ev=c(ev,all)
  # }

  return(ev)

  rm(Y)
}

############################################
# set up dataset for covariance structures #
############################################

#' Set up for dataset for covariance structures
#'
#' This function allows you to set up dataset for covariance structures.
#' @param dataset dataset used for spatial covariance xy.
#' @param strucType either "temporal" or "spatial".
#' @keywords cats
#' @export
#' @examples
#' covStrucData(dataset, strucType="temporal")
#'

covStrucData = function(dataset,strucType) {
  
  if (strucType == "temporal") {
    
    dataset$monthfrom0 <- as.factor(dataset$monthfrom0)
    
  }
 
  dataset$dummy <- as.factor(dataset$dummy)
  
  return(dataset)
  
}

################################################
# Set up for spatial autocorrelation structure #
################################################

#' Set up for spatial autocorrelation structure
#'
#' This function allows you to Set up for spatial autocorrelation structure, by creating pos.
#' @param dataset dataset used for spatial covariance xy.
#'
#' @keywords cats
#' @export
#' @examples
#' spACsetup()
#'

spACsetup = function(dataset) {
  
  startLat=max(dataset$Lat)
  startLon=min(dataset$Lon)
  
  
  endLat=min(dataset$Lat)
  endLon=max(dataset$Lon)
  
  
  Lat=dataset$Lat
  Lon=dataset$Lon
  
  y=round((startLat-Lat)/5e4,digits = 0)
  x=round((Lon-startLon)/5e4,digits = 0)
  
  pos=numFactor(x,y)
  parseNumLevels(levels(pos))
  return(pos)

}

######################################
# formulas #
######################################

#' write formula for model
#'
#' This function allows you to write formula for model.
#' @param formulaType defaults to "cond".
#' @param response name of response variable (as character), defaults to NULL.
#' @param predictors name of predictor variables (as character)
#' @param effExt offset or autocorrelation structure.
#' @keywords cats
#' @export
#' @examples
#' formFun()
#'

formFun = function(formulaType=c("cond"),response=NULL,predictors,effExt) {



  if (formulaType == "cond") {

    if (effExt=="KmOffsetZi" || effExt=="HrOffsetZi") {

      form=paste(response,"~",predictors,sep = "")

    } else {

      form=paste(response,"~",predictors, effExt,sep = "")

    }

    form=formula(eval(parse(text = form)))
    return(form)

  } else {
    
    if (effExt==FALSE) {
      
      form=paste("~",predictors,sep = "")
      
    } else {
      
      if (predictors != "1") {
        
        form=paste("~",predictors, effExt,sep = "")
        
      } else {
        
        if (strsplit(effExt,"+")[[1]][1]=="+") {
          k = paste(strsplit(effExt,"+")[[1]][-1],collapse = "")
          form=paste("~",k,sep = "")
        }
        
      }
      
    }

    form=formula(eval(parse(text = form)))
    return(form)

  }



}

############################################################
# create model #
############################################################

#' Create model
#'
#' This function allows you to write a glmmTMB model.
#' @param fams family type string
#'
#' @param fam family iteration
#'
#' @param linkTypes link type string
#'
#' @param link link iteration
#'
#' @param dataset dataset to be used in model
#'
#' @param fcond formula for conditional model
#'
#' @param fzeroi formula for logistic model
#'
#' @keywords cats
#' @export
#' @examples
#' modelFun()
#'

modelFun = function(fams,fam,linkTypes,link,dataset,fcond,fzeroi) {

  familyFull=paste(fams[fam],'(link = ','"',linkTypes[link],'"',')',sep="")

  modelText = paste("try(glmmTMB(formula = fcond, data = dataset, family=",
                    familyFull,", ziformula = fzeroi,verbose=TRUE, se = TRUE))",sep="")

  start=Sys.time()

  model=eval(parse(text=modelText))

  sysTimeDiff(start)

  return(model)

}

##########################################################
# 2D matrix plot of variance covariance matrix for conditional and zero inflated (vcov, using color2D.matplot) #
##########################################################


#' 2D matrix plot of variance covariance matrices
#'
#' This function allows you to plot a 2D variance covariance matrix.
#' @param filePath file pathway to plot location, e.g. filePath = paste(Path,"Plots/vcov varcorr matrix plots/",sep = "")
#'
#' @param ModelRefNo model reference number
#'
#' @param ModelRefFull full model reference
#'
#' @param summ model summary object
#'
#' @keywords cats
#' @export
#' @examples
#' plot2DMatrix()
#'

# filePath = paste(Path,"Plots/vcov varcorr matrix plots/",sep = "")

plot2DMatrix = function(filePath,ModelRefNo,ModelRefFull,summ) {

  abbrv=c("cond","zi")
  full=c("conditional","zero-inflated")
  CorZ=data.frame(abbrv,full)

  rm(abbrv,full)

  for (cz in 1:2) {

    vcovmComm=paste("vcovm=summ$vcov$",CorZ$abbrv[cz],sep = "")
    eval(parse(text=vcovmComm))
    rm(vcovmComm)
    tik=length(colnames(vcovm))-0.5

    png(filename=paste(filePath,ModelRefNo, " vcov ",CorZ$abbrv[cz],".png", sep = ""),width=1000,height=1000)


    color2D.matplot(vcovm,cs1=c(0.5,0.9),cs2=c(1,0.6),cs3=c(1,1),
                    axes = FALSE,show.legend = TRUE,show.values = TRUE)
    axis(1,at=0.5:tik, labels=colnames(vcovm),las = 2) # at=0.5:3.5,
    axis(2,at=0.5:tik,labels=rownames(vcovm),las = 2) # ,at=0.5:3.5
    title(main = paste(ModelRefFull," variance-covariance matrix, ",CorZ$full[cz],sep = ""))
    dev.off()

    rm(vcovm,tik)
  }
  rm(cz)

}

############################################################
# Fitted vs explanatory variables: conditional/zero inflated
############################################################

#' Plot Fitted vs explanatory variables: conditional/zero inflated
#'
#' This function allows you to Plot Fitted vs explanatory variables: conditional/zero inflated.
#' @param y vector for y axis (fitted values)
#' @param data dataframe containing explanatory variables
#' @param parameters parameters
#' @param paramAbbrv parameters abbreviation
#' @param paramFullNam full name of parameters
#' @param fullPath Full file pathway to save .png
#' @param modelRefNo model reference number
#' @param modelRefFull full model reference
#' @param yName name for y axis
#' @param paramTypeNam name of parameter type
#' @keywords cats
#' @export
#' @examples
#' parametersPlotFun()
#'
# VarNames = e.g. condVars

parametersPlotFun = function(y,data,parameters,paramAbbrv,paramFullNam,fullPath,modelRefNo,modelRefFull,yName,paramTypeNam) {

  png(filename=paste(fullPath,modelRefNo,  yName ,"~", paramTypeNam, "explanatory variables.png", sep = ""),width=1000,height=1000)

  l=length(parameters)
  l=ceiling(l/2)
  op=par(mfrow=c(2,l))

  for (cv in 1:length(parameters)) {
    for (g in 1:length(paramAbbrv)) {
      if (parameters[cv]==paramAbbrv[g]) {
        var=paramFullNam[g]
      }
    }

    plot(y~data[,parameters[cv]],
         main = paste(modelRefFull,", ",yName," ~ ",parameters[cv],sep = ""),
         ylab=paste(modelRefNo," Fitted",sep = ""),
         xlab=paste(parameters[cv]," (",var,")",sep=""))

    lines(lowess(y~data[,parameters[cv]]),col = "green")

  }

  par(op)
  dev.off()

}


####################################
# Barplot fun
##################

# https://www.youtube.com/watch?v=4Y3ZjFRwMX0

# barsFun(coeffCond$Estimate)

#' Create labelled barplot
#'
#' This function allows you to Create labelled barplot.
#' @param bars vector of values to plot
#' @param main name of barplot for main title
#' @param xlab label for x-axis
#' @param barNames labels for each bar
#' @keywords cats
#' @export
#' @examples
#' barsFun()
#'
# VarNames = e.g. condVars

barsFun = function(bars,main,xlab,barNames) {

  barplot(bars,horiz=TRUE, names.arg=barNames, cex.names=1,las = 2,col="lightgrey")
  title(main = main,cex.main=1.9, adj=1)
  title( xlab=xlab,cex.lab=1.5,line = 5)

}
#
#   coeffCond=subset(coeffCond,coeffCond$variables!="(Intercept)")
#
#   coeffCond=coeffCond[order(coeffCond$Estimate),]
#
#
#   barNames=coeffCond$variables
#
#
#   png(filename=paste(Path,"Plots/",ModelRefNo, " Coefficients ~ Conditional.png", sep = ""),width=1000,height=1000)
#   op=par(mfrow=c(2,2),mar=c(7.5,12,1,0.5) + 0.1) #  c(bottom, left, top, right)
#
#
#   barplot(coeffCond$Estimate,
#           horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
#   title(main = "(a)",cex.main=1.9, adj=1)
#   title( xlab="Slope Estimate",cex.lab=1.5,line = 5)
#
#   barplot(coeffCond$Std..Error,
#           horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
#   title( main = "(b)",cex.main=1.9, adj=1)
#   title( xlab="Std Error", cex.lab=1.5,line = 5)
#
#   barplot(coeffCond$z.value,
#           horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
#   title(main = "(c)",cex.main=1.9, adj=1)
#   title( xlab="z value", cex.lab=1.5,line = 5)
#
#   barplot(coeffCond$Pr...z..,
#           horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
#   title(main = "(d)",cex.main=1.9, adj=1)
#   title( xlab="Pr(>|z|)", cex.lab=1.5,line = 5)
#
#   par(op)
#
#   dev.off()
#
#
#
#
#
#   png(filename=paste(Path,"Plots/",ModelRefNo, " Slope Estimate ~ Conditional.png", sep = ""),width=1000,height=1000)
#
#   op=par(mar=c(5,12,1,0.5) + 0.1) #  c(bottom, left, top, right)
#
#   barplot(coeffCond$Estimate, xlab="Slope Estimate (Conditional)",
#           horiz=TRUE, names.arg=c1, cex.lab=1.5, cex.names=1,las = 2,col="lightgrey")
#
#
#
#   dev.off()
#
#   rm(c1,op)



#####
# test
#########

k=function(hello) {

  a=1:5
  b=a^2
  plot(a~b)
  return(paste(hello,"nice",sep = ""))

}

#
#
#   x <- stats::runif(12); y <- stats::rnorm(12)
#   i <- order(x, y); x <- x[i]; y <- y[i]
#   plot(x,y, main = "arrows(.) and segments(.)")
#   ## draw arrows from point to point :
#   s <- seq(length(x)-1)  # one shorter than data
#   arrows(x[s], y[s], x[s+1], y[s+1], col = 1:3)
#   s <- s[-length(s)]
#   segments(x[s], y[s], x[s+2], y[s+2], col = "pink")




#################################################
                  # End #
#################################################
