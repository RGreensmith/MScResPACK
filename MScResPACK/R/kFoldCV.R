##################################################
# K-fold cross-validation #
##################################################
# NameFull=formulas$Species[m]

#' K-fold cross-validation
#'
#' This function allows you to carry out K-fold cross-validation, specifying k.
#' @param sppColRef Name of species column. Also used as abbreviation of names for references.
#' @param dataset Dataframe object to be used in k-fold cross validation models.
#' @param NameFull Full name of the species
#' @param k Number the dataset should be split into and model iterations
#' @param formulas df containing formulas for models
#' 
#' @keywords cats
#' @export
#' @examples
#' kFoldCV()

kFoldCV = function(sppColRef,dataset,NameFull,k,formulas) {
  
  k2=k-2
  df=c("dataset",letters[1:k2])
  rm(k2)
  splitRatio=c(5:2)

  for (y in 1:c(k-1)) {
    if (y != k-1) {
      set.seed(103)
      sample = sample.split(eval(parse(text = paste(df[y],"$",sppColRef,sep = ""))), SplitRatio = 1/splitRatio[y])
      eval(parse(text = paste("test",y," = subset(dataset, sample == TRUE)",sep = "")))
      eval(parse(text = paste(df[y+1]," = subset(dataset, sample == FALSE)",sep = "")))
      
    } else {
      set.seed(103)
      sample = sample.split(c[,sppColRef], SplitRatio = 1/2)
      
      eval(parse(text = paste("test",k-1," = subset(c, sample == TRUE)",sep = "")))
      eval(parse(text = paste("test",k,"  = subset(c, sample == FALSE)", sep = "")))
    }
  }
  
  
  #########################################
  # rbinding test dfs to create train dfs #
  #########################################
  
  tLoop = 1:5
  
  for (a in 1:5) {
    
    tLoop2=subset(tLoop,tLoop != a)
    testString = paste("test",tLoop2,sep = "",collapse = ",")

    trainDfs=paste("train",a,"=rbind(",testString,")",sep = "")
    
    eval(parse(text=trainDfs))
    print(trainDfs)

  }
  
  # checks
  # for (a in 1:5) {
  #   add=paste("sum(length(test",a,"$X)+length(train",a,"$X))",sep = "")
  #   print(eval(parse(text=add)))
  # }

  ####################################
  # Write formulas #
  ####################################
  
  
  fcond=formFun("cond",response = sppColRef,predictors = formulas$fCond[m],effExt = effortVec[effort])
  
  if (length(grep("dummy",effortVec[effort],ignore.case = FALSE))>0) {
    
    fzeroi=formFun("zi",response = NULL,predictors = formulas$fZi[m],effExt = FALSE)
    
  } else {
    
    fzeroi=formFun("zi",response = NULL,predictors = formulas$fZi[m],effExt = effortVec[effort])
    
  }
  
  #####################################################################################################
  # Create model and predict #
  #####################################################################################################
  
  
  familyFull=paste(fams[fam],'(link = ','"',linkTypes[link],'"',')',sep="")
  
  ModelRefNo=paste(formulas$model_index[m],formulas$Model_number[m],".",fam,".",link,".",effort,sep = "")
  
  mse=rep(NA,times = 5)
  
  for (a in 1:5) {
    modelText = paste("try(glmmTMB(formula = fcond, data = train",a,", family=",familyFull,", ziformula = fzeroi,verbose=TRUE, se = TRUE))",sep="")
    modelcv=eval(parse(text=modelText))
    rm(modelText)
    
    if (exists("modelcv")==TRUE && typeof(modelcv)=="list") {
      pcvText=paste("try(predict(modelcv, newdata = test",a,",type = ",'"',"response",'"',"))",sep = "")
      pcv=eval(parse(text=pcvText))
      rm(pcvText)
      
      if (exists("pcv")==TRUE && typeof(pcv)=="double") {
        dfText=paste("test",a,sep="")
        test=eval(parse(text=dfText))
        rm(dfText)
        
        png(filename=paste(Path,"Plots/",ModelRefNo," pcv~test",a,".png", sep = ""),width=1000,height=1000)
        
        plot(pcv~test[,sppColRef],
             main = paste(NameFull," k = ",a," prediction",sep = ""))
        lines(lowess(pcv~test[,sppColRef]))
        dev.off()
        
        
        squaredErr=(test[,sppColRef]-pcv)^2
        
        png(filename=paste(Path,"Plots/",ModelRefNo," squared error ~ fit",a,".png", sep = ""),width=1000,height=1000)
        
        plot(squaredErr~pcv,
              main = paste(NameFull," k = ",a," squared error",sep = ""))
        lines(lowess(squaredErr~pcv))
        dev.off()
        
        
        mse[a]=mean((test[,sppColRef]-pcv)^2)
        
        sqrErrStText=paste("summaryTable$sqrErr",a,"[st]=round(mse[a],digits = 2)",sep = "")
        eval(parse(text = sqrErrStText))
        rm(sqrErrStText)
        
      }
    }
    
    
    
    
    
  }
  
  mMSqrErr=round(mean(mse),digits = 2)
  
  png(filename=paste(Path,"Plots/",ModelRefNo," mse boxplot",a,".png", sep = ""),width=1000,height=1000)
  
  boxplot(mse,main = paste("mean MSE =   ",mMSqrErr))
  dev.off()

  lst=list(mse,mMSqrErr)
  names(lst)[[1]]="meanSqrErr"
  names(lst)[[2]]="meanMeanSqrErr"
  
  return(lst) # returns the average of all mean square errors
  
}



###############################################################################################################
# End #
###############################################################################################################