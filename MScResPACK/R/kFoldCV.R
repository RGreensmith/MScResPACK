##################################################
# K-fold cross-validation #
##################################################

NameFull=formulas$Species[m]
NameAbbrv=formulas$dataset_name[m]

kFoldCV = function(sppColRef,dataset,NameFull,NameAbbrv) {
  
  ###
  set.seed(103)
  
  sample = sample.split(dataset[,sppColRef], SplitRatio = 1/5)
  
  test1 = subset(dataset, sample == TRUE)
  a = subset(dataset, sample == FALSE)
  
  ###
  set.seed(103)
  
  sample = sample.split(a[,sppColRef], SplitRatio = 1/4)
  
  test2 = subset(a, sample == TRUE)
  b  = subset(a, sample == FALSE)
  
  ###
  set.seed(103)
  
  sample = sample.split(b[,sppColRef], SplitRatio = 1/3)
  
  test3 = subset(b, sample == TRUE)
  c  = subset(b, sample == FALSE)
  
  ###
  set.seed(103)
  
  sample = sample.split(c[,sppColRef], SplitRatio = 1/2)
  
  test4 = subset(c, sample == TRUE)
  test5  = subset(c, sample == FALSE)
  
  #########
  rm(a,b,c)
  
  train1 = rbind(test2,test3,test4,test5)
  train2 = rbind(test1,test3,test4,test5)
  train3 = rbind(test1,test2,test4,test5)
  train4 = rbind(test1,test2,test3,test5)
  train5 = rbind(test1,test2,test3,test4)
  
  # checks
  # for (a in 1:5) {
  #   add=paste("sum(length(test",a,"$X)+length(train",a,"$X))",sep = "")
  #   print(eval(parse(text=add)))
  # }
  
  
  ####################################
  # Write formulas #
  ####################################
  
  
  fcond=formFun("cond",response = NameAbbrv,predictors = formulas$fCond[m],effExt = effortVec[effort])
  
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
        
        plot(pcv~test[,sppColRef])
        lines(lowess(pcv~test[,sppColRef]))
        dev.off()
        
        
        squaredErr=(test[,sppColRef]-pcv)^2
        
        png(filename=paste(Path,"Plots/",ModelRefNo," squared error ~ fit",a,".png", sep = ""),width=1000,height=1000)
        
        plot(squaredErr~pcv)
        lines(lowess(squaredErr~pcv))
        dev.off()
        
        
        mse[a]=mean((test[,sppColRef]-pcv)^2)
        
        sqrErrStText=paste("summaryTable$sqrErr",a,"[st]=round(mse[a],digits = 2)",sep = "")
        eval(parse(text = sqrErrStText))
        rm(sqrErrStText)
      }
    }
    
    
    
    
    
  }
  
  png(filename=paste(Path,"Plots/",ModelRefNo," mse boxplot",a,".png", sep = ""),width=1000,height=1000)
  
  boxplot(mse,main = paste("mean MSE =   ",round(mean(mse),digits = 2)))
  dev.off()
  
  summaryTable$MeansqrErr[st]=round(mean(mse),digits = 2)
}



###############################################################################################################
# End #
###############################################################################################################