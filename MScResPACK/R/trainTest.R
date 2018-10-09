
column = sppColRef
splitBy = 5
trainTest = function(dataset,column,splitBy) {
  
  splitVec = c(1:splitBy)
  rev(splitVec)
  
  ###
  
  set.seed(103)
  sample = sample.split(dataset[,column], SplitRatio = 1/splitBy)
  
  test1 = subset(dataset, sample == TRUE)
  a = subset(dataset, sample == FALSE)
  
  for (s in 1:length(splitVec)) {
    
    set.seed(103)
    
    sample = sample.split(a[,column], SplitRatio = 1/4)
    
    test2 = subset(a, sample == TRUE)
    b  = subset(a, sample == FALSE)
    
  }
  
  
  

  set.seed(103)
  
  sample = sample.split(a[,column], SplitRatio = 1/4)
  
  test2 = subset(a, sample == TRUE)
  b  = subset(a, sample == FALSE)
  
  ###
  set.seed(103)
  
  sample = sample.split(b[,column], SplitRatio = 1/3)
  
  test3 = subset(b, sample == TRUE)
  c  = subset(b, sample == FALSE)
  
  ###
  set.seed(103)
  
  sample = sample.split(c[,column], SplitRatio = 1/2)
  
  test4 = subset(c, sample == TRUE)
  test5  = subset(c, sample == FALSE)
  
  #########
  rm(a,b,c)
  
  train1 = rbind(test2,test3,test4,test5)
  train2 = rbind(test1,test3,test4,test5)
  train3 = rbind(test1,test2,test4,test5)
  train4 = rbind(test1,test2,test3,test5)
  train5 = rbind(test1,test2,test3,test4)
  
}

###########################################################################
# End #
###########################################################################