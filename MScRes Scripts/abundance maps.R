##################################
# Map of predicted #
##################################


st=1
m=1
effort=1
fam=1
link=1

for (m in c(2)) { # length(formulas$model_index)
  
  Path=paste(wd,"Abundance models- whole study area/Species/",formulas$spp_Group[m],"/",formulas$Species[m],"/",
             formulas$Model_Type[m],"/",sep = "")
  
  ########################################################
  # Load df #
  ########################################################
  
  dataset = read.csv(paste(wd,formulas$csv[m],".csv",sep=""), stringsAsFactors=FALSE)
  
  ############################################################
  # Preparing abundance dataset #
  ############################################################
  
  seasonsInd=seasonsIndFun(dataset)
  dataset=prepDF(dataset)
  
  #####################################################################
  # cap extreme values #
  #####################################################################
  
  dataset = capXtreme(formulas,m,dataset,"X")
  
  sppColRef=formulas$dataset_name[m]
    
  
  # arguments #
  
  fileNm = "BAT_raster"
  leglab = "Depth (m)"
  baseRefsDf = data.frame(fileNm,leglab)
  
  wdExtension = paste(Path,"Plots/",sep = "")
  
  mapName = paste(ModelRefNo," Map of predicted",sep = "")
  
  legTOP = bquote("Model prediction of " ~ .(formulas$Species[m])~ ~ abundance ~ (per ~ km^2))
  
  
  # create map #
  
  bathymetryR=mapFun(baseRefsDf = baseRefsDf, legTOP = legTOP,mapsVis = "both",
                     basemapOutline = "Env_outline",
                     basemapDF = NULL, topmapDF = predictedR, wdExtension = wdExtension,
                     mapName = mapName,countOnly = FALSE,bubble = FALSE)
  
  rm(predictedR)
  
}
