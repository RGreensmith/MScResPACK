
##################################################################
# Working directory #
##################################################################

rm(list=ls())
# wd="C:/Users/Rose/Documents/Rose/"
# wd="C:/Users/Tom/Documents/Rose_MScRes/Recent work backup/"
# wd="C:/Users/Laptop User/Documents/"
wd="M:/My Documents/Work/"
setwd(wd)

##################################################################
# Data #
###################################################################

# formulas <- read.csv(paste(wd,"Copy of FormulasTest4 (2)",".csv",sep=""), stringsAsFactors=FALSE)
# Ovexpl <- read.csv(paste(wd,"OVexpl_reduced_cols (2).csv",sep=""), stringsAsFactors=FALSE)
# covFunTable <- read.csv(paste(wd,"covFunTable new cabund df (2).csv",sep=""), stringsAsFactors=FALSE)

formulas = MScResPACK::formulas
# Ovexpl = MScResPACK::Ovexpl
# covFunTable = MScResPACK::covFunTable



##################################
# Map of predicted #
##################################

m=1

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
  
  # seasonsInd=seasonsIndFun(dataset)
  # dataset=prepDF(dataset)
  
  #####################################################################
  # cap extreme values #
  #####################################################################
  
  dataset = capXtreme(formulas,m,dataset,"X")
  
  sppColRef=formulas$dataset_name[m]
    
  ######################################################################
  # arguments #
  ######################################################################
  
  fileNm = "BAT_raster"
  leglab = "Depth (m)"
  baseRefsDf = data.frame(fileNm,leglab)

  legTOP = bquote("Abundance of " ~ .(formulas$Species[m])~ ~ abundance ~ (per ~ km^2))
  mapsVis = "topWbaseCont"
  basemapOutline = "Env_outline"
  basemapDF = NULL
  topmapDF = dataset[,sppColRef]
  wdExtension = paste(Path,"Plots/",sep = "")
  mapName = paste(ModelRefNo," Map of abundance",sep = "")
  countOnly = TRUE
  bubble = FALSE
  
  #######################################################################
  # create map #
  #######################################################################
  
  bathymetryR=mapFun(baseRefsDf, legTOP, mapsVis,basemapOutline,
                     basemapDF, topmapDF, wdExtension,
                     mapName,countOnly,bubble)
  
  rm(predictedR)
  
}


#######################################################
# End #
#######################################################