##################################
# Map of predicted #
##################################

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
