

# arguments #

fileNm = "BAT_raster"
leglab = "Depth (m)"
baseRefsDf = data.frame(fileNm,leglab)

wdExtension = paste(Path,"Plots/",sep = "")

mapName = paste(ModelRefNo," Map of predicted",sep = "")

legTOP = bquote("Model predicted for " ~ .(formulas$Species[m])~ ~ abundance ~ (per ~ km^2))

topmapDF = dtst

names(topmapDF)=c("Val","Lon","Lat")

topmapDF=topmapDF[order(topmapDF$Val),] 

# create map #

mapFun(baseRefsDf = baseRefsDf, legTOP = legTOP, mapsVis = "both",
       basemapOutline = "Env_outline",
       basemapDF = NULL,topmapDF = residualsR,
       wdExtension = wdExtension ,mapName = mapName,countOnly = FALSE,bubble = FALSE)

colBASE = rainbow(length(raster::unique(baseRaster)),start = 0.51,end = 0.70)

colScheme="Greys"
n1=RColorBrewer::brewer.pal.info[colScheme,1]
colBASE = RColorBrewer::brewer.pal(n1,colScheme)

colScheme="RdYlGn"
n1=RColorBrewer::brewer.pal.info[colScheme,1]
colTOP = RColorBrewer::brewer.pal(n1,colScheme)

colTOP = topo.colors(length(unique(topmapDF$Val)))

colTOP = rainbow(length(unique(topmapDF$Val)),start = 0.75, end = 0.15,alpha = 0.80)
colTOP = rainbow(length(unique(topmapDF$Val)),start = 0.738, end = 0.15,alpha = 1)
colTOP = rainbow(length(unique(topmapDF$Val)),start = 0.675, end = 0.175)
colTOP = rainbow(length(unique(topmapDF$Val)),start = 0.5, end = 0.175)


plot(c(1:length(cex)),col = colTOP,pch = 19)

t=c(1:5)
g=rep(1,times = length(t))
colLeg = rainbow(length(t),start = 0, end = 0.5)


plot(t~g,cex = t,col = colLeg,pch = 19,xlim = c(0.9,1.1), frame.plot=FALSE)

for (legPos in 1:length(t)) {
  
  text(1.075,t[legPos],paste(t[legPos]))
  
}

text(c(1-0.075),3,paste(legTOP),srt = 90)
cex=scaleFun(topmapDF$Val,a=0.5,b=6.5)



png(filename=paste(wdExtension,mapName," ",baseRefsDf$baseNm[y],"3.png", sep = ""),width=1000,height=1000)
op=par(mar=c(3,4,2.2,5) + 0.1) #  c(bottom, left, top, right)

plot(baseRaster,col = colBASE,legend = FALSE,par(bg = 'white')) # 



points(topmapDF$Lon,topmapDF$Lat,cex = cex,col = colTOP,pch = 19)


plot(baseRaster, legend.only=TRUE, col = colBASE,
     legend.width=1, legend.shrink=0.75,
     smallplot=c(0.94, 0.95,     0.09, 0.49),
     legend.args=list(text=legBASE,
                      side=2, font=2, line=1.2, cex=1.1))
plot(outline,lwd = 0.5,add = TRUE)
contour(baseRaster,add = TRUE, drawlabels=TRUE,col="black",lwd=0.01)

par(op)
dev.off()




plot(topmapDF$Val,cex = cex,col = colTOP,pch = 19)













png(filename=paste(wdExtension,mapName," ",baseRefsDf$baseNm[y],"colours.png", sep = ""),width=1000,height=1000)
##------ Some palettes ------------
demo.pal <-
  function(n, border = if (n < 32) "light gray" else NA,
           main = paste("color palettes;  n=", n),
           ch.col = c("rainbow(n)", "heat.colors(n)",
                      "terrain.colors(n)", "topo.colors(n)",
                      "cm.colors(n)"))
  {
    nt <- length(ch.col)
    i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
    plot(i, i+d, type = "n", yaxt = "n", ylab = "", main = main)
    for (k in 1:nt) {
      rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
           col = eval(parse(text = ch.col[k])), border = border)
      text(2*j,  k * j + dy/4, ch.col[k])
    }
  }
n <- if(.Device == "postscript") 64 else 16
# Since for screen, larger n may give color allocation problem
demo.pal(n)
dev.off()

