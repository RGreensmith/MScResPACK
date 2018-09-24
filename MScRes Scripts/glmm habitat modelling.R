##################################################################
                      # Packages #
##################################################################
# find_rtools()
# install.packages("plotrix")
# install.packages("corrplot")
# install.packages("gstat")
# install.packages("sp")
# install.packages("stringr")
# install.packages("plyr")
# install.packages("glmmTMB")
# install.packages("raster")
# install.packages("nlme")
# install.packages("RColorBrewer")
# install.packages("lattice")
# install.packages("caTools")
# install.packages("MCMCpack")
# install.packages("coda")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("emmeans")
# install.packages("car")
# install.packages("devtools")
# devtools::install_github("RGreensmith/MScResPACK",subdir = "MScResPACK")


library(MCMCpack)
library(coda)
library(ggplot2); theme_set(theme_bw())
library(reshape2)
library(emmeans)
library(plotrix)
library(corrplot)
library(gstat)
library(sp)
library(stringr)
library(plyr)
library(glmmTMB)
library(raster)
library(nlme)
library(RColorBrewer)
library(lattice)
library(caTools)
library(car)
library(MScResPACK)

# https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf

# library(R2admb)
# install.packages("glmmADMB",
# repos="http://glmmadmb.r-forge.r-project.org/repos")
# library(glmmADMB)

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
Ovexpl = MScResPACK::Ovexpl
covFunTable = MScResPACK::covFunTable


####################################
# Families #
###################################

fams=c("poisson","nbinom1","nbinom2")

####################################
# Link #
####################################

linkTypes=c("log")
# ,"logit","probit"
##########################################################################################################################
##########################################################################################################################

                                 #      Model creation and outputs Script      #

##########################################################################################################################
# Notes #
#########

# timing things #

start=Sys.time()
# your code #
end=Sys.time()
diffFun=end-start
print(diffFun)
rm(start,end,diffFun)
##########################################################################################################################

#### LOOPS THROUGH EFFORT FIRST BUT MODEL REF IS STILL: formulas$model_index[m],formulas$Model_number[m],".",fam,".",effort ####

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
  # create empty summary table, create spatial autocorrelation vector #
  #####################################################################

  if (formulas$Last_or_first_model_of_section[m]=="first"){

    ##################################
    # Cap extreme values #
    ##################################

    dataset = capXtreme(formulas,m,dataset,"X")

    sppColRef=formulas$dataset_name[m]

    ############################################
    # effects and extensions vector #
    ############################################

    effortVec=effectExtensFun(formulas,m)

    ####################################
    # Creating Summary table #
    ###################################

    tableLength=tableLenFun(fams,effortVec)
    summaryTable=summaryTblFun(tableLength)

    ############################################
    # Print model section status #
    ############################################

    print(paste("model of section = ",formulas$Last_or_first_model_of_section[m], ",  m = ",m,",  current species is   " ,formulas$Species[m],
                ",    next species is   ",formulas$Species[m+1],",   # CREATED #  TABLE OF ",formulas$Species[m]," SECTION",sep = ""))

  }

  #####################################################################

  for (effort in 1:length(effortVec)) { # :length(effortVec)

    if (effort > 1) {
      #
      # dataset$dummy <- numFactor(dataset$dummy)
      # levels(dataset$dummy)

      dataset$monthfrom0 <- as.factor(dataset$monthfrom0)
      dataset$dummy <- as.factor(dataset$dummy)

      dataset$LonSpCov=dataset$Lon
      dataset$LatSpCov=dataset$Lat
      # dataset$MonthSpCov=dataset$Month

      x=dataset$LonSpCov
      y=dataset$LatSpCov
      pos=numFactor(x,y)
      parseNumLevels(levels(pos))

      # dataset$SeasonNumeric <- numFactor(dataset$SeasonNumeric)
      # levels(dataset$SeasonNumeric)
      #


    }


    for (fam in 1:length(fams)) {

      for (link in 1:length(linkTypes)) {

        NameFull=formulas$Species[m]
        NameAbbrv=formulas$dataset_name[m]

        ####################################
        # Write formulas #
        ####################################


        fcond=formFun("cond",response = NameAbbrv,predictors = formulas$fCond[m],effExt = effortVec[effort])
        fzeroi=formFun("zi",response = NULL,predictors = formulas$fZi[m],effExt = effortVec[effort])

        print(paste("m = ",m,"   family = ",fam,"   link = ",link,"  effort = ",effort))
        print(fcond)
        print(fzeroi)



        #####################################################################################################
        # Create model #
        #####################################################################################################


        familyFull=paste(fams[fam],'(link = ','"',linkTypes[link],'"',')',sep="")

        modelText = paste("try(glmmTMB(formula = fcond, data = dataset, family=",familyFull,", ziformula = fzeroi,verbose=TRUE, se = TRUE))",sep="")

        ptm <- proc.time()
        model=eval(parse(text=modelText))
        proc.time() - ptm

        rm(fcond,modelText)

        if (exists("model")==TRUE && typeof(model)=="list") {


          if (model$sdr$pdHess==TRUE) {

            ModelRefNo=paste(formulas$model_index[m],formulas$Model_number[m],".",fam,".",link,".",effort,sep = "")
            ModelRefFull=paste(formulas$Species[m],", model no. ",formulas$Model_number[m],".",fam,".",link,".",effort,sep = "")
            modelRefEnd=paste(", model ",formulas$Model_number[m],".",fam,".",link,".",effort,sep = "")

            ####################################
            # Export whole summary #
            ####################################

            out <- capture.output(summary(model))
            cat(ModelRefNo, out, file=paste(Path,formulas$model_index[m], " model summaries.csv", sep = ""), sep="\n", append=TRUE)

            cat(ModelRefNo, out, file=paste(Path,formulas$model_index[m], " model summaries.txt", sep = ""), sep="\n", append=TRUE)
            rm(out)
            summ=summary(model)
            print(summ)

            ################################################################################################################
            # 2D matrix plot of variance covariance matrix for conditional and zero inflated (vcov, using color2D.matplot) #
            ################################################################################################################

            abbrv=c("cond","zi")
            full=c("conditional","zero-inflated")
            CorZ=data.frame(abbrv,full)

            rm(abbrv,full)

            for (cz in 1:2) {

              vcovmComm=paste("vcovm=summ$vcov$",CorZ$abbrv[cz],sep = "")
              eval(parse(text=vcovmComm))
              rm(vcovmComm)
              tik=length(colnames(vcovm))-0.5

              png(filename=paste(Path,"Plots/vcov varcorr matrix plots/",ModelRefNo, " vcov ",CorZ$abbrv[cz],".png", sep = ""),width=1000,height=1000)


              color2D.matplot(vcovm,cs1=c(0.5,0.9),cs2=c(1,0.6),cs3=c(1,1),
                              axes = FALSE,show.legend = TRUE,show.values = TRUE)
              axis(1,at=0.5:tik, labels=colnames(vcovm),las = 2) # at=0.5:3.5,
              axis(2,at=0.5:tik,labels=rownames(vcovm),las = 2) # ,at=0.5:3.5
              title(main = paste(ModelRefFull," variance-covariance matrix, ",CorZ$full[cz],sep = ""))
              dev.off()

              rm(vcovm,tik)
            }
            rm(cz)



            ########################################################################
            # Confidence Interval barplot of parameter estimates and sigma boxplot #
            ########################################################################

            CI=confint(model)

            low=CI[,1]
            up=CI[,2]

            png(filename=paste(Path,"Plots/vcov varcorr matrix plots/",ModelRefNo," estimate and confidence intervals",".png", sep = "")
                ,width=1000,height=1000)

            bp=barplot(CI[,3],names.arg = attr(CI,"dimnames")[[1]],
                       las = 2,cex.names  =0.7,
                       ylim = c(min(CI[,2])-5 ,max(CI[,2])))

            segments(bp,low,bp,up)
            title(main = paste(ModelRefFull,", slope estimate (2.5 % - 97.5 % CI)",sep = ""))

            dev.off()


            rm(CI,bp,low,up)


            ####################################
            # Fill summary table #
            ####################################

            if (formulas$Model_number[m]==1 && fam==1 && link==1 && effort ==1) {
              st=1
            } else {
              st=st+1
            }
            print(st)


            aicTab=summ$AICtab
            mcall=summ$call
            mcall=as.character(mcall)
            mgc=model$obj$fn()
            summaryTable$modelref[st]=ModelRefNo
            summaryTable$loglikelihood[st]=aicTab[3]
            summaryTable$deviance[st]=aicTab[4]
            summaryTable$dfResid[st]=aicTab[5]
            summaryTable$overdispersion[st]=summ$sigma
            summaryTable$family[st]=summ$family
            summaryTable$link[st]=summ$link
            summaryTable$obs[st]=summ$nobs
            summaryTable$formulaCond[st]=mcall[2]
            summaryTable$formulaZi[st]=mcall[5]
            summaryTable$formulaDisp[st]=mcall[7]
            summaryTable$aic[st]=aicTab[1]
            summaryTable$bic[st]=aicTab[2]


            summaryTable$fitIter[st]=model$fit$iterations
            summaryTable$fitEvalGrad[st]=model$fit$evaluations[2]
            summaryTable$fitEvalFunc[st]=model$fit$evaluations[1]
            summaryTable$fitObjective[st]=model$fit$objective
            summaryTable$fitMessage[st]=model$fit$message
            summaryTable$fitConvergence[st]=model$fit$convergence
            summaryTable$sdrPDHess[st]=model$sdr$pdHess
            summaryTable$optimMethod[st]=model$obj$method
            summaryTable$mgc[st]=mgc[1]
            rm(aicTab,mcall,mgc)

            write.csv(summaryTable, file=paste(Path,formulas$model_index[m],effort, " summary table.csv", sep = ""))

            #############################################################
            # Extract coefficients #
            #############################################################

            ############################
            # Conditional coefficients #
            ############################

            coeffC=summ$coefficients$cond
            coeffCond=data.frame(unlist(coeffC))
            coeffCond$variables=attributes(coeffCond)$row.names
            coeffCond$modelRef=rep(ModelRefNo)
            coeffCond=format(coeffCond,digits=3)

            coeffCond$Estimate=as.numeric(coeffCond$Estimate)
            coeffCond$Std..Error=as.numeric(coeffCond$Std..Error)
            coeffCond$z.value=as.numeric(coeffCond$z.value)
            coeffCond$Pr...z..=as.numeric(coeffCond$Pr...z..)

            coeffCond$variables=as.character(coeffCond$variables)
            coeffCond$modelRef=as.character(coeffCond$modelRef)

            rm(coeffC)

            for (a in 1:length(Ovexpl$Abbrv)) {
              for (b in 1:length(coeffCond$variables)) {
                if (Ovexpl$Abbrv[a]==coeffCond$variables[b]) {
                  coeffCond$variables[b]=Ovexpl$Expl_Condensed[a]
                }
              }
            }
            rm(a,b)


            write.csv(coeffCond, file=paste(Path,"Model objects/Coefficients/",ModelRefNo, " coefficients table ~ Conditional.csv", sep = ""))

            ##############################
            # Zero-inflated coefficients #
            ##############################

            coeffZ=summ$coefficients$zi
            coeffZi=data.frame(unlist(coeffZ))
            coeffZi$variables=attributes(coeffZi)$row.names
            coeffZi$modelRef=rep(ModelRefNo)
            coeffZi=format(coeffZi,digits=3)

            coeffZi$Estimate=as.numeric(coeffZi$Estimate)
            coeffZi$Std..Error=as.numeric(coeffZi$Std..Error)
            coeffZi$z.value=as.numeric(coeffZi$z.value)
            coeffZi$Pr...z..=as.numeric(coeffZi$Pr...z..)

            coeffZi$variables=as.character(coeffZi$variables)
            coeffZi$modelRef=as.character(coeffZi$modelRef)

            rm(coeffZ)

            for (a in 1:length(Ovexpl$Abbrv)) {
              for (b in 1:length(coeffZi$variables)) {
                if (Ovexpl$Abbrv[a]==coeffZi$variables[b]) {
                  coeffZi$variables[b]=Ovexpl$Expl_Condensed[a]
                }
              }
            }
            rm(a,b)

            write.csv(coeffZi, file=paste(Path,"/Model objects/Coefficients/",ModelRefNo, " coefficients table ~ Zero-inflated.csv", sep = ""))

            ######################################
            # Creating coeffCond_AllModels table #
            ######################################

            if (formulas$Last_or_first_model_of_section[m]=="first"){

              EstimateC=coeffCond$Estimate
              Std..ErrorC=coeffCond$Std..Error
              z.valueC=coeffCond$z.value
              Pr...z..C=coeffCond$Pr...z..
              variablesC=coeffCond$variables
              modelRefC=coeffCond$modelRef

            } else {
              EstimateC=append(EstimateC,coeffCond$Estimate,after = length(EstimateC))
              Std..ErrorC=append(Std..ErrorC,coeffCond$Std..Error,after = length(Std..ErrorC))
              z.valueC=append(z.valueC,coeffCond$z.value,after = length(z.valueC))
              Pr...z..C=append(Pr...z..C,coeffCond$Pr...z..,after = length(Pr...z..C))
              variablesC=append(variablesC,coeffCond$variables,after = length(variablesC))
              modelRefC=append(modelRefC,coeffCond$modelRef,after = length(modelRefC))

            }

            ####################################
            # Creating coeffZi_AllModels table #
            ####################################

            if (formulas$Last_or_first_model_of_section[m]=="first"){

              EstimateZ=coeffZi$Estimate
              Std..ErrorZ=coeffZi$Std..Error
              z.valueZ=coeffZi$z.value
              Pr...z..Z=coeffZi$Pr...z..
              variablesZ=coeffZi$variables
              modelRefZ=coeffZi$modelRef

            } else {
              EstimateZ=append(EstimateZ,coeffZi$Estimate,after = length(EstimateZ))
              Std..ErrorZ=append(Std..ErrorZ,coeffZi$Std..Error,after = length(Std..ErrorZ))
              z.valueZ=append(z.valueZ,coeffZi$z.value,after = length(z.valueZ))
              Pr...z..Z=append(Pr...z..Z,coeffZi$Pr...z..,after = length(Pr...z..Z))
              variablesZ=append(variablesZ,coeffZi$variables,after = length(variablesZ))
              modelRefZ=append(modelRefZ,coeffZi$modelRef,after = length(modelRefZ))

            }

            ##########################################
            # Create vector of conditional variables #
            ##########################################

            condVars=strsplit(formulas$fCond[m],split = '+',fixed = TRUE)
            # refer by condVars[[1]][cv] to each variable, where cv=1:length(condVars[[1]])

            ############################################
            # Create vector of zero-inflated variables #
            ############################################

            ziVars=strsplit(formulas$fZi[m],split = '+',fixed = TRUE)
            # refer by ziVars[[1]][zv] to each variable, where zv=1:length(ziVars[[1]])

            #############
            # Residuals #
            #############

            r=residuals(model)
            # default is type = "response", pearson residuals aren't compatible with zero-inflation models yet

            dtst= data.frame(r, dataset$Lon, dataset$Lat,dataset$lonlat)

            colnames(dtst)=c("r","Lon","Lat","lonlat")

            dtst=ddply(dtst,"lonlat",numcolwise(mean))

            residualsR = rastFun(dtst$Lon,dtst$Lat,dtst$r,overZero = FALSE)

            rm(dtst)

            ###########
            # Fitted #
            ###########

            f=fitted(model, model = "count")
            # fz=fitted(model,model = "zero")

            #############
            # Predicted #
            #############

            p=predict(model,type = "response")

            # returns expected value; this is mu*(1-p) for zero-inflated models and mu otherwise
            # Denoting mu as the mean of the conditional distribution and p as the zero-inflation probability

            dtst= data.frame(p, dataset$Lon, dataset$Lat,dataset$lonlat)

            colnames(dtst)=c("p","Lon","Lat","lonlat")

            dtst=ddply(dtst,"lonlat",numcolwise(mean))

            predictedR = rastFun(dtst$Lon,dtst$Lat,dtst$p,overZero = FALSE)

            rm(dtst)

            #############################################################
            # Plots #
            #############################################################
            #######################
            # Residuals vs fitted #
            #######################

            png(filename=paste(Path,"Plots/",ModelRefNo," residuals ~ fitted.png", sep = ""),width=1000,height=1000)

            plot(r~f,  main = paste(ModelRefFull,", Residuals ~ Fitted",sep = ""),ylab = "Residuals",xlab = "Fitted") #
            dev.off()

            #######################
            # Residuals vs fitted by MONTH #
            #######################

            png(filename=paste(Path,"Plots/",ModelRefNo," residuals ~ fitted by month.png", sep = ""),width=1000,height=1000)

            print(xyplot(r~f|dataset$Month,  main = paste(ModelRefFull,", Residuals ~ Fitted | Month",sep = ""),ylab = "Residuals",xlab = "Fitted"))
            dev.off()

            #######################


            ######################################
            # Fitted and resid vs explanatory variables conditional #
            ######################################

            parametersPlotFun(f,dataset,condVars[[1]],Ovexpl$Abbrv,Ovexpl$Expl_Condensed,paste(Path,"Plots/",sep = ""),
                              ModelRefNo,ModelRefFull,"Fitted","cond")

            parametersPlotFun(r,dataset,condVars[[1]],Ovexpl$Abbrv,Ovexpl$Expl_Condensed,paste(Path,"Plots/",sep = ""),
                              ModelRefNo,ModelRefFull,"Residuals","cond")

            ######################################
            # Fitted and resid vs explanatory variables zero inflated #
            ######################################


            if (fzeroi != "~1") {

              parametersPlotFun(f,dataset,ziVars[[1]],Ovexpl$Abbrv,Ovexpl$Expl_Condensed,paste(Path,"Plots/",sep = ""),
                                ModelRefNo,ModelRefFull,"Fitted","zi")

              parametersPlotFun(r,dataset,ziVars[[1]],Ovexpl$Abbrv,Ovexpl$Expl_Condensed,paste(Path,"Plots/",sep = ""),
                                ModelRefNo,ModelRefFull,"Residuals","zi")

            }


            ##########################
            # Predicted vs residuals #
            ##########################

            png(filename=paste(Path,"Plots/",ModelRefNo, " residuals ~ predicted.png", sep = ""),width=1000,height=1000)
            plot(r~p,
                 main = paste(ModelRefFull,", Residuals ~ Predicted",sep = ""),
                 ylab = "Residuals",
                 xlab = "Predicted")
            dev.off()



            ############################
            # Bubble plot of residuals #
            ############################

            png(filename=paste(Path,"Plots/",ModelRefNo, " bubble plot of residuals.png", sep = ""),width=1000,height=1000)
            print(bubble(residuals, "r", col = c("grey","blue"),  main = paste(ModelRefFull,", Residuals",sep = "")))
            dev.off()

            rm(residuals)

            ##################################
            # Map of predicted and residuals #
            ##################################

            png(filename=paste(Path,"Plots/",ModelRefNo, " Maps of p and r.png", sep = ""),width=1000,height=1000)
            layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
            plot(r~p, main = paste(ModelRefFull,", Residuals ~ Predicted",sep = ""),xlab = "Predicted", ylab="Residuals")
            plot(residualsR, main = paste(ModelRefFull,", Residuals",sep = ""))
            plot(predictedR, main = paste(ModelRefFull,", Predicted",sep = ""))

            dev.off()



            ##################################
            # Map of predicted #
            ##################################

            # arguments #

            fileNm = "BAT_s"
            leglab = "Depth (m)"
            baseRefsDf = data.frame(fileNm,leglab)

            wdExtension = paste(Path,"Plots/",sep = "")

            mapName = paste(ModelRefNo," Map of predicted",sep = "")

            legTOP = bquote("Model prediction of " ~ .(formulas$Species[m])~ ~ abundance ~ (per ~ km^2))


            # create map #

            bathymetryR=mapFun(baseRefsDf = baseRefsDf, legTOP = legTOP,mapsVis = "top",basemapOutline = "Env_outline",
                               topmapDF = predictedR, wdExtension = wdExtension ,mapName = mapName,countOnly = FALSE,bubble = FALSE)

            rm(predictedR)

            ##################################
            # Map of residuals #
            ##################################


            # arguments #

            fileNm = "BAT_s"
            leglab = "Depth (m)"
            baseRefsDf = data.frame(fileNm,leglab)

            wdExtension = paste(Path,"Plots/",sep = "")

            mapName = paste(ModelRefNo," Map of residuals",sep = "")

            legTOP = bquote("Model residuals for " ~ .(formulas$Species[m])~ ~ abundance ~ (per ~ km^2))


            # create map #

            mapFun(baseRefsDf = baseRefsDf, legTOP = legTOP,
                   mapsVis = "top",basemapOutline = "Env_outline",basemapDF = bathymetryR,topmapDF = residualsR,
                   wdExtension = wdExtension ,mapName = mapName,countOnly = FALSE,bubble = FALSE)

            rm(residualsR)


            ##################################
            # Map of ### JUST PREDICTED ### by SEASON over contour of each expl variable except substrate #
            ##################################

            for (seas in 1:length(seasonsInd$seasonRef)) { # season index

              ############################################
              # rasterising predicted for selected season #
              ############################################

              pred1S= data.frame(p, dataset$Lon, dataset$Lat,dataset$lonlat,dataset$Season)
              pred1S=subset(pred1S,pred1S$dataset.Season == seasonsInd$seasonName[seas])

              predS=ddply(pred1S,"dataset.lonlat",numcolwise(mean))

              x=predS$dataset.Lon
              y=predS$dataset.Lat
              z=predS$dataset.p

              predictedSR=rastFun(x,y,z,overZero=FALSE)


              rm(pred1S,predS)


              for (cv in 1:length(condVars[[1]])) {

                ########################################################
                # rasterising explanatory variables for selected season #
                ########################################################

                Expl1S= data.frame(dataset[condVars[[1]][cv]], dataset$Lon, dataset$Lat,dataset$lonlat,dataset$Season)
                Expl1S=subset(Expl1S,Expl1S$dataset.Season == seasonsInd$seasonName[seas])

                ExplS=ddply(Expl1S,"dataset.lonlat",numcolwise(mean))

                x=ExplS$dataset.Lon
                y=ExplS$dataset.Lat
                zname=paste("dataset.",condVars[[1]][cv],sep = "")
                z=ExplS[zname]

                ExplVariableSR=rastFun(x,y,z,overZero=FALSE)

                rm(Expl1S,ExplS,)

                ############################################################
                # Create season plots #
                ############################################################
                # seasonsInd

                for (g in 1:length(Ovexpl$Abbrv)) {
                  if (condVars[[1]][cv]==Ovexpl$Abbrv[g]) {
                    var=Ovexpl$Expl_Full[g]
                  }
                }



                png(filename= paste(Path,"Plots/predicted maps by season/",ModelRefNo, " P ",
                                    condVars[[1]][cv]," s.",seas," ",seasonsInd$seasonName[seas],".png", sep = "")
                    ,width=1000,height=1000)

                op=par(mar=c(5,3,3,2)) #  c(bottom, left, top, right)

                plot(predictedSR,
                     main =  paste("Predicted ",seasonsInd$seasonName[seas]," Abundance of ",formulas$Species[m],sep = ""),cex.main = 1.5)

                contour(ExplVariableSR,add = TRUE, drawlabels=TRUE)

                par(op)

                title(sub = paste("Contours: ",var,sep=""),line = 3.4,cex.sub=1.5)

                dev.off()




              }

            }
            rm(seas,predictedSR,ExplVariableSR,op,g,var,cv)

            ###################################
            # Coefficients barplots Conditional #
            #####################################

            coeffCond=subset(coeffCond,coeffCond$variables!="(Intercept)")

            coeffCond=coeffCond[order(coeffCond$Estimate),]


            c1=coeffCond$variables


            png(filename=paste(Path,"Plots/",ModelRefNo, " Coefficients ~ Conditional.png", sep = ""),width=1000,height=1000)
            op=par(mfrow=c(2,2),mar=c(7.5,12,1,0.5) + 0.1) #  c(bottom, left, top, right)


            barplot(coeffCond$Estimate,
                    horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
            title(main = "(a)",cex.main=1.9, adj=1)
            title( xlab="Slope Estimate",cex.lab=1.5,line = 5)

            barplot(coeffCond$Std..Error,
                    horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
            title( main = "(b)",cex.main=1.9, adj=1)
            title( xlab="Std Error", cex.lab=1.5,line = 5)

            barplot(coeffCond$z.value,
                    horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
            title(main = "(c)",cex.main=1.9, adj=1)
            title( xlab="z value", cex.lab=1.5,line = 5)

            barplot(coeffCond$Pr...z..,
                    horiz=TRUE, names.arg=c1, cex.names=1,las = 2,col="lightgrey")
            title(main = "(d)",cex.main=1.9, adj=1)
            title( xlab="Pr(>|z|)", cex.lab=1.5,line = 5)

            par(op)

            dev.off()





            png(filename=paste(Path,"Plots/",ModelRefNo, " Slope Estimate ~ Conditional.png", sep = ""),width=1000,height=1000)

            op=par(mar=c(5,12,1,0.5) + 0.1) #  c(bottom, left, top, right)

            barplot(coeffCond$Estimate, xlab="Slope Estimate (Conditional)",
                    horiz=TRUE, names.arg=c1, cex.lab=1.5, cex.names=1,las = 2,col="lightgrey")



            dev.off()

            rm(c1,op)

            #######################################
            # Coefficients barplots Zero_inflated #
            #######################################

            coeffZi=subset(coeffZi,coeffZi$variables!="(Intercept)")
            coeffZi=coeffZi[order(coeffZi$Estimate),]

            c2=coeffZi$variables


            png(filename=paste(Path,"Plots/",ModelRefNo, " Coefficients ~ Zi.png", sep = ""),width=1000,height=1000)
            op=par(mfrow=c(2,2),mar=c(7.5,12,1,0.5) + 0.1) #  c(bottom, left, top, right)


            barplot(coeffZi$Estimate,
                    horiz=TRUE, names.arg=c2, cex.names=1,las = 2,col="lightgrey")
            title(main = "(a)",cex.main=1.9, adj=1)
            title( xlab="Slope Estimate",cex.lab=1.5,line = 5)

            barplot(coeffZi$Std..Error,
                    horiz=TRUE, names.arg=c2, cex.names=1,las = 2,col="lightgrey")
            title( main = "(b)",cex.main=1.9, adj=1)
            title( xlab="Std Error", cex.lab=1.5,line = 5)

            barplot(coeffZi$z.value,
                    horiz=TRUE, names.arg=c2, cex.names=1,las = 2,col="lightgrey")
            title(main = "(c)",cex.main=1.9, adj=1)
            title( xlab="z value", cex.lab=1.5,line = 5)

            barplot(coeffZi$Pr...z..,
                    horiz=TRUE, names.arg=c2, cex.names=1,las = 2,col="lightgrey")
            title(main = "(d)",cex.main=1.9, adj=1)
            title( xlab="Pr(>|z|)", cex.lab=1.5,line = 5)

            par(op)

            dev.off()




            png(filename=paste(Path,"Plots/",ModelRefNo, " Slope Estimate ~ zi.png", sep = ""),width=1000,height=1000)

            op=par(mar=c(5,12,1,0.5) + 0.1) #  c(bottom, left, top, right)

            barplot(coeffZi$Estimate, xlab="Slope Estimate (Zero-inflated)",
                    horiz=TRUE, names.arg=c2, cex.lab=1.5, cex.names=1,las = 2,col="lightgrey")



            dev.off()

            rm(c2,op)

            ##############################################
            # Residuals density #
            ##############################################


            # r = residuals(fm1)
            # f = fitted(fm1)
            # p = predict(fm1)



            # summaryTable$shapiroStat_W[st] = shapiro.test(r)[1]
            # summaryTable$shapiroStat_P[st] = shapiro.test(r)[2]

            kDensity = density(r)


            png(filename=paste(Path,"Plots/",ModelRefNo, " r density.png", sep = ""),width=1000,height=1000)
            op=par(mfrow=c(2,1))
            plot(density(r))
            hist(r)
            par(op)
            dev.off()



            summaryTable$dwStat[st]=durbinWatsonTest(r,simulate=TRUE)


            # plot(sleepstudy$Reaction~sleepstudy$Days)
            # s=summary(fm1)
            # abline(a = coef(s)$cond[1],b=coef(s)$cond[2])


            #########################################################
            # Random walk metropolis sampling #
            #########################################################


            if (is.null(model$sdr$par.random)) {
              rawcoef = with(model$obj$env,last.par)
            } else {
              rawcoef <- with(model$obj$env,last.par[-random])
            }


            equalTest=all.equal(c(model$obj$fn(rawcoef)),
                                -c(logLik(model)),
                                tolerance=1e-7)

            summaryTable$metropEqual[st]=equalTest


            fn <- function(x) -model$obj$fn(x)
            V <- vcov(model,full=TRUE)

            s1 <- system.time(m1 <- try(MCMCmetrop1R(fn,rawcoef,V=V)))


            colnames(m1) = dimnames(V)[[1]]

            png(filename=paste(Path,"Plots/",ModelRefNo, " metrop T and D %03d.png", sep = ""),width=1000,height=1000)
            op=par(mfrow=c(4,2))
            plot(m1,smooth = TRUE)
            par(op)
            dev.off()



            png(filename=paste(Path,"Plots/",ModelRefNo, " metrop aCorr %03d.png", sep = ""),width=1000,height=1000)
            op=par(mfrow=c(4,2))
            autocorr.plot(m1)
            par(op)
            dev.off()

            rm(m1)
            #################################################################
            # Save Model #
            #################################################################

            nm=paste(formulas$model_index[m],formulas$Model_number[m],"_",fam,"_",link,"_",effort,sep = "")
            save(model,file=paste(Path,"Model objects/",nm, sep = ""))

            rm(ModelRefNo,nm,condVars,ziVars)

            #######################################
            # k-Fold Cross-Validation (k = 5) #
            #######################################

            #########################################
            # Setting up training and test datasets #
            #########################################

            sppColRef=formulas$dataset_name[m]

            set.seed(103)

            sample = sample.split(dataset[,sppColRef], SplitRatio = 1/5)

            test1 = subset(dataset, sample == TRUE)
            a = subset(dataset, sample == FALSE)

            set.seed(103)

            sample = sample.split(a[,sppColRef], SplitRatio = 1/4)

            test2 = subset(a, sample == TRUE)
            b  = subset(a, sample == FALSE)

            set.seed(103)

            sample = sample.split(b[,sppColRef], SplitRatio = 1/3)

            test3 = subset(b, sample == TRUE)
            c  = subset(b, sample == FALSE)

            set.seed(103)

            sample = sample.split(c[,sppColRef], SplitRatio = 1/2)

            test4 = subset(c, sample == TRUE)
            test5  = subset(c, sample == FALSE)

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

            #####################################################################################################
            # Create model and predict #
            #####################################################################################################

            NameFull=formulas$Species[m]
            NameAbbrv=formulas$dataset_name[m]


            if (effortVec[effort]=="KmOffsetZi" || effortVec[effort]=="HrOffsetZi") {
              fcond=paste(NameAbbrv,"~",formulas$fCond[m],sep = "")
            } else {
              fcond=paste(NameAbbrv,"~",formulas$fCond[m], effortVec[effort],sep = "")
            }


            fcond=formula(eval(parse(text = fcond)))

            # fzeroi=paste("~",formulas$fZi[m],"+MfromYrMP",sep = "")
            # fzeroi=formula(eval(parse(text = fzeroi)))
            #


            if (formulas$fZi[m] != "1") {

              fzeroi=paste("~",formulas$fZi[m],sep = "")

              if (effortVec[effort]=="KmOffsetZi") {
                fzeroi=paste(fzeroi,"+offset(Km_s)",sep = "")
              }

              if (effortVec[effort]=="HrOffsetZi") {
                fzeroi=paste(fzeroi,"+offset(Hr_s)",sep = "")
              }






            }




            if (formulas$fZi[m] == "1") {

              i=0
              fzeroi=paste("~",formulas$fZi[m],sep = "")

              if (effortVec[effort]=="KmOffsetZi" && i==0) {
                fzeroi=paste("~offset(Km_s)",sep = "")
                i=i+1
              } else if (effortVec[effort]=="KmOffsetZi" && i!=0) {
                fzeroi=paste(fzeroi,"+offset(Km_s)",sep = "")
                i=i+1
              }

              if (effortVec[effort]=="HrOffsetZi" && i==0) {
                fzeroi=paste("~offset(Hr_s)",sep = "")
              } else if (effortVec[effort]=="HrOffsetZi" && i!=0) {
                fzeroi=paste(fzeroi,"+offset(Hr_s)",sep = "")
                i=i+1
              }



              rm(i)
            }


            fzeroi=formula(eval(parse(text = fzeroi)))


            print(paste("m = ",m,"   family = ",fam,"   link = ",link,"  effort = ",effort))
            print(fcond)
            print(fzeroi)

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


            #########################################
          }



        }

        #################################################################
        # Remove Model #
        #################################################################

        if (exists("model")==TRUE) {
          rm(model)
          rm(NameFull,NameAbbrv)
          rm(summ,f,fz,p,r,fzeroi)
        }


      }


    }



  }


  ###################################################################
            # Exporting summary and coefficient tables #
  ###################################################################

  if (formulas$Last_or_first_model_of_section[m]=="first"){


    ###################################################################
    # Export summary table #
    ###################################################################

    write.csv(summaryTable, file=paste(Path,formulas$model_index[m],effort, " summary table.csv", sep = ""))

    out <- capture.output(summaryTable)
    cat( out, file=paste(Path,formulas$model_index[m],effort, " summary table.txt", sep = ""), sep="\n", append=TRUE)

    rm(out)

    ###################################################################
    # Export coefficients table ~ Conditional #
    ###################################################################

    coeffCond_AllModels=data.frame(EstimateC,Std..ErrorC,z.valueC,Pr...z..C,variablesC,modelRefC)
    coeffCond_AllModels=format(coeffCond_AllModels,digits=3)

    write.csv(coeffCond_AllModels, file=paste(Path,formulas$model_index[m],effort, " coefficients table ~ conditional.csv", sep = ""))

    out <- capture.output(coeffCond_AllModels)
    cat( out, file=paste(Path,formulas$model_index[m],effort, " coefficients table ~ conditional.txt", sep = ""), sep="\n", append=TRUE)
    rm(out)

    ###################################################################
    # Export coefficients table ~ Zero-inflated #
    ###################################################################

    coeffZi_AllModels=data.frame(EstimateZ,Std..ErrorZ,z.valueZ,Pr...z..Z,variablesZ,modelRefZ)
    coeffZi_AllModels=format(coeffZi_AllModels,digits=3)

    write.csv(coeffZi_AllModels, file=paste(Path,formulas$model_index[m],effort, " coefficients table ~ Zero-inflated.csv", sep = ""))

    out <- capture.output(coeffZi_AllModels)
    cat( out, file=paste(Path,formulas$model_index[m],effort, " coefficients table ~ Zero-inflated.txt", sep = ""), sep="\n", append=TRUE)
    rm(out)

    ###################################################################
    # Remove tables #
    ###################################################################

    rm(summaryTable,coeffCond,coeffZi,coeffCond_AllModels,coeffZi_AllModels)
    print(paste("model of section = ",formulas$Last_or_first_model_of_section[m], ",  m = ",m,",  current species is   " ,formulas$Species[m],
                ",    next species is   ",formulas$Species[m+1],",    REMOVING TABLES OF ",formulas$Species[m]," SECTION",sep = ""))
  } # original was =="last"
  rm(Path,ModelRefNo,ModelRefFull,modelRefEnd,EstimateC,EstimateZ,modelRefC,modelRefZ,
     Pr...z..C,Pr...z..Z,Std..ErrorC,Std..ErrorZ,variablesC,variablesZ,z.valueC,z.valueZ,l)

}
# rm(m,st,effort,effortVec,fam,fams,link,linkTypes)



##
###############################################################################################################
                                                   # End #
###############################################################################################################
