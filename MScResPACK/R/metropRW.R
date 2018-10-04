#########################################################
# Random walk metropolis sampling #
#########################################################


#' Random walk metropolis sampling wrapper function
#'
#' This function allows you to carry out Random Walk Metroplis Sampling on a model, and creates two plots.
#' @param model model object of glmmTMB class
#' 
#' @param filePath file pathway, must end with "/" or beginning of the file name
#' @keywords cats
#' @export
#' @examples
#' filePath = paste(Path,"Plots/",ModelRefNo, sep = "")
#' metropRW(model,filePath)
#'

metropRW = function(model,filePath) {
  
  if (is.null(model$sdr$par.random)) {
    rawcoef = with(model$obj$env,last.par)
  } else {
    rawcoef <- with(model$obj$env,last.par[-random])
  }
  
  isEqual=all.equal(c(model$obj$fn(rawcoef)),
                      -c(logLik(model)),
                      tolerance=1e-7)
  
  fn <- function(x) -model$obj$fn(x)
  V <- vcov(model,full=TRUE)
  
  timer <- system.time(m1 <- try(MCMCmetrop1R(fn,rawcoef,V=V,verbose = 1)))
  
  
  colnames(m1) = dimnames(V)[[1]]
  
  png(filename=paste(filePath," metrop T and D %03d.png", sep = ""),width=1000,height=1000)
  op=par(mfrow=c(4,2))
  plot(m1,smooth = TRUE)
  par(op)
  dev.off()
  

  png(filename=paste(filePath," metrop aCorr %03d.png", sep = ""),width=1000,height=1000)
  op=par(mfrow=c(4,2))
  autocorr.plot(m1)
  par(op)
  dev.off()
  
  metrop=list(m1,isEqual,timer)
  return(metrop)
  
}
##############################################################
# End #
##############################################################