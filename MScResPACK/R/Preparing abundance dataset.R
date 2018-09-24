############################################################
# Preparing abundance dataset #
############################################################

#' Create index of seasons in abundance dataset
#'
#' This function allows you create index of seasons in abundance dataset.
#' @param dataset dataframe used to create seasons index
#'
#' @keywords cats
#' @export
#' @examples
#' seasonsIndFun()
#'

seasonsIndFun = function(dataset) {

  seasons=unique(dataset$Season)
  seasonsInd=data.frame(seasons,c(2,3,4,1))
  names(seasonsInd)=c("seasonName","seasonRef")
  seasonsInd=seasonsInd[order(seasonsInd$seasonRef),]
  seasonsInd$seasonName=as.character(seasonsInd$seasonName)

  return(seasonsInd)

}

#' Add dummy variable to dataset and convert the dummy, month and season variables to factors
#'
#' This function allows you add dummy variable to dataset and convert the dummy, month and season variables to factors.
#' @param dataset dataframe used to create dummy variable to dataset and convert the dummy, month and season variables to factors
#'
#' @keywords cats
#' @export
#' @examples
#' prepDF()
#'
#'
prepDF = function(dataset) {

  dataset$dummy=rep(1, times = length(dataset$Hr))
  dataset$dummy=as.factor(dataset$dummy)
  dataset$Month=as.factor(dataset$Month)
  dataset$Season=as.factor(dataset$Season)

  return(dataset)

}

# dataset=dataset[,-1]
# dataset$Yearbins=as.factor(dataset$Yearbins)

############################################################################
# End #
############################################################################
