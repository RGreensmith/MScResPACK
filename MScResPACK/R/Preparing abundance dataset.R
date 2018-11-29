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

  seasonName=unique(dataset$Season)
  seasonRef = rep(NA, times = length(seasons))
  seasonsInd=data.frame(seasonName,seasonRef)
  
  for (seas in 1:length(seasonsInd$seasonName)) {
    
    if (seasonsInd$seasonName[seas]=="Spring") { 
      
      seasonsInd$seasonRef[seas]=1
      
    } else if (seasonsInd$seasonName[seas]=="Summer") {
      
      seasonsInd$seasonRef[seas]=2
      
    } else if (seasonsInd$seasonName[seas]=="Autumn") {
      
      seasonsInd$seasonRef[seas]=3
      
    } else if (seasonsInd$seasonName[seas]=="Winter") {
      
      seasonsInd$seasonRef[seas]=4
      
    }
  }
  
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
