#' Makes unique colours for all but the controls.
#'
#' The plans need each plot coloured by genotype
#'
#' @param genos a list of genotypes requiring colours.
#' @param SetControl Any predefined colours to use this can be '#FF0000' or 'red'.
#'
#' @return A named list of colours with the geno as the name.
#'
#' @examples
#' ColMaker(LETTERS[1:12])
#'
#'
ColMaker <- function(genos, SetControl = c("Mb 311" = "#000000")) {

   for(i in unique(genos)) {
      if(!(i %in% names(SetControl))) {
         Col = paste0("#",paste(as.hexmode(round(runif(3,20,240),0)), collapse = ""))
         names(Col) <- i
         SetControl = c(SetControl, Col)
      }
   }
   return(SetControl)
}

#' Makes spaces for the plants.
#'
#' This is only in one dimension.
#'
#' @param pos Is the dater frame output from \code{\link{MakeMainPlan}} or something with the same hasders.
#' @param BlockFreq this is how frequent paths are in the trial.
#' @param BlockSapcing This can be a number or a list of numbers the length of the number of paths.
#' @param PlantSpacing This can be a number or a list of numbers the length of the rows or columns.
#'
#' @return The spacing of the plant requested
#'
#' @examples
#' MakeSpace(3,2,1,0.66)
#'
MakeSpace <- function(pos, BlockFreq, BlockSapcing, PlantSpacing) {
   if(length(PlantSpacing)>1){
      ps <- PlantSpacing[pos]
      Space <- sum(PlantSpacing[1:pos])
   } else {
      ps <- PlantSpacing
      Space <- pos*PlantSpacing
   }


   if(length(BlockSapcing)==1) {
      if(pos > BlockFreq) {Space <- Space + (floor((pos-1)/BlockFreq)*BlockSapcing)}
   } else {
      if(pos > BlockFreq) {
         BlockSapce <- BlockSapcing[1:floor((pos-1)/BlockFreq)]
         Space <- Space + sum(BlockSapce)
      }
   }
   Space <- Space-(ps/2)
   return(Space)
}
