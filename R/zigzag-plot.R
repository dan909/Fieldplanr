loadNamespace("ggplot2")

#' Makes a basic diagram from the output of \code{\link{MakeMainPlan}}.
#'
#' It is often needed to make minor changes (for dead plants etc.) to the output of \code{\link{MakeMainPlan}} using \code{PlotFieldPlan} is a good way to do that.
#'
#' @param List Is the dater frame output from \code{\link{MakeMainPlan}} or something with the same hasders.
#' @param DefaultCols Are a list of key value pair sets for any pre-defined colours to use.
#' @param SZ Is the text size in the graph this defaults to 1.5 but can be adjusted for large and small field plans.
#' @param TD This allows the text in the field plan to be rotated by n degrees (default = 0).
#' @param Label The type of labelling to do on the plan. This can be name/genotype ('geno'), plot number ('number'), location eg. 2-3 ('loc'), just a dot ('plant'), or the UID/number of the plant ('UID').
#' @param FlipR This flips the rows (default = FALSE).
#'
#' @return A \code{ggplot2} heatmap style graph that can be saved with  \code{ggave}.
#'
#' @examples
#' \dontrun{PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4), SZ = 5)}
#' \dontrun{PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4,2,3), SZ = 3)}
#' \dontrun{PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4,2,3), SZ = 3, TD = 90, Label ='number')}
#' \dontrun{PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4,2,3), DefaultCols = c("A" = "red", "B" = "green", "C" = "blue"), SZ = 3)}
#' \dontrun{PlotFieldPlan(MakeMainPlan(LETTERS[1:14], 1:3, 1:5,2,3), DefaultCols = c("A" = "red", "B" = "green", "C" = "blue"), SZ = 3, Label ='UID')}
#'
#' @export

PlotFieldPlan <- function(List, DefaultCols = c("Mb 311" = "#000000"), SZ=1.5, TD=0, Label = "geno",FlipR=FALSE) {
   X <- List
   X$geno <- as.character(X$geno)
   X$row <- as.numeric(as.character(X$row))
   X$col <- as.numeric(as.character(X$col))
   X$geno[X$geno %in% c("X","NA"," ")] <- NA

   Label <- casefold(Label)
   if(!Label %in% c("geno", "number", "no", "loc", 'plant', 'uid')) {stop("Not a valid Label")}

   require(ggplot2)

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

   P <- ggplot(X,aes(col,row, fill = geno)) + theme_bw() +
      geom_tile(show.legend = F) + scale_fill_manual(values = ColMaker(X$geno, DefaultCols))

   switch (Label,
           'geno' = P <- P + geom_text(aes(label = geno), show.legend = F, size=SZ, angle=TD),
           'loc'   = P <- P + geom_text(aes(label = paste(row,col,sep='-')), show.legend = F, size=SZ, angle=TD),
           'no' = P <- P + geom_text(aes(label = Plot), show.legend = F, size=SZ, angle=TD),
           'number' = P <- P + geom_text(aes(label = Plot), show.legend = F, size=SZ, angle=TD),
           'plant' = P <- P + geom_point(size=SZ),
           'uid' = P <- P + geom_text(aes(label = UID), show.legend = F, size=SZ, angle=TD)
   )

   if(FlipR) {
      P <- P + scale_y_continuous(breaks = X$row, expand = c(0, 0)) + scale_x_continuous(position = "top", breaks = X$col, expand = c(0, 0))
   } else {
      P <- P + scale_y_reverse(breaks = X$row, expand = c(0, 0)) + scale_x_continuous(position = "top", breaks = X$col, expand = c(0, 0))
   }


   print(P)
}





#' Makes a more advanced diagram from the output of \code{\link{MakeMainPlan}} that can show path spacing.
#'
#' It is often needed to make minor changes (for dead plants etc.) to the output of \code{\link{MakeMainPlan}} using \code{PlotFieldPlan} is a good way to do that.
#'
#' @param List Is the dater frame output from \code{\link{MakeMainPlan}} or something with the same handers.
#' @param rowPlantSpacing Spacing typically in m between plants in each row.
#' @param colPlantSpacing Spacing typically in m between plants in each column.
#' @param rowBlockSapcing If there are blocks this is the spacing between there rows. This can be a list if it is the same length as the number of expected row paths.
#' @param colBlockSapcing If there are blocks this is the spacing between there columns. This can be a list if it is the same length as the number of expected column paths.
#' @param rowBlockFreq There is a new block every n rows of plants.
#' @param colBlockFreq There is a new block every n columns of plants.
#' @param DefaultCols Are a list of key value pair sets for any pre-defined colures to use.
#' @param SZ This is the text size in the graph this defaults to 1.5 but can be adjusted for large and small field plans.
#' @param TD This allows the text in the field plan to be rotated by n degrees (default = 0).
#' @param FlipR This flips the rows (default = FALSE).
#' @param PlantPos This sets if to label the plant or the plot in the tick marks.
#'
#' @return A  \code{ggplot2} heatmap style graph that can be saved with \code{ggave}. This shows the spacing of all of the plants.
#'
#' @examples
#' \dontrun{
#' PlotFieldPlanAdvanced(MakeMainPlan(LETTERS[1:12], 1:3, 1:4), SZ = 5)
#' #' PlotFieldPlanAdvanced(MakeMainPlan(LETTERS[1:12], 1:3, 1:4), rowPlantSpacing = 1.5, colPlantSpacing = 0.5, colBlockSapcing = 1, colBlockFreq = 2, SZ = 5)
#' PlotFieldPlanAdvanced(MakeMainPlan(LETTERS[1:12], 1:3, 1:4),colBlockFreq = 2, SZ = 5, PlantPos = F)
#'}
#' @export

PlotFieldPlanAdvanced <- function(List, rowPlantSpacing = 0.66, colPlantSpacing = 0.75, rowBlockSapcing = 0.66, colBlockSapcing = 0.75, rowBlockFreq = 15, colBlockFreq = 4, DefaultCols = c("Mb 311" = "#000000"), SZ=1.5, TD=0,FlipR=FALSE,PlantPos=TRUE) {

   X <- List
   X$geno <- as.character(X$geno)
   X$row <- as.numeric(as.character(X$row))
   X$col <- as.numeric(as.character(X$col))
   X$geno[X$geno == "X"] <- NA

   require(ggplot2)

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


   MakeSpace <- function(pos, BlockFreq, BlockSapcing, PlantSpacing) {
      Space <- pos*PlantSpacing
      if(length(BlockSapcing)==1) {
         if(pos > BlockFreq) {Space <- Space + (floor((pos-1)/BlockFreq)*BlockSapcing)}
      } else {
         if(pos > BlockFreq) {
            BlockSapce <- BlockSapcing[1:floor((pos-1)/BlockFreq)]
            Space <- Space + sum(BlockSapce)
         }
      }
      Space <- Space-(PlantSpacing/2)
      return(Space)
   }



   # make real spacings
   RowSpacings <- c()
   ColSpacings <- c()
   X$Xpos <- 0
   X$Ypos <- 0
   for (plant in 1:nrow(X)) {

      ColSpace <- MakeSpace(X$col[plant], colBlockFreq, colBlockSapcing, colPlantSpacing)
      X$Xpos[plant] <- ColSpace
      ColSpacings <- c(ColSpacings, ColSpace)

      RowSpace <- MakeSpace(X$row[plant], rowBlockFreq, rowBlockSapcing, rowPlantSpacing)
      X$Ypos[plant] <- RowSpace
      RowSpacings <- c(RowSpacings, RowSpace)
   }
   RowSpacings <- unique(RowSpacings)
   ColSpacings <- unique(ColSpacings)


   P <- ggplot(X,aes(Xpos,Ypos, fill = geno)) + theme_bw() +
      geom_tile(show.legend = F) + geom_text(aes(label = geno), show.legend = F, size=SZ, angle=TD) +
      scale_fill_manual(values = ColMaker(X$geno, DefaultCols)) + ylab("") + xlab("") +
      theme(axis.text.x = element_text(angle=90, hjust=1)) +
      theme(panel.grid.major.x = element_blank()) + geom_hline(yintercept = 0) +
      theme(panel.grid.major.y = element_blank()) + geom_vline(xintercept = 0)


   if (!PlantPos) {
      RowSpacings <- c(0,RowSpacings+(rowPlantSpacing/2))
      ColSpacings <- c(0,ColSpacings+(colPlantSpacing/2))
   }

   P <- P + scale_x_continuous(position = "top", breaks = ColSpacings, expand = c(0, 0))

   if (FlipR) {
      P <- P + scale_y_continuous(breaks = RowSpacings, expand = c(0, 0))
   } else {
      P <- P + scale_y_reverse(breaks = RowSpacings, expand = c(0, 0))
   }


   return(P)
}

