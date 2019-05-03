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
#' \dontrun{
#' PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4), SZ = 5)
#'
#' PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4,2,3), SZ = 3)
#'
#' PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4,2,3), SZ = 3, TD = 90, Label ='number')
#'
#' PlotFieldPlan(MakeMainPlan(LETTERS[1:12], 1:3, 1:4,2,3), DefaultCols = c("A" = "red", "B" = "green", "C" = "blue"), SZ = 3)
#'
#' PlotFieldPlan(MakeMainPlan(LETTERS[1:15], 1:3, 1:5,2,3), DefaultCols = c("A" = "red", "B" = "green", "C" = "blue"), SZ = 3, Label ='UID')
#' }
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
#' @param rowPlantSpacing Spacing typically in m between plants in each row. This can be a list of all the spacings if they are complex.
#' @param colPlantSpacing Spacing typically in m between plants in each column. This can be a list of all the spacings if they are complex.
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
#'
#' PlotFieldPlanAdvanced(MakeMainPlan(LETTERS[1:12], 1:3, 1:4), rowPlantSpacing = 1.5, colPlantSpacing = 0.5, colBlockSapcing = 1, colBlockFreq = 2, SZ = 5)
#'
#' PlotFieldPlanAdvanced(MakeMainPlan(LETTERS[1:14], 1:4, 1:4, 2,2), rowPlantSpacing = 1.5, colPlantSpacing = 0.5, colBlockSapcing = 0.5, colBlockSapcing = 0.5, colBlockFreq = 2, rowBlockFreq = 2,  SZ = 5, PlantPos = F)
#'
#' PlotFieldPlanAdvanced(MakeMainPlan(LETTERS[1:12], 1:3, 1:4, 4, 4),rowBlockFreq = 4, colBlockSapcing = 0.5, colBlockFreq = 8, SZ = 5, PlantPos = F)
#'
#' PlotFieldPlanAdvanced(MakeMainPlan(c(LETTERS[1:12],LETTERS[1:4]), 1:4, 1:4), colPlantSpacing = c(2,1,1,2), rowPlantSpacing = c(2,1,1,2), colBlockFreq = 2, SZ = 5, PlantPos = F)
#'
#' PlotFieldPlanAdvanced(MakeMainPlan(c(LETTERS[1:12],LETTERS[1:4]), 1:4, 1:4, 2,2), colPlantSpacing = c(.5,.5,1,1,1,1,.5,.5), rowPlantSpacing = c(.5,.5,1,1,1,1,.5,.5), colBlockFreq = 2, SZ = 5, PlantPos = T)
#'}
#' @export

PlotFieldPlanAdvanced <- function(List, rowPlantSpacing = 0.66, colPlantSpacing = 0.75, rowBlockSapcing = 0.66, colBlockSapcing = 0.75, rowBlockFreq = 15, colBlockFreq = 4, DefaultCols = c("Mb 311" = "#000000"), SZ=1.5, TD=0,FlipR=FALSE,PlantPos=TRUE) {

   X <- List
   X$geno <- as.character(X$geno)
   X$row <- as.numeric(as.character(X$row))
   X$col <- as.numeric(as.character(X$col))
   X$geno[X$geno == "X"] <- NA
   Cols <- ColMaker(X$geno, DefaultCols)
   require(ggplot2)


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


   P <- ggplot(X,aes(Xpos,Ypos, fill = geno)) + theme_bw()

   if(length(rowPlantSpacing)>1 | length(colPlantSpacing)>1) {
      if(length(rowPlantSpacing)==1) {rowPlantSpacing <- rep(rowPlantSpacing,max(X$row))}
      if(length(colPlantSpacing)==1) {colPlantSpacing <- rep(colPlantSpacing,max(X$col))}

      RowLines <- c(RowSpacings-(rowPlantSpacing/2),RowSpacings+(rowPlantSpacing/2))
      ColLines <- c(ColSpacings-(colPlantSpacing/2),ColSpacings+(colPlantSpacing/2))

      P <- P + geom_text(aes(label = geno), show.legend = F, size=SZ, angle=TD) +
         ylab("") + xlab("") + geom_hline(yintercept = RowLines) +
         geom_vline(xintercept = ColLines) +
         theme(axis.text.x = element_text(angle=90, hjust=1)) +
         theme(panel.grid.major.x = element_blank()) + geom_hline(yintercept = 0) +
         theme(panel.grid.major.y = element_blank()) + geom_vline(xintercept = 0)

      for(i in 1:nrow(X)) {
         P <- P + annotate("rect", xmin = ColSpacings[X$col[i]]-(colPlantSpacing[X$col[i]]/2),
                           xmax = ColSpacings[X$col[i]]+(colPlantSpacing[X$col[i]]/2),
                  ymin = RowSpacings[X$row[i]]-(rowPlantSpacing[X$row[i]]/2),
                  ymax = RowSpacings[X$row[i]]+(rowPlantSpacing[X$row[i]]/2),
                 alpha = .5, fill = Cols[X$geno[i]])
      }

   } else {
      P <- P + geom_tile(show.legend = F) +
         geom_text(aes(label = geno), show.legend = F, size=SZ, angle=TD) +
         scale_fill_manual(values = Cols) + ylab("") + xlab("") +
         theme(axis.text.x = element_text(angle=90, hjust=1)) +
         theme(panel.grid.major.x = element_blank()) + geom_hline(yintercept = 0) +
         theme(panel.grid.major.y = element_blank()) + geom_vline(xintercept = 0)
   }



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

