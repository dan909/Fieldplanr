

PlotFieldPlan <- function(List, DefaultCols = c("Mb 311" = "#000000"), SZ=1.5, TD=0, Label = "geno") {
   X <- List
   X$geno <- as.character(X$geno)
   X$row <- as.numeric(as.character(X$row))
   X$col <- as.numeric(as.character(X$col))
   X$geno[X$geno %in% c("X","NA"," ")] <- NA

   Label <- casefold(Label)
   if(!Label %in% c("geno", "number", "no", "loc", 'plant')) {stop("Not a valid Label")}

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
           'plant' = P <- P + geom_point(size=SZ)
   )


   P <- P + scale_y_reverse(breaks = X$row) + scale_x_continuous(position = "top", breaks = X$col)

   print(P)
}







PlotFieldPlanAdvanced <- function(List, rowPlantSpacing = 0.66, colPlantSpacing = 0.75, rowBlockSapcing = 0.66, colBlockSapcing = 0.75, rowBlockFreq = 15, colBlockFreq = 4, DefaultCols = c("Mb 311" = "#000000"), SZ=1.5, TD=0) {

   X <- List
   X$geno <- as.character(X$geno)
   X$row <- as.numeric(as.character(X$row))
   X$col <- as.numeric(as.character(X$col))
   X$geno[X$geno == "X"] <- NA

   library(ggplot2)

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


   ggplot(X,aes(Xpos,Ypos, fill = geno)) + theme_bw() +
      geom_tile(show.legend = F) + geom_text(aes(label = geno), show.legend = F, size=SZ, angle=TD) +
      scale_fill_manual(values = ColMaker(X$geno, DefaultCols)) +
      scale_y_reverse(breaks = RowSpacings) + ylab("") + xlab("") +
      scale_x_continuous(position = "top", breaks = ColSpacings) +
      theme(axis.text.x = element_text(angle=90, hjust=1)) +
      theme(panel.grid.major.x = element_blank()) + geom_hline(yintercept = 0) +
      geom_hline(yintercept = max(RowSpacings)+rowPlantSpacing) +
      theme(panel.grid.major.y = element_blank()) + geom_vline(xintercept = 0) +
      geom_vline(xintercept = max(ColSpacings)+colPlantSpacing)

}

