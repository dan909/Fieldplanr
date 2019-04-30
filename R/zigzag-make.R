
#' Makes a set of plots for only one genotype.
#'
#' These are subplots to the main list or just a basic field setup. \code{MakeSubPlan} is called by \code{\link{MakeMainPlan}}.
#'
#' @param xopp Is the name of the genotype clone etc.
#' @param colse The columns in the field this clone is in eg 1 to 7 (1:7) or 3 to 5 (3:5).
#' @param rowse The rows in the field this clone in in eg 1 to 5 (1:5) or 2 to 6 (2:6).
#' @param zz The zig-zag. This can be rows ('R'), columns ('C'), no zig-zag columns ('NC'), or no zig-zag rows ('NR').
#' @param ori The origin of the plot so zig-zags are correct. This can be top-left ('TL'), bottom-left ('BL'), top-right ('TR'), or bottom-right ('BR'). Not all are available for all \code{zz} options.
#' @return A list of the posishons of the plants \code{xopp} in the \code{colse} and \code{rowse} starting at \code{ori} and zig-zaging according to \code{zz}.
#'
#' @examples
#' MakeSubPlan("A1", 1:3, 1:4)
#'
#'
#' @export

MakeSubPlan <- function(xopp,colse,rowse,zz="R",ori="TL") {

   if(!ori %in% c("TL", "TR", "BL", "BR")) {stop("Not a valid origin")}
   if(!zz %in% c("R", "C", "NR", "NC")) {stop("Not a valid origin")}

   Setup <- paste(zz,ori,sep = "-")
   if(!Setup %in% c("R-TL","R-TR","R-BL", "C-TL","C-BL","C-TR", "NR-TL", "NC-TL")) {stop("Not valid setup yet (in sub level)")}

   if(zz %in% c("C", "NC")) {cols <- rowse} else {cols <- colse}
   if(zz %in% c("C", "NC")) {rows <- colse} else {rows <- rowse}

   if(zz %in% c("C", "R")) {ZZ <- TRUE} else {ZZ <- FALSE}

   Out <- data.frame()

   for(R in if(Setup=="R-BL"){rows <- rev(rowse)}else{rows}) {
      if(R == rows[1]) {
         A <- 1 # set to odd on first row
      } else {
         A <- A+1
      }
      if(A %% 2 == 0) { # is even
         for(C in if(ZZ && (ori=="TL"|ori=="BR")){rev(cols)}else{cols}) {
            Out <- rbind.data.frame(Out, cbind('geno'=xopp, 'row'=R, 'col'=C))
         }
      } else {
         for(C in if(ZZ && (ori=="TR"|ori=="BL")){rev(cols)}else{cols}) {
            Out <- rbind.data.frame(Out, cbind('geno'=xopp, 'row'=R, 'col'=C))
         }
      }
   }
   if(zz %in% c("C", "NC")) {colnames(Out) <- c('geno', 'col', 'row')}
   return(Out)
}



#' Makes a set of plots from a list of genotypes.
#'
#' These can include subplots, in doing so this will call \code{\link{MakeSubPlan}} to make the subplots. Any sub plots can have difering zig-zags and origins dfinde in \code{zz} is called by \code{ori}.
#'
#' @param xopplist is the name of the genotype clone etc.
#' @param rowse the columns in the field n:n eg 1 to 7 (1:7) or 1 to 5 (1:5). This should normaly start at 1.
#' @param colse The rows in the field n:n eg 1 to 5 (1:5) or 1 to 6 (1:6).  This should normaly start at 1.
#' @param nr The number of rows in the sub plots.
#' @param nc The number of columns in the sub plots.
#' @param zigzag The zig-zag. This can be rows ('R'), columns ('C'), no zig-zag columns ('NC'), or no zig-zag rows ('NR').
#' @param origin The origin of the plot so zig-zags are correct. This can be top-left ('TL'), bottom-left ('BL'), top-right ('TR'), or bottom-right ('BR'). Not all are available for all \code{zz} options.
#' @param zz The sup plot zig-zag. This can be rows ('R'), columns ('C'), no zig-zag columns ('NC'), or no zig-zag rows ('NR').
#' @param ori The sup plot origin of the plot so zig-zags are correct. This can be top-left ('TL'), bottom-left ('BL'), top-right ('TR'), or bottom-right ('BR'). Not all are available for all \code{zz} options.
#' @return A list of the posishons of the plants \code{xopp} in the \code{colse} and \code{rowse} starting at \code{ori} and zig-zaging according to \code{zz}.
#' @param FlipR If to flip all the row numbers, use if 1,1 is at the bottom right.
#' @param UID The UID for the first plant.
#'
#' @examples
#' MakeMainPlan(LETTERS[1:12],1:2,1:6,1,1)
#'
#' @export

MakeMainPlan <- function(xopplist,rowse,colse,nr=1,nc=1,zigzag="R",origin="TL",zz="R",ori="TL",FlipR=F,UID=1) {
   Xop <- 0
   Out <- data.frame()

   if(!origin %in% c("TL", "TR", "BL", "BR")) {stop("Not a valid origin")}
   if(!zigzag %in% c("R", "C", "NR", "NC")) {stop("Not a valid origin")}

   Setup <- paste(zigzag,origin,sep = "-")
   if(!Setup %in% c("R-TL","R-BL","R-TR","R-BL","C-TL","C-BL","NR-TL","NR-BL","NC-TL")) {stop("Not valid setup yet")}
   cat(Setup)

   if(zigzag %in% c("C", "NC")) {cols <- rowse} else {cols <- colse}
   if(zigzag %in% c("C", "NC")) {rows <- colse} else {rows <- rowse}

   if(zigzag %in% c("C", "R")) {ZZ <- TRUE} else {ZZ <- FALSE}

   for(R in if(Setup %in% c("R-BR","R-BL","NR-BL")){rows <- rev(rowse)}else{rows}) {
      if(R %% 2 == 0) { # is even
         for(C in if(ZZ && (origin=="TL"|origin=="BR")){rev(cols)}else{cols}) {
            Xop <- Xop +1
            RN <- R*nr
            CN <- C*nc
            #print(sprintf("Row=%i col=%i, geno=%s", R, C, xopplist[Xop]))
            Out <- rbind.data.frame(Out, MakeSubPlan(xopplist[Xop],(CN-nc+1):CN,(RN-nr+1):RN,zz,ori))
         }
      } else {
         for(C in if(ZZ && (origin=="TR"|origin=="BL")){rev(cols)}else{cols}) {
            Xop <- Xop +1
            RN <- R*nr
            CN <- C*nc
            #print(sprintf("Row=%i col=%i, geno=%s", R, C, xopplist[Xop]))
            Out <- rbind.data.frame(Out, MakeSubPlan(xopplist[Xop],(CN-nc+1):CN,(RN-nr+1):RN,zz,ori))
         }
      }
   }

   if(zigzag %in% c("C", "NC")) {colnames(Out) <- c('geno', 'col', 'row')}

   if(FlipR) {
      Out$row <- (max(as.numeric(as.character(Out$row)))-as.numeric(as.character(Out$row)))+1
      Out$col <- as.numeric(as.character(Out$col))
   } else {
      Out$row <- as.numeric(as.character(Out$row))
      Out$col <- as.numeric(as.character(Out$col))
   }

   Out$ROW <- ceiling(Out$row/nr)
   Out$COL <- ceiling(Out$col/nc)
   Out$Plot <- ceiling((1:nrow(Out))/(nr*nc))

   OUT$UID <- UID:(UID+nrow(Out))

   return(Out)
}

