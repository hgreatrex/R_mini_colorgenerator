########################################################################################
# MAKE YOUR OWN COLOR RAMPS, HLG Sept 2021
########################################################################################
#---------------------------------------------------
# package:       currently either "brewer" or "viridis"
# pal:           the sub-palette you want e.g. "Blues"
# n:             how many output colours you want
# reverse :      set to true if you want the colors to go the other way
# removeend:      1 removes the final color, 2 the two final colors etc
# removestart:    1 removes the first color, 2 the two first colors etc
# rgb             set to true to output rgb values
#---------------------------------------------------

palette.function <- function(package,pal,n,reverse=FALSE,removeend=FALSE,removestart=FALSE,rgb=FALSE){
   require(RColorBrewer)
   require(viridis)
   require(paletteer)
   
   if(tolower(package) %in% "brewer"){
     inner <- brewer.pal(n = 9, name = pal) 
   }else if(tolower(package) %in% "viridis"){
     inner <- viridis(9, option = pal)
   }else{
      stop("Choose either 'brewer' or 'viridis'")
   }
   
   if(removeend > 0){inner <-  inner[-((9-(removeend-1)):9)]}
   if(removestart > 0){inner <-  inner[-(1:(1+(removestart-1)))]}
   if(reverse == TRUE){inner <- rev(inner)}
   
   fullpal <- colorRampPalette(inner)(n)
   if(rgb == TRUE){
      fullpal <- t(col2rgb(fullpal))
   }
   
   core <- paste(package,pal,n,sep="_")
   if(removeend   > 0){core <- paste(core,paste("remend",removeend,sep=""),sep="_")}
   if(removestart > 0){core <- paste(core,paste("remstart",removestart,sep=""),sep="_")}
   if(reverse ==TRUE) {core <- paste(core,"reversed",sep="_")}
   if(rgb ==TRUE) {core <- paste(core,"rgb",sep="_")}else{core <- paste(core,"hex",sep="_")}
   
   out <- vector(2,mode="list")
   out[[1]] <- core
   out[[2]] <- fullpal
   return(out)
}
