#' Canopy Relief ratio (CRR)
#' 
#' Compute canopy relief ratio
#' 
#' @param z laser echo heights
#' @return data.frame with FR, RR, IR, BL according to the notation in the reference
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' data(las2)
#' laser.crr (las2$z,las2$ID)
laser.crr <- function(z,id){
  df <- aggregate(list(z=z),list(id=id),function(x) c(Havg=mean(x),Hmin=min(x),Hmax=max(x)))
  df$ID <- row.names(df)
  df$CRR <- (df$z[,"Havg"] - df$z[,"Hmin"])/(df$z[,"Hmax"]-df$z[,"Hmin"])
  df <- subset(df,select=c(ID:CRR))
  return(df)
  }