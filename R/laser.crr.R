#' Canopy Relief Ratio CRR
#' 
#' Compute canopy relief ratio, a quantitative measure of the relative shape of the canopy.
#' 
#' @param z laser echo heights
#' @return data.frame with FR, RR, IR, BL according to the notation in the reference
#' @references e.g. Mura, M., McRoberts, R. E., Chirici, G., & Marchetti, M. 2015. Estimating and mapping forest structural diversity using airborne laser scanning data. Remote Sensing of Environment, 170, 133–142.
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @name laser.crr
#' @export
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