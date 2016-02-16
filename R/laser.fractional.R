#' LiDAR models of fractional cover
#' 
#' Compute the four lidar-based models of canopy fractional cover proposed by Hopkinson and Chasmer (2009)
#' 
#' @param z laser echo heights
#' @param i laser echo intensity
#' @param r laser return number
#' @param n laser number of returns
#' @param c laser classification 2 = ground
#' @param id id separate metrics will be computed for each id
#' @param gtv ground threshold value default 1.3  
#' @return data.frame with FR, RR, IR, BL according to the notation in the reference
#' @references Hopkinson, C., & Chasmer, L. (2009). Testing LiDAR models of fractional cover across multiple forest ecozones. Remote Sensing of Environment, 113(1), 275–288.
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' data(las2)
#' laser.fractional(las2$z,las2$i,las2$r,las2$n,las2$c,las2$ID)
laser.fractional <- function(z,i,r,n,cl,id,gtv=1.3){
  df <- aggregate(list(it=i,
                       ic = ifelse(z>gtv,i,0),
                       gs=ifelse(r==1 & n==1 & cl==2,i,0),
                       gl=ifelse(r==n & r!=1 & cl==2,i,0),
                       fs=ifelse(r==1,i,0),
                       il=ifelse(r!=1,i,0),
                       zc1 =ifelse(z>gtv & r ==1,1,0),
                       zt1=ifelse(r==1,1,0),
                       zc =ifelse(z>gtv,1,0),
                       zt=rep(1,length(z))),
                  by=list(ID=id),sum)
  
  df$ID <- row.names(df)
  df$FR <- df$zc1 /df$zt1
  df$RR <- df$zc /df$zt
  df$IR <- df$ic / df$it
  df$BL <- 1 - (((df$gs/df$it) + sqrt(df$gl/df$it)) /  ((df$fs/df$it) + sqrt(df$il/df$it)))
  df <- subset(df,select=c(ID,FR:BL))
  return(df)
}

