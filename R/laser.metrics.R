#' Compute the height and density variables from point cloud data
#' 
#' Compute the height and density variables from point cloud data according to Naesset 2004.
#' 
#' @param x surface height
#' @param id unique idenfitier for which variables will be aggregated
#' @param gtv ground threshold value. Default is 2
#' @param ctv canopy threshold value. Default is 95 percentile 
#' @param percentiles the percentiles to be computed. Defaults to seq(0.1,0.9,0.1)
#' @param prefix  optional prefix of varaible names
#' @param suffix optional suffix of varaible names
#' @param distpre indicator of distribution metrics. Default is "H"
#' @param denspre indicator of density metrics. Default is "D"
#' @return data.frame with the numbers and proportion of echoes in differnt categories.
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @references Næsset, E. (2004). Accuracy of forest inventory using airborne laser scanning: Evaluating the first Nordic full-scale operational project. Scandinavian Journal of Forest Research / Issued Bimonthly by the Nordic Forest Research Cooperation Committee , 19, 554–557.
#' @note Date: Nov 2, 2016 
#' @examples 
#' data(las)
#' #First returns
#' las1 <- subset(las,r==1)
#' Vars1 <- laser.metrics(las1$z,las1$ID,gtv=1.3,suffix='.F')
#' head(Vars1)
#'
#' # Last returns uten single! 
#' las2 <- subset(las,r==n & r != 1) 
#' Vars2 <- laser.metrics(las2$z,las2$ID,gtv=1.3,suffix='.L')
#'
#' head(Vars2)

laser.metrics <- function(x,id,gtv=2,ctv = function(x){quantile(x[x >= gtv],probs=0.95,type=2)},percentiles=seq(0.1,0.9,0.1),prefix="",suffix="",distpre="H",denspre="D"){
	dist <- laser.distribution(x,id,percentiles=percentiles,gtv=gtv,prefix=paste(prefix,distpre,sep=""),suffix=paste(suffix,"",sep=""))
	dens <- laser.density(x,id,gtv=gtv, ctv=ctv,prefix=paste(prefix,denspre,sep=""),suffix=paste(suffix,"",sep=""))
	LM <- merge( dist, dens, by = "ID",all = TRUE , sort = FALSE)
	return(LM)
}


