#' Show the NEWS file.
#' 
#' Show the NEWS file of the lasR package.     
#' 
#' @return None
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' lasRNews()

lasRNews <- function () 
{
	newsfile <- file.path(system.file(package = "lasR"),"NEWS")
	file.show(newsfile)
}
