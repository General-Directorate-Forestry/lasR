.onAttach <- function(lib, pkg)
	cat( paste( "lasR version", packageDescription("lasR")$Version," - WARNING BETA VERSION"), paste("Created",packageDescription("lasR")$Date),"Type lasRNews() to see new features/changes/bug fixes.", sep="\n")
