
.First.lib <- function(lib, pkg) {
    library.dynam("spatgraphs", pkg, lib)
    v <- read.dcf(file=system.file("DESCRIPTION", package="spatgraphs"),
                  fields="Version")
    cat(paste("Loaded Spatial graph functions (\"spatgraphs\") ", v, "\n"))
}
