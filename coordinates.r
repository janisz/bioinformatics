if( ! require("bio3d")) {
  install.packages("bio3d", repos="http://cran.rstudio.com/")
}
library(bio3d)

distances <- function(pdb_file) {
  pdb <- read.pdb( pdb_file )
  coordinates <- pdb$atom[c("x","y","z")]
  return(dist(coordinates))
}

head(distances(system.file("examples/1hel.pdb", package="bio3d")))
