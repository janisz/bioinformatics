find_N_least_corelated_features <- function(data, N) {
    corelation <- abs(cor(data))
    ind <- which( upper.tri(corelation,diag=F) , arr.ind = TRUE )
    corelation_vector <- data.frame(
            col = dimnames(corelation)[[2]][ind[,2]] ,
            row = dimnames(corelation)[[1]][ind[,1]] ,
            val = corelation[ ind ],
            stringsAsFactors=FALSE
    )
    corelation_sorted <- corelation_vector[ order(corelation_vector["val"]), ]
    return(head(corelation_sorted, n=N))
}

cluster <- function(data, centers) {
  scaled_data <- scale(data)
  least_corelated_features <- find_N_least_corelated_features(scaled_data, centers)
  features <- unique(c(least_corelated_features$col, least_corelated_features$row))
  least_corelated_data <- subset(scaled_data, select = features)
  pca <- prcomp(least_corelated_data)
  return(head(kmeans(pca$x, centers)))
}

cluster(mtcars, 3)
