## Cluster Pre-Processing

pre_process <- function () {
  
}





## DBSCAN

dbscan_pipeline <- function (data,eps_score = 0.4 , min_points = 5, scale_data = TRUE  ) {
  
  library("fpc")
  library("factoextra")
  
  results <- dbscan(data, eps = eps_score, MinPts = min_points, scale = scale_data, method = c("hybrid", "raw", "dist"))
  
  "cluster_plot <- fviz_cluster(algorithm, data, geom = point)"
  
  return(results)
  
}


## K-Means

kmeans_pipeline <- function (data, distance = "euclidean", assessment = TRUE) {
  
  library(tidyverse)  # data manipulation
  library(cluster)    # clustering algorithms
  library(factoextra) # clustering visualization
  
  distance <- get_dist()
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  
  if (assessment == TRUE) {
    
    fviz_nbclust(data, kmeans, method = "wss")
    fviz_nbclust(data, kmeans, method = "silhouette")
    fviz_gap_stat(gap_stat)
    
  } else {}
  
  set.seed(123)
  results <- kmeans(data = data, centers = 2, nstart = 25)
  
  data_labeled <- data %>%
    as_tibble() %>%
    mutate(cluster = results$cluster)
  
  descriptives <- data_labeled %>%
    group_by(cluster) %>%
    summarise_all("mean")
  
  fviz_cluster(results, data = data)
  
  return(data_labeled)
  return(descriptives)
  
}


## Hierarchical (Agglomerative)

hierarichal_pipeline <- function() {
  
  # methods to assess
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average", "single", "complete", "ward")
  
  # function to compute coefficient
  ac <- function(x) {
    agnes(df, method = x)$ac
  }
  
  map_dbl(m, ac)
  
  if (assessment == TRUE) {
    
    fviz_nbclust(df, FUN = hcut, method = "wss")
    fviz_nbclust(df, FUN = hcut, method = "silhouette")
    gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
    fviz_gap_stat(gap_stat)
    
  } else {}
  
  if (method == "agglomerative") {
    
    diana
    
  } ifelse (method == "divisive") {
    
  } else {}
  
  fviz_cluster(list(data = df, cluster = sub_grp))
  
}


## Partitioning Around Medoids (PAM) / K-Medoids

pam_pipeline <- function() {
  
}



## Cluster Summary Statistics

summary_statistics <- function () {
  
}