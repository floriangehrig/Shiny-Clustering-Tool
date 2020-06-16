library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(plotly)
library(reshape)
library(tidyr)
library(wakefield)

## Load Algorithms -------------------------------------------------
source("Clustering Functions.R")

## Random Data Creation --------------------------------------------------------

df <- data.frame(id=c(1:1000))

vars<-12

for (i in 1:vars) {
  
  nam <- data.frame(likert_7(1000))
  names(nam) <- paste("Benefit", i)
  
  df <- cbind(df, nam)
}

## Data Preprocessing ---------------------------------------------------

df <- df[-1]
df_processed <- df %>% lapply(as.numeric) %>% data.frame() %>% scale()

kmeans_pipeline(data=df)
