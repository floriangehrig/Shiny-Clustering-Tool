#' Load useful packages
library(cluster)
library(ggplot2)
library(readr)
library(Rtsne)
library(plotly)
library(highcharter)
library(dplyr)
library(tidyr)
library(purrr)

outlierreplacement <- function(dataframe){
  dataframe %>%          
    map_if(is.numeric, ~ replace(.x, .x %in% boxplot.stats(.x)$out, NA)) %>%
    bind_cols %>%
    
}



#' Load data
df <- read_csv2(file="bank.csv")

#Pre-processing
character_vars <- lapply(df, class) == "character"
df[, character_vars] <- lapply(df[, character_vars], as.factor)
df <- df %>% select(-y) %>% select(c(1,6)) %>% na.omit()
df<-df %>% outlierreplacement() %>% data.frame() %>% na.omit()

#Modeling


input <- df %>% scale() %>% data.frame()
output <- vector(mode="list")
model <- vector(mode="list")
measure <- c()
k<-c()

for (i in 1:10) {
  
  model[[i]] <- kmeans(input, centers = i)
  names(model)[i]<- paste("k = ",i)
  
  measure[i] <- model[[i]][[5]]
  k[i] <- i 
  
  output[[i]] <- df %>% mutate(cluster = model[[i]][[1]]) %>% mutate(k = i)
  names(output)[i]<- paste("k = ",i)
  
}


yy<-do.call(rbind.data.frame, output)
character_vars <- lapply(yy, class) == "integer"
yy[, character_vars] <- lapply(yy[, character_vars], as.factor)


# Data Visualization
ggplotly(ggplot(data.frame(k,measure), aes(x=k,y=measure)) + geom_line() +scale_x_continuous(breaks=unique(data$k))
ggplot(yy, aes(x=age, y=balance, color = cluster, alpha=0.1)) + geom_point() + facet_wrap(~k)

summary(yy)