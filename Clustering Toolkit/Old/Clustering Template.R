#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(plotly)
library(highcharter)
#' Load data
df <- read_csv2(file="bank.csv")

character_vars <- lapply(df, class) == "character"
df[, character_vars] <- lapply(df[, character_vars], as.factor)

df <- df %>% select(-y) %>% select(1:7)

#' Compute Gower distance
gower_dist <- daisy(df, metric = "gower")

gower_mat <- as.matrix(gower_dist)


sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

Sil_plot <- data.frame(x=1:8, y=sil_width) %>%
  mutate(y=round(y,digits = 3))


k <- Sil_plot %>%
  filter(y==max(y,na.rm = TRUE)) %>%
  select(x)%>%
  as.numeric()

pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  lapply(round, digits = 2) %>%
  data.frame() %>%
  mutate(cluster = factor(pam_fit$clustering))

df<-df%>%mutate(cluster = factor(pam_fit$clustering)) %>% data.frame()

library("htmltools")

df <- split(df,df$cluster)
str(df)
list<-list(NA,NA,NA,NA,NA,NA,NA,NA)


cols<-viridisLite::viridis(7)
cols
cols <- substr(cols, 0, 7)


for (i in 1:length(df)) {

list[[i]]<-map(names(df[[i]]), function(x){
    df[[i]][[x]] %>% 
    hchart(showInLegend = FALSE) %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_title(text = x) %>% 
    hc_yAxis(title = list(text = ""))%>%
    hc_colors(cols)
}) %>% 
  hw_grid(rowheight = 225, ncol = 3) %>% browsable()
print(list[[i]])
}

cols<-viridisLite::viridis(7)
cols
cols <- substr(cols, 0, 7)

hchart(tsne_data, "scatter",hcaes(x = "X", y = "Y", group="cluster", fillOpacity = 0.5 ))%>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_colors(cols)

hchart(Sil_plot, "line", hcaes(x = "x", y = "y"))
