# Part II: Case-Study einer klassischen K-Means Cluster Analyse

# X. Herangehensweise

    # 1. Packages laden
    # 2. Datensatz laden
    # 3. EDA (Deskriptivstatistiken, Plot-Matrizen)
    # 4. data Pre-Processing (Standardisierung, ggf. Outlier-Bereinigung, Imputation)
    # 5. Cluster-Anzahl bestimmen (Silhouette Plot)
    # 6. Cluster-Analyse (Deskriptivstatistiken, Mittelwertunterschiede)
    # 7. Cluster-Validierung (Interne- / Externe Validierung)

# 1. Loading required packages
    
    library("DataExplorer")
    library("autoEDA")
    library("cluster")
    library("factoextra")
    library("clValid")
    library("gclus")
    library("fpc")
    source("https://raw.githubusercontent.com/lukereding/random_scripts/master/eda.R")
    library("foreign")
    library("reshape")
    library("purrr")


# 2. Loading the required Dataset

    
    input <- read.spss("C:/Users/Python/Downloads/18121040.sav", to.data.frame=TRUE, max.value.labels = 20)
    
    beautify <- function(input_data,v1,v2) {
      
      library("stringr")
      
      start <- match(v1, names(input_data))
      end <- match(v2, names(input_data))
      
      attributes<- attributes(input_data)[[4]]
      attributes<- attributes[start:end]
      attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()
      
      
      input_data<-input_data[,start:end] %>% lapply(as.numeric) %>% as.data.frame()
      names(input_data)<-attributes
      
      return(input_data)
      
    }
    
    df<-beautify(input, v1 = "S2Q1r1", v2 ="S2Q1r38") 
    
    df_short<-df 
    names(df_short)<-c(1:38)
    
    library("psych")
    library("GPArotation")
    library("corrplot")
    library("nFactors")
    
    describe(df_short)
    corrplot(cor(df_short))
    lowerCor(df_short)
    round(corr.test(df_short, use = "pairwise.complete.obs")$p,2)
    round(corr.test(df_short, use = "pairwise.complete.obs")$ci,2)
    alpha(df_short)
    splitHalf(df_short)
    
    index <- seq(1,nrow(df_short))
    
    index_EFA <- sample(index, floor((0.5*nrow(df_short))))
    index_CFA <- index[!(index %in% index_EFA)]
    
    df_EFA <- df_short[index_EFA,]
    df_CFA <- df_short[index_CFA,]
    
    group_var <-vector("numeric", nrow(df_short))
    group_var[index_EFA] <- 1
    group_var[index_CFA] <- 2
    
    df_short_split <- cbind(df_short, group_var)
    
    describeBy(df_short_split, group = group_var)
    statsBy(df_short_split, group = "group_var")
    
    options(fit.indices = c("CFI", "GFI", "RMSEA", "BIC"))
    
    ev <- eigen(cor(df)) 
    ap <- parallel(subject=nrow(df),var=ncol(df), rep=100, cent=.05)
    nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
    
    plotnScree(nS)
    
    fa1<-factanal(x = df, factors =4)
    print(fa1, digits=2, cutoff=.5, sort=TRUE)
    
    parallel <- fa.parallel(df, fm = 'minres', fa = 'fa')
    factors <- fa(df,nfactors = 15,rotate = "oblimin",fm="minres")
    print(factors$loadings,cutoff = 0.3)
    fa.diagram(factors)
    factors
    
    factanal(x = na.omit(auto_copy), factors = 3, data = na.ona.omit,     rotation = "varimax")
    
# 3. EDA [Exploratory Data Analysis] (ggf. noch Bar Chart mit Top2 / ggf. sortieren nach Mean bzw. Faktor)
    
    strong_eda <- function(input_data, plot = T, details = F) {
      
      library("ggplot2")
      library("plotly")
      library("ggthemr")
      library("stringr")
      library("formattable")
      library("dplyr")
      
      var_width = 40
      
      input_data_percent <- input_data %>% gather("variable", "level")
      input_data_percent <- lapply(input_data_percent, as.factor) %>% as.data.frame()
      input_data_percent <- input_data_percent  %>% group_by(variable, level) %>% summarise(n=n()) %>% as.data.frame() %>% ungroup() %>% group_by(variable) %>%
        mutate(percent = round(n/sum(n),3)) %>% as.data.frame() %>%
        mutate(variable = str_wrap(variable, width = var_width))
      
      input_data_stats <- input_data %>% gather("variable", "n") %>% as.data.frame()
      input_data_stats$variable <- factor(input_data_stats$variable)
      input_data_stats$n <- as.numeric(input_data_stats$n)
      input_data_stats <- input_data_stats %>% group_by(variable) %>% summarise(mean=mean(n),sd=sd(n)) %>% as.data.frame() %>%
        mutate(mean = paste("Mean:",round(mean,3)),sd = paste("SD:",round(sd,2))) %>%
        mutate(variable = str_wrap(variable, width = var_width))
      
      input_data_stats
      
      if (plot == T) {
        
        ggthemr("flat")
        
        gx<-ggplot(input_data_percent, aes(x=level, y=percent, group=level)) + 
          geom_bar(aes(fill=level),stat = "identity")  + 
          geom_text(aes(label = scales::percent(percent)),size = 1.5, position = position_stack(vjust = 0.5)) + 
          geom_text(
            size    = 3,
            data    = input_data_stats,
            mapping = aes(x = 3, y = 0.35, label = mean, group=mean),
            hjust   = -0.1,
            vjust   = -1
          ) + 
          geom_text(
            size    = 3,
            data    = input_data_stats,
            mapping = aes(x = 1, y = 0.35, label = sd, group=sd),
            hjust   = -0.1,
            vjust   = -1
          ) + 
          theme( strip.text = element_text(size = 5)) + 
          facet_wrap(~variable)
        
        gx
        
      }
      
      if (details == T) {
        
        plot_missing(input_data)
        ggplot(stack(input_data), aes(x = ind, y = values)) + geom_boxplot()
        summary(input_data)
        plot_correlation(input_data, type = 'continuous')
        
      }
      
      output<-list(input_data_stats,input_data_percent,gx)
      
      return(output)
      
    }
    
    eda_attitudes<-strong_eda(input_data=df)

     
# 4. Cluster Option Evaluation
    

    cluster_eval <- function(input_data, cluster, clust_methods = c("hierarchical", "kmeans",  "pam", "sota", "diana","clara","model"), val_methods = c("internal", "stability"), stats = TRUE, plot = TRUE) {
      
      
      
      valid_test <- clValid(input_data2, c(2:(cluster+1)),
                            clMethods = clust_methods,
                            validation = val_methods
      ) 
      output<-valid_test@measures %>% round(2)
      
      dimension<-dim(output)[3]
      method<-attributes(output)$dimnames[[3]]
      
      frame<-vector(mode = "list", dimension)
      
      for (i in 1:dimension) {
        
        temp<-output[1:7,1:cluster,i] %>% as.data.frame() %>% mutate(method = method[[i]], approach = rownames(output))
        names(temp) <- c(as.character(2:(cluster+1)), "method","approach")
        frame[[i]] <- assign(method[[i]], temp)
        
      }
      
      data <- Reduce(function(...) merge(..., all=T), frame) %>% arrange(method)
      
      data <- data %>% gather("cluster","value",-c("method","approach"))
      data$cluster <- factor(data$cluster,ordered = T, levels = c(2:(cluster+1)))
 
      
      if (plot==TRUE) {
        
        
        ggthemr("flat")
        g<-ggplot(data, aes(x=cluster, y=value, color = method, group = method)) + geom_point() +geom_line(stat = "identity") + facet_wrap(~approach, scales = "free")
        
        g<-ggplotly(g,tooltip = c("value","method" ,"cluster")) 
        
        print(g)


        
      } else {}

      
      if (stats == TRUE) {
      
      data_analysis <- data %>% spread("approach","value",-c(1,3:4))
      str(data_analysis)
      summary(data_analysis)
      
      formattable(data_analysis, list(`AD` = color_bar("#FA614B"),
                                      `ADM` = color_bar("#FA614B"),
                                      `APN` = color_bar("#FA614B"),
                                      `Connectivity` = color_bar("#FA614B"),
                                      `Dunn` = color_bar("#FA614B"),
                                      `FOM` = color_bar("#FA614B"),
                                      `Silhouette` = color_bar("#FA614B")
      ) 
      )
      
      } else {}
      

    }
    
    cluster_eval(input_data, var_start = ,var_end = , cluster = 6, plot = TRUE)
    


# 5. Cluster Modeling

    n_cluster <- 3
    model <- kmeans(df_short, n_cluster)
    data_clustered <- cbind(df_short,cluster = factor(model$cluster))

    
# 6. Cluster Interpretation
    

# 7. Cluster Validation
    
    library(tidyverse)
    library(caret)
    
    set.seed(123)
    
    train_index <- data_clustered$cluster %>%
      createDataPartition(p = 0.8, list = FALSE)
    train_data <- data_clustered[train_index, ]
    test_data <- data_clustered[-train_index, ]
    
    train_scaled <- train_data %>% 
      preProcess()

    train_transformed <- train_scaled %>% predict(train_data)
    test_transformed <- train_scaled %>% predict(test_data)
    

    
    # Fit the model
    
    library(MASS)
    library(mda)
    library(klaR)
    
    model_types <- c("lda","mda")
    models <- vector(mode = "list", length(model_types))
    predictions <- vector(mode = "list", length(model_types))
    matrices <- vector(mode = "list", length(model_types))
    overall <- vector(mode = "list", length(model_types))
    specific <- vector(mode = "list", length(model_types))
    confusions <- vector(mode = "list", length(model_types))
    
    
    for (i in 1:length(models)) {
      
      func <- get(model_types[i])
      
      model_temp <- func(cluster~., data = train_transformed)
      
      models[[i]] <- assign(paste(model_types[1],"_model", sep=""),model_temp)
      
      predictions[[i]] <- models[[i]] %>% predict(test_transformed)
      
      matrix_temp <- predictions[i][[1]][[1]]
      if (i == 2) matrix_temp <- predictions[[2]]
      matrices[[i]] <- confusionMatrix(matrix_temp, test_data$cluster)
      
      overall_tmp <- as.data.frame(matrices[[i]][[3]])
      overall[[i]] <- overall_tmp %>% mutate(kpi = rownames(overall_tmp)) 
      
      specific_tmp <- as.data.frame(matrices[[i]][[4]])
      specific[[i]] <- specific_tmp %>% mutate(method = model_types[i], cluster =rownames(specific_tmp))
      
      confusions[[i]] <- matrices[[i]][[2]]

    }
    attributes(confusions[[1]])$class <- "matrix"
    library(highcharter)
    hchart(confusions[[1]], scale="column", col = cm.colors(256))    
    library(data.table)
    a<-melt(specific) %>% dplyr::select(method,cluster,variable,value)
    
    ggplot(a, aes(x=factor(method), y = value)) + geom_bar(aes(fill = cluster), position = "dodge", stat = "identity") + facet_wrap(~variable, scales = "free") + 
      geom_text(size=3,position = position_dodge(0.9), aes(x = factor(method), y = value*0.5, label = round(value,2), group = cluster))
    

    library(reshape2)
    overall <- melt(overall) %>% dplyr::select(kpi,value,L1) %>% spread("L1",value = "value")
    colnames(overall) <- c("kpi", model_types)
    overall_plot <- overall %>% gather("model","value",-kpi)
    
    ggplot(overall_plot, aes(x=model, y = value)) + geom_bar(aes(fill = model), stat = "identity") + facet_wrap(~kpi, scales = "free") + 
      geom_text(aes(label = scales::percent(value)),size = 4, position = position_stack(vjust = 0.5))

    dd <- data_clustered[,-39]
    autoplot(prcomp(dd), data = data_clustered, colour = 'cluster',
             loadings = TRUE, loadings.colour = 'blue',
             loadings.label = TRUE, loadings.label.size = 3)
    
    
    
    

    
