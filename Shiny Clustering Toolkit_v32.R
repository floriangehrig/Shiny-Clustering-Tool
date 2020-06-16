
# 1. Loading general packages
    
    # Data Loading    
    library("readxl")
    library("foreign")
    library("xlsx")

    # Data Cleaning / Manipulation
    library("tidyverse")
    library("tidyr")
    library("dplyr")
    library("stringr")
    library("stringi")
    library("reshape")
    library("purrr")
    library("data.table")
    library("reshape2")
    
    # Dimension Reduction
    library("psych")
    library("GPArotation")
    library("corrplot")
    library("nFactors")

    library("corrplot")
    library("factoextra")
    library("htmlwidgets")
    library("htmltools")
    
    # Clustering
    library("clValid")
    library("cluster")
    library("gclus")
    library("fpc")
    library("dbscan")
    
    # Classification
    library("caret")
    library("MASS")
    library("mda")
    library("klaR")
    
    # Visualization
    library("ggfortify")
    library("ggrepel")
    library("plotly")
    library("ggforce")
    library("ggthemr")
    library("highcharter")
    
    # Fonts & Stylings
    library(extrafont)
    loadfonts(device = "win")
    
    # Interactive Toolkits
    library("formattable")
    library("DT")
    library('officer')
    library("rvg")
    library("mschart")
    library("RColorBrewer")

    # Options
    options(max.print=1000000)
    
    
# 2. Loading the required Dataset -----------------------------------------------------------

    input_first <- read.spss("C:/Users/Florian Gehrig/Desktop/18121034.sav", to.data.frame=TRUE) %>% arrange(record)
    str(attributes(input_first))
    summary(input_first$record)
    summary(ids)
    
    ids_cleaned <- read.xlsx("C:/Users/Florian Gehrig/Box/Dean Foods (2018)/Quantitative/03_Analysis/DATASET/Final excl Bad Respondents/IDs to be cleaned/Fluid_DataCleanup_1.27.19_respondents.xlsx",1) %>% filter(remove == 1) %>% select(record) %>% as.vector()
    ids<- ids_cleaned[[1]]
    inputs<-input_first %>% select(record) %>% filter(record > 7080)
    ids
    dim(input_first)
    input_first<-input_first[ !input_first$record %in% ids,]

    glossars<-read_excel("C:/Users/Florian Gehrig/Box/Dean Foods (2018)/Quantitative/03_Analysis/R Syntax/Variable Glossar.xlsx", sheet = "Tabelle1") %>% as.data.frame() 
    definitions<-read_excel("C:/Users/Florian Gehrig/Box/Dean Foods (2018)/Quantitative/03_Analysis/R Syntax/Category Definitions.xlsx", sheet = "Tabelle1") %>% as.data.frame() 
    

    scales<-vector(mode="list", 100)
    
        scales[[1]]<-c("Agree completely","Agree slightly","Agree somewhat","Disagree somewhat","Disagree slightly","Disagree completely") %>% rev()
        scales[[2]]<-c("Definitely will NOT consume", "Probably will NOT consume",   "Might or might not consume",  "Probably WILL consume" ,  "Definitely WILL consume"  )

        
# X.Data Subsetting
        
        #Query Dataframe
        names(input)[grep("S2Q1r.*",names(input))] #Name Query
            
            # Variable Definitions
            demographics<-c("^Q1$","^Q2","^Q4","^Q5","^Q6A","^Q6B","^Q9","^Q11$","^Q13","^Q14","hQ14","^Q15","^Q16","S3Q1A","S3Q2A","S6Q2","S6Q3","S6Q4")
            attitudes<-c("^S2Q1r.*$")
            stores <- c("S3Q3A.*\\d+3$","S3Q3B.*\\d+3$","S3Q3C.*\\d+3$")
            occasions<- c("^S4Q1$","^S4Q2$","^S4Q3","^S4Q4","^S4Q5","^S4Q7", "S4Q8","^S4Q9.*\\d+$","^S4Q17I\\d+$")
            #c("S4Q6","S4Q2", "S4Q4", "S4Q5", "S4Q3", "S4Q1$","^S4Q7", "S4Q8", "hS4Q9","^S4Q9.*\\d+$", "S4Q11.*c\\d+$","S4Q11X1r\\d{1,3}c2$","S4Q12.*\\d{1,3}$","S4Q13A.*\\d{1,3}$","S4Q13B.*\\d{1,3}$","S4Q14A.*\\d{1,3}$","S4Q14B.*\\d{1,3}$","S4Q15.*\\d{1,3}$","S4Q16","^S4Q17I\\d+$")
            consumption <- c("^S3Q1C.*\\d{1,3}$","^S3Q1D.*\\d+$","^S3Q1E.*\\d+$","^S3Q2B.*\\d{1,3}$","^S3Q2C.*\\d+$","^S3Q2D.*\\d+$","^S3Q2Er\\d+$")
            ratings <- c("^S5Q1.*\\d+$")
            "S6Q5.*\\d+$"
        
        # Subsetting Function
        subselect <- function(input_data, variable, relabel = T) {
          names<-names(input_data)
          names_temp <- c(names[grep(variable,names)])
          attributes<- attributes(input_data)[[1]]
          attributes<- attributes[names(attributes) %in% names_temp]
          attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()
          
          
          input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame()
          
          if(relabel == TRUE) {
            
            names(input_data)<-attributes
            
            
          }
          
          
          return(input_data)
          
        } # without grouping Variable
        
        beautify <- function(input_data, variable, group_var="") {
          
          names<-names(input_data)
          names_temp <- c(names[grep(variable,names)],names[grep(group_var,names)])
          
          attributes<- attributes(input_first)[[1]]
          attributes<- attributes[names(attributes) %in% names_temp]
          attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()

          input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame()
          names(input_data)<-attributes
          
          names(input_data)[grepl(group_var,names(input_data))] <- "Grouping"
          colnames(input_data)[is.na(colnames(input_data))] <- "Grouping"
          
          return(input_data)
          
        } # with grouping variable
        
        str(beautify(input, variable ="S2Q1r.*", group_var = "Grouping"))

# X. Exploratory Data Analysis
    

    # Automatic Reporting (with / without Grouping) ------------------------------------------
        library(ggrepel)
  response <- function(input_data, glossar_data, variables, group = "Cluster", benchmark = c("percentage points","index","absolute"), top_n = 3, plots_per_page=4, save = F, nrows = 1, ncols = 2, font_family = "Arial Narrow") {
      
      df_list<-vector(mode="list", length(variables))
      plot_list<-vector(mode = "list", length(variables))
      
      #Conditions
      names<-names(input_data)
      my_pres<-read_pptx(path="C:/Users/Florian Gehrig/Documents/Benutzerdefinierte Office-Vorlagen/Vivaldi Template.pptx")
      mytheme <- mschart_theme(
        axis_title = fp_text(font.family = "Arial Narrow", color = "black", font.size = 13, bold = TRUE),
        axis_text = fp_text(font.family = "Arial Narrow", color = "black", font.size = 11),
        legend_text = fp_text(font.family = "Arial Narrow", color = "black", font.size = 11, bold = TRUE),
        grid_major_line = fp_border(color = "#d4d8dd", style = "solid"),
        grid_major_line_x = fp_border(style = "none"),
        grid_minor_line = fp_border(color = "#f2f5f9", style = "dashed"),
        
        axis_ticks = fp_border(color = "#bfc3c9", width = 2) 
      )
      
      for(i in 1:length(variables)) {
        
        #Definitions
        current_var<-grepl(variables[i],glossar_data$Variable)
        current<-variables[i]
        names_temp <- c(names[grep(current,names)],names[grep(current,names)])
        
        attributes<- attributes(input_first)[[1]]
        attributes<- attributes[names(attributes) %in% names_temp]
        attributes<- sub(".*-","", attributes) %>% as.vector()
        
        # Load Lists
        temps<-vector(mode="list", 3)
        temps_plots<-vector(mode = "list", 3)
        temps_df <- vector(mode = "list", 3)
        
        together <- function(input) {
          
        output <- input %>% 
            gather("Dimension","Level", -Grouping) %>%
            mutate(Level = factor(Level), Dimension = factor(Dimension)) %>%
            group_by(Grouping, Dimension, Level) %>% 
            filter(is.na(Level) == F) %>% 
            summarise(n=n()) %>%
            ungroup() %>%
            group_by(Grouping,Dimension) %>%
            mutate(N = sum(n)) %>%
            group_by(Grouping,Dimension, Level) %>% 
            mutate(p = n/N) %>%
            select(Grouping,Dimension, Level, n, N, p) %>% as.data.frame()
          
          return(output)
          
        }

        if(glossar_data$Type[grepl(variables[i],glossar_data$Variable)] == "Single") {
          
          
          input_data <- as.data.frame(input_data) # only for single
          input_data$Grouping <- factor(input_data$Grouping)
          
          
          temp1 <- beautify(input_data = input_data, variable = variables[i], group_var=group) %>% together()
          
          temp2 <- beautify(input_data = input_data, variable = variables[i], group_var="Grouping") %>% together()

          temp3 <- temp2 %>% select(Level, p) %>% mutate(p2 = p) %>% select(-p)
        
          if("percentage points" %in% benchmark) {
            
            temp_perc<-merge(temp1, temp3, by = c("Level")) %>% mutate(p3 = p-p2) %>% select(Grouping, Dimension, Level, n, N, p3, -p2) %>% mutate(p = p3) %>% select(-p3)
            temp_perc<-rbind(temp_perc,temp2)
            
            temps[[1]]<-temp_perc
            
          } 
          
          if ("index" %in% benchmark) {
            
            temp_percpoints<-merge(temp1, temp3, by = c("Level")) %>% mutate(p3 = (p-p2)/p2) %>% select(Grouping, Dimension, Level, n, N, p3, -p2) %>% mutate(p = p3) %>% select(-p3)
            temp_percpoints<-rbind(temp_percpoints,temp2)
            
            temps[[2]]<-temp_percpoints
            
          } 
          
          if ("absolute" %in% benchmark) {
            
            temp_abs<-rbind(temp1,temp2)
            
            temps[[3]]<-temp_abs
            
          }
          
          
          if(glossar_data$Scale[current_var] == "Continous") {
            
          for (z in 1:length(temps)) {
            
            temp <- temps[[z]]
            
            temp$Level <- as.numeric(temp$Level)
            temp <- temp %>% select(-N) %>% group_by_at(vars(-n,-Level,-p)) %>% mutate(mean=Level*p) %>% select(-Level,-p) %>% summarise(mean=sum(mean)) 
            
            temps_df[[z]]<-temp
            
            plot<-ggplot(temp, aes(x=Dimension,y=mean)) + geom_bar(aes(fill=factor(Dimension)), stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
              geom_text(aes(x=Dimension,y=mean,label=mean),size = 2, position = position_stack(vjust = 0.5))  + ggtitle(attributes[1]) + 
              theme_minimal() + theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank(),
                                      plot.title = element_text(hjust=0.5, size=14,  family="Arial Narrow", face="bold"))+ ylab("mean") 
            
            if (length(levels(temp$Grouping)) > 8) {
              
              print("Plot dimensions are too large for one slide. They will be split up.")
              n_pages_needed <- ceiling(length(levels(temp$Grouping))/(nrows*ncols))
              
              plot_bundle<-vector(mode="list",n_pages_needed)
              
              for (j in 1:n_pages_needed) {
                
                plot<-plot + coord_flip() + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j)
                
                plot_bundle[[j]]<-plot
                
              }
              
              temps_plots[[z]]<-plot_bundle
              
            } else {
              
              temps_plots[[z]]<-plot + facet_wrap(~Grouping)
              
            } 
            
            
          }
            
            plot_list[[i]]<-temps_plots
            df_list[[i]]<-temps_df
            

          } else { # Discrete
            
            for (w in 1:length(temps)) {
              
              temp <- temps[[w]] %>% as.data.frame() %>% mutate_if(is.character,as.factor)                
              temps_df[[w]]<-temp
              
              plot<-ggplot(temp, aes(x=Level,y=round(p,2), group = Grouping)) + geom_bar(aes(fill=factor(Level)), stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
                geom_text(aes(x=length(levels(Level)),y=max(p)+0.01, label=paste("n=",N, sep=" ")),size =3) + # N Count
                geom_text(aes(x=Level,y=round(p,2),label=scales::percent(round(p,2))),size = 2, position = position_stack(vjust = 0.5)) +
                ylab("percent") + scale_y_continuous(labels = scales::percent)  + ggtitle(attributes[1]) + theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, size=7), plot.title = element_text(hjust=0.5, size=14,  family="Arial Narrow", face="bold"))
              
              too_big<-(any(sapply(strsplit(levels(temp$Level), " "), length)) > 15 & length(levels(temp$Level)) > 15)
              
              if (too_big == TRUE) {
                
                print("Plot dimensions are too large for one slide. They will be split up.")
                n_pages_needed <- ceiling(length(levels(temp$Grouping))/(nrows*ncols))
                
                plot_bundle<-vector(mode="list",n_pages_needed)
                
                for (j in 1:n_pages_needed) {
                  
                  plot<-plot + coord_flip() + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j)
                  plot_bundle[[j]]<-plot
                  
                }
                
                temps_plots[[w]]<-plot_bundle
                
              } else {
                
                temps_plots[[w]]<-plot + facet_wrap(~Grouping)
              }
              
            }
            
            
            plot_list[[i]]<-temps_plots
            df_list[[i]]<-temps_df

 
          } # Continous loop close
          
          
          } else if(glossar_data$Type[current_var] == "Dual") {  ## 2 Dimensions 
          
          
          print("Hello")
          
          temp1 <- beautify(input_data = input_data, variable = variables[i], group_var=group)  %>% together() %>% as.data.frame()  %>% mutate_if(is.character,as.factor)
          temp2 <- beautify(input_data = input_data, variable = variables[i], group_var="Grouping") %>% together() %>% as.data.frame()  %>% mutate_if(is.character,as.factor)
          temp3 <- temp2 %>% select(Dimension, p) %>% mutate(p2 = p) %>% select(-p)

          temp<-rbind(temp1,temp2)
          
          print("Hello")

          if(glossar_data$Scale[current_var] == "Continous") {
            
            temp$Level <- as.numeric(temp$Level)
            temp <- temp %>% select(-N) %>% group_by_at(vars(-n,-Level,-p)) %>% mutate(mean=Level*p) %>% select(-Level,-p) %>% summarise(mean=sum(mean)) 
            
            df_list[[i]]<-temp
            temp<-df_list[[i]] %>% as.data.frame() %>% mutate_if(is.character,as.factor)
            
            plot<-ggplot(temp, aes(x=Dimension,y=mean)) + geom_bar(aes(fill=factor(Dimension)), stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
              geom_text(aes(x=Dimension,y=mean,label=mean),size = 2, position = position_stack(vjust = 0.5)) +
              ylab("mean")  + ggtitle(attributes[1]) + theme_minimal()
            
            too_big<-(any(sapply(strsplit(levels(temp$Dimension), " "), length)) > 15 & length(levels(temp$Dimension)) > 15)
            
            if (too_big == TRUE) {
              
              print("Plot dimensions are too large for one slide. They will be split up.")
              n_pages_needed <- ceiling(length(levels(temp$Grouping))/(nrows*ncols))
              
              plot_bundle<-vector(mode="list",n_pages_needed)
              
              for (j in 1:n_pages_needed) {
                
                plot<-plot + coord_flip() + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j)
                plot_bundle[[j]]<-plot
                
              }
              
              plot_list[[i]]<-plot_bundle
              
            } else {
              
              plot_list[[i]]<-plot + facet_wrap(~Grouping)
              
            }

          } else { ## Discrete

                if(length(levels(temp1$Level)) == 2 | length(levels(temp1$Dimension)) == 2) { ## Binary
                  
                  temp1 <- temp1 %>%  filter(Level == "Selected") %>% mutate_if(is.character,as.factor)
                  temp <- temp %>%  filter(Level == "Selected") %>% mutate_if(is.character,as.factor)
                  temp2 <- temp2 %>% filter(Level == "Selected") %>% mutate_if(is.character,as.factor)
                  temp3 <- temp2 %>% select(Dimension, p) %>% mutate(p2 = p) %>% select(-p)
                  
                  if("percentage points" %in% benchmark) {
                    
                    temp_perc<-merge(temp1, temp3, by = c("Dimension")) %>% mutate(p3 = p-p2) %>% select(Grouping, Dimension, Level, n, N, p3, -p2) %>% mutate(p = p3) %>% select(-p3)
                    temp_perc<-rbind(temp_perc,temp2)
                    temp_perc <- temp_perc %>% mutate(pos = ifelse(p < 0, "Negative", "Positive"))
                    
                    temps[[1]]<-temp_perc
                    
                  } 
                  
                  if ("index" %in% benchmark) {
                    
                    temp_percpoints<-merge(temp1, temp3, by = c("Dimension")) %>% mutate(p3 = (p-p2)/p2) %>% select(Grouping, Dimension, Level, n, N, p3, -p2) %>% mutate(p = p3) %>% select(-p3)
                    temp_percpoints<-rbind(temp_percpoints,temp2)
                    temp_percpoints <- temp_percpoints %>% mutate(pos = ifelse(p < 0, "Negative", "Positive"))
                    
                    temps[[2]]<-temp_percpoints
                    
                  } 
                  
                  if ("absolute" %in% benchmark) {
                    
                    temp_abs<-rbind(temp1,temp2)
                    temp_abs <- temp_abs %>% mutate(pos = ifelse(p < 0, "Negative", "Positive"))
                    
                    
                    temps[[3]]<-temp_abs
                    
                  }
                  
                  
                  for (m in 1:length(temps)) {
                    
                    temp <- temps[[m]]
                    
                    temp$Grouping<-as.factor(temp$Grouping)

                    plot<-ggplot(temp, aes(x=Dimension,y=round(p,2))) + geom_bar(aes(fill=pos),stat = "identity") +
                    geom_text(aes(y=(p/2),label=scales::percent(round(p,2))),size = 1.75, angle = 90) +
                    ylab("percent") +
                    theme_minimal() + ggtitle(attributes[1]) +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1,size=6),axis.text.y = element_text(size=6),legend.text = element_text(size=6),legend.title = element_text(size=6)) 
                    
                    if (length(unique(temp$N)) <= length(levels(temp$Grouping))) {
                      
                      plot <- plot + geom_text(x=length(levels(temp$Dimension))/2,y=(max(temp$p)/2)-0.1, label=paste("n=",temp$N, sep=" "),size =2) 
                      
                    } else {
                      
                      plot <- plot + geom_text(aes(y=1,label=paste("n=",N)),size = 1.25, angle = 90) 

                    }
                    
                    temps_plots[[m]]<-plot + facet_wrap(~Grouping)
                    

                      levels<-levels(temp$Grouping)
                      
                      tempo <- temp
                      
                      tempo$p <- round(tempo$p,2)
                      
                      categories <- levels(tempo$Grouping)
                      colors<- c(brewer.pal(length(categories),"Accent" ))
                      names(colors)<-categories
                      
                      sizes<-vector(mode="list", length(levels))
                      
                      for (b in 1:length(levels)) {
                        
                        sizes[[b]]<-assign(levels[b],fp_text(font.size = 7, bold = T, color = "black"))
                        
                      }
                      
                      names(sizes)<-levels

                      my_chart <-  ms_barchart(data = tempo, x = "Dimension", y = "p", group = "Grouping") %>% set_theme(mytheme) %>%
                        chart_data_fill(values = colors) %>% 
                        chart_data_stroke(values = colors) %>% 
                        chart_data_labels(show_val = T, show_percent = T, position = 'ctr') %>%
                        chart_labels_text(values = sizes) %>%
                        chart_settings(vary_colors=T) 
                      
                      str(tempo)
                      
                      my_pres<- my_pres%>% add_slide(layout = "4c_Title & Full Content", master = "1_VPG2017v3") %>%
                        ph_with_text(type = "title", str = paste(attributes[i])) %>%
                        ph_with_chart(chart = my_chart, type = "body", index=3)
                      
                  } 
                  
                  plot_list[[i]] <- temps_plots
                  
                } else if(glossar_data$Scale2[current_var] == "Ordinal") {
                  
                  for (k in 1:length(scales)) {
                    
                    if (setequal(temp$Level,scales[[k]]) == TRUE) {
                      
                      temp$Level<-factor(temp$Level, ordered = TRUE, levels = scales[[k]])
                      
                      temp2<-temp %>% group_by(Grouping,Dimension) %>% 
                        summarise(mean = sum(p*as.numeric(Level)), n=sum(n),Lower2 = sum(p[Level %in% scales[[k]][1:top_n]]),Top2 = sum(p[Level %in% scales[[k]][(length(scales[[k]])-(top_n-1)):length(scales[[k]])]]), Middle = 1-Top2-Lower2) 
                      
                      ordinal <-"Percent" 
                      
                      if (ordinal == "Percent") { 
                        
                        temp2<- temp2 %>%
                        gather("Level","p",-Grouping,-Dimension,-n, -mean) %>%
                        group_by(Grouping) %>%
                        mutate(MEANS=mean(mean))
                        
                        temp3 <- temp2 %>% filter(Level=="Top2") %>% arrange(Grouping)
                        
                      
                      } else {
                        
                        temp2 <- temp2 %>%
                          gather("Level","p",-Grouping,-Dimension,-n, -mean) %>%
                          group_by(Grouping) %>%
                          mutate(MEANS=mean(mean)) %>% select(-p) %>% mutate(p = mean, Level ="mean")
                        
                        temp3 <- temp2
                        
                        
                      }

                      temp5 <- temp3 %>% filter(Grouping == "Average") %>% select(Grouping,Dimension,p) %>% mutate(p2 = p) %>% select(-p) 
                      
                      temp4<-temp3 %>% as.data.frame() %>% filter(Grouping == "Average") %>% select(Dimension,p) %>% mutate(p2 = p) %>% select(-p) 
                      
                      
                      if("percentage points" %in% benchmark) {
                        
                        temp_pp<-merge(temp3, temp4, by = c("Dimension")) %>% mutate(p3 = p-p2) %>% select(Grouping, Dimension, Level, n, p3,mean, -p2) %>% mutate(p = p3) %>% select(-p3) %>% arrange(Grouping)
                        temp_pp<-merge(temp_pp, temp5, id = c("Grouping","Dimension"), all.x = TRUE)
                        temp_pp$p[which(temp_pp$Grouping == "Average")]<-temp_pp$p2[which(temp_pp$Grouping == "Average")]
                        temp_pp<-temp_pp %>% select(-p2)
                        temp_pp <- temp_pp %>% mutate(pos = ifelse(p < 0, "Negative", "Positive"))
                        
                        temps[[1]]<-temp_pp

                      } 
                      
                      if ("index" %in% benchmark) {
                        
                        temp_perco<-merge(temp3, temp4, by = c("Dimension")) %>% mutate(p3 = (p-p2)/p2) %>% select(Grouping, Dimension, Level, n, p3,mean, -p2) %>% mutate(p = p3) %>% select(-p3) %>% arrange(Grouping)
                        temp_perco<-merge(temp_perco, temp5, id = c("Grouping","Dimension"), all.x = TRUE)
                        temp_perco$p[which(temp_perco$Grouping == "Average")]<-temp_perco$p2[which(temp_perco$Grouping == "Average")]
                        temp_perco<-temp_perco %>% select(-p2)
                        temp_perco <- temp_perco %>% mutate(pos = ifelse(p < 0, "Negative", "Positive"))
                        
                        temps[[2]]<-temp_perco

                      } 
                      
                      if ("absolute" %in% benchmark) {
                        
                        temp3 <- temp3 %>% mutate(pos = ifelse(p < 0, "Negative", "Positive"))
                        
                        temps[[3]]<- temp3
                        
                      }

                      
                      tempx<-merge(temp,temp2 %>% select(Grouping,Dimension, mean), all.x = TRUE) %>% group_by(Grouping) %>% mutate(MEANS = mean(mean))
                      
                        

                        # Top-n,Middle,Lower-n
                       
                      temp2_summarise <- temp2[!duplicated(temp2[,c('Grouping', 'Dimension')]), ] %>% select(Grouping,Dimension,mean)
                      
                      print(head(temp2_summarise, 300))
                      
                        plot1<-ggplot(temp2, aes(x=Dimension,y=round(p,2))) + geom_bar(aes(fill=factor(Level)), stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
                          geom_text(aes(x=Dimension,y=round(p,2),group=factor(Level),label=scales::percent(round(p,2))),size = 1.75, position = position_stack(vjust = 0.5)) +
                          geom_text(stat="identity", x=length(levels(temp2$Dimension))+1.5,y=(max(temp2$p)/2)-0.1, label=paste("n=",temp2$n, sep=" "),size =2) + # N Count
                          geom_text(stat="identity", x=length(levels(temp2$Dimension))+1.5,y=(max(temp2$p)/2)+0.1, label=paste("Ø=",round(temp2$MEANS,2), sep=" "),size =2) + # N Count
                          ylab("percent") + ggtitle(attributes[1]) + scale_y_continuous(labels = scales::percent) + expand_limits(x=length(levels(temp2$Dimension))+2) + theme_minimal() + 
                          theme(axis.text.x = element_text(size=6),axis.text.y = element_text(size=6),legend.text = element_text(size=6),legend.title = element_text(size=6))
                        
                        
                        # All Levels
                        plot3<-ggplot(temp, aes(x=Dimension,y=round(p,2),group=factor(Level))) + geom_bar(aes(fill=factor(Level)), stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
                          geom_text(x=length(levels(temp$Dimension))+1.5,y=(max(temp$p)/2)-0.1, label=paste("n=",temp$N, sep=" "),size =2) + # N Count
                          geom_text(data= tempx, x=length(levels(tempx$Dimension))+1.5,y=(max(tempx$p)/2)+0.1, label=paste("Ø=",round(tempx$MEANS,2), sep=" "),size =2) + # N Count
                          geom_text(aes(x=Dimension,y=round(p,2),label=scales::percent(round(p,2))),size = 1.75, position = position_stack(vjust = 0.5)) +
                          scale_y_continuous(labels = scales::percent) + labs(fill = "Levels") +
                          expand_limits(x=length(levels(temp$Dimension))+2) + theme_minimal()
                        
                        
                        too_big<-(length(levels(temp2$Dimension)) > 15)
                        
                        if (too_big == TRUE) {
                          
                          for (m in 1:length(temps)) {
                            
                            # Change in Top n
                            plot2<-ggplot(temps[[m]], aes(x=Dimension,y=p)) + geom_bar(aes(fill=pos),stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
                              geom_text(aes(x=Dimension,y=p,label=scales::percent(round(p,4))),size = 1.75, position = position_stack(vjust = 0.5)) +
                              geom_text(data= temp, x=length(levels(temp$Dimension))+1.5,y=(max(temp$p)/2)-0.1, label=paste("n=",temp$N, sep=" "),size =2) + # N Count
                              geom_text(data= tempx, x=length(levels(tempx$Dimension))+1.5,y=(max(tempx$p)/2)+0.1, label=paste("Ø=",round(tempx$MEANS,2), sep=" "),size =2) + # N Count
                              scale_y_continuous(labels = scales::percent) +
                              expand_limits(x=length(levels(temp$Dimension))+2)  + ggtitle(attributes[1]) + theme_minimal() +
                              theme(text = element_text(family = "Arial Narrow", color = "grey20"), plot.title = element_text(hjust=0.5, size=14,  family="Arial Narrow", face="bold"),
                                    axis.text.x = element_text(size=6),axis.text.y = element_text(size=6),legend.text = element_text(size=6),legend.title = element_text(size=6))
                            
                          
                          print("Plot dimensions are too large for one slide. They will be split up.")
                          n_pages_needed <- ceiling(length(levels(temp2$Grouping))/(nrows*ncols))
                          
                          plot_bundle1<-vector(mode="list",n_pages_needed)
                          plot_bundle2<-vector(mode="list",n_pages_needed)
                          plot_bundle3<-vector(mode="list",n_pages_needed)
                          
                          for (j in 1:n_pages_needed) {
                            
                            plot1<-plot1  + coord_flip()  + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j) + geom_text(data=temp2_summarise, aes(x=temp2_summarise$Dimension,y=1.08),label=paste("Ø",round(temp2_summarise$mean,2)),size = 2) +
                              theme(text = element_text(family = font_family),
                                    axis.text.x = element_text(size=6), 
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.text.y = element_text(size=6),
                                    legend.text = element_text(size=6),
                                    legend.title = element_text(size=7, family = font_family, face = "bold")) 
                            
                            plot2<-plot2 + coord_flip() + geom_text(data=temp2,aes(x=Dimension,y=1.08,label=paste("Ø",round(mean,2))),size = 2) + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j)+
                              theme(text = element_text(family = font_family),
                                    axis.text.x = element_text(size=6), 
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.text.y = element_text(size=6),
                                    legend.text = element_text(size=6),
                                    legend.title = element_text(size=7, family = font_family, face = "bold")) 
                            
                            plot3<-plot3 + coord_flip() + geom_text(data=temp2,aes(x=Dimension,y=1.08,label=paste("Ø",round(mean,2))),size = 2) + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j)  +
                              theme(text = element_text(family = font_family),
                                    axis.text.x = element_text(size=6), 
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.text.y = element_text(size=6),
                                    legend.text = element_text(size=6),
                                    legend.title = element_text(size=7, family = font_family, face = "bold"))

                            
                            plot_bundle1[[j]]<-plot1
                            plot_bundle2[[j]]<-plot2
                            plot_bundle3[[j]]<-plot3
                            
                          } 
                          
                          temps_plots[[m]]<-plot_bundle2
                          
                        }

                          
                        plot_list[[i]]<-c(plot_bundle1,temps_plots,plot_bundle3)
                          
                          
                          
                          
                        } else {
                          
                          
                          
                          plot_bundle<-vector(mode="list",3)
                          
                          for (m in 1:length(temps)) {
                            
                            # Change in Top n
                            plot2<-ggplot(temps[[m]], aes(x=Dimension,y=p)) + geom_bar(aes(fill=pos),stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
                              geom_text(aes(x=Dimension,y=p,label=scales::percent(round(p,4))),size = 1.75, position = position_stack(vjust = 0.5)) +
                              geom_text(data= temp, x=length(levels(temp$Dimension))+1.5,y=(max(temp$p)/2)-0.1, label=paste("n=",temp$N, sep=" "),size =2) + # N Count
                              geom_text(data= tempx, x=length(levels(tempx$Dimension))+1.5,y=(max(tempx$p)/2)+0.1, label=paste("Ø=",round(tempx$MEANS,2), sep=" "),size =2) + # N Count
                              scale_y_continuous(labels = scales::percent) +
                              expand_limits(x=length(levels(temp$Dimension))+2)  + ggtitle(attributes[1]) + theme_minimal() +
                              theme(text = element_text(family = "Arial Narrow", color = "grey20"), plot.title = element_text(hjust=0.5, size=14,  family="Arial Narrow", face="bold"),
                                    axis.text.x = element_text(size=6),axis.text.y = element_text(size=6),legend.text = element_text(size=6),legend.title = element_text(size=6))
                            
                            
                          temps_plots[[m]] <- plot2 + geom_text(data=temps[[m]],aes(x=Dimension,y=1.08,label=paste("Ø",round(mean,2))),size = 2, angle = 90) + facet_wrap(~Grouping)
                          }
                          
                          plot_bundle[[1]] <- temps_plots
                          plot_bundle[[2]] <- plot1 + geom_text(data=temp2,aes(x=Dimension,y=1.08,label=paste("Ø",round(mean,2))),size = 2, angle = 90) + facet_wrap(~Grouping)
                          plot_bundle[[3]] <- plot3 + geom_text(data=temp2,aes(x=Dimension,y=1.08,label=paste("Ø",round(mean,2))),size = 2, angle = 90) + facet_wrap(~Grouping)
                          
                          plot_list[[i]]<-plot_bundle
                          
                          print(plot_bundle)
                          
                          
                        }
                        
                        
                      
                    } else {}
                 
                }
               
                
                } else { ## Categorical
                  
                  if("percentage points" %in% benchmark) {
                    
                    temp_perc<-merge(temp1, temp3, by = c("Level")) %>% mutate(p3 = p-p2) %>% select(Grouping, Dimension, Level, n, N, p3, -p2) %>% mutate(p = p3) %>% select(-p3)
                    temp_perc<-rbind(temp_perc,temp2)
                    
                    temps[[1]]<-temp_perc
                    
                  } 
                  
                  if ("index" %in% benchmark) {
                    
                    temp_percpoints<-merge(temp1, temp3, by = c("Level")) %>% mutate(p3 = (p-p2)/p2) %>% select(Grouping, Dimension, Level, n, N, p3, -p2) %>% mutate(p = p3) %>% select(-p3)
                    temp_percpoints<-rbind(temp_percpoints,temp2)
                    
                    temps[[2]]<-temp_percpoints
                    
                  } 
                  
                  if ("absolute" %in% benchmark) {
                    
                    temp_abs<-rbind(temp1,temp2)
                    
                    temps[[3]]<-temp_abs
                    
                  }
                  
                  for (a in 1:length(temps)) {
                    
                    plot<-ggplot(temps[[a]], aes(x=Dimension,y=round(p,2),group=factor(Level))) + geom_bar(stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
                      geom_text(x=length(levels(temp$Dimension)),y=max(temp$p), label=paste("n=",temp$N, sep=" "),size =3) + # N Count
                      geom_text(aes(x=Dimension,y=round(p,2),label=scales::percent(round(p,2))),size = 2, position = position_stack(vjust = 0.5)) +
                      geom_text(data=temp2,aes(x=Dimension,y=1.08,label=paste("Ø",round(mean,2))),size = 2, angle=90) +
                      ylab("percent") + scale_y_continuous(labels = scales::percent)  + ggtitle(attributes[1]) + theme_minimal()
                    
                    if (too_big == TRUE) {
                      
                      n_pages_needed <- ceiling(length(levels(temp$Grouping))/(nrows*ncols))
                      plot_bundle<-vector(mode="list",n_pages_needed)
                      
                      for (j in 1:n_pages_needed) {
                        
                        plot<-plot + coord_flip() + facet_wrap_paginate(~Grouping, nrow = nrows, ncol = ncols, page = j)
                        plot_bundle[[j]]<-plot
                        
                        
                      }
                      
                      temps_plots[[a]]<-plot_bundle
                      
                    } else {
                      
                      temps_plots[[a]]<-plot + facet_wrap(~Grouping)
                      
                    }
                  
                    plot_list[[i]]<-temps_plots
                    
                  }
                
   
        
              }

     
            
          } # Continous loop close
          
          

        } else if(glossar_data$Type[current_var] == "Triple") {
          
          
          input_data <- as.data.frame(input_data) # only for single
          
          temp1<- beautify(input_data = input_data, variable = variables[i], group_var=group) %>% 
            gather("Dimension","Level", -Grouping) %>%
            mutate(Level = factor(Level), Dimension = factor(Dimension)) %>%
            group_by(Grouping, Dimension, Level) %>% 
            filter(is.na(Level) == F) %>% 
            summarise(n=n()) %>%
            ungroup() %>%
            group_by(Grouping,Dimension) %>%
            mutate(N = sum(n)) %>%
            group_by(Grouping,Dimension, Level) %>% 
            mutate(p = n/N) %>%
            select(Grouping,Dimension, Level, n, N, p)
          
          temp2<- beautify(input_data = input_data, variable = variables[i], group_var="Grouping") %>% 
            gather("Dimension","Level", -Grouping) %>%
            mutate(Level = factor(Level), Dimension = factor(Dimension)) %>%
            group_by(Grouping, Dimension, Level) %>% 
            filter(is.na(Level) == F) %>% 
            summarise(n=n()) %>%
            ungroup() %>%
            group_by(Grouping,Dimension) %>%
            mutate(N = sum(n)) %>%
            group_by(Grouping,Dimension, Level) %>% 
            mutate(p = n/N) %>%
            select(Grouping,Dimension, Level, n, N, p)
          
          temp<-rbind(temp1,temp2)
        
            
          temp<-temp %>% as.data.frame()  
          temp$Dimension <- as.character(temp$Dimension)
            
          temp<-temp%>%separate(Dimension,into = c("Category","Dimension"), sep=" - ") %>% mutate_if(is.character,as.factor)
          too_big<-length(levels(temp$Grouping)) > 8

          
          if(glossar_data$Scale[current_var] == "Continous") {
            
            temp$Level <- as.numeric(temp$Level)
            temp <- temp %>% select(-N) %>% group_by_at(vars(-n,-Level,-p)) %>% mutate(mean=Level*p) %>% select(-Level,-p) %>% summarise(mean=sum(mean)) 
            

            df_list[[i]]<-temp
            
            temp<-df_list[[i]] %>% as.data.frame() %>% mutate_if(is.character,as.factor)
            
            plot_list[[i]]<-ggplot(temp, aes(x=Dimension,y=mean)) + geom_bar(aes(fill=factor(Dimension)), stat = "identity", position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
              geom_text(aes(x=Dimension,y=mean,label=mean),size = 2, position = position_stack(vjust = 0.5)) +
              ylab("mean") + facet_wrap(~Grouping)
            
            
          } else {
            
            
            df_list[[i]]<-temp
            
            temp<-df_list[[i]] %>% as.data.frame() %>% mutate_if(is.character,as.factor)
            
            library(ggforce)
            
            n_pages_needed <- 3
            
            plot_bundle<-vector(mode="list",n_pages_needed)
            
            for (j in 1:n_pages_needed) {
              

                plot_bundle[[j]]<-ggplot(temp, aes(x=Category,y=round(p,2),group=factor(Level))) + 
                  geom_bar(aes(fill=factor(Level)), stat = "identity", position = "stack") + 
                  geom_text(aes(x=Category,y=1.05, label=N),size =2) + # N Count
                  geom_text(aes(x=Category,y=round(p,2),label=round(p,2)),size = 2, position = position_stack(vjust = 0.5)) +
                  theme(axis.text.x = element_text(colour="grey20",size=8,face="plain",angle = 90, hjust = 1)) +
                  scale_y_continuous(limits = c(0, 1.1)) +
                  facet_wrap_paginate(~Grouping+Dimension, nrow = 2, ncol = 2, page = j)

              
            }
            plot_list[[i]]<-plot_bundle
            
          } # Continous loop close
          
          
          
          
        }
        
        
        
      } # For loop close 
      
      
      if (save == TRUE) {
        
        pdf("plots.pdf", width = 11.69, height = 8.27)
        
        for (i in 1:length(plot_list)) {
          print(plot_list[[i]])
        }
        
        dev.off()
        
       
  
      }

      
      output<-c(df_list,plot_list)
      print(my_pres,target="C:/Users/Florian Gehrig/Documents/Powerpoint.pptx")
      return(plot_list)
    
    }
    ## additionally automatic NA-Screener , Q-Q Plot, Normalization, rough feeling for data structure, correlation

  motivations<-response(input,glossars,variables = "^S2Q1r.*" , group = "Cluster", save = T)
     
    
    # Flatliner Removal
    flatline_remove<- function(input_data, eliminate = TRUE) {
    
    flatliners<-apply(input_data, 1, function(x) { length(unique(x[!is.na(x)])) == 1 })
    
    if(eliminate == FALSE) {
      
      return(input_data[flatliners,])
      
    } else {
      
      return(input_data[!flatliners,])
      
    }
    
           
    }
    df<-flatline_remove(df) 


    # Missing Value Treatment
    
    impute_na <- function(input_data, mode = "mean") {
      
      library(Hmisc)
      
      impute(input_data, ifelse(mode=="mean",mean, median))
      
    }
    # impute_na()
     op<-fa(subset_data,5, rotate = "oblimin", fm="pa")
     subset_data<-op$scores %>% as.data.frame()

     subset_data<-subselect(input_first,variable = "^S2Q1r.*", relabel = F) %>% sapply(as.numeric) %>% as.data.frame()
     subset_data<-subset_data[,-c((ncol(subset_data)-1),ncol(subset_data))] %>% gather("VarID", "Value")
     
    # Exploratory Factor Analysis
    efa <- function (input_data, cutoff, n_factors, rotate = "oblimin", fm = "minres", ...) {
      
      library(htmltools)
      library(htmlwidgets)
      library(rpivotTable)
      library(psych)
      
      df_short<-input_data
      names(df_short) <- 1:ncol(input_data)
      
      df_info <- input_data %>% gather("Item", "Value") 
      df_info$Item <- as.factor(df_info$Item)
      df_info <- df_info %>% group_by(Item) %>% summarise(mean = mean(Value)) %>% mutate(index = 1:ncol(input_data))
      
      factors <- fa(df_short,nfactors = n_factors,rotate = rotate,fm=fm, ...)
      factors<-fa.sort(factors,polar=FALSE)
      factors<-print(factors$loadings,cutoff = cutoff)
      
      class(factors) <- "matrix"
      loads <- data.frame(factors)
      
      low_variables <- rownames(loads[apply(loads, 1, function(row) {all(row < cutoff)}),]) %>% as.numeric()
      
      
      for(i in 1:ncol(loads)) {
        
        loads[[i]][which(loads[[i]] < cutoff)] <- NA
        
      }
      
      style_widget <- function(hw=NULL, style="", addl_selector="") {
        stopifnot(!is.null(hw), inherits(hw, "htmlwidget"))
        
        # use current id of htmlwidget if already specified
        elementId <- hw$elementId
        if(is.null(elementId)) {
          # borrow htmlwidgets unique id creator
          elementId <- sprintf(
            'htmlwidget-%s',
            htmlwidgets:::createWidgetId()
          )
          hw$elementId <- elementId
        }
        
        htmlwidgets::prependContent(
          hw,
          htmltools::tags$style(
            sprintf(
              "#%s %s {%s}",
              elementId,
              addl_selector,
              style
            )
          )
        )
      }

      
      df_cutted <- df_short[,-low_variables]
      
      return(df_cutted)
      
    }
    
    # action = 'delete' kills irrelevant factors, action = 'factorize' sums up factors, action = 'both' does both of it
    
        data_factorized<-efa(input_data = subset_data,cutoff=0.35,n_factors=8, rotate = "oblimin", fm = "pa") 
    
    
    # Confirmatory Factor Analysis
    
# 4. Cluster Option Evaluation ------------------------------------------------------------------------------------------------------------------------
    
    # KMeans, Hierarchical, PAM, Sota, Diana, Clara, Model

    cluster_eval <- function(input_data, cluster, clust_methods = c("hierarchical", "kmeans",  "pam", "sota", "diana","clara","model"), val_methods = c("internal"), distances = c("euclidean"), linkage = c("average"), stats = TRUE, plot = TRUE) {
      
      
      
      valid_test <- clValid(input_data, c(2:(cluster+1)),
                            clMethods = clust_methods,
                            validation = val_methods, verbose = TRUE,
                            metric = distances,
                            method = linkage,
                            maxitems = nrow(input_data)
      )
      
      output<-valid_test@measures %>% round(2)
      
      dimension<-dim(output)[3]
      method<-attributes(output)$dimnames[[3]]
      
      frame<-vector(mode = "list", dimension)
      
      for (i in 1:dimension) {
        
        temp<-output[1:3,1:cluster,i] %>% as.data.frame() %>% mutate(method = method[[i]], approach = rownames(output))
        names(temp) <- c(as.character(2:(cluster+1)), "method","approach")
        frame[[i]] <- assign(method[[i]], temp)
        
      }
      
      data <- Reduce(function(...) merge(..., all=T), frame) %>% arrange(method)
      
      data <- data %>% gather("cluster","value",-c("method","approach"))
      data$cluster <- factor(data$cluster,ordered = T, levels = c(2:(cluster+1)))
 
      
      if (plot==TRUE) {
        
        
        g<-ggplot(data, aes(x=cluster, y=value, color = method, group = method)) + geom_point() +geom_line(stat = "identity") + facet_wrap(~approach, scales = "free")
        
        g<-ggplotly(g,tooltip = c("value","method" ,"cluster")) 
        
        
      } else {}

      
      if (stats == TRUE) {
      
      data_analysis <- data %>% spread("approach","value",-c(1,3:4))
      print(valid_test)
      summary(valid_test)
      
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
      
      output<-vector(mode="list", 2)
      output[[1]]<-g
      output[[2]]<-valid_test
      
      return(output)

    }
    
        valuation3<-cluster_eval(subset_data, clust_methods = c("hierarchical","agnes", "kmeans",  "pam"), distances = c("correlation"),linkage = c("ward"),cluster = 8, plot = T)
        
        print(valuation3)

    # DBSCAN
    db_scan<-dbscan(subset_data, eps = 2, MinPts = 2, scale = T, method = "raw")
    print(db_scan)
    dbscan::kNNdistplot(subset_data, k =  3)
    abline(h = 0.15, lty = 2)


# 5. Cluster Profiling ----------------------------------------------

        # Subset Dataframe to Data Analysis
        ncols<-ncol(subset_data)
        subset_data<-subset_data[,-c(ncols-1,ncols)]
    library(amap)
        
        
        cluster_output<-pam(subset_data,k=6, diss =F, metric = "manhattan")$cluster %>% sapply(as.factor) %>% as.data.frame()
        names(cluster_output) <- "Cluster"
        grouping<-rep("Average", nrow(input_first)) %>% sapply(as.factor) %>% as.data.frame()
        names(grouping)<-"Grouping"
        input<-cbind(input_first,cluster_output, grouping)
        
        # IF Cluster contained
        clustering<-input_first$QCL_1 %>% sapply(as.factor)%>% as.data.frame() 
        names(clustering)<- "Cluster"
        grouping<-rep("Average", nrow(input_first))%>% as.data.frame()
        names(grouping)<-"Grouping"
        input<-cbind(input_first,clustering, grouping)
        

        # Visualize Cluster in first principal components
        a<-fviz_cluster(km.res, geom = "point", ellipse.type = "norm", palette = "jco", ggtheme = theme_minimal(), show.clust.cent = T, ellipse = F)
        ggplotly(a)


# 7. Cluster Discrimination / Classification Learning -----
    

    # Subset into Training & Test-Set
    
data_clustered <- cbind(subset_data,cluster_output)    
    set.seed(123)
    
    train_index <- data_clustered$Cluster %>% createDataPartition(p = 0.8, list = FALSE)
    train_data <- data_clustered[train_index, ]
    test_data <- data_clustered[-train_index, ]
    
    train_scaled <- train_data %>% preProcess()

    train_transformed <- train_scaled %>% predict(train_data)
    test_transformed <- train_scaled %>% predict(test_data)
    
    # Fit the model
    
    model_types <- c("lda","mda")
    models <- vector(mode = "list", length(model_types))
    predictions <- vector(mode = "list", length(model_types))
    matrices <- vector(mode = "list", length(model_types))
    overall <- vector(mode = "list", length(model_types))
    specific <- vector(mode = "list", length(model_types))
    confusions <- vector(mode = "list", length(model_types))
    
    
    for (i in 1:length(models)) {
      
      func <- get(model_types[i])
      
      model_temp <- func(Cluster~., data = train_transformed)
      
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
    
    hchart(confusions[[1]], scale="column", col = cm.colors(256))
    
    a<-melt(specific) %>% dplyr::select(method,cluster,variable,value)
    
    ggplot(a, aes(x=factor(method), y = value)) + geom_bar(aes(fill = cluster), position = "dodge", stat = "identity") + facet_wrap(~variable, scales = "free") + 
      geom_text(size=3,position = position_dodge(0.9), aes(x = factor(method), y = value*0.5, label = round(value,2), group = cluster))

    overall <- melt(overall) %>% dplyr::select(kpi,value,L1) %>% spread("L1",value = "value")
    colnames(overall) <- c("kpi", model_types)
    overall_plot <- overall %>% gather("model","value",-kpi)
    
    ggplot(overall_plot, aes(x=model, y = value)) + geom_bar(aes(fill = model), stat = "identity") + facet_wrap(~kpi, scales = "free") + 
      geom_text(aes(label = scales::percent(value)),size = 4, position = position_stack(vjust = 0.5))

    dd <- data_clustered[,-39]
    autoplot(prcomp(dd), data = data_clustered, colour = 'cluster',
             loadings = TRUE, loadings.colour = 'blue',
             loadings.label = TRUE, loadings.label.size = 3)

  
    
