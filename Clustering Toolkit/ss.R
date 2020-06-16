library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(htmltools)
library(htmlwidgets)
library(GPArotation)
library(psych)
library(dplyr)
library(foreign)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(pracma)
library(tidyverse)
library(highcharter)
library(shinydashboardPlus)
library(purrr)
library(clValid)
library(kohonen)
library(amap)
library(openxlsx)
library(rmarkdown)
library(readxl)
library(lsa)

# Inputs
algorithms<-c("hierarchical","agnes","diana","kmeans", "pam", "clara" , "fanny",  "model", "sota","som")
description<-c("Hierachical Clustering",
               "Agglomerative Nesting",
               "Divisive Analysis Clustering",
               "K-Means Clustering",
               "Partitioning around medoids",
               "Clustering Large Application",
               "Fuzzy Clustering",
               "Gaussian Mixture Models",
               "Self-Organizing Tees",
               "Self-Organizing Maps"
)
validation <- c("internal","stability","biological")
distance <- c("euclidean","manhattan","pearson","spearman","kendall","cosine","gower")
linkage <- c("ward","average","single","complete")

subselect <- function(input_data, variable, relabel = T) {
  names<-names(input_data)
  names_temp <- c(names[grep(variable,names)])
  attributes<- attributes(input_data)[[4]]
  attributes<- attributes[names(attributes) %in% names_temp]
  attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()
  
  
  input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame()
  
  if(relabel == TRUE) {
    
    names(input_data)<-attributes
    
  }
  
  return(input_data)
  
} # without grouping Variable

top2_values <- function(input_data) {
  
  output<-input_data %>% group_by(Grouping, Variable, Item, Value) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(Grouping,Variable,Item) %>%
    mutate(N=sum(n)) %>%
    ungroup() %>%
    group_by(Grouping, Variable, Item, Value) %>%
    mutate(percent = n/N)  %>%
    ungroup() %>%
    group_by(Grouping,Variable,Item) %>%
    mutate(Top2 = sum(percent[Value %in% c(5,6)])) %>%
    select(Grouping, Variable,Item,Top2) %>% unique()
  
  return(output)
  
  
}

info_loading <- "Shiny is busy. Please wait."
your_color01 <- "blue"
your_color02 <- "black"


glossars<-read_excel("C:/Users/Florian Gehrig/Documents/Category Definitions.xlsx", sheet = "Tabelle2") %>% as.data.frame() %>% mutate(Overarching = factor(Overarching))

ui <- dashboardPage(
  
  dashboardHeader(title = "EFA Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("I. Dimension Reduction", tabName = "factor", icon = icon("list-alt")),
      menuItem("II. Cluster Evaluation", tabName = "cluster", icon = icon("dashboard")),
      menuItem("III. Cluster Profiling", tabName = "profile", icon = icon("bar-chart-o")),
      menuItem("IV. Cluster Decsriptives", tabName = "describe", icon = icon("bar-chart-o")),
      menuItem("V. Discriminant Analysis", tabName = "seperate", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "factor",
              fluidRow(
                column(width = 3,
                       boxPlus(collapsible = T, width =12,title = "Controls",
                               fileInput(inputId = "upload", "Upload your file and get started!"),
                               uiOutput("Variables"),
                               searchInput(
                                 inputId = "text", 
                                 label = "Enter your variable:", 
                                 placeholder = "Use 'regex'", 
                                 btnSearch = icon("search"), 
                                 btnReset = icon("remove"), 
                                 width = "100%"
                               ),
                               sliderInput(inputId = "nfactors", "Number of factors:", 1, 15, 8),
                               sliderInput(inputId = "cutof", "Factor Loading Cutoff", 0, 1, 0.35),
                               sliderInput(inputId = "niterations", "Number of Iterations", 0, 25, 2),
                               selectInput(inputId = "rotation", 
                                           label = "Type of rotation:",
                                           choices = list("none","varimax", "quartimax", "bentlerT", "geominT", "promax", "oblimin", "simplimax", "bentlerQ","geominQ","cluster"),
                                           selected = "oblimin"),
                               selectInput(inputId = "fm", 
                                           label = "Factoring algorithm:",
                                           choices = list("minres","wls","gls","pa","ml"),
                                           selected = "pa"),
                               column(6, align="center", offset = 0,
                                      submitButton("Update Dashboard", icon("refresh"), width = '235%'),
                                      tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 150%; font-size- 30px;}"))
                               
                               
                       )),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div(info_loading,id="loadmessage")),
                column(width = 9,
                       fluidRow(tabBox(width = 12,height = "950px",
                                       tabPanel("Factor Loadings", highchartOutput(outputId = "Visual", height=900)),
                                       tabPanel("Correlation Matrix", highchartOutput("Tab1",height=900)),
                                       tabPanel("Parallel Analysis", highchartOutput(outputId = "parallel",height=900)),
                                       tabPanel("Statistics", tableOutput("describe")),
                                       tabPanel("Data Table", DT::dataTableOutput(outputId = "Tables"))
                       ))
                )
                
              )
              
              
      ),
      tabItem(tabName = "cluster",
              fluidRow(boxPlus(collapsible = T, width =3,title = "Controls",
                               pickerInput(
                                 inputId = "cluster_alg", 
                                 label = "Choose an clustering algorithm:", 
                                 choices = algorithms, 
                                 choicesOpt = list(subtext = description),
                                 multiple = TRUE,
                                 selected = c("hierarchical")
                               ),
                               pickerInput(
                                 inputId = "val_methods", 
                                 label = "Choose validation statistics:", 
                                 choices = validation,
                                 multiple = TRUE,
                                 selected = "internal"
                               ),
                               sliderInput("ncluster",label = "Number of Clusters", min=1, max=15, value=c(2,5)),
                               fluidRow(column(width=6,selectInput("distance", "Distance:",choices = distance,selected="euclidean")),
                                        column(width=5,selectInput("linkage","Linkage:",choices=linkage,selected="ward"))),
                               sliderInput("neighbours", "Number of neighbours:", 1, 50, 10),
                               selectInput(
                                 inputId = "formatting", label = "Make a choice :", 
                                 choices = list("Raw data","Factorize items", "Delete low items"), 
                                 selected = "Raw data"),
                               selectInput(
                                 inputId = "standardized", label = "Check if standardized", 
                                 choices = list("Unstandardized","Standardized"), 
                                 selected = "Unstandardized"),
                               submitButton()
              ),
              tabBox(width=9, 
                     tabPanel("Cluster Indices",htmlOutput("chart1")),
                     tabPanel("Cluster Input",DT::dataTableOutput("clusterinput"))
                     
              )
              )
      ),
      tabItem(tabName="profile",
              fluidRow(boxPlus(collapsible = T, width =3,title = "Controls",
                               selectInput(
                                 inputId = "cluster_alg_final", 
                                 label = "Choose an clustering algorithm:", 
                                 choices = algorithms
                               ),
                               sliderInput("ncluster_final",label = "Number of Clusters", min=1, max=15, value=c(2,5)),
                               fluidRow(column(width=6,selectInput("distance_final", "Distance:",choices = distance,selected="euclidean")),
                                        column(width=5,selectInput("linkage_final","Linkage:",choices=linkage,selected="ward"))),
                               selectInput(
                                 inputId = "formatting_final", label = "Select the data input", 
                                 choices = list("Raw data","Factorize items", "Delete low items"), 
                                 selected = "Raw data"),
                               selectInput(
                                 inputId = "standardized_final", label = "Check if standardized", 
                                 choices = list("Unstandardized","Standardized"), 
                                 selected = "Unstandardized"),
                               submitButton()
              ),
              box(width=9,DT::dataTableOutput("clusterdata"))
              )
      ),
      tabItem( tabName = "describe",
               fluidRow(
                 boxPlus(collapsible = T, width =2,title = "Controls",
                         searchInput(
                           inputId = "interest_variable", 
                           label = "Enter your variable of interest:", 
                           placeholder = "Use 'regex'", 
                           btnSearch = icon("search"), 
                           btnReset = icon("remove"), 
                           width = "100%"
                         ),
                         uiOutput("group_var"),
                         selectInput(
                           inputId = "kpi_format", label = "Select the format", 
                           choices = list("Mean Absolute","Mean Relative", "Top 2 Absolute","Top 2 Relative"), 
                           selected = "percent"),
                         column(6, align="center", offset = 0,
                                submitButton("Update Dashboard", icon("refresh"), width = '235%'),
                                tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 150%; font-size- 30px;}"))
                         
                 ),
                 tabBox(width=10,height="950px",
                        tabPanel("Tab1",
                                 highchartOutput("ns",height=200),
                                 highchartOutput("descriptives",height=700)),
                        tabPanel("Tab2",
                                 highchartOutput("average",height=900)
                        )
                 )))
    )
  )
)

server <- function(input, output, session) {
  
  
  # Page I --------------------------------------
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  # Main Dataframe
  input_zero <- reactive({
    
    inFile<-req(input$upload)
    
    read<-read.spss(inFile$datapath, to.data.frame=TRUE, max.value.labels = 20)
    
    return(read)
  }) # Loading
  input_first <- reactive({
    
    inFile<-req(input$upload)
    
    Inputs<-req(input$text)
    
    read<-input_zero()
    
    if (is.null(inFile)) {
      
      return(NULL)
      
    } else if(!isempty(input$text)) {
      
      input_filtered<-subselect(read,variable = input$text) %>% sapply(as.numeric) %>% as.data.frame()
      
      return(input_filtered)
      
    } 
    
  }) # Subsetting
  input_second <- reactive({
    
    inFile2<-input$Variables
    
    if (isempty(inFile2) == TRUE) {
      
      input_data<-input_first()
      
      return(input_data)
      
    } else  {
      
      input_data<-input_first()
      input_data<-input_data[!names(input_first()) %in% inFile2]
      
      return(input_data)
      
    }
    
  }) # Additional Selection
  input_third <- reactive({
    
    input_data<-input_second()
    
    parallel<-fa.parallel(input_data, fm = input$fm, fa = 'fa')
    parallel_plot <- data.frame(x=c(1:ncol(input_data)), parallel$fa.values,parallel$fa.sim,parallel$fa.simr) %>% gather("variable", "value", -x) 
    
    return(parallel_plot)
    
  }) # Parrallel Plot Data
  
  output$Variables <- renderUI({
    choice <- names(input_first())
    pickerInput(
      inputId = "Variables", 
      label = "Select all variables you wish to exclude:", 
      choices = choice, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
    
  })
  output$Visual <- renderHighchart({
    
    input_data<-input_second()
    
    input_data_2 <- input_data %>% gather("Item", "Value") 
    input_data_2$Item <- as.factor(input_data_2$Item)
    input_data_2 <- input_data_2 %>% group_by(Item) %>% summarise(mean = mean(Value)) %>% mutate(index = 1:ncol(input_data))
    
    factors <- fa(input_data,
                  nfactors = input$nfactors,
                  rotate = as.character(input$rotation),
                  fm=as.character(input$fm), 
                  n.iter = input$niterations,
                  scores=TRUE,
                  oblique.scores=FALSE)
    factors<-fa.sort(factors,polar=FALSE)
    factors<-print(factors$loadings,cutoff = round(input$cutof))
    class(factors) <- "matrix"
    loads <- data.frame(factors)
    
    row.names(factors)
    
    for(i in 1:ncol(loads)) {
      
      loads[[i]][which(loads[[i]] < input$cutof)] <- NA
      
    }
    
    loads_plot <- loads %>% mutate(Item = row.names(factors)) %>% gather("Factor", "Loading", - Item) %>% arrange(Factor, Loading)
    factor_plot <- hchart(loads_plot, "bar", hcaes(x=factor(Item), y=Loading, color=Factor)) %>% hc_title(text = paste(input$nfactors,"Factors - Cutoff:",input$cutof,"-",input$niterations,"Iterations -",input$fm,"-",input$rotation))%>%
      hc_exporting(enabled=T)
    
    factor_plot
    
  })
  output$Tables <- DT::renderDataTable({
    
    input_data<-input_second()
    ncols<-ncol(input_data)
    
    df_short<-input_data
    names(df_short) <- 1:ncol(input_data)
    
    
    df_info <- input_data %>% gather("Item", "Value") 
    df_info$Item <- as.factor(df_info$Item)
    df_info <- df_info %>% group_by(Item) %>% summarise(mean = mean(Value)) %>% mutate(index = 1:ncol(input_data))
    
    
    factors <- fa(df_short,nfactors = input$nfactors,rotate = as.character(input$rotation),fm=as.character(input$fm), n.iter = 2)
    factors<-fa.sort(factors,polar=FALSE)
    factors<-print(factors$loadings,cutoff = round(input$cutof))
    
    class(factors) <- "matrix"
    loads <- data.frame(factors)
    
    for(i in 1:ncol(loads)) {
      
      loads[[i]][which(loads[[i]] < input$cutof)] <- NA
      
    }
    
    loads_table <- loads %>% mutate(i = as.numeric(rownames(loads))) %>% arrange(i) %>% cbind(df_info[,1:2])%>% gather("Factor", "Loading",-i, -Item,-mean) %>% na.omit() %>% mutate_if(is.character,as.factor) %>% mutate_if(is.numeric,round,2)
    rownames(loads_table) <- loads_table$i
    factor_table <- browsable(datatable(loads_table[,-1], class = 'cell-border stripe', filter = 'top'))
    
    factor_table
    
    
  })
  output$Tab1 <- renderHighchart({
    
    input_data<-input_second()
    
    hchart(round(cor(input_data),2)) %>% hc_add_theme(hc_theme_smpl()) %>% 
      hc_xAxis(labels = list(enabled = F)) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, style=list(
            fontSize="7px"
          ))
        )) %>% hc_title(text="Correlation Matrix") %>%
      hc_exporting(enabled = T)
    
  })
  output$Tab2 <- renderHighchart({
    
    input_data<-input_second()
    
    hchart(round(corr.test(input_data, use = "pairwise.complete.obs")$p,2))  %>% hc_add_theme(hc_theme_smpl()) 
    
    
  })
  output$parallel <- renderHighchart({
    
    input_data<-input_second()
    
    library(psych)
    
    parallel_plot <- input_third()
    hchart(parallel_plot, 'line', hcaes(x = 'x', y = 'value', group = 'variable')) %>% hc_tooltip(shared=T,table=T)
    
  })
  output$describe <- renderTable({
    
    describe(input_second()) 
    
    
  }) 
  
  # Page 2: Cluster Evaluation  --------------------------------------
  
  reduced_data<-reactive({
    
    inFile2<-input$excludes
    
    if (isempty(inFile2) == TRUE) {
      
      input_data<-input_second()
      
      return(input_data)
      
    } else  {
      
      input_data<-input_second()
      input_data<-input_data[!names(input_second()) %in% inFile2]
      
    }
    
    if (input$standardized == "Standardized") {
      
      input_data<-scale(input_data)
      
    } else {
      
      
    }
    
    factors <- fa(input_data,
                  nfactors = input$nfactors,
                  rotate = as.character(input$rotation),
                  fm=as.character(input$fm), 
                  n.iter = input$niterations,
                  scores=TRUE,
                  oblique.scores=FALSE)
    factors<-fa.sort(factors,polar=FALSE)
    factors<-print(factors$loadings,cutoff = round(input$cutof))
    
    class(factors) <- "matrix"
    loads <- data.frame(factors)
    
    low_variables <- rownames(loads[apply(loads, 1, function(row) {all(row < round(input$cutof))}),]) %>% as.numeric()
    
    cleaned_data<-input_data[,! rownames(loads) %in% low_variables ]
    return(factors)
    
    
  })
  factorize<-reactive({
    
    input_data <- input_second() 
    
    factors <- fa(input_data,
                  nfactors = input$nfactors,
                  rotate = as.character(input$rotation),
                  fm=as.character(input$fm), 
                  n.iter = input$niterations,
                  scores=TRUE,
                  oblique.scores=FALSE)
    
    print("Input is factorized!")
    
    input_data<-factors$scores
    
    input_data
    
  })
  deletevars<-reactive({
    
    input_data<-input_second()
    ncols<-ncol(input_data)
    
    df_short<-input_data
    names(df_short) <- 1:ncol(input_data)
    
    
    df_info <- input_data %>% gather("Item", "Value") 
    df_info$Item <- as.factor(df_info$Item)
    df_info <- df_info %>% group_by(Item) %>% summarise(mean = mean(Value)) %>% mutate(index = 1:ncol(input_data))
    
    
    factors <- fa(df_short,nfactors = input$nfactors,rotate = as.character(input$rotation),fm=as.character(input$fm), n.iter = 2)
    factors<-fa.sort(factors,polar=FALSE)
    factors<-print(factors$loadings,cutoff = round(input$cutof))
    
    class(factors) <- "matrix"
    loads <- data.frame(factors)
    
    low_variables <- rownames(loads[apply(loads, 1, function(row) {all(row < input$cutof)}),]) %>% as.numeric()
    
    
    for(i in 1:ncol(loads)) {
      
      loads[[i]][which(loads[[i]] < input$cutof)] <- NA
      
    }
    
    loads_table <- loads %>% mutate(i = as.numeric(rownames(loads))) %>% arrange(i) %>% cbind(df_info[,1:2])%>% gather("Factor", "Loading",-i, -Item,-mean) %>% na.omit() %>% mutate_if(is.character,as.factor) %>% mutate_if(is.numeric,round,2)
    rownames(loads_table) <- loads_table$i
    factor_table <- browsable(datatable(loads_table[,-1], class = 'cell-border stripe', filter = 'top'))
    
    df_cutted <- df_short[,-low_variables]
    
    df_cutted
  })
  output$chart1 <- renderUI({
    
    if(input$formatting == "Factorize items"){
      
      input_data <- factorize()
      
    } else if(input$formatting == "Delete low items") {
      
      input_data <- deletevars()
      
    } else {
      
      input_data <- input_second()
      
      if (input$standardized == "Standardized") {
        
        input_data<-scale(input_data)
        
      } else {
        
      }      
    }
    
    valid_test <- clValid(input_data, c(input$ncluster[1]:input$ncluster[2]),
                          
                          clMethods = input$cluster_alg,
                          validation = input$val_methods, verbose = TRUE,
                          metric = input$distance,
                          method = input$linkage,
                          maxitems = nrow(input_data),
                          neighbSize = input$neighbours
    )
    
    output<-valid_test@measures %>% round(2)
    
    dimension<-dim(output)[3]
    method<-attributes(output)$dimnames[[3]]
    
    frame<-vector(mode = "list", dimension)
    
    for (i in 1:dimension) {
      
      temp<-output[1:3,1:(input$ncluster[2]-input$ncluster[1]+1),i] %>% as.data.frame() %>% mutate(method = method[[i]], approach = rownames(output))
      names(temp) <- c(as.character((input$ncluster[1]):input$ncluster[2]), "method","approach")
      frame[[i]] <- assign(method[[i]], temp)
      
    }
    
    data <- Reduce(function(...) merge(..., all=T), frame) %>% arrange(method)
    
    data <- data %>% gather("cluster","value",-c("method","approach"))
    data$cluster <- factor(data$cluster,ordered = T, levels = c(input$ncluster[1]:input$ncluster[2]))
    data$method <- factor(data$method)
    data$approach <- factor(data$approach)
    
    input <- data
    levels <- levels(input$approach) %>% as.vector()
    print(levels)  
    
    list<-vector(mode="list",3)
    
    for(x in 1:length(levels)) {
      
      list[[x]]<-input %>% filter(approach == levels[x]) %>%
        hchart("line", hcaes(x = cluster, y = value, group = method)) %>% 
        hc_title(text = levels[x]) %>% hc_tooltip(shared=T,table=T)
      
      
    }
    
    list<-list %>% hw_grid(ncol = 3) %>% browsable()
    
    print(list)  
  })
  output$clusterinput <- DT::renderDataTable({
    
    if(input$formatting == "Factorize items"){
      
      input_data <- factorize()
      
    } else if(input$formatting == "Delete low items") {
      
      input_data <- deletevars()
      
    } else {
      
      input_data <-input_second()
    }
    browsable(datatable(input_data, class = 'cell-border stripe', filter = 'top'))
    
  })
  
  # Page 3: Cluster Selection:  -----------------------------------------------
  
  cluster_columns <- reactive ({
    
    if(input$formatting_final == "Factorize items"){
      
      input_data <- factorize()
      
    } else if(input$formatting_final == "Delete low items") {
      
      input_data <- deletevars()
      
    } else {
      
      input_data <- input_second()
      
      if (input$standardized_final == "Standardized") {
        
        input_data<-scale(input_data)
        
      } else {
        
      }
      
    }
    
    
    output_list<- vector(mode="list", 1)
    
    
    
    if (input$distance_final == "euclidean") {
      
      input_data <- dist(input_data, method = "euclidean")
      
    } else if (input$distance_final == "manhattan") {
      
      input_data <- dist(input_data, method = "manhattan")
      
    } else if  (input$distance_final == "pearson") {
      
      c<-cor(t(as.matrix(input_data)), method="pearson", use = "complete.obs")
      print(c)
      input_data <- as.dist(1-c)
      
    } else if  (input$distance_final == "spearman") {
      
      c<-cor(t(as.matrix(input_data)), method="spearman")
      print(c)
      input_data <- as.dist(1-c)
      
    } else if  (input$distance_final == "kendall") {
      
      c<-cor(t(as.matrix(input_data)), method="kendall")
      print(c)
      input_data <- as.dist(1-c)
      
    } else if (input$distance_final == "cosine") {
      
      input_data <- as.dist(1-cosine(t(input_data)))
      
    } else if (input$distance_final == "gower") {
      
      input_data <- daisy(input_data, metric = "gower")
      
    }
    
    #set.seed(123)
    
    for (k in input$ncluster_final[1]:input$ncluster_final[2]) {
      
      if(input$cluster_alg_final == "kmeans") {
        
        output<-kmeans(input_data, centers = k,iter.max = 1000, diss=T)
        output<-output$cluster %>% sapply(as.factor) %>% as.data.frame()
        names(output) <- paste("kmeans-",k,"cluster", sep ="")
        
      }  else if(input$cluster_alg_final == "pam") {
        
        output<-pam(input_data, k = k, diss=T)
        output<-output$clustering %>% sapply(as.factor) %>% as.data.frame()
        names(output) <- paste("PAM-",k,"Cluster", sep ="")
        
      } else if(input$cluster_alg_final == "hierarchical") {
        
        output<-hclust(input_data, method = input$linkage_final, diss=T)
        output<-cutree(output, k = k) %>% sapply(as.factor) %>% as.data.frame()
        names(output) <- paste("Hierarchical-",k,"Cluster", sep ="")
        
      } else if(input$cluster_alg_final == "agnes") {
        
        output<-agnes(input_data, method = input$linkage_final, diss=T)
        output<-cutree(output, k = k) %>% sapply(as.factor) %>% as.data.frame()
        names(output) <- paste("Agnes-",k,"Cluster", sep ="")
        
      } else if(input$cluster_alg_final == "diana") {
        
        output<-diana(input_data, diss=T)
        output<-cutree(output, k = k) %>% sapply(as.factor) %>% as.data.frame()
        names(output) <- paste("Diana-",k,"Cluster", sep ="")
        
      }
      
      j<-(input$ncluster_final[2]+1)-k
      output_list[[j]]<-output
    }
    
    new<-do.call(cbind, output_list)
    return(new)
    
  })
  output$clusterdata <- DT::renderDataTable({
    
    input_data <- cluster_columns()
    
    new<-cbind(record = input_zero()[,1],input_data)
    
    datatable(data = new
              , extensions = 'Buttons'
              , options = list( 
                dom = "Blfrtip"
                , buttons = 
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ) ) # end of buttons customization
                
                # customize the length menu
                , lengthMenu = list( c(10, 20, -1) # declare values
                                     , c(10, 20, "All") # declare titles
                ) # end of lengthMenu customization
                , pageLength = 10
                
              ) # end of options
              
    )
    
    
  })
  
  # Page 4. Cluster Descriptives --------------------------------------------------------------------
  
  descriptive_set <- reactive({
    
    input_data <- cbind(input_zero(), cluster_columns(), Average = factor(rep("Average",nrow(input_zero()))))
    
    return(input_data)
    
  })
  output$group_var <- renderUI({
    
    selectizeInput(inputId = "group_var",
                   choices = names(descriptive_set()),
                   selected = "Q5",
                   label = "Select the variable to group with:")
    
  })
  output$descriptives <- renderHighchart({
    
    if(isempty(input$interest_variable) == T | isempty(input$group_var) == T) {
      
    } else {
      
      reformat <- function(input_data, variable, group_var="") {
        
        names<-names(input_data)
        names_temp <- c(names[grep(variable,names)],names[grep(group_var,names)])
        names_temp_2  <- c(names[grep(variable,names)])
        
        attributes<- attributes(input_zero())[[4]]
        attributes<- attributes[names(attributes) %in% names_temp_2]
        attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()
        
        attributes<-data.frame(Variable = names_temp_2, Item = attributes)
        
        input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame() %>% sapply(as.integer) %>% as.data.frame()
        names(input_data)[grepl(group_var,names(input_data))] <- "Grouping"
        
        input_data <- input_data %>% gather("Variable", "Value", - Grouping)
        
        input_data <- merge(input_data, attributes, by = "Variable", all.x = T) %>% select(Grouping, Variable, Item, Value)
        
        return(input_data)
        
      } 
      
      input_data <- cbind(descriptive_set(), Average2 = factor(rep("Average",nrow(descriptive_set()))))
      
      if(input$kpi_format == "Mean Absolute") {
        
        inputs_cluster<-reformat(input_data = input_data, variable = input$interest_variable, group_var = input$group_var) %>% group_by(Grouping, Variable, Item) %>%
          summarise(mean=mean(Value))
        
        inputs_cluster<-merge(inputs_cluster, glossars, by = "Variable", all.x=T) %>% select(Grouping, Overarching,Item,mean) %>% arrange(Grouping, Overarching) %>% mutate(mean = round(mean,2))
        inputs_cluster$Grouping <- factor(inputs_cluster$Grouping)
        inputs_cluster$Item <- factor(inputs_cluster$Item)
        inputs_cluster$Overarching <- factor(inputs_cluster$Overarching)
        
        min<-1
        max<-6
        
        fntltp <- JS("function(){
                     return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                     this.series.yAxis.categories[this.point.y] + ': <b>' +
                     Highcharts.numberFormat(this.point.value, 2)+'</b>';
                     ; }")
        
        
        cor_colr <- list( list(0, '#F8F5F5'),
                          list(1, '#2fbfa4'))
        
        highchart() %>%
          hc_add_series(data = inputs_cluster, type = "heatmap", hcaes(x = "Grouping", y = "Item", value = "mean")) %>%
          hc_xAxis(categories = levels(inputs_cluster$Grouping), title = NULL) %>%
          hc_yAxis(categories = levels(inputs_cluster$Item), title = NULL)  %>%
          hc_plotOptions(
            series = list(
              boderWidth = 0,
              dataLabels = list(enabled = TRUE))) %>%
          hc_tooltip(formatter = fntltp) %>%
          hc_legend(align = "right", layout = "vertical",
                    margin = 0, verticalAlign = "top",
                    y = 25, symbolHeight = 280) %>%
          hc_colorAxis(stops= cor_colr,min=min,max=max) %>% hc_exporting(enabled=T)
        
    } else if (input$kpi_format == "Top 2 Absolute") {
      
      inputs_top2<-reformat(input_data = input_data, variable = input$interest_variable, group_var = input$group_var) 
      inputs_top2$Value<-factor(inputs_top2$Value)
      inputs_top2$Grouping <- factor(inputs_top2$Grouping)
      inputs_top2$Item <- factor(inputs_top2$Item)
      
      inputs_top2<-top2_values(inputs_top2)
      inputs_top2<-merge(inputs_top2, glossars, by = "Variable", all.x=T) %>% select(Grouping, Overarching,Item,Top2) %>% arrange(Grouping, Overarching) %>% mutate(Top2 = round(Top2,3)*100)
      
      inputs_top2$Overarching <- factor(inputs_top2$Overarching)
      inputs_top2$Grouping <- factor(inputs_top2$Grouping)
      inputs_top2$Item <- factor(inputs_top2$Item)
      
      
      fntltp <- JS("function(){
                   return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                   this.series.yAxis.categories[this.point.y] + ': <b>' +
                   Highcharts.numberFormat(this.point.value, 2)+'</b>';
                   ; }")
      
      
      cor_colr <- list( list(0, '#F8F5F5'),
                        list(1, '#c842f4')
      )
      
      min<- (0)
      max<-(100)
      
      highchart() %>%
        hc_add_series(data = inputs_top2, type = "heatmap", hcaes(x = "Grouping", y = "Item", value = "Top2")) %>%
        hc_xAxis(categories = levels(inputs_top2$Grouping), title = NULL) %>%
        hc_yAxis(categories = levels(inputs_top2$Item), title = NULL)  %>%
        hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE))) %>%
        hc_tooltip(formatter = fntltp) %>%
        hc_legend(align = "right", layout = "vertical",
                  margin = 0, verticalAlign = "top",
                  y = 25, symbolHeight = 280) %>%
        hc_colorAxis(stops= cor_colr,min=min,max=max) %>% hc_exporting(enabled=T)
      
  } else if  (input$kpi_format == "Top 2 Relative") {
    
    inputs_top2<-reformat(input_data = input_data, variable = input$interest_variable, group_var = input$group_var) 
    inputs_top2$Value<-factor(inputs_top2$Value)
    inputs_top2$Grouping <- factor(inputs_top2$Grouping)
    inputs_top2$Item <- factor(inputs_top2$Item)
    
    inputs_top2<-top2_values(inputs_top2) %>% select(Grouping, Item,Top2) %>% arrange(Grouping, Item) %>% mutate(Top2 = round(Top2,2))
    
    #inputs_top2$Overarching <- factor(inputs_top2$Overarching)
    inputs_top2$Grouping <- factor(inputs_top2$Grouping)
    inputs_top2$Item <- factor(inputs_top2$Item)
    
    inputs_average<-reformat(input_data = input_data, variable = input$interest_variable, group_var = "Average2")
    inputs_average$Value<-factor(inputs_average$Value)
    inputs_average$Grouping <- factor(inputs_average$Grouping)
    inputs_average$Item <- factor(inputs_average$Item)
    
    inputs_average<-top2_values(inputs_average) %>% 
      select(Item,Top2) %>% 
      mutate(TopAve = round(Top2,2)) %>% select(-Top2)
    
    inputs_average$Item <- factor(inputs_average$Item)
    
    inputs_final<-merge(inputs_top2,inputs_average, by = "Item", all.x=T) %>% 
      as.data.frame() %>%
      mutate(delta = (round(Top2-TopAve,3)*100))
    
    
    
    fntltp <- JS("function(){
                 return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                 this.series.yAxis.categories[this.point.y] + ': <b>' +
                 Highcharts.numberFormat(this.point.value, 2)+'</b>';
                 ; }")
    
    
    cor_colr <- list( list(0, '#e83030'),
                      list(0.5, '#F8F5F5'),
                      list(1, '#008c3a')
    )
    
    min<- (-50)
    max<-(50)
    
    highchart() %>%
      hc_add_series(data = inputs_final, type = "heatmap", hcaes(x = "Grouping", y = "Item", value = "delta")) %>%
      hc_xAxis(categories = levels(inputs_final$Grouping), title = NULL) %>%
      hc_yAxis(categories = levels(inputs_final$Item), title = NULL)  %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(formatter = fntltp) %>%
      hc_legend(align = "right", layout = "vertical",
                margin = 0, verticalAlign = "top",
                y = 25, symbolHeight = 280) %>%
      hc_colorAxis(stops= cor_colr,min=min,max=max) %>% hc_exporting(enabled=T)
    
} 
      
      }
    
    })
  
  output$average  <- renderHighchart({
    
    if(isempty(input$interest_variable) == T | isempty(input$group_var) == T) {
      
    } else {
      
      reformat <- function(input_data, variable, group_var="") {
        
        names<-names(input_data)
        names_temp <- c(names[grep(variable,names)],names[grep(group_var,names)])
        names_temp_2  <- c(names[grep(variable,names)])
        
        attributes<- attributes(input_zero())[[4]]
        attributes<- attributes[names(attributes) %in% names_temp_2]
        attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()
        
        attributes<-data.frame(Variable = names_temp_2, Item = attributes)
        
        input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame() %>% sapply(as.integer) %>% as.data.frame()
        names(input_data)[grepl(group_var,names(input_data))] <- "Grouping"
        
        input_data <- input_data %>% gather("Variable", "Value", - Grouping)
        
        input_data <- merge(input_data, attributes, by = "Variable", all.x = T) %>% select(Grouping, Variable, Item, Value)
        
        return(input_data)
        
      } 
      
      input_data <- cbind(descriptive_set(), Average2 = factor(rep("Average",nrow(descriptive_set()))))
      
      if(input$kpi_format == "mean") {
        
        inputs_cluster<-reformat(input_data = input_data, variable = input$interest_variable, group_var = input$group_var) %>% group_by(Grouping, Variable, Item) %>%
          summarise(mean=mean(Value))
        
        inputs_cluster<-merge(inputs_cluster, glossars, by = "Variable", all.x=T) %>% select(Grouping, Overarching,Item,mean) %>% arrange(Grouping, Overarching, Item) %>% mutate(mean = round(mean,2))
        inputs_cluster$Grouping <- factor(inputs_cluster$Grouping)
        inputs_cluster$Item <- factor(inputs_cluster$Item)
        inputs_cluster$Overarching <- factor(inputs_cluster$Overarching)
        
        min<-as.integer(min(inputs_cluster$mean))
        max<-as.integer(max(inputs_cluster$mean))
        
        
      } else if (input$kpi_format == "percent") {
        
        inputs_average<-reformat(input_data = input_data, variable = input$interest_variable, group_var = "Average2")
        inputs_average$Value<-factor(inputs_average$Value)
        inputs_average$Grouping <- factor(inputs_average$Grouping)
        inputs_average$Item <- factor(inputs_average$Item)
        
        inputs_average<-top2_values(inputs_average)
        inputs_average<-merge(inputs_average, glossars, by = "Variable", all.x=T) %>% 
          select(Overarching, Item,Top2) %>% 
          mutate(TopAve = round(Top2,2)) %>% select(-Top2)
        
        inputs_average$Item <- factor(inputs_average$Item)
        
      }
      
      
      hchart(inputs_average, "bar", hcaes(x=Item, y=TopAve, group = Overarching)) %>%hc_exporting(enabled=T)
    }
    
  })
  output$ns <- renderHighchart({
    
    if(isempty(input$interest_variable) == T | isempty(input$group_var) == T) {
      
    } else {
      
      reformat <- function(input_data, variable, group_var="") {
        
        names<-names(input_data)
        names_temp <- c(names[grep(variable,names)],names[grep(group_var,names)])
        names_temp_2  <- c(names[grep(variable,names)])
        
        attributes<- attributes(input_zero())[[4]]
        attributes<- attributes[names(attributes) %in% names_temp_2]
        attributes<- sub(".*:(.*) - .*", "\\1", attributes) %>% as.vector()
        
        attributes<-data.frame(Variable = names_temp_2, Item = attributes)
        
        input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame() %>% sapply(as.integer) %>% as.data.frame()
        names(input_data)[grepl(group_var,names(input_data))] <- "Grouping"
        
        input_data <- input_data %>% gather("Variable", "Value", - Grouping)
        
        input_data <- merge(input_data, attributes, by = "Variable", all.x = T) %>% select(Grouping, Variable, Item, Value)
        
        return(input_data)
        
      } 
      
      input_data <- cbind(descriptive_set(), Average2 = factor(rep("Average",nrow(descriptive_set()))))
      
      names<-names(input_data)
      names_temp <- names[grep(input$group_var,names)]
      
      input_data<-input_data[,names(input_data) %in% names_temp] %>% as.data.frame()
      names(input_data) <- "Grouping"
      input_data <- input_data %>% arrange(Grouping)
      input_data$Grouping <- as.character(input_data$Grouping)
      
      input_data <- input_data %>% group_by(Grouping) %>% summarise(n=n())
      
      hchart(input_data,"bar", hcaes(x=Grouping,y=n, color=Grouping)) %>% hc_exporting(enabled=T)
      
      
    }
    
  })
  
  }

shinyApp(ui, server)


#Deployment --------------
#library(rsconnect)
#rsconnect::deployApp("C:/Users/Florian Gehrig/Documents/EFA Dashboard")
