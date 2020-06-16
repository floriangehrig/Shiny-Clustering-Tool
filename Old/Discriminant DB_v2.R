## app.R ##
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tagsinput)
library(pracma)
library(highcharter)
library(tidyverse)
library(caret)
library(foreign)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 3,
             box(width = 12,
               fileInput(inputId = "upload", "Upload your file and get started!"),
               tagsTextInput(inputId = "predictors", "Choose your predictor variables"),
               uiOutput("predictorsubset"),
               uiOutput("y"),
               pickerInput(inputId = "preprocess",
                           label = "Select the pre-processing steps",
                           choices = c("center", "scale", "YeoJohnson", "nzv", "pca"),
                           multiple = T
                           ),
               sliderInput(inputId = "trainsize", "Define the share of the training sample", 0, 1, 0.8),
               selectInput(inputId = "modeltype",
                           label = "Select your classification model:",
                           choices = c("lda","lda2","stepLDA"),
                           selected = "lda"),
               selectInput(inputId = "automation",
                           label = "Automatic Parameter Tuning:",
                           choices = c("Yes","No"),
                           selected = "Yes"),
               pickerInput(inputId = "ctrl_method",
                           label = "Select the re-sampling method",
                           choices = c("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV","none","oob","timeslice", "adaptive_cv", "adaptive_boot","adaptive_LGOCV"),
                           choicesOpt = list(subtext = c("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV","none","oob","timeslice", "adaptive_cv", "adaptive_boot","adaptive_LGOCV") ),
                           multiple = F),
               sliderInput(inputId = "valid_its", "Number of iterations", 1, 1000, 10),
               sliderInput(inputId = "cv_repeats", "Number of Cross-Validation repeats", 1, 1000, 10),
               sliderInput(inputId = "p_score", "Training % for LGOCV", 0, 1, 0.5),
               
               
    
               uiOutput("tweaking"),
               materialSwitch(inputId = "add_knob", label = "Additional subsetting"),
               uiOutput("additionals"),
               fluidRow(width = 12,
                 column(width=5,
                        actionButton("update_model","Update model")
                        ),
                 column(width=4,
                        actionButton("update_vars","Update variables")
                        )
               )

               
             )
             
          ),
      column(width=9,
             fluidRow(
               valueBoxOutput(width=3,"accuracy"),
               valueBoxOutput(width=3,"acc_no_inf_rate"),
               valueBoxOutput(width=3,"acc_p_value"),
               valueBoxOutput(width=3,"kappa")
               ),
             tabBox(width=12,
                    tabPanel(id="Tab1",
                             title="Classification Performance",
                             fluidRow(
                               column(width=6,highchartOutput("confusion", height = "600px")),
                               column(width=6,highchartOutput("group_kpis", height = "600px"))
                    )),
                    tabPanel(id="Tab2",
                             title = "Feature Importance",
                             highchartOutput("imp", height = "600px")
                    )

                 ))
    )
    
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  # Dynamic Model Inputs
  output$tweaking <- renderUI({
    
    model <- input$modeltype
    
    if (input$automation == "Yes") {
      
      sliderInput(inputId = "tunelength", "Define the Tuning Length:", 1, 100, 10)
      
    } else {
      
      
      if(model == "lda") {
        
        
      } else if (model == "lda2") {
        
        sliderInput(inputId = "lda2_dimen", "Number of Discriminant Functions:", 1, 15, 8)
        
      } else if (model == "stepLDA") {
        
        list(
          
          sliderInput(inputId = "stepLDA_maxvar", "Maximum Number of Variables:", 1, 100, 10),
          searchInput(
            inputId = "stepLDA_direction", 
            label = "Search Direction:", 
            btnSearch = icon("search"), 
            btnReset = icon("remove"), 
            width = "100%"
          )
          
        )
        
        
      } else {
        
      }
      
      
    }
    
    
    
  })
  
  # Dynamic Variable Inputs
  
  predictor_vars <- reactive({
    
    names<-names(input_data())
    
    if(isempty(input$predictors) == T) {
      
      return(NULL)
      
    } else {
      
      ids<-input$predictors
      variable<-strsplit(ids,",")[[1]]
      predictor_vars <- c()
      
      for (i in 1:length(variable)) {
        
        predictor_vars <- append(predictor_vars, names[grepl(variable[i],names)])
        
      }
      
      return(predictor_vars)
      
    }

    
  })
  
  output$predictorsubset <- renderUI({
    
    choice <- predictor_vars()
    
    pickerInput(
      inputId = "predictorsubset", 
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
  output$y <- renderUI({
    
    selectInput(
      inputId = "y", 
      label = "Select your Grouping Variable:", 
      choices = names(input_data())
      
      )
    
  })
  
  # Data Importing & Pre-Processing
  input_data <- reactive({
    
    upload<-req(input$upload)
    
    if(grepl(".sav", upload$datapath) == T) {
      
      read<-read.spss(upload$datapath, to.data.frame=TRUE, max.value.labels = 20)
      
    } else if (grepl(".csv",upload$datapath) == T) {
      
      read<-read.csv(upload$datapath)
      
    }
    
    
    return(read)
    
    
  })
  
  observeEvent(input$update_model, {
    
    
    input_subset <- reactive({
      
      data<-input_data()
      y<-isolate(input$y)
      
      excludes<-isolate(input$predictorsubset)
      vars <-isolate(predictor_vars())
      
      if(isempty(excludes) == F) {
        
        pre_data<-data[,which(names(data) %in% vars)]
        predictor_final<-pre_data[,-which(names(pre_data) %in% excludes)] %>% sapply(as.numeric) %>% as.data.frame()
        
        
      } else {
        
        predictor_final<-data[,which(names(data) %in% vars)]  %>% sapply(as.numeric) %>% as.data.frame()
        
      }
      
      
      y<-data[,names(data) %in% y] %>% as.vector() %>% factor()
      
      input_subset<-cbind(y = factor(y), predictor_final)
      
      return(input_subset)
      
    })
    data_processed <- reactive({
      
      data <- input_subset()
      
      if(isempty(isolate(input$preprocess)) == F) {
        
        set.seed(123)
        
        prcs<-preProcess(select(data, - y), 
                         method = isolate(input$preprocess))
        processed <- predict(prcs, select(data, - y))
        
        input_processed<-cbind(select(data, y), processed)
        
      } else {
        
        input_processed <- data
        
      }
      
 
      
      return(input_processed)
      
    })
    
    train_controls <-reactive({
      
      trainControl(method=input$ctrl_method,
                   number = input$valid_its,
                   repeats = input$cv_repeats,
                   savePredictions = "final",
                   search = "grid")
      
    })
    
    # Data Split-Up
    index <- reactive({
      
      data <- data_processed()
      
      index<-createDataPartition(data$y,
                                 p = input$trainsize,
                                 list = FALSE,
                                 times = 1)
      
    })
    
    train_data <- reactive({
      
      data <- data_processed() 
      
      train_data <- data[isolate(index()),]
      
      return(train_data)
      
    })
    test_data <- reactive({
      
      data <- data_processed()
      
      test_data <- data[-isolate(index()),]
      
      return(test_data)    
      
    })
    
    # Model Training & Prediction
    train_model <- reactive({
      
      data <- train_data()
      model<-input$modeltype
      
      if(model == "lda") {
        
        if(input$automation == "Yes") {
          
          train_model <- train(y ~ ., data = data, method = "lda")
          
        } else if (input$automation == "No") {
          
          train_model <- train(y ~ ., data = data, method = "lda")
          
        }
        
        
      } else {
        
      }
      
      return(train_model)
      
    })
    predictions <- reactive({
      
      data <- test_data()
      
      predictions <- predict(train_model(), select(data,-y))
      
      return(predictions)
      
    })
    confusion_matrix <- reactive({
      
      data<-test_data()
      
      confusion_matrix<-confusionMatrix(factor(predictions()), data$y)
      
      return(confusion_matrix)
      
    })
    
    
    output$confusion <- renderHighchart({
      
      data<-confusion_matrix()
      
      kpi_table<-data$table %>% as.data.frame()
      
      hchart(kpi_table, type = "heatmap", hcaes(x = Prediction, y = Reference, value = Freq)) %>%
        hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE)
          )) 
      
    })
    output$group_kpis <- renderHighchart({
      
      data<-confusion_matrix()
      
      kpi_group<-data$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% separate(rowname, c("Type", "Name"), ": ") %>% mutate(Class = factor(Name)) %>% select(-Type,-Name) 
      kpi_group<- kpi_group %>% gather("KPI","Value",-Class) %>% mutate_if(is.character, as.factor) %>% mutate(Value = round(Value,2))
      
      hchart(kpi_group,"line",hcaes(x=Class,y=Value, group="KPI")) %>% hc_yAxis(max = 1) %>% hc_tooltip(shared=T,table=T) %>%
        hc_chart(event= list(
          selection = JS(
            "function selectPointsByDrag(e) {
            
            // Select points
            Highcharts.each(this.series, function (series) {
            Highcharts.each(series.points, function (point) {
            if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
            point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
            point.select(true, true);
            }
            });
            });
            
            // Fire a custom event
            Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });
            
            return false; // Don't zoom
    }"
             )
          ),
          zoomType= "xy") 
      
    })
    output$accuracy <- renderValueBox({
      
      if(isempty(input$upload) == F) {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[1,1]*100,2)
        
        valueBox(
          paste(value,"%"), "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "green"
        )
        
      } else {
        
        valueBox(
          "-", "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "green"
        )
        
        
      }
      
    })
    output$acc_p_value <- renderValueBox({
      
      if(isempty(input$upload) == F) {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[6,1]*100,2)
        
        valueBox(
          paste(value,"%"), "P-Value", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "blue"
        )
        
      } else {
        
        valueBox(
          "-", "P-Value", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "blue"
        )
        
        
      }
      
      
    })
    output$acc_no_inf_rate <- renderValueBox({
      if(isempty(input$upload) == F) {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[5,1]*100,2)
        
        valueBox(
          paste(value,"%"), "No Information Rate", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "yellow"
        )
        
      } else {
        
        valueBox(
          "-", "No Information Rate", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "yellow"
        )
        
        
      }
      
      
      
    })
    output$kappa <- renderValueBox({
      
      if(isempty(input$upload) == F) {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[2,1]*100,2)
        
        valueBox(
          paste(value,"%"), "Kappa", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "red"
        )
        
      } else {
        
        valueBox(
          "-", "Kappa", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "red"
        )
        
        
      }
      
      
    })
    
    var_imp <- reactive({
      
      model<-train_model()
      data <- data_processed()
      
      var_imp <- varImp(model)
      
      imp<-var_imp[1] %>% as.data.frame() %>% tibble::rownames_to_column() %>% mutate(Variable = factor(rowname)) %>% select(-rowname)
      imp<- imp %>% gather("Class","Value",-Variable) %>% mutate_if(is.character,as.factor) %>% mutate(Value = round(Value,2)) 
      
      return(imp)
      
    })
    
    output$imp <- renderHighchart({
      
      imp <- var_imp() %>%
        group_by(Variable) %>% summarise(Value = mean(Value)) %>% arrange(desc(Value))
      
      hchart(imp, "bar", hcaes(x=Variable,y=Value, colour=Value))
      
    })
    output$additionals <- renderUI({
      
      if(input$add_knob == T) {
        
        list(
          selectInput(inputId = "topapproach", label="Select the approach:",choices = c("Average","Class-Specific")),
          sliderInput(inputId = "topvars", label="Select the most important:", 1, (ncol(input_subset())-1), 1)
        )
        
      } else {
        
      }
      
    })
    
    observeEvent(input$update_vars, {
      
      imp <- var_imp() 
      print(imp)
      imp <- imp %>%
        group_by(Variable) %>% summarise(Value = mean(Value)) %>% arrange(desc(Value)) %>% select(Variable) %>% sapply(as.character)
      
      imp <- names(input_subset())[which(!names(input_subset()) %in% imp[1:input$topvars])]
      imp <- c(imp[!grepl("^y$",imp)],input$predictorsubset)
      print(imp)
      updatePickerInput(session = session, inputId = "predictorsubset",
                        selected = imp)
      
    })
    
    
    

  })
  

  

  
  
}






shinyApp(ui, server)