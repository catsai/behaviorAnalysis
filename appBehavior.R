##
# This is a Shiny Application to analyze behavioral data.
# This application can analyze Open Field, Novel Object Recognition,
# and Novel Object Location experiments from the Heller lab tracking
# software.
# Made by Connie Tsai 2018
##

## Load appropriate libraries
library(shiny)
library(DT)

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rhandsontable)

source("openField.R", local = T)
source("novelObject.R", local = T)
source("barnesMaze.R", local = T)


## Define UI 
ui <- fluidPage(
  
  # Application Title
  titlePanel("Mouse Behavior Analysis for Heller Lab"),
  
  # Sidebar Layout of Application
  sidebarLayout(
    # Side panel allows for input of experiment info and downloading of files
    sidebarPanel(
      selectInput("expType", "Choose experiment type",
                  c("Open Field", "Novel Object Recognition", "Novel Object Location", "Barnes Maze")),
      uiOutput("fileUpload"),
      uiOutput("fileDownload")
    ),
    # Main panel shows cleaned and analyzed data
    mainPanel(
      uiOutput("tabPresentation")
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  
  # Dynamic file upload UI option
  okFileTypes <- c("text/csv", "text/plain", ".csv", ".xls")
  output$fileUpload <- renderUI({
    if(is.null(input$expType))
      return()
    switch(input$expType,
           "Open Field" = list(fileInput("ofFile", "Select Open Field file:", accept = okFileTypes)),
           "Novel Object Recognition" = list(numericInput("expDuration", "Experiment Length in Minutes:", 10),
                                             fileInput("norTrainFile", "Select NOR training file:", accept = okFileTypes),
                                             fileInput("norTestFile", "Select NOR testing file:", accept = okFileTypes)),
           "Novel Object Location" = list(numericInput("expDuration", "Experiment Length in Minutes:", 10),
                                          fileInput("nolTrainFile", "Select NOL training file:", accept = okFileTypes),
                                          fileInput("nolTestFile", "Select NOL testing file:", accept = okFileTypes)),
           "Barnes Maze" = list(radioButtons("barnesFileType", label = h3("Training or Testing type?"),
                                             choices = list("Training" = "train", "Testing" = "test"), selected = "train"),
                                numericInput("escapeBox", "Escape Box Number", value = 5, min = 2, max = 21),
                                textInput("barnesLatency", label = h3("Input latencies in seconds:")),
                                fileInput("barnesFile", "Select Barnes Maze File", accept = okFileTypes))
    )
  })
  
  
  # Dynamic file downloaded UI option
  output$fileDownload <- renderUI({
    if(is.null(input$expType)){
      return()
    } else if(input$expType == "Open Field"){
      list(downloadButton("cleanOFData", "Download cleaned raw data to: "),
           downloadButton("analyzeOFData", "Download analyzed data to: "))
    } else if(input$expType == "Novel Object Recognition" || input$expType == "Novel Object Location") {
      list(downloadButton("cleanTrainData", "Download cleaned training data to: "),
           downloadButton("cleanTestData", "Download cleaned testing data to: "),
           downloadButton("analyzeTrainData", "Download analyzed training data to: "),
           downloadButton("analyzeTestData", "Download analyzed testing data to: "))
    } else if(input$expType == "Barnes Maze") {
      list(downloadButton("analyzedBarnes", "Download analyzed Barnes maze data to: "),
           downloadButton("cleanBarnes", "Download cleaned Barnes maze data to: "))
    }
  })
  
  # Download data as files
  output$cleanOFData <- downloadHandler(filename = function() {paste(input$expType, "_Cleaned", ".csv", sep = "")},
                                        content = function(file) {write.csv(dataFiles()$clean, file)})
  output$analyzeOFData <- downloadHandler(filename = function() {paste(input$expType, "_Analyzed", ".csv", sep = "")},
                                          content = function(file){write.csv(dataFiles()$analyzed, file)})
  output$cleanTrainData <- downloadHandler(filename = function() {paste(input$expType, "_Cleaned_Train", ".csv", sep = "")},
                                           content = function(file){write.csv(dataFiles()$cleanTrain, file)})
  output$cleanTestData <- downloadHandler(filename = function() {paste(input$expType, "_Cleaned_Test", ".csv", sep = "")},
                                          content = function(file){write.csv(dataFiles()$cleanTest, file)})
  output$analyzeTrainData <- downloadHandler(filename = function() {paste(input$expType, "_Analyzed_Train", ".csv", sep = "")},
                                             content = function(file){write.csv(dataFiles()$trainSummary, file)})
  output$analyzeTestData <- downloadHandler(filename = function() {paste(input$expType, "_Analyzed_Test", ".csv", sep = "")},
                                            content = function(file){write.csv(dataFiles()$testSummary, file)})
  output$analyzedBarnes <- downloadHandler(filename = function() {paste(input$expType, "_Analyzed", ".csv", sep = "")},
                                           content = function(file){write.csv(dataFiles()$analyzedBarnes, file)})
  output$cleanBarnes <- downloadHandler(filename = function() {paste(input$expType, "_Cleaned", ".csv", sep = "")},
                                        content = function(file){write.csv(dataFiles()$cleanBarnes, file)})
  
  
  # Dynamic analysis of data, returns cleaned and analyzed data as list of data frames
  dataFiles <- reactive({
    switch(input$expType,
           "Open Field" = if(!is.null(input$ofFile)) {
             openFieldData <- read.delim(input$ofFile$datapath, stringsAsFactors = F )
             openFieldResults <- analyzeOpenField(openFieldData, input$expDuration)
             return(openFieldResults)
           },
           "Novel Object Recognition" = if(!is.null(input$norTrainFile) && !is.null(input$norTestFile)) {
             train <- read.delim(input$norTrainFile$datapath, stringsAsFactors = F)
             test <- read.delim(input$norTestFile$datapath, stringsAsFactors = F)
             validate(need(length(unique(paste(train$sn, train$animal, sep = ""))) ==
                             length(unique(paste(test$sn, test$animal, sep = ""))), 
                           "Please check that same number of animals run on both days"))
             NORresults <- analyzeNovelObject(train, test, input$expType, input$expDuration)
             return(NORresults)
           },
           "Novel Object Location" = if(!is.null(input$nolTrainFile) && !is.null(input$nolTestFile)) {
             train <- read.delim(input$nolTrainFile$datapath, stringsAsFactors = F)
             test <- read.delim(input$nolTestFile$datapath, stringsAsFactors = F)
             validate(need(length(unique(paste(train$sn, train$animal, sep = ""))) ==
                             length(unique(paste(test$sn, test$animal, sep = ""))), 
                           "Please check that same number of animals run on both days"))
             NOLresults <- analyzeNovelObject(train, test, input$expType, input$expDuration)
             return(NOLresults)
           },
           "Barnes Maze" = if(!is.null(input$barnesFile) && !is.null(input$barnesLatency)) {
             barnesData <- read.delim(input$barnesFile$datapath, stringsAsFactors = F)
             recLatency <- unlist(strsplit(input$barnesLatency, split = "[[:punct:]]+|[[:space:]]+"))
             recLatency <- as.numeric(recLatency[recLatency != ""])
             barnesResults <- analyzeBarnesData(barnesData, input$barnesFileType, input$escapeBox, recLatency)
             return(barnesResults)
           }
    )
  })
  
  # # Graph analyzed data - still testing ideas on how to update graph
  # output$novelBoxPlot <- renderPlot({
  #   if(is.null(dataFiles())) {
  #     return()
  #   }
  #   thisData <- dataFiles()$analyzed
  #   thisData$genotype <- as.factor(thisData$genotype)
  #   thisEndTime <- min(input$exploreTime, ncol(thisData)-12)
  #   if(length(input$analyzedTable_rows_selected)){
  #     removeRows <- input$analyzedTable_rows_selected
  #     thisData <- thisData[-removeRows, ]
  #   }
  #   
  #   yName <- paste("Preference at ", as.character(input$exploreTime), " Minutes")
  #   ggplot(thisData, aes(x = thisData$genotype, y = thisData[,(12+thisEndTime)], color = thisData$genotype)) +
  #     geom_boxplot() + geom_jitter(width = .2) + theme_bw() + labs(x = "Genotype", y = yName)
  # })
  
  
  # Analyze data and create presentation
  output$tabPresentation <- renderUI({
    # Analyze data
    currData <- dataFiles()
    
    # Render analyzed data as table
    options(DT.options = list(dom = "Bt", autoFill = list(columns = 1, focus = "click"),
                              scrollX = T, scrollY = "40vh", scrollCollapse = T, paging = F,
                              fixedHeader = T, fixedColumns = T, keys = T, buttons = c("copy")))
    output$analyzedTable <- renderDT(currData$analyzed, editable = T, server = T,
                                     extensions = c("Buttons", "FixedHeader", "FixedColumns", "AutoFill", "KeyTable"))
    # Make analyzed data table editable
    proxyTable = dataTableProxy('analyzedTable')
    observeEvent(input$analyzedTable_cell_edit, {
      info = input$analyzedTable_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      analyzedTable[i, j] <<- DT::coerceValue(v, analyzedTable[i, j])
      replaceData(proxyTable, analyzedTable, resetPaging = FALSE)
    })
    
    # Render tab panels to visualize analyzed and cleaned data
    if(input$expType == "Open Field") {
      tabsetPanel(tabPanel("Analyzed", DTOutput("analyzedTable")),
                  tabPanel("Cleaned", renderDT(currData$clean, 
                                               extensions = c("Buttons", "FixedHeader", "FixedColumns"))))
    } else if(input$expType == "Novel Object Recognition" || input$expType == "Novel Object Location") {
      tabsetPanel(tabPanel("Analyzed", DTOutput("analyzedTable")),
                           # # Graph analyzed data - still testing ideas on how to update graph
                           # fluidRow(column(7, DTOutput("analyzedTable")), 
                           #          column(5, list(
                           #            sliderInput(inputId = "exploreTime", label = "Time", min = 1, max = input$expDuration, step = 1, value = 5),
                           #            plotOutput("novelBoxPlot"))))),
                  tabPanel("Cleaned",
                           fluidRow(column(6, renderDT(currData$cleanTrain, caption = "Training", 
                                                       extensions = c("Buttons", "FixedHeader", "FixedColumns"))),
                                    column(6, renderDT(currData$cleanTest, caption = "Testing",
                                                       extensions = c("Buttons", "FixedHeader", "FixedColumns"))))))
    } else if(input$expType == "Barnes Maze") {
      tabsetPanel(tabPanel("Analyzed", renderDT(currData$analyzedBarnes, caption = "Analyzed", 
                                                extensions = c("Buttons", "FixedHeader", "FixedColumns"))),
                  tabPanel("Cleaned", renderDT(currData$cleanBarnes, caption = "Cleaned")))
    }
  })
}




# Run the application
shinyApp(ui = ui, server = server)
