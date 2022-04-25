library(shiny)
library(shinydashboard)
library(MASS)
library(dplyr)
library(DT)
library(plyr)
library(pathview)
library(png)

source("./SeqMADE_0223update.r")

org <- read.table("./org.txt",colClasses=c('character'),header = T)

################################################################################

header <- dashboardHeader(title = "SeqMADE")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dataset", tabName = "Dataset", icon = icon("table")),
    menuItem("Module", tabName = "Module", icon = icon("table")),
    menuItem("Case Group", tabName = "Case_Group", icon = icon("th")),
    menuItem("Control Group", tabName = "Control_Group", icon = icon("th")),
    menuItem("Result", tabName = "Result", icon = icon("table")),
    menuItem("Node", tabName = "Node", icon = icon("th")),
    menuItem("Fig", tabName = "Fig", icon = icon("image"))
  )
)


body <- dashboardBody(
  tabItems(
      tabItem(
        tabName = "Dataset",
        fluidRow(
          column(width = 4, 
               h2(textOutput("text1")),
               box(
                 fileInput("file1", "Choose Dataset File(txt)",
                         multiple = TRUE,
                         accept = c("text/txt",
                                    "text/comma-separated-values,text/plain",
                                    ".txt")),
                 width = NULL
               ),
               box(
                 checkboxInput("header1", "Header", TRUE),
                 width = NULL
               ),
               box(
                 radioButtons("sep1", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = "\t"),
                 width = NULL
               ),
               box(
                 radioButtons("quote1", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
                 width = NULL
               )
          ),
          column(width = 8,
               h2(textOutput("text2")),
               box(dataTableOutput("table1"), width = NULL))
        )
      ),
      
      tabItem(
        tabName = "Module",
        fluidRow(
          column(width = 4, 
               h2(textOutput("text3")),
               box(
                 fileInput("file2", "Choose Module File(txt)",
                           multiple = TRUE,
                           accept = c("text/txt",
                                      "text/comma-separated-values,text/plain",
                                      ".txt")),
                 width = NULL
               ),
               box(
                 checkboxInput("header2", "Header", TRUE),
                 width = NULL
               ),
               box(
                 radioButtons("sep2", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = "\t"),
                 width = NULL
               ),
               box(
                 radioButtons("quote2", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 width = NULL
               )
          ),
          column(width = 8, 
               h2(textOutput("text4")),
               box(dataTableOutput("table2"), width = NULL))
        )
      ),
    
      tabItem(
        tabName = "Case_Group",
        fluidRow(
          column(width = 4,
           h2(textOutput("text5")),
           box(numericInput("case_num_b", "The minimum column 
               number of case group", 1),width = NULL),
           box(numericInput("case_num_e", "The maximum column 
               number of case group", 1), width = NULL)
          ),
        
          column(width = 8,
            h2(textOutput("text6")),
            box(dataTableOutput("table3"),width = NULL)
          )
        )
      ),
  
      tabItem(
        tabName = "Control_Group",
        fluidRow(
          column(width = 4,
            h2(textOutput("text7")),
            box(numericInput("control_num_b", "The minimum column 
               number of control group", 1), width = NULL),
            box(numericInput("control_num_e", "The maximum column 
               number of control group", 1), width = NULL)
          ),
          column(width = 8,
            h2(textOutput("text8")),
            box(dataTableOutput("table4"), width = NULL)
          )
        )
      ),
    
      tabItem(
        tabName = "Result",
        fluidRow(
          sidebarPanel(
            box(
              h2(textOutput("text9")),
              actionButton(
              inputId = "bttn",
              label = h2("APPLY!"),
              style = "pill", 
              color = "danger"
            ),width = NULL),
        
            box(
              selectInput(inputId = "result", h2("Choose a dataset to download:"),
                      choices = c("result1", "result2", "result3")),
              h2(textOutput("text10")),
              downloadButton("downloadData", h2("Download")),
              width = NULL)
        
          ),
          mainPanel(
            column(width = 12,
              h2(textOutput("text11")),
              box(dataTableOutput("table5"), width = NULL)
            ),
          )
        )
      ),
      
      tabItem(
        tabName = "Node",
        fluidRow(
          column(width = 4,
            box(selectInput(inputId = "result2", h2("Choose a dataset:"),
                    choices = c("result1", "result2", "result3")),
              width = NULL),
            box(numericInput("fdr", "The maximum level of fdr", 0, min = 0, max = 1),
              step = 0.01, width = NULL),
            box(textInput(inputId = "search", "Search module in your dataset"),width = NULL),
            box(
              h2(textOutput("text12")),
              actionButton(
              inputId = "bttn2",
              label = h2("Go!"),
              style = "pill", 
              color = "danger",
              ),width = NULL
            ),
            box(uiOutput(outputId = "module"),width = NULL),
            box(uiOutput(outputId = "module2"),width = NULL),
            box(
              fileInput("file3", "If cannot find the module, please upload edge file.",
                        multiple = TRUE,
                        accept = c("text/txt",
                                   "text/comma-separated-values,text/plain",
                                   ".txt")),
              width = NULL
            ),
            box(downloadButton("downloadData2", h2("Download")),width = NULL)
          ),
          column(width = 8,
            h2(textOutput("text14")),
            box(dataTableOutput("table6"),width = NULL)
          )
        )
      ),
      
      tabItem(
        tabName = "Fig",
        fluidRow(
          column(width = 12,
               h2(textOutput("text15")),
               box(plotOutput(outputId = "plot", inline = TRUE),width = NULL)
          )
        )
      )
   )
)
ui <- dashboardPage(header, sidebar, body)

##########################################################################

server <- function(input, output) {
  exprs <- reactive({
    req(input$file1)
    data <- read.table(input$file1$datapath,
                       header = input$header1,
                       sep = input$sep1,
                       quote = input$quote1)
    data <- dplyr::filter(data, !duplicated(data[,1]))
    return(data)
  })
  
  networkModule <- reactive({
    req(input$file2)
    data <- read.table(input$file2$datapath,
                       header = input$header2,
                       sep = input$sep2,
                       quote = input$quote2)
    data <- dplyr::filter(data, !duplicated(data[,1]))
    return(data)
  })
  
  edge <- reactive({
    req(input$file3)
    data <- read.table(input$file3$datapath,
                       header = TRUE,
                       sep = "\t",
                       )
    return(data)
  })
  
  output$table1 <- renderDataTable({
    
    dataset <- exprs()
    
    return(dataset)
    
  }
  , options = list(scrollX = TRUE, pageLength = 10))
  
  output$table2 <- renderDataTable({
    
    module <- networkModule()
    return(module)
  },
  options = list(scrollX = TRUE, pageLength = 10))
  
  output$table3 <- renderDataTable({
    
    req(input$case_num_b < input$case_num_e)
    
    dataset <- exprs()
    
    return(
      dataset[, input$case_num_b:input$case_num_e])
  },
  options = list(scrollX = TRUE, pageLength = 10))
  
  output$table4 <- renderDataTable({
    
    req(input$control_num_b < input$control_num_e)
    
    dataset <- exprs()
    
    return(
      dataset[, input$control_num_b:input$control_num_e])
  },
  options = list(scrollX = TRUE, pageLength = 10))
  
  datasetOutput <- reactive({
    exprs <- exprs()
    networkModule <- networkModule()
    case <- colnames(exprs)[input$case_num_b:input$case_num_e]
    control <- colnames(exprs)[input$control_num_b:input$control_num_e]
    
    N <- length(case) + length(control)
    factors <- Factor(exprs,case,control)
    modulematrix <- moduleMatrix(exprs,networkModule)
    result1 = nbGLM(factors,N,networkModule,modulematrix,distribution="NB")
    result2 = nbGLMdir(factors,N,networkModule,modulematrix,distribution="NB")
    result3 = nbGLMdirperm(exprs, case, control, factors,networkModule
                           ,modulematrix,1000,distribution="NB")
    result = list(result1 = result1, result2 = result2, result3 = result3)
    return(result)
  }
  )
  
  bttn_click <- 0
  output$table5 <- DT::renderDataTable({
    if(input$bttn > bttn_click){
      bttn_click <- bttn_click + 1
      dataset <- switch (input$result,
                         "result1" = datasetOutput()$result1,
                         "result2" = datasetOutput()$result2,
                         "result3" = datasetOutput()$result3
      )
      return(dataset)}
  }, options = list(scrollX = TRUE, pageLength = 10), rownames = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$result, ".txt", sep = "")
    },
    content = function(file) {
      dataset <- switch (input$result,
                         "result1" = datasetOutput()$result1,
                         "result2" = datasetOutput()$result2,
                         "result3" = datasetOutput()$result3
      )
      write.table(dataset, file, row.names = TRUE)
    }
  )
  
  bttn_click <- 0
  names <- reactive({
    if(input$bttn2 > bttn_click){
      bttn_click <- bttn_click + 1
      data1 <- switch (input$result2,
                       "result1" = datasetOutput()$result1,
                       "result2" = datasetOutput()$result2,
                       "result3" = datasetOutput()$result3
      )
      data2 <- dplyr::filter(data1, fdr <= input$fdr)
      names1 <- rownames(data2)
      modulename <- networkModule()[,1, drop = F]
      modulename["key"] <-apply(modulename[,1, drop=F], 1,
                                function(x) grepl(tolower(input$search),tolower(x)))
      names2 = dplyr::filter(modulename, key == TRUE)[,1]
      names <- intersect(names1, names2)
      return(names)
    }
  })
  
  bttn_click <- 0
  names_id <- reactive({
    if(input$bttn2 > bttn_click){
      modulename <- org
      modulename["key"] <- apply(org[,1, drop=F], 1,
                                function(x) grepl(tolower(input$search),tolower(x)))
      search_data <- dplyr::filter(modulename, key == TRUE)
      req(search_data)
      names_id <- apply(search_data, 1, function(x) 
                  paste(x["name"][[1]],x["id"][[1]]))
      names_id <- as.data.frame(names_id)
      return(names_id[,1])
    }
  })
  
  output$module <- renderUI({
      names <- names()
      if (length(names) == 0){names = "NULL"}
      selectInput("module", "Choose one module", choices = names)
  })
  
  output$module2 <- renderUI({
    names <- names_id()
    if (length(names) == 0){names = "NULL"}
    selectInput("module2", "Confirm the right module id", choices = names)
  })
  
  Node_total <- reactive({
    data <- data.frame(Gene = exprs()[,1])
    case <- colnames(exprs())[input$case_num_b:input$case_num_e]
    control <- colnames(exprs())[input$control_num_b:input$control_num_e]
    data["Direction"] <- apply(exprs(), 1, cal_direction, 
                               case = case, control = control)
    data["fc_change"] <- apply(exprs(), 1, cal_fc, 
                               case = case, control = control)
    rownames(data) <- data[,1]
    return(data)
  })
  
  node <- reactive({
    Module <- networkModule()
    colnames(Module) <- c("Module", "Genes")
    genes <- dplyr::filter(Module, Module == input$module)[,2]
    genelist <- strsplit(genes, ";")[[1]]
    data <- data.frame(Gene = genelist)
    result <- plyr::join(data, Node_total(), by="Gene", type = "left")
    return(result)
  })
  
  output$table6 <- DT::renderDataTable({
    req(length(names()) > 1 | names() !="NULL")
    nodetable <- node()
    return(nodetable)
  }, options = list(scrollX = TRUE, pageLength = 10), rownames = TRUE)
  
  
  output$plot <- renderImage({
    if (is.null(input$file3)){
      req(length(names_id()) > 1 | names_id() !="NULL")
      data <- Node_total()
      id <- substr(input$module2, nchar(input$module2)-4,nchar(input$module2))
      pathview(gene.data = data[, 2, drop =F], pathway.id =
                 id, species ="hsa", out.suffix = id, 
               gene.idtype =gene.idtype.list[1], kegg.native = T, same.layer = F)
      list(src = paste("./hsa", id, ".", id, ".png", sep=""),
           width = '100%')
    }
    else{
      req(input$file3)
      data1 <- data.frame(Gene = edge()[,1])
      node1 <- plyr::join(data1, Node_total(), by="Gene", type = "left")
      data2 <- data.frame(Gene = edge()[,2])
      node2 <- plyr::join(data2, Node_total(), by="Gene", type = "left")
      node <- merge(node1, node2, all = TRUE)
      node <- dplyr::filter(node, !duplicated(node[,1]))
      graph <- graph_from_data_frame(edge(), directed = TRUE, vertices=node)
      set.seed(50) 
      l<-layout.fruchterman.reingold(graph) 
      V(graph)$size <- degree(graph)*10+30  
      V(graph)$color <- ifelse(V(graph)$Direction == "1", "red", "green")
      V(graph)$label.color <- 'black' 
      E(graph)$arrow.size=1 
      png("./result.png")
      plot(graph, layout=l)
      dev.off()
      list(src = "./result.png",
           width = '100%')
    }
    
  }	, deleteFile = FALSE)
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(input$module, ".txt", sep = "")
    },
    content = function(file) {
      dataset <- node()
      write.table(dataset, file, row.names = TRUE)
    }
  )
  
  
  output$text1 <- renderText({
    "Choose your Dataset file"
  })
  output$text2 <- renderText({
    "Dataset Display"
  })
  output$text3 <- renderText({
    "Choose your Module file"
  })
  output$text4 <- renderText({
    "Module Display"
  })
  output$text5 <- renderText({
    "Choose your case group"
  })
  output$text6 <- renderText({
    "Case group display"
  })
  output$text7 <- renderText({
    "Choose your control group"
  })
  output$text8 <- renderText({
    "Control group display"
  })
  output$text9 <- renderText({
    "Press this button to analysis data"
  })
  output$text10 <- renderText({
    "Press this button to download the result you choose"
  })
  output$text11 <- renderText({
    "Result Display"
  })
  output$text12 <- renderText({
    "Press this button to search module"
  })
  output$text14 <- renderText({
    "Node data Display"
  })
  output$text15 <- renderText({
    "Figure Display"
  })
}


shinyApp(ui,server)
