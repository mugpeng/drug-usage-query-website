##################################################
## Project: Rescue the Princess
## File name: test.R
## Date: Fri Sep 10 07:13:11 2021
## Author: Peng
## Email: mugpeng@foxmail.com
## R_Version: R version 4.0.5 (2021-03-31)
## R_Studio_Version: 1.4.1106
## Platform Version: macOS Mojave 10.14.6
##################################################

library(shiny)

ui <- fluidPage(
  actionButton("getQueue", "Get list of queued files"),
  verbatimTextOutput("devel"),
  DT::dataTableOutput("fileList")     
)

shinyServer <- function(input, output) {
  
  tbl <- eventReactive(input$getQueue, {
    mtcars
  })
  
  output$fileList <- DT::renderDataTable({
    tbl()
  }, selection = 'single')
  
  output$devel <- renderPrint({
    req(length(input$fileList_cell_clicked) > 0)
    input$fileList_cell_clicked
  })
}

ui <- fluidPage(
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    tabPanel("page_1", 
             "Welcome!",
             actionButton("page_12", "next")
    ),
    tabPanel("page_2", 
             "Only one page to go",
             actionButton("page_21", "prev"),
             actionButton("page_23", "next")
    ),
    tabPanel("page_3", 
             "You're done!",
             actionButton("page_32", "prev")
    )
  )
)

server <- function(input, output, session) {
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
}

shinyApp(ui = ui, server = server)

shinyApp(ui = ui, server = shinyServer)

phar_data_all$`编号` <- paste0(1:nrow(phar_data_all)) 
phar_data_all$`编号` <- paste0(nchar(phar_data_all$`编号`), phar_data_all$`编号`)
phar_data_all$`编号` <- gsub("^1", "00000", phar_data_all$`编号`)
phar_data_all$`编号` <- gsub("^2", "0000", phar_data_all$`编号`)
phar_data_all$`编号` <- gsub("^3", "000", phar_data_all$`编号`)
phar_data_all$`编号`<- gsub("^4", "00", phar_data_all$`编号`)
phar_data_all$`编号` <- gsub("^5", "0", phar_data_all$`编号`)
phar_data_all$`编号` <- gsub("^6", "", phar_data_all$`编号`)
phar_data_all$`编号` <- paste0("MUST-MEDICATION-", phar_data_all$`编号`)

phar_data_all$包装 <- str_trim(phar_data_all$包装, side = "both")
phar_data_all$用法用量 <- str_trim(phar_data_all$用法用量, side = "both")
phar_data_all$性状 <- str_trim(phar_data_all$性状, side = "both")

phar_data <- phar_data_all[,c(18,6,16,17,15,1,2,5,7,12)]
phar_data_all2 <- phar_data_all[,c(18,6,16,17,15,1,2,5,7,12,3,4,8,10,11,14)]
phar_discreption <- phar_data_all[,c(18,3, 13, 9)]

