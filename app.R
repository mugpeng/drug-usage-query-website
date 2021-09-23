##################################################
## Project: drug-usage-shiny-page
## File name: app.R
## Date: Thu Sep  9 21:20:46 2021
## Author: Peng
## Email: mugpeng@foxmail.com
## R_Version: R version 4.0.5 (2021-03-31)
## R_Studio_Version: 1.4.1106
## Platform Version: macOS Mojave 10.14.6
##################################################

my_packages <- c("shiny", "plotly", "ggplot2")
tmp <- sapply(my_packages, function(x) library(x, character.only = T)); rm(tmp)

# read table ----
phar_data <- readRDS("./data/phar_data.rds")
phar_data_all <- readRDS("./data/phar_data_all.rds")
phar_discreption <- readRDS("./data/phar_discreption.rds")

phar_data_plot <- phar_data
phar_data_plot[phar_data_plot$分类 %in% "-",]$分类 <- "未知分类"
order_names <- names(table(phar_data_plot$分类)[order(table(phar_data_plot$分类))])
phar_data_plot$分类 <- factor(phar_data_plot$分类, levels = order_names)

## menu
tmp <- "麻婆豆腐 辣子鸡丁 东坡肘子 豆瓣鲫鱼 口袋豆腐 酸菜鱼 夫妻肺片 蚂蚁上树 叫化鸡 茄汁鱼卷 鱼香肉丝 干煸冬笋 魔芋烧鸭 锅贴鱼片 麻辣肉丁 鱼香茄饼 冬菜肉末 粉蒸鸡"
tmp2 <- "鱼香茄子、番茄炒蛋、小炒肉、炒土豆丝、麻婆豆腐、红烧排骨"
my_menu1 <- as.data.frame(strsplit(tmp, split = " "))
my_menu2 <- as.data.frame(strsplit(tmp2, split = "、"))
my_menu <- unique(unlist(unname(c(my_menu1, my_menu2))))

# Define UI ----
ui <- fluidPage(
  titlePanel("药品信息百科在线查询工具 v0.1"),
  fluidRow(
    p("欢迎你使用这个网站，🐂 项目地址在：", a("mugpeng/drug-usage-query-website", 
                               href = "https://github.com/mugpeng/drug-usage-query-website"),
      align = "center"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "select", strong("根据药物的分类进行筛选："), 
        choices = c("展示全部", "化学药品", "生物制品", 
                    "中成药", "中药")
      ),
      br(),
      p("这个网站目前包括现有在中国大陆上市的大部分药物的信息，主要包括它们的", 
        span("用法用量，药品说明书等。", style = "color:red")),
      br(),
      p("你可以通过右边的内容进行浏览，在界面", span("主要信息", style = "color:red"),
        "时，如果你点击表格中", span("编号", style = "color:red"),
        "对应的位置，可以看到该药详细的用法用量等介绍，你可以滑动弹出的内容，浏览全部内容。"),
      br(),
      p("我也在持续更新中，如果你在使用过程中有任何的问题，可以在上面的地址中提交问题，我也会及时作出回复，谢谢！"),
      br(),
      p("数据来源其他项目：", a("Censyu/yp-120ask-spider", href = "https://github.com/Censyu/yp-120ask-spider")),
      p("网站作者：", a("mugpeng", href = "https://github.com/mugpeng")),
      p("也欢迎你stars 我的其他项目～🌟"),
      br(),
      br(),
      actionButton("getQueue", "今天吃点什么好呢？"),
      textOutput("dish_name")
    ),
    mainPanel(
      verbatimTextOutput("devel"),
      tabsetPanel(
        # tabPanel("Plot", plotOutput("plot")), 
        # tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("主要信息", DT::dataTableOutput("phar_df")),
        tabPanel("全部信息", DT::dataTableOutput("phar_df_all")),
        tabPanel("数据可视化汇总", fluidRow(
          plotly::plotlyOutput("plot1"),
          p("使用鼠标，你可以放大上面的图像～", align = "center")
        )
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  dish_name <- eventReactive(input$getQueue, {
    sample(my_menu, 1)
  })
  output$dish_name <- renderPrint({
    dish_name()
  })
  my_data <- reactive({switch(input$select, 
                              "展示全部" = phar_data,
                              "化学药品" = subset(phar_data, 分类 == "化学药品"),
                              "生物制品" = subset(phar_data, 分类 == "生物制品"),
                              "中成药" = subset(phar_data, 分类 == "中成药"),
                              "中药" = subset(phar_data, 分类 == "中药"))
  })
  my_data2 <-  reactive({switch(input$select, 
                                "展示全部" = phar_data_all,
                                "化学药品" = subset(phar_data_all, 分类 == "化学药品"),
                                "生物制品" = subset(phar_data_all, 分类 == "生物制品"),
                                "中成药" = subset(phar_data_all, 分类 == "中成药"),
                                "中药" = subset(phar_data_all, 分类 == "中药"))
  })
  output$phar_df <- DT::renderDataTable({ 
    my_data()
  }, selection = 'single')
  output$phar_df_all <- DT::renderDataTable({ 
    my_data2()
  }, selection = 'single')
  output$devel <- renderPrint({
    req(length(input$phar_df_cell_clicked) > 0)
    tmp <- input$phar_df_cell_clicked
    if(tmp$value %in% phar_discreption$`编号`) {
      for ( i in c("包装", "用法用量", "性状", "网址")) {
        cat(paste0(sprintf("--%s--", i), "\n"))
        cat(paste0(phar_discreption[phar_discreption$`编号` == tmp$value, i],
                   "\n\n"))
      }
    } else print("没有信息，或是你没有点击编号对应的按钮，请重新试试！")
  })
  p1 <- reactive({ggplot2::ggplot(phar_data_plot) + geom_bar(aes(x = 分类, fill = 分类)) + theme_bw() + theme(panel.grid = ggplot2::element_blank(),
                                                                                                 axis.title = element_text(size = 14, face = "bold"), 
                                                                                                 axis.text = element_text(size = 14), 
                                                                                                 legend.title = element_text(size = 14)) + 
      paletteer::scale_fill_paletteer_d("RColorBrewer::Set3") + coord_flip() +
      labs(x = "", y = "")})
  output$plot1 <- plotly::renderPlotly({
    p1()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
