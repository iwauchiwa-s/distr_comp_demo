library(shiny)
library(shinyWidgets)

# created by S. S. v.1 2021/09/14

# Define UI
ui <- shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("全国学力テスト　正答数分布の比較"),
  
  # Sidebar with sliders
  sidebarPanel(
    
    tags$h5("（説明文....to be described...）"),
        
    tags$h4("平均と標準偏差の設定"),
    
    sliderInput("mean", label = h5("平均正答数:"), min = 0, 
                max = 20, step = 0.1, value = 8.5),
    
    sliderInput("stdev", label = h5("標準偏差:"), min = 0, 
                max = 10, step = 0.1, value = 3.0),
    
    br(),
    tags$h6("宮城教育大学"),
    tags$h6("（データクレジット...to be described...）"),
    br(),
    tags$h6("以下、アプリのプラン...."),
    tags$h6("例えば、教科の選択メニュー（未対応）"),
    selectInput("kyoka", "教科", 
                choices = list("国語" = 1, "算数" = 2))
    
  ),
  
  # Show plots
  mainPanel(
  	tags$h3("令和３年度　小学校国語　正答数分布"),
  	tags$h5("縦軸：割合、横軸：正答数"),
  	tags$h4("赤：全国、青：仙台市、緑：宮城県、黒：設定した分布"),
  	tags$h5("（このデータはダミーです）"),
  	plotOutput("graphA")
  )
  
))

server <- function(input, output) {

  # 全国データ
  elm_lang_all_mean <- 9.1
  elm_lang_all_stdev <- 3.1
  #仙台市データ
  elm_lang_sendai_mean <- 9.3
  elm_lang_sendai_stdev <- 2.8
  #宮城県データ
  elm_lang_miyagi_mean <- 8.9
  elm_lang_miyagi_stdev <- 3.5
  
  output$graphA <- renderPlot({

    curve(dnorm(x,elm_lang_all_mean,elm_lang_all_stdev),1,14, 
          col = "red", ylim=c(0,0.2), xlab = "", ylab="", lwd=3)
    curve(dnorm(x,input$mean,input$stdev),1,14, add = TRUE,lwd=3)
    
    # color shade
    n <- 1000 
    x <- seq(1, 14, length=n) 
    y1 <- dnorm(x,elm_lang_all_mean,elm_lang_all_stdev) 
    y2 <- dnorm(x,input$mean,input$stdev) 
    polygon( c(x,rev(x)), c(y1,rev(y2)), col="yellow")
    
    curve(dnorm(x,elm_lang_sendai_mean, elm_lang_sendai_stdev),1,14, add = TRUE, col ="blue",lwd=3)
    curve(dnorm(x,elm_lang_miyagi_mean, elm_lang_miyagi_stdev),1,14, add = TRUE, col ="green",lwd=3)
    
    
  })
}

shinyApp(ui, server)
