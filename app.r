library(shiny)
library(shinyWidgets)

# created by S. Sugawara v.1 2021/09/14

# Define UI
ui <- shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("全国学力調査　正答数分布の比較"),
  
  # Sidebar with sliders
  sidebarPanel(
    
    tags$h4("比較したい全国学力調査の結果をプルダウンメニューから選んでください"),
    
    selectInput("slct", "調査年度・教科の選択", 
                choices = list(
                  "R03_小6_国語 "= 1,
                  "R03_小6_算数 "= 2,
                  "R03_中3_国語 "= 3,
                  "R03_中3_数学 "= 4,
                  "H31_小6_国語 "= 5,
                  "H31_小6_算数 "= 6,
                  "H31 中3_国語 "= 7,
                  "H31 中3_数学 "= 8,
                  "H30_小6_国語A"= 9,
                  "H30_小6_国語B"= 10,
                  "H30_小6_算数A"= 11,
                  "H30_小6_算数B"= 12,
                  "H30_中3_国語A"= 13,
                  "H30_中3_国語B"= 14,
                  "H30_中3_数学A"= 15,
                  "H30_中3_数学B"= 16,
                  "H29_小6_国語A"= 17,
                  "H29_小6_国語B"= 18,
                  "H29_小6_算数A"= 19,
                  "H29_小6_算数B"= 20,
                  "H29_中3_国語A"= 21,
                  "H29_中3_国語B"= 22,
                  "H29_中3_数学A"= 23,
                  "H29_中3_数学B"= 24,
                  "H28_小6_国語A"= 25,
                  "H28_小6_国語B"= 26,
                  "H28_小6_算数A"= 27,
                  "H28_小6_算数B"= 28,
                  "H28_中3_国語A"= 29,
                  "H28_中3_国語B"= 30,
                  "H28_中3_数学A"= 31,
                  "H28_中3_数学B"= 32,
                  "H27_小6_国語A"= 33,
                  "H27_小6_国語B"= 34,
                  "H27_小6_算数A"= 35,
                  "H27_小6_算数B"= 36,
                  "H27_中3_国語A"= 37,
                  "H27_中3_国語B"= 38,
                  "H27_中3_数学A"= 39,
                  "H27_中3_数学B"= 40
                ), selected = 1
    ),
    
    tags$h4("自分の県・市町村・学校などの平均正答数と標準偏差を設定してください"),
    
    sliderInput("mean", label = h5("平均正答数:"), min = 0, 
                max = 30, step = 0.1, value = 0),
    
    sliderInput("stdev", label = h5("標準偏差:"), min = 0, 
                max = 10, step = 0.1, value = 0),
    
    br(),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    downloadButton('downloadReport'),
    br(),
    
    tags$h6("宮城教育大学"),
    tags$h6("（国立教育政策研究所の「全国学力・学習状況調査」の報告書および公表データを使用しています）"),
    br()
    
  ),
  
  # Show plots
  mainPanel(
    setBackgroundColor("peachpuff"),
    tags$h4("縦軸：生徒・児童数の割合、横軸：正答数"),
    tags$h4("赤：全国平均、黄：平均ゾーン、黒：設定した分布"),
    plotOutput("graphA")
  )
  
))

server <- function(input, output) {
  
  md <- reactive({
    # N、平均、トップ、ボトムのデータ
    if (input$slct == 1){
      # R3_小6_国語 ####
      av_jp <- 9.1
      sd_jp <- 3.1
      av_tp <-  10.0
      sd_tp <-  2.8
      av_bt <- 8.6
      sd_bt <-3.3
      nd <- 14
    }
    if (input$slct == 2){
      # R3_小6_算数 ####
      av_jp <- 11.3
      sd_jp <-  3.5
      av_tp <-  11.9
      sd_tp <-  3.1
      av_bt <- 10.8
      sd_bt <- 3.4
      nd <- 16
    }
    if (input$slct == 3){
      # R3_中3_国語 ####
      av_jp <- 9.1
      sd_jp <- 2.8
      av_tp <-  9.6
      sd_tp <- 2.8
      av_bt <- 8.4
      sd_bt <-3.0
      nd <- 14
    }
    if (input$slct == 4){
      # R3_中3_数学 ####
      av_jp <- 9.2
      sd_jp <- 3.7
      av_tp <-  10.1
      sd_tp <-  3.6
      av_bt <- 8.2
      sd_bt <-3.8
      nd <- 16
    }
    if (input$slct == 5){
      # H31_小6_国語 ###
      av_jp <- 9.0
      sd_jp <- 3.4
      av_tp <-  10.4
      sd_tp <-  2.9
      av_bt <- 8.2
      sd_bt <-3.5
      nd <- 14
    }
    if (input$slct == 6){
      # H31_小6_算数 ###
      av_jp <- 9.3
      sd_jp <- 3.1
      av_tp <-  10
      sd_tp <- 2.8
      av_bt <- 9
      sd_bt <-3.1
      nd <- 14
    }
    if (input$slct == 7){
      # H31 中3_国語 ###
      av_jp <- 7.3
      sd_jp <- 2.4
      av_tp <- 7.8
      sd_tp <- 2.1
      av_bt <- 6.8
      sd_bt <-2.5
      nd <- 10
    }
    if (input$slct == 8){
      # H31 中3_数学 ###
      av_jp <- 9.7
      sd_jp <- 4.2
      av_tp <-  10.6
      sd_tp <-  4.0
      av_bt <- 8.4
      sd_bt <-4.3
      nd <- 16
    }
    if (input$slct == 9){
      # H30_小6_国語A ##
      av_jp <- 8.5
      sd_jp <- 2.7
      av_tp <-  9.2
      sd_tp <- 2.4
      av_bt <- 8.1
      sd_bt <-2.8
      nd <- 12
    }
    if (input$slct == 10){
      # H30_小6_国語B ##
      av_jp <- 4.4
      sd_jp <- 1.9
      av_tp <-  4.9
      sd_tp <- 1.9
      av_bt <- 4.2
      sd_bt <-1.9
      nd <- 8
    }
    if (input$slct == 11){
      # H30_小6_算数A ##
      av_jp <- 8.9
      sd_jp <- 3.2
      av_tp <-  9.5
      sd_tp <- 3.0
      av_bt <- 8.5
      sd_bt <-3.2
      nd <- 14
    }
    if (input$slct == 12){
      # H30_小6_算数B ##
      av_jp <- 5.2
      sd_jp <- 2.7
      av_tp <-  5.9
      sd_tp <- 2.5
      av_bt <- 4.9
      sd_bt <-2.6
      nd <- 10
    }
    if (input$slct == 13){
      # H30_中3_国語A ##
      av_jp <-  24.4
      sd_jp <-  5.2
      av_tp <-  25.6
      sd_tp <-  4.4
      av_bt <-  22.9
      sd_bt <- 5.7
      nd <- 32
    }
    if (input$slct == 14){
      # H30_中3_国語B ##
      av_jp <- 5.6
      sd_jp <- 2.0
      av_tp <-  5.9
      sd_tp <- 1.8
      av_bt <- 5.2
      sd_bt <-2.1
      nd <- 9
    }
    if (input$slct == 15){
      # H30_中3_数学A ##
      av_jp <-  24.0
      sd_jp <-  8.1
      av_tp <-  25.9
      sd_tp <-  7.4
      av_bt <-  21.3
      sd_bt <- 8.4
      nd <- 36
    }
    if (input$slct == 16){
      # H30_中3_数学B ##
      av_jp <- 6.7
      sd_jp <- 3.5
      av_tp <-  5.5
      sd_tp <- 3.3
      av_bt <- 7.4
      sd_bt <-3.4
      nd <- 14
    }
    if (input$slct == 17){
      # H29_小6_国語A ##
      av_jp <-  11.2
      sd_jp <-  2.8
      av_tp <-  12.0
      sd_tp <-  2.3
      av_bt <-  10.8
      sd_bt <- 2.9
      nd <- 15
    }
    if (input$slct == 18){
      # H29_小6_国語B ##
      av_jp <- 5.2
      sd_jp <- 2.2
      av_tp <-  5.8
      sd_tp <- 2.0
      av_bt <- 4.9
      sd_bt <-2.2
      nd <- 9
    }
    if (input$slct == 19){
      # H29_小6_算数A ##
      av_jp <-  11.8
      sd_jp <-  3.1
      av_tp <-  12.7
      sd_tp <-  2.6
      av_bt <-  11.4
      sd_bt <- 3.2
      nd <- 15
    }
    if (input$slct == 20){
      # H29_小6_算数B ##
      av_jp <- 5.1
      sd_jp <- 2.6
      av_tp <- 5.8
      sd_tp <- 2.6
      av_bt <- 4.7
      sd_bt <-2.5
      nd <- 11
    }
    if (input$slct == 21){
      # H29_中3_国語A ##
      av_jp <-  24.9
      sd_jp <-  5.7
      av_tp <-  26.3
      sd_tp <-  4.7
      av_bt <-  23.0
      sd_bt <- 6.1
      nd <- 32
    }
    if (input$slct == 22){
      # H29_中3_国語B ##
      av_jp <- 6.5
      sd_jp <- 2.2
      av_tp <-  7.0
      sd_tp <- 1.9
      av_bt <- 6.0
      sd_bt <-2.4
      nd <- 9
    }
    if (input$slct == 23){
      # H29_中3_数学A ##
      av_jp <-  23.5
      sd_jp <-  8.4
      av_tp <-  26.2
      sd_tp <-  7.5
      av_bt <-  20.7
      sd_bt <- 8.7
      nd <- 36
    }
    if (input$slct == 24){
      # H29_中3_数学B ##
      av_jp <- 7.3
      sd_jp <- 3.3
      av_tp <- 8.2
      sd_tp <- 3.3
      av_bt <- 6.3
      sd_bt <-3.0
      nd <- 15
    }
    if (input$slct == 25){
      # H28_小6_国語A
      av_jp <-  11.0
      sd_jp <-  3.1
      av_tp <-  11.8
      sd_tp <-  2.9
      av_bt <-  10.5
      sd_bt <- 3.0
      nd <- 15
    }
    if (input$slct == 26){
      # H28_小6_国語B
      av_jp <- 5.8
      sd_jp <- 2.4
      av_tp <-  6.4
      sd_tp <- 2.3
      av_bt <- 5.5
      sd_bt <-2.5
      nd <- 10
    }
    if (input$slct == 27){
      # H28_小6_算数A
      av_jp <-  12.4
      sd_jp <-  3.4
      av_tp <-  13.2
      sd_tp <-  2.9
      av_bt <-  12.0
      sd_bt <- 3.5
      nd <- 16
    }
    if (input$slct == 28){
      # H28_小6_算数B
      av_jp <- 6.2
      sd_jp <- 2.8
      av_tp <-  7.0
      sd_tp <- 2.7
      av_bt <- 5.8
      sd_bt <-2.8
      nd <- 13
    }
    if (input$slct == 29){
      # H28_中3_国語A
      av_jp <-  25.1
      sd_jp <-  5.4
      av_tp <-  26.1
      sd_tp <-  4.5
      av_bt <-  23.5
      sd_bt <- 5.7
      nd <- 33
    }
    if (input$slct == 30){
      # H28_中3_国語B
      av_jp <- 6.0
      sd_jp <- 2.3
      av_tp <-  6.5
      sd_tp <- 2.0
      av_bt <- 5.7
      sd_bt <-2.4
      nd <- 9
    }
    if (input$slct == 31){
      # H28_中3_数学A
      av_jp <-  22.6
      sd_jp <-  8.3
      av_tp <-  25.0
      sd_tp <-  7.5
      av_bt <-  19.5
      sd_bt <- 8.2
      nd <- 36
    }
    if (input$slct == 32){
      # H28_中3_数学B ##
      av_jp <- 6.7
      sd_jp <- 3.6
      av_tp <- 7.6
      sd_tp <- 3.6
      av_bt <- 5.6
      sd_bt <-3.2
      nd <- 15
    }
    if (input$slct == 33){
      # H27_小6_国語A
      av_jp <- 9.8
      sd_jp <- 2.8
      av_tp <-  10.6
      sd_tp <-  2.5
      av_bt <- 9.3
      sd_bt <-2.9
      nd <- 14
    }
    if (input$slct == 34){
      # H27_小6_国語B
      av_jp <- 5.9
      sd_jp <- 2.4
      av_tp <-  6.9
      sd_tp <- 2.1
      av_bt <- 5.6
      sd_bt <-2.4
      nd <- 9
    }
    if (input$slct == 35){
      # H27_小_6_算数A
      av_jp <-  12.1
      sd_jp <-  3.4
      av_tp <-  13.0
      sd_tp <-  2.9
      av_bt <-  11.6
      sd_bt <- 3.6
      nd <- 16
    }
    if (input$slct == 36){
      # H27_小6_算数B
      av_jp <- 5.9
      sd_jp <- 3.0
      av_tp <-  6.7
      sd_tp <- 2.8
      av_bt <- 5.5
      sd_bt <-2.9
      nd <- 13
    }
    if (input$slct == 37){
      # H27_中3_国語A
      av_jp <-  25.2
      sd_jp <-  5.9
      av_tp <-  26.7
      sd_tp <-  5.0
      av_bt <-  23.1
      sd_bt <- 6.3
      nd <- 33
    }
    if (input$slct == 38){
      # H27_中3_国語B
      av_jp <- 6.0
      sd_jp <- 2.0
      av_tp <-  6.4
      sd_tp <- 1.7
      av_bt <- 5.5
      sd_bt <-2.0
      nd <- 9
    }
    if (input$slct == 39){
      # H27_中3_数学A
      av_jp <-  23.4
      sd_jp <-  8.0
      av_tp <-  25.6
      sd_tp <-  7.2
      av_bt <-  20.1
      sd_bt <- 7.9
      nd <- 36
    }
    if (input$slct == 40){
      # H27_中3_数学B ##
      av_jp <- 6.4
      sd_jp <- 3.8
      av_tp <- 7.2
      sd_tp <- 3.7
      av_bt <- 5.1
      sd_bt <-3.3
      nd <- 15
    }
    
    dname <- c(
      "R03_小6_国語 ",
      "R03_小6_算数 ",
      "R03_中3_国語 ",
      "R03_中3_数学 ",
      "H31_小6_国語 ",
      "H31_小6_算数 ",
      "H31 中3_国語 ",
      "H31 中3_数学 ",
      "H30_小6_国語A",
      "H30_小6_国語B",
      "H30_小6_算数A",
      "H30_小6_算数B",
      "H30_中3_国語A",
      "H30_中3_国語B",
      "H30_中3_数学A",
      "H30_中3_数学B",
      "H29_小6_国語A",
      "H29_小6_国語B",
      "H29_小6_算数A",
      "H29_小6_算数B",
      "H29_中3_国語A",
      "H29_中3_国語B",
      "H29_中3_数学A",
      "H29_中3_数学B",
      "H28_小6_国語A",
      "H28_小6_国語B",
      "H28_小6_算数A",
      "H28_小6_算数B",
      "H28_中3_国語A",
      "H28_中3_国語B",
      "H28_中3_数学A",
      "H28_中3_数学B",
      "H27_小6_国語A",
      "H27_小6_国語B",
      "H27_小6_算数A",
      "H27_小6_算数B",
      "H27_中3_国語A",
      "H27_中3_国語B",
      "H27_中3_数学A",
      "H27_中3_数学B"
    )
    
    # selected name for report
    dnm <- dname[as.integer(input$slct)]
    
    
    return(list(av_jp,sd_jp,av_tp,sd_tp,av_bt,sd_bt, nd, dnm))
    
  }) ## reactive wrap end
  
  
  
  
  output$graphA <- renderPlot({
    
    n <- 1000 
    x1 <- seq(1, md()[[7]], length=n) 
    # set vertical axis range
    mx1 <- max( dnorm(x1,md()[[1]],md()[[2]]))
    mx2 <- max( dnorm(x1,md()[[3]],md()[[4]]))
    mx3 <- max( dnorm(x1,md()[[5]],md()[[6]]))
    mx4 <- max( dnorm(x1,input$mean,input$stdev))
    mx <- max(mx1,mx2,mx3,mx4) * 1.1
    # トップとボトム
    curve(dnorm(x,md()[[3]],md()[[4]]),1,md()[[7]], 
          col = "gray", ylim=c(0,mx), xlab = "", ylab="", lwd=1)
    curve(dnorm(x,md()[[5]],md()[[6]]),1,md()[[7]], add = TRUE, col = "gray",lwd=1)
    # color shade
    # トップ
    y1 <- dnorm(x1,md()[[3]],md()[[4]]) 
    # ボトム
    y2 <- dnorm(x1,md()[[5]],md()[[6]]) 
    polygon( c(x1,rev(x1)), c(y1,rev(y2)), col="yellow")
    
    # 全国平均
    curve(dnorm(x,md()[[1]],md()[[2]]),1,md()[[7]], add = TRUE,
          col = "red", lwd=3)
    # 設定値
    curve(dnorm(x,input$mean,input$stdev),1,md()[[7]], add = TRUE,lwd=3)
    
    
  })
  
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      param <- list(
        slct = input$slct,
        av_set = input$mean,
        sd_set = input$stdev,
        d1 = md()[[1]],
        d2 = md()[[2]],
        d3 = md()[[3]],
        d4 = md()[[4]],
        d5 = md()[[5]],
        d6 = md()[[6]],
        d7 = md()[[7]],
        d8 = md()[[8]]
      )
      
      library(rmarkdown)
      out <- render('report.Rmd', param = param, switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
}

shinyApp(ui, server)
