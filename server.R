
library(shiny)
library(ggplot2)
theme_set(theme_bw(base_family="HiraKakuProN-W3")) 

df <- read.csv("data/ssi_open_report_1.csv",
                 header=TRUE,
                 fileEncoding="CP932")
list.notdist <- c("集計対象医療機関数", "手術件数", "SSI件数", "SSI発生率") # データが単一の値のもののリスト
list.dist <- c("各医療機関の手術件数の分布", "各医療機関のSSI発生率の分布", "年齢") # データが分布のもののリスト

shinyServer(function(input, output) {
    output$selectYears <- renderUI({
        tagList(
            selectInput(
                inputId = "year", 
                label = "年", 
                choices = unique(df$Year), 
                selected = 2021,
                multiple = FALSE
            )
        )
    })
    
    output$selectCategory <- renderUI({
        tagList(
            selectInput(
                inputId = "category", 
                label = "大分類", 
                choices = c("全ての手術手技", unique(df[df$Year==input$year,]$大分類)),
                selected = "全ての手術手技",
                multiple = FALSE
            )
        )
    })
    
    output$selectData <- renderUI({
        tagList(
            selectInput(
                inputId = "dataname",
                label = "データ",
                choices = unique(df[df$Year==input$year,]$データ),
                selected = "SSI発生率",
                multiple = FALSE
            )
        )
    })
    
    output$selectClass <- renderUI({
        if(any(list.notdist == input$dataname)){
            
            tagList(
                selectInput(
                    inputId = "classname",
                    label = "種別",
                    choices = unique(df[df$Year==input$year &
                                            df$データ==input$dataname,]$クラス),
                    selected = unique(df[df$Year==input$year &
                                             df$データ==input$dataname,]$クラス)[1],
                    multiple = FALSE
                )
            )
            
        }
    })
    
    df.selected <- reactive({
        if(any(list.notdist==input$dataname)){
            
            if (input$category=="全ての手術手技"){
                
                df.selected <- df[df$Year==input$year &
                                      df$大分類!="1. 全手術手技合計" &
                                      df$データ==input$dataname &
                                      df$クラス==input$classname,]
                
            }else{
                
                df.selected <- df[df$Year==input$year &
                                      df$大分類==input$category &
                                      df$データ==input$dataname &
                                      df$クラス==input$classname,]
                
            }
            
        }else{
            
            if (input$category=="全ての手術手技"){
                
                df.selected <- df[df$Year==input$year &
                                      df$大分類!="1. 全手術手技合計" &
                                      df$データ==input$dataname,]
                
            }else{
                
                df.selected <- df[df$Year==input$year &
                                      df$大分類==input$category &
                                      df$データ==input$dataname,]
                
            }
            
        }
        
        df.selected$手術手技[df.selected$手術手技==""] <- NA
        df.selected <- na.omit(df.selected)
        return(df.selected)
    })
    
    output$outFile <- renderDataTable({
        data.frame(df.selected())
    })
    
    output$plot0 <- renderPlot({
        
        if (input$dataname=="SSI発生率"){
            label.val <- round(df.selected()$値, 1)
        }else if (input$dataname=="年齢"){
            label.val <- round(df.selected()$値, 0)
        }else{
            label.val <- df.selected()$値
        }
        
        if (any(c("手術件数", "各医療機関の手術件数の分布", "SSI件数") == input$dataname)){
            unit.str <- paste(input$dataname, "(件)")
        }else if (any(c("各医療機関のSSI発生率の分布", "SSI発生率") == input$dataname)){
            unit.str <- paste(input$dataname, "(%)")
        }else if ("年齢" == input$dataname){
            unit.str <- paste(input$dataname, "(歳)")
        }else{
            unit.str <- input$dataname
        }
        
        if(any(list.notdist==input$dataname)){
            
            ggplot(data.frame(df.selected()), aes(x=値, y=手術手技)) +
                geom_col() + 
                geom_text(aes(label=label.val, x=df.selected()$値), hjust=-0.1, size=input$fontsize/4) +
                labs(title=paste(input$dataname, "（", input$category, ": ", input$classname, "）", 
                                 sep=""), 
                     x=unit.str) +
                theme(plot.title=element_text(hjust=0.5),
                      text=element_text(size=input$fontsize))
            
        }else{
            
            ggplot(data.frame(df.selected()), aes(x=値, y=手術手技, col=クラス)) +
                geom_point() + 
                geom_text(aes(label=label.val, x=df.selected()$値), vjust=-0.5, size=input$fontsize/4) +
                labs(title=paste(input$dataname, "（", input$category, ": ", input$classname, "）", 
                                 sep=""), 
                     x=unit.str) +
                theme(plot.title=element_text(hjust=0.5),
                      text=element_text(size=input$fontsize))
            
        }
    },
    
    height = reactive(input$height * 100)
    )

})
