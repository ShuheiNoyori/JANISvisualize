
library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("院内感染対策サーベイランス 手術部位感染部門"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("selectYears"),
            
            uiOutput("selectCategory"),
            
            uiOutput("selectData"),
            
            uiOutput("selectClass"),
            
            sliderInput("height","グラフの縦幅", min=4, max=8, step=2, value=8),
            
            sliderInput("fontsize","グラフのフォントサイズ",min=16, max=20, step=2, value=16)
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot0")),
                        tabPanel("Table", dataTableOutput("outFile"))
            )
        )
    )
))
