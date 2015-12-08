library(animation)
library(gplots)
library(shiny)
library(heatmap3)

ui <- fluidPage(
  titlePanel("Chess Results"),
  
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(condition="input.tabs==2",
                       radioButtons("player", label = h3("Player"),
                                    choices = list("White" = 1, "Black" = 0),selected = 1),
                       
                       radioButtons("piece", label = h3("Piece"),
                                    choices = list("Pawn" = 5, "Knight" = 1,
                                                   "Bishop" = 2,"Rook" = 0,
                                                   "Queen" = 3, "King" = 4),
                                    selected = 5),
                       
                       sliderInput("round", "Turn:",
                                   min = 1, max = 65, value = 1, step = 1,animate=TRUE))
      
    ),
    #fluidRow(NULL),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Results", value=1, plotOutput("winScatter")),
        tabPanel("Moves", value=2, plotOutput("heatmap")),
        id = "tabs"
        )
      #textOutput("status"),
      #plotOutput("winScatter"),
      #plotOutput("heatmap")
    )
  )
)




server <- function(input, output) {
  
  source("elo.R")
  source("helper.R")
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 599)
  
  getBoard <- function(p,r) {
    board <- vector(mode="numeric",length = 8 * 8)
    start = ind(p,r - 1,1,1)
    print(paste("start is",start))
    for(i in 1:64)
    {
      board[i] = all[i + start - 1]
    }
    M = matrix(board, nrow=8, ncol=8, byrow = TRUE)
    print(M)
    return(M)
  }
  
  output$status <- renderText({
    paste("Player", input$player, "Piece", input$piece, "Round", input$round)
  })
  
  output$heatmap <- renderPlot({
    r <- input$round
    p <- as.numeric(input$piece) + 6 * as.numeric(input$player)
    board <- getBoard(p,r)
    heatmap3(apply(board,2,rev), Rowv=NA, Colv=NA, col = cm.colors(256), scale="none", margins=c(5,10), balanceColor=T,labRow=c(1,2,3,4,5,6,7,8),labCol=c('a','b','c','d','e','f','g','h'))
    
  })
  
  output$winScatter <- renderPlot({
    #hist(rnorm(input$num), col="green")
    par(new = FALSE)
    symbol = "*"
    plot(wins1, wins2,xlab="white ELO",ylab="black ELO",main="Games played by ELO rating",pch=symbol,xlim=elo_range,ylim=elo_range,col="green")
    par(new = TRUE)
    
    plot(draws1, draws2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="orange")
    par(new = TRUE)
    plot(losses1, losses2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="red")
    par(new = TRUE)
    line = seq(from = 1000, to = 3000, by = 25)
    plot(line, line,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="black")
  })
}

shinyApp(ui = ui, server = server)