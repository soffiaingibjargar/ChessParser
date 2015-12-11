library(animation)
library(gplots)
library(ggplot2)
library(shiny)
library(heatmap3)
library(reshape2)
library(scales)
library(RColorBrewer)

ui <- navbarPage(title = "Chess Results",
                #tabPanel(title = "Results",
                #  plotOutput("winScatter")),
                tabPanel(title = "Results",
                  sidebarLayout(
                    sidebarPanel(
                      width=4,
                      checkboxGroupInput("results", 
                                         label = h3("Game result"), 
                                         choices = list("White wins" = 1, 
                                                        "Draw" = 2, 
                                                        "Black wins" = 3,
                                                        "Show line" = 4),
                                         selected = c(1,2,3)),
                      helpText("Should we add text here....")
                      ),
                  mainPanel(
                    plotOutput("winScatter",
                      width = "700px", 
                      height = "700px"
                               ),
                    helpText("...or here?")
                    )
                  )
                ),
                tabPanel(title = "Moves",
                  sidebarLayout(
                    sidebarPanel(
                      radioButtons("player", label = h3("Player"),
                                   choices = list("White" = 1, "Black" = 0),selected = 1),
                       
                       radioButtons("piece", label = h3("Piece"),
                                    choices = list("Pawn" = 5, "Knight" = 1,
                                                   "Bishop" = 2,"Rook" = 0,
                                                   "Queen" = 3, "King" = 4),
                                    selected = 5),
                       
                      sliderInput("round", "Turn:",
                                   min = 1, max = 65, value = 1, step = 1,animate=TRUE)
                    ),
                    mainPanel(
                      plotOutput("heatmap")
                    )
                  )
                ),
                tabPanel(title = "Captures",
                         plotOutput("captures"),
                         plotOutput("cap_spaces"))
)




server <- function(input, output) {
  
  source("elo.R")
  source("helper.R")
  
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 599)
  
  getBoard <- function(p,r) {
    board <- vector(mode="numeric",length = 8 * 8)
    start = ind(p,r - 1,1,1)
    #print(paste("start is",start))
    for(i in 1:64)
    {
      board[i] = all[i + start - 1]
    }
    M = matrix(board, nrow=8, ncol=8, byrow = TRUE)
    #print(M)
    return(M)
  }
  
  #helpfunction
  output$status <- renderText({
    paste("Player", input$player, "Piece", input$piece, "Round", input$round)
  })
  
  output$heatmap <- renderPlot({
    
    my_palette <- colorRampPalette(c("white", "blue"))(n = 599)
    pairs.breaks = seq(from=0, to=1, length.out=600)
    print(pairs.breaks)
    #tryBreaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    r <- input$round
    p <- as.numeric(input$piece) + 6 * as.numeric(input$player)

    board <- getBoard(p,r)
    print(board)
    heatmap3(apply(board,2,rev), Rowv=NA, Colv=NA, col = my_palette, scale="none", margins=c(5,10), balanceColor=F,labRow=c(1,2,3,4,5,6,7,8),labCol=c('a','b','c','d','e','f','g','h'), add.expr = {abline(h=1.5);abline(h=2.5);abline(h=3.5);abline(h=4.5);abline(h=5.5);abline(h=6.5);abline(h=7.5);abline(h=0.5);abline(h=8.5);abline(v=0.5);abline(v=1.5);abline(v=2.5);abline(v=3.5);abline(v=4.5);abline(v=5.5);abline(v=6.5);abline(v=7.5);abline(v=8.5)}, breaks = pairs.breaks)
  })
  
  output$captures <- renderPlot({
    #heatmap(captures)
    my_palette <- colorRampPalette(c("white", "red"))(n = 1000)
    par(las=1)
    heatmap3(apply(captures,2,rev), Rowv=NA, Colv=NA, labRow = c('P','K','Q','B','N','R'),
             scale="none",col=my_palette,main="Frequency of captures by piece",
             xlab = "Captured Piece", ylab = "Capturing Piece", 
             axis(1,1:nc,labels= labCol,las= 2,line= -0.5 + offsetCol,tick= 0,cex.axis= cexCol,hadj=adjCol[1],padj=adjCol[2]))
  })
  
  output$cap_spaces <- renderPlot({
    my_palette <- colorRampPalette(c("white", "red"))(n = 1000)
    #add.expr = {abline(h=1.5);abline(h=2.5);abline(h=3.5);abline(h=4.5);abline(h=5.5);abline(h=6.5);abline(h=7.5);abline(h=0.5);abline(h=8.5);abline(v=0.5);abline(v=1.5);abline(v=2.5);abline(v=3.5);abline(v=4.5);abline(v=5.5);abline(v=6.5);abline(v=7.5);abline(v=8.5)}
    print(cap_spots)
    temp <- apply(cap_spots_m,2,rev)
    print(temp)
    print(class(temp))
    heatmap3(temp, Rowv=NA, Colv=NA, labRow = c('1','2','3','4','5','6','7','8'),scale="none",col=my_palette,main="Frequency of captures by space")
    #cap_spots$X <- with(cap_spots, reorder(X, X))
    #ggplot(melt(cap_spots), aes(variable, Name))
  })
  
  
  output$winScatter <- renderPlot({
    #hist(rnorm(input$num), col="green")
    print(input$results)
    par(new = FALSE)
    symbol = 19
    size = 0.75
    
    
    #whiteColor = "orange"
    #drawColor = "gray"
    #blackColor = "black"
  
    whiteColor = brewer.pal(8, "YlOrBr")[4]
    drawColor = brewer.pal(8, "Greys")[4]
    blackColor = "black"
    
    e = c()

    plot(e,xlab=NA,ylab=NA, yaxt = "n", xaxt = "n", main="Games played by ELO rating",pch=symbol,xlim=elo_range,ylim=elo_range,col="green",las=1,cex=size, bty="n", cex.main = 2)
    mtext(side = 2, expression(bold("Black ELO")), line = 3, las=1, adj=0, padj=-23, cex=1.3)
    mtext(side = 1, expression(bold("White ELO")), line = 2, adj=1, padj=1, cex=1.3)
    
    axis(side = 2, at = axTicks(1), labels = formatC(axTicks(1), big.mark = ".", format = "d"), las = 2)
    axis(side = 1, at = axTicks(1), labels = formatC(axTicks(1), big.mark = ".", format = "d"), las = 1)

    par(new = TRUE)
    if(is.element("1", input$results))
    {
      plot(wins1, wins2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col=whiteColor,las=1,cex=size)
    }
    par(new = TRUE)
    if(is.element("3", input$results))
    {  
      plot(losses1, losses2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col=blackColor,las=1,cex=0.7)
    }
    par(new = TRUE)
    if(is.element("2", input$results))
    {
      plot(draws1, draws2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col=drawColor,las=1,cex=size)
    }
    par(new = TRUE)
    print(input$results)
    if(is.element("4", input$results))
    {  
      print("test")
      #line = seq(from = 1000, to = 3000, by = 25)
      line <- c(1000:3000)
      lines(line, line, lwd=2)
      #abline(0,1)
    }
    #line = seq(from = 1000, to = 3000, by = 25)
    #plot(line, line,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="black",las=1)
    par(new = TRUE)
    legend("topleft",
           legend=c("White wins", "Draw", "Black wins"),
           lty=c(0,0,0), pch=c(16, 16, 16), col=c(whiteColor, drawColor, blackColor),bty="n",cex=1.4)


  })
}

shinyApp(ui = ui, server = server)