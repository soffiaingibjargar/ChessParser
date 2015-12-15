library(animation)
library(gplots)
library(ggplot2)
library(shiny)
library(heatmap3)
library(reshape2)
library(scales)
library(RColorBrewer)
#library(shinyapps)

ui <- navbarPage(title = "Reykjavik Open 2009 - 2015",
                
                 tabPanel(title = "All",
                  sidebarLayout(
                    sidebarPanel(
                      width=3,
                      checkboxGroupInput("results", 
                                         label = h3("Game result"), 
                                         choices = list("White wins" = 1, 
                                                        "Draw" = 2, 
                                                        "Black wins" = 3,
                                                        "Show line" = 4),
                                         selected = c(1,2,3)),
                      br(),
                      radioButtons("radioRound", label = h3("Rounds"),
                                   choices = list("All" = 1, "By round" = 2),
                                   selected = 1),
                      br(),
                      conditionalPanel(
                        condition = "input.radioRound==2",
                        sliderInput("round", "Round:",
                                    min = 1, max = 10, value = 1, step = 1,animate=FALSE,ticks=F))
                      
                      ),
                  mainPanel(
                    plotOutput("winScatter",
                      width = "600px", 
                      height = "600px"
                      )
                    )
                  )
                ),
                tabPanel(title = "Results",
                  sidebarLayout(
                    sidebarPanel(
                      width=3,
                      textOutput("elo_explain"),
                      br(),
                      textOutput("elo_explain2")),
                  mainPanel(
                    plotOutput("elo_results"),
                    br(),
                    plotOutput("elo_length")
                    ))
                         
                  ),
                tabPanel(title = "Moves",
                  sidebarLayout(
                    sidebarPanel(
                      width=3,
                      radioButtons("player", label = h3("Player"),
                                   choices = list("White" = 1, "Black" = 0),selected = 1),
                      br(),
                       
                       radioButtons("piece", label = h3("Piece"),
                                    choices = list("Pawn" = 5, "Knight" = 1,
                                                   "Bishop" = 2,"Rook" = 0,
                                                   "Queen" = 3, "King" = 4),
                                    selected = 5),
                      br(),
                      
                      sliderInput("gameTurn", "Turn:",
                                   min = 1, max = 62, value = 1, step = 1,animate=TRUE,ticks=F)
                    ),
                    mainPanel(
                      fluidRow(
                      column(width=10, plotOutput("heatmap",
                                 width = "900px", 
                                 height = "600px"
                                 )),
                      column(width=2, img(src="colorLegendDemo.png",width="200px"))
                      )
                    )
                  )
                ),
                tabPanel(title = "Captures",
                         sidebarLayout(
                           sidebarPanel(
                             width=3,
                             radioButtons("norm", label = h3("Captures by pieces"),
                                          choices = list("Count all captures" = 1, "Scale captures" = 0),
                                          selected = 1),
                             br(),
                             textOutput("scale_explain"),
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             
                             radioButtons("pieces", label = h3("Piece being captured"),
                                          choices = list("All" = "A","Pawn" = "P", "Knight" = "N",
                                                         "Bishop" = "B","Rook" = "R",
                                                         "Queen" = "Q"),
                                          selected = "A")
                           ),
                           mainPanel(
                             fluidRow(
                               #column(width=6, offset=1.5, plotOutput("captures")),
                               #column(width=5, offset=0.2 , plotOutput("cap_spaces"))
                               br(),
                               column(width=12, align="center", plotOutput("captures",
                                                           width = "750", 
                                                           height = "550"),
                                      plotOutput("captures_legend", 
                                                 width="50%", height="170px")),
                               column(width=12, align="center", plotOutput("cap_spaces",
                                                           width = "750", 
                                                           height = "550"),
                                      plotOutput("cap_spaces_legend", 
                                                 width="50%", height="170px")
                                      )
                               #plotOutput("captures"),
                               #plotOutput("cap_spaces")
                              )
                             )
                           )
                         )
)




server <- function(input, output) {
  
  source("elo.R")
  source("helper.R")
  source("myHeatmap.R")
  
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 599)
  
  getBoard <- function(p,r) {
    board <- vector(mode="numeric",length = 8 * 8)
    start = ind(p,r - 1,1,1)
    #print(paste("start is",start))
    total = 0
    for(i in 1:64)
    {
      #print(i + start - 1)
      board[i] = all[i + start - 1]
      total <- total + board[i]
    }
    M = matrix(board, nrow=8, ncol=8, byrow = TRUE)
    #print(total)
    return(M)
  }
  
  #helpfunction
  output$status <- renderText({
    paste("Player", input$player, "Piece", input$piece, "Round", input$round)
  })
  
  
  output$heatmap <- renderPlot({
    
    my_palette <- colorRampPalette(c("white", "blue"))(n = 599)
    pairs.breaks <- seq(from=0, to=1, length.out=600)
    #myBreaks <- sqrt(sqrt(pairs.breaks))
    myBreaks <- pairs.breaks ^ (1/3)
    myBreaks <- 1 - myBreaks
    #print(pairs.breaks)
    #print(myBreaks)
    #tryBreaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    r <- input$gameTurn
    p <- as.numeric(input$piece) + 6 * as.numeric(input$player)

    board <- getBoard(p,r)
    
    #print(board)
    hTitle = heatmapTitle()
    chessHeatmap(apply(board,2,rev), Rowv=NA, Colv=NA, col = my_palette, scale="none", margins=c(5,10), balanceColor=F,labRow=c(1,2,3,4,5,6,7,8),labCol=c('a','b','c','d','e','f','g','h'), add.expr = {abline(h=1.5);abline(h=2.5);abline(h=3.5);abline(h=4.5);abline(h=5.5);abline(h=6.5);abline(h=7.5);abline(h=0.5);abline(h=8.5);abline(v=0.5);abline(v=1.5);abline(v=2.5);abline(v=3.5);abline(v=4.5);abline(v=5.5);abline(v=6.5);abline(v=7.5);abline(v=8.5)}, breaks = myBreaks, main = paste(hTitle, ", ", heatmapTotal(),sep=""))
    #testHeatmap(apply(board,2,rev), Rowv=NA, Colv=NA, col = my_palette, scale="none", margins=c(5,10), balanceColor=F,labRow=c(1,2,3,4,5,6,7,8),labCol=c('a','b','c','d','e','f','g','h'), add.expr = {abline(h=1.5);abline(h=2.5);abline(h=3.5);abline(h=4.5);abline(h=5.5);abline(h=6.5);abline(h=7.5);abline(h=0.5);abline(h=8.5);abline(v=0.5);abline(v=1.5);abline(v=2.5);abline(v=3.5);abline(v=4.5);abline(v=5.5);abline(v=6.5);abline(v=7.5);abline(v=8.5)}, breaks = myBreaks, main = hTitle)
  })
  output$captures <- renderPlot({
    if(is.element("1", input$norm))
    {
      my_palette <- colorRampPalette(c("white", brewer.pal(8, "PuOr")[8]))(n = 1000)
      par(las=1)
      myHeatmap(apply(captures / 6214,2,rev), Rowv=NA, Colv=NA, labRow = c('Pawn','King','Queen','Bishop','Knight','Rook'),
                scale="none",col=my_palette,main="Average number of captures in each game by piece",
                xlab = "Capturing Piece", ylab = expression(bold("Captured Piece")), cex.main = 2.2,
                axis(1,1:nc,labels= labCol,las= 2,line= -0.5 + offsetCol,tick= 0,cex.axis= cexCol,hadj=adjCol[1],padj=adjCol[2]))
    }
    else
    {
      my_palette <- colorRampPalette(c("white", brewer.pal(8, "PuOr")[8]))(n = 1000)
      par(las=1)
      myHeatmap(apply(captures_norm / 6214 ,2,rev), Rowv=NA, Colv=NA, labRow = c('Pawn','King','Queen','Bishop','Knight','Rook'),
                scale="none",col=my_palette,main="Average number of captures in each game by piece",
                xlab = "Capturing Piece", ylab = expression(bold("Captured Piece")), cex.main = 2.2, cex.lab=2,
                axis(1,1:nc,labels= labCol,las= 2,line= -0.5 + offsetCol,tick= 0,cex.axis= cexCol,hadj=adjCol[1],padj=adjCol[2]))
    }
    
  })
  output$captures_legend <- renderPlot({
    #heatmap(captures)

    my_palette <- colorRampPalette(c("white", brewer.pal(8, "PuOr")[8]))(n = 1000)
    par(las=1)
    if(is.element("1", input$norm))
    {
      dummy.x <- seq(min(captures / 6214, na.rm = TRUE), max(captures / 6214, na.rm = TRUE), 
                     length = length(my_palette))
    }
    else
    {
      dummy.x <- seq(min(captures_norm / 6214, na.rm = TRUE), max(captures_norm / 6214, na.rm = TRUE), 
                     length = length(my_palette))
    }
    dummy.z <- matrix(dummy.x, ncol = 1)
    image(x = dummy.x, y = 1, z = dummy.z, yaxt = "n", col = my_palette, xlab = "", cex = 0.2, ylab="")
  })
  output$cap_spaces_legend <- renderPlot({
    my_palette <- colorRampPalette(c("white", brewer.pal(8, "PuBu")[8]))(n = 1000)
    #add.expr = {abline(h=1.5);abline(h=2.5);abline(h=3.5);abline(h=4.5);abline(h=5.5);abline(h=6.5);abline(h=7.5);abline(h=0.5);abline(h=8.5);abline(v=0.5);abline(v=1.5);abline(v=2.5);abline(v=3.5);abline(v=4.5);abline(v=5.5);abline(v=6.5);abline(v=7.5);abline(v=8.5)}
    #print(cap_spots)
    if(is.element("A", input$pieces))
    {
      temp <- apply(cap_spots_m,2,rev)
    }
    else if(is.element("R", input$pieces))
    {
      temp <- apply(space_R,2,rev)
    }
    else if(is.element("N", input$pieces))
    {
      temp <- apply(space_N,2,rev)
    }
    else if(is.element("B", input$pieces))
    {
      temp <- apply(space_B,2,rev)
    }
    else if(is.element("Q", input$pieces))
    {
      temp <- apply(space_Q,2,rev)
    }
    #else if(is.element("K", input$pieces))
    #{
    #  temp <- apply(space_K,2,rev)
    #}
    else if(is.element("P", input$pieces))
    {
      temp <- apply(space_P,2,rev)
    }
    temp <- temp / 6214
    dummy.x <- seq(min(temp, na.rm = TRUE), max(temp, na.rm = TRUE), 
                   length = length(my_palette))
    dummy.z <- matrix(dummy.x, ncol = 1)
    image(x = dummy.x, y = 1, z = dummy.z, yaxt = "n", col = my_palette, xlab = "", cex = 0.2, ylab="")

  })
  output$cap_spaces <- renderPlot({
    my_palette <- colorRampPalette(c("white", brewer.pal(8, "PuBu")[8]))(n = 1000)
    #add.expr = {abline(h=1.5);abline(h=2.5);abline(h=3.5);abline(h=4.5);abline(h=5.5);abline(h=6.5);abline(h=7.5);abline(h=0.5);abline(h=8.5);abline(v=0.5);abline(v=1.5);abline(v=2.5);abline(v=3.5);abline(v=4.5);abline(v=5.5);abline(v=6.5);abline(v=7.5);abline(v=8.5)}
    #print(cap_spots)
    if(is.element("A", input$pieces))
    {
      temp <- apply(cap_spots_m,2,rev)
    }
    else if(is.element("R", input$pieces))
    {
      temp <- apply(space_R,2,rev)
    }
    else if(is.element("N", input$pieces))
    {
      temp <- apply(space_N,2,rev)
    }
    else if(is.element("B", input$pieces))
    {
      temp <- apply(space_B,2,rev)
    }
    else if(is.element("Q", input$pieces))
    {
      temp <- apply(space_Q,2,rev)
    }
    #else if(is.element("K", input$pieces))
    #{
    #  temp <- apply(space_K,2,rev)
    #}
    else if(is.element("P", input$pieces))
    {
      temp <- apply(space_P,2,rev)
    }
    #print(temp)
    print(class(temp))
    temp <- temp / 6214
    #testHeatmap(temp, Rowv=NA, Colv=NA, labRow = c('1','2','3','4','5','6','7','8'),scale="none",col=my_palette,main="Frequency of captures by space")
    chessHeatmap(temp, Rowv=NA, Colv=NA, labRow = c('1','2','3','4','5','6','7','8'),scale="none",col=my_palette,main="Average number of captures in each game by space")
    dummy.x <- seq(min(temp, na.rm = TRUE), max(temp, na.rm = TRUE), 
                   length = length(my_palette))
    dummy.z <- matrix(dummy.x, ncol = 1)
    #print("dummy.x is")
    #print(dummy.x)
    #par(new = TRUE)
    #image(x = dummy.x, y = 1, z = dummy.z, yaxt = "n", col = my_palette, xlab = "", cex = 0.4)
    #cap_spots$X <- with(cap_spots, reorder(X, X))
    #ggplot(melt(cap_spots), aes(variable, Name))
  })
  
  
  heatmapTitle <- function(){
    add = 0
    if(input$player == 0)
      currentPlayer = "black"
    else
    {
      currentPlayer = "white"
      add = add + 6
    }
    if(input$piece == 0)
      currentPiece = "rooks"
    else if(input$piece == 1)
      currentPiece = "knights"
    else if(input$piece == 2)
      currentPiece = "bishops"
    else if(input$piece == 3)
      currentPiece = "queens"
    else if(input$piece == 4)
      currentPiece = "kings"
    else if(input$piece == 5)
      currentPiece = "pawns"
    
    currentIndex = (as.numeric(input$piece) + add) * 65 + as.numeric(input$gameTurn)
    print("currentIndex")
    print(currentIndex)
    currentTotal = totals[currentIndex]
    print(currentTotal)
    #print(totals)
    paste("Distribution of", currentPlayer, currentPiece, "at turn", input$gameTurn)
  }
  heatmapTotal <- function(){
    add = 0
    if(input$player == 0)
      currentPlayer = "black"
    else
    {
      currentPlayer = "white"
      add = add + 6
    }
    if(input$piece == 0)
      currentPiece = "rooks"
    else if(input$piece == 1)
      currentPiece = "knights"
    else if(input$piece == 2)
      currentPiece = "bishops"
    else if(input$piece == 3)
      currentPiece = "queens"
    else if(input$piece == 4)
      currentPiece = "kings"
    else if(input$piece == 5)
      currentPiece = "pawns"
    
    currentIndex = (as.numeric(input$piece) + add) * 65 + as.numeric(input$gameTurn)
    print("currentIndex")
    print(currentIndex)
    currentTotal = totals[currentIndex]
    print(currentTotal)
    #print(totals)
    paste("total:", currentTotal)
  }
  
  
  output$winScatter <- renderPlot({
    par(new = FALSE)
    symbol = 20
    size = 0.7
    
    whiteColor = brewer.pal(8, "BrBG")[3]
    #drawColor = brewer.pal(8, "Greys")[5]
    #drawColor = brewer.pal(8, "PuBu")[5]
    drawColor = "#98b8cd"
    print(brewer.pal(8, "PuBu")[4])
    print(brewer.pal(8, "PuBu")[5])
    blackColor = "black"
    
    e = c()

    plot(e,xlab=NA,ylab=NA, yaxt = "n", xaxt = "n", main="Games played by ELO rating",pch=symbol,xlim=elo_range,ylim=elo_range,col="green",las=1,cex=size, bty="n", cex.main = 2)
    mtext(side = 2, expression(bold("Black ELO")), line = 3, las=1, adj=0, padj=-19, cex=1.3)
    mtext(side = 1, expression(bold("White ELO")), line = 2, adj=1, padj=1, cex=1.3)
    
    axis(side = 2, at = axTicks(1), labels = formatC(axTicks(1), big.mark = ".", format = "d"), las = 2)
    axis(side = 1, at = axTicks(1), labels = formatC(axTicks(1), big.mark = ".", format = "d"), las = 1)

    if(input$radioRound == 1)
    {
      lossPlot1 = losses1
      lossPlot2 = losses2
      winPlot1 = wins1
      winPlot2 = wins2
      drawPlot1 = draws1
      drawPlot2 = draws2
    }
    else
    {
      lossPlot1 = allLosses1[[input$round]]
      lossPlot2 = allLosses2[[input$round]]
      winPlot1 = allWins1[[input$round]]
      winPlot2 = allWins2[[input$round]]
      drawPlot1 = allDraws1[[input$round]]
      drawPlot2 = allDraws2[[input$round]]
    }
    allGames = 0
    par(new = TRUE)
    if(is.element("3", input$results))
    {  
      allGames = allGames + length(lossPlot1)
      plot(lossPlot1, lossPlot2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col=blackColor,las=1,cex=size)
    }
    par(new = TRUE)
    if(is.element("1", input$results))
    {
      allGames = allGames + length(winPlot1)
      plot(winPlot1, winPlot2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col=whiteColor,las=1,cex=size)
    }
    par(new = TRUE)
    if(is.element("2", input$results))
    {
      allGames = allGames + length(drawPlot1)
      plot(drawPlot1, drawPlot2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col=drawColor,las=1,cex=size)
    }
    #print("number of games in round")
    #print(allGames)
    par(new = TRUE)
    #print(input$results)
    if(is.element("4", input$results))
    {  
      #line = seq(from = 1000, to = 3000, by = 25)
      line <- c(1000:3000)
      lines(line, line, lwd=1.7)
      #abline(0,1)
    }
    #line = seq(from = 1000, to = 3000, by = 25)
    #plot(line, line,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="black",las=1)
    par(new = TRUE)
    legend("topleft",
           legend=c("White wins", "Draw", "Black wins"),
           lty=c(0,0,0), pch=c(16, 16, 16), col=c(whiteColor, drawColor, blackColor),bty="n",cex=1.4)


  })
  
  output$elo_results <- renderPlot({
    colScheme = brewer.pal(8, "Blues")
    resColors = c(colScheme[7], colScheme[4], colScheme[2])
    #resColors = c("black", "grey", "white")
    par(mar = c(5, 4.1,8,2))
    bp <- barplot(all_by_elo, main="Game outcome by ELO difference", las=1, xlab="Difference of players' ELO rating", yaxt="n", col=resColors, cex.main = 1.8, font.lab = 2, cex.lab = 1.2)
    returns = runif(10)
    axis(2, at=pretty(returns), lab=paste(pretty(returns) * 100, "%",sep=""), las=TRUE)
    tickmarks = bp - 0.5
    tickmarks <- c(tickmarks, 2 * tickmarks[length(tickmarks)] - tickmarks[length(tickmarks) - 1])
    print(tickmarks)
    axis(side=1, at = tickmarks, labels = (0:17) * 50)
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    l <- legend(x=16, y=1.35, legend = c("Lower rated player won", "Draw", "Higher rated player won"), xpd=NA, fill=c(resColors[3], resColors[2], resColors[1]), cex = 1.2)
    #print(l)
  })
  output$elo_length <- renderPlot({
    
    mElos <- elos + 25
    plot(mElos, length_by_elo, main="Average number of moves in a game by ELO difference", xlab = "Difference of players' ELO rating", ylim = c(0,60), las=1, cex.main = 1.8, ylab=NA, type="o", pch=15, cex=2, lwd=2, bty="n", xaxt = "n", font.lab = 2, cex.lab = 1.2)
    axis(side=1, at=c(elos, 850), label=c(elos, 850))
    mtext(expression(bold("Moves")), side = 2, las = 1, line = 0, at=65, cex=1.2)
    #axis(side=1, at = tickmarks, labels = (0:17) * 50)
  })
  output$elo_explain <- renderText({
    "The chess matches were split into groups based on the difference 
 of the players' ELO rating (0-49, 50-99, 100-149, ...). For each group, 
    the average length and the ratio of wins, draws and losses were computed."
  })
  output$elo_explain2 <- renderText({
    "Matches where the difference is 850 or higher are not shown because there are too few matches in each group."
  })
  output$scale_explain <- renderText({
    "Scaling divides the number of captures made by a piece by how many such pieces are in the game."
  })
}

shinyApp(ui = ui, server = server)