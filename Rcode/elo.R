library(ggplot2)
path <- "..\\Elo\\elo_results.csv"
result <- read.csv(path, sep=",")

e1 = result$elo1
e2 = result$elo2
res = result$res
eloRound = result$round

wins1 = c()
wins2 = c()
losses1 = c()
losses2 = c()
draws1 = c()
draws2 = c()

size = length(res)

allWins1 <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
allWins2 <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
allLosses1 <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
allLosses2 <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
allDraws1 <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
allDraws2 <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())

#print("size")
#print(size)
for(i in 1:size)
{
  #print(paste("i is", i))
  currentRound = eloRound[i]
  #print(currentRound)
  #print(res[i])
  if (res[i] == "1-0")
  {
    wins1 <- c(wins1, c(e1[i]))
    wins2 <- c(wins2, c(e2[i]))
    
    #print(paste("current round", currentRound))
    #print("allWins1")
    #print(allWins1[[currentRound]])
    temp = allWins1[[currentRound]]
    allWins1[[currentRound]] = c(temp, c(e1[i]))
    temp = allWins2[[currentRound]]
    allWins2[[currentRound]] = c(temp, c(e2[i]))
  }
  else if (res[i] == "0-1")
  {
    losses1 <- c(losses1, c(e1[i]))
    losses2 <- c(losses2, c(e2[i]))
    
    temp = allLosses1[[currentRound]]
    allLosses1[[currentRound]] = c(temp, c(e1[i]))
    temp = allLosses2[[currentRound]]
    allLosses2[[currentRound]] = c(temp, c(e2[i]))
  }
  else
  {
    draws1 <- c(draws1, c(e1[i]))
    draws2 <- c(draws2, c(e2[i]))
    
    temp = allDraws1[[currentRound]]
    allDraws1[[currentRound]] = c(temp, c(e1[i]))
    temp = allDraws2[[currentRound]]
    allDraws2[[currentRound]] = c(temp, c(e2[i]))
  }
}
elo_range <- c(1000,3000)
par(new = FALSE)
symbol = 176
size = 0.9
rPlot <- plot(wins1, wins2,xlab="white ELO",ylab="black ELO",main="Games played by ELO rating",pch=symbol,xlim=elo_range,ylim=elo_range,col="green", cex=size)
par(new = TRUE)

rPlot <- plot(draws1, draws2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="orange", cex=size)
par(new = TRUE)
rPlot <- plot(losses1, losses2,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="red", cex=size)
#plot(result$elo1,result$elo2,xlab="higher ELO",ylab="higher ELO",main="Games played by ELO rating",pch=1,xlim=c(1000,3000),ylim=c(1000,3000))
par(new = TRUE)
#plot(c(2000), c(2000),pch=10,axes = FALSE,xlab='',ylab='',xlim=c(1000,3000),ylim=c(1000,3000))

line = seq(from = 1000, to = 3000, by = 25)
rPlot <- plot(line, line,pch=symbol,axes = FALSE,xlab='',ylab='',xlim=elo_range,ylim=elo_range,col="black")

win_diff <- wins1- wins2
loss_diff <- losses1 - losses2
draw_diff <- draws1 - draws2
#print("win") 
#print(summary(win_diff))
#print("loss") 
#print(summary(loss_diff))
#print("draw")
#print(summary(draw_diff))