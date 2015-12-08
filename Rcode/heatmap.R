library(animation)
library(gplots)
saveGIF({
  col_breaks = c(seq(0,0.01,length=2), # for red
                 seq(0.0101, 0.7,length=30), # for yellow
                 seq(0.701,1,length=30)) # for green
  for(i in 0:40)
  {
    print(i)
    piece = 0
    my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 61)
    
    path = paste("..\\Results\\distribution_p",piece,"_r",i,".csv",sep = "")
    
    board <- read.csv(path, sep=",")
    
    #print(board)
    total = sum(board) - 36
    #print(total)
    
    board <- board[,2:9] / total
    #print(board)
    board_matrix <- data.matrix(board)
    print(board_matrix)
    board_heatmap <- heatmap.2(board_matrix, Rowv=NA, Colv=NA, scale="column", margins=c(5,10),col=my_palette,main = paste("Round",i + 1),breaks=col_breaks)
  }
})