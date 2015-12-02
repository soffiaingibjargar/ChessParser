library(animation)
library(gplots)
saveGIF({
  for(i in 0:64)
  {
    piece = 1
    my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
    
    path = paste("..\\Results\\distribution_p",piece,"_r",i,".csv",sep = "")
    board <- read.csv(path, sep=",")
    #row.names(board) <- board$X
    print(i)
    print(board)
    
    board <- board[,2:9]
    #print(board)
    board_matrix <- data.matrix(board)
    #print(board_matrix)
    board_heatmap <- heatmap.2(board_matrix, Rowv=NA, Colv=NA, scale="column", margins=c(5,10),col=my_palette,main = paste("Round",i + 1))
  }
})