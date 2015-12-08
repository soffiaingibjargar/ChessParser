piece = 0
i = 10

path = paste("..\\Results\\distribution_p",piece,"_r",i,".csv",sep = "")
board <- read.csv(path, sep=",")
#row.names(board) <- board$X
#print(board)
total = sum(board) - 36
#print(total)

board <- board[,2:9] / total
#print(board)
board_matrix <- data.matrix(board)
print(board_matrix)
#board_heatmap <- heatmap.2(board_matrix, Rowv=NA, Colv=NA, scale="column", margins=c(5,10),col=my_palette,main = paste("Round",i + 1),breaks=col_breaks)
d3heatmap(board, scale = "column", dendrogram = "none",
          color = "Blues")