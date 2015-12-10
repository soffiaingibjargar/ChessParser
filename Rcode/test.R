library(Unicode)
#piece = 0
#i = 10
library(plyr)
#path = paste("..\\Results\\distribution_p",piece,"_r",i,".csv",sep = "")
#board <- read.csv(path, sep=",")
#row.names(board) <- board$X
#print(board)
#total = sum(board) - 36
#print(total)

#board <- board[,2:9] / total
#print(board)
#board_matrix <- data.matrix(board)
#print(board_matrix)
#board_heatmap <- heatmap.2(board_matrix, Rowv=NA, Colv=NA, scale="column", margins=c(5,10),col=my_palette,main = paste("Round",i + 1),breaks=col_breaks)
#d3heatmap(board, scale = "column", dendrogram = "none",
          #color = "Blues")
source("helper.R")
a <- "\U00B5"
b <- "\U0A60"
v = c(1,2,3,4)
my_palette <- colorRampPalette(c("white", "red"))(n = 1000)
#p <- plot(v)
#p <- plot(v) + text(2,3,"TEST")
king <- readPNG("king.png")
p <- heatmap(apply(captures,2,rev), Rowv=NA, Colv=NA, labRow = c('P','K','Q','B','N','R'),scale="none",col=my_palette,main="Frequency of captures by piece",xlab = "Captured Piece", ylab = "Capturing Piece", add.expr={text(1,6,b);abline(v=c(3.5,6.5), lwd=3)}, labCol=NA)