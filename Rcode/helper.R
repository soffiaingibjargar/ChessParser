ind <- function(p, r, m, n){
  return(p*8*8*65 + r*8*8 + (m-1)*8+n)
}

all <- vector(mode="numeric",length = 12 * 65 * 8 * 8)
print(all[0])

for(p in 0:11)
{

  for(r in 0:64)
  {
    path = paste("..\\Results\\distribution_p",p,"_r",r,".csv",sep = "")
    board <- read.csv(path, sep=",")
    
    total = sum(board) - 36
    board <- board[,2:9] / total
    #board_matrix <- data.matrix(board)

    #print(board)
    for(m in 1:8)
    {
      for(n in 1:8)
      {
        #print(board[m, n])
        all[ind(p,r,m,n)] <- board[m, n]
      }
    }
  }
}

