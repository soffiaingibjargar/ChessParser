library(heatmap3)
ind <- function(p, r, m, n){
  return(p*8*8*65 + r*8*8 + (m-1)*8+n)
}

all <- vector(mode="numeric",length = 12 * 65 * 8 * 8)
#print(all[0])

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

path2 = "..\\captures.csv"
captures <- read.csv(path2, sep=",")
captures <- data.matrix(captures)
print(captures)
#heatmap3(apply(captures,2,rev), Rowv=NA, Colv=NA, labRow = c('R','N','B','Q','K','P'))
path3 = "..\\captured_spots.csv"
cap_spots <- read.csv(path3, sep=",")
cap_spots_m <- data.matrix(cap_spots)
print(cap_spots)
captures_norm = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captures_norm.csv")



space_R = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captured_spots_R.csv")

space_N = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captured_spots_N.csv")

space_B = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captured_spots_B.csv")

space_Q = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captured_spots_Q.csv")

space_K = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captured_spots_K.csv")

space_P = read.csv("C:/Users/Soffía/PycharmProjects/ChessParser/captured_spots_P.csv")
