library(heatmap3)
ind <- function(p, r, m, n){
  return(p*8*8*65 + r*8*8 + (m-1)*8+n)
}

all <- vector(mode="numeric",length = 12 * 65 * 8 * 8)
#print(all[0])

# totals contains the number of on the board, by piece and round
# totals[p * 65 + r + 1], zero-indexed
totals <- c(mode="numeric",length=65*12)
for(p in 0:11)
{

  for(r in 0:64)
  {
    path = paste("www//Results//distribution_p",p,"_r",r,".csv",sep = "")
    board <- read.csv(path, sep=",")
    print(path)
    print(board)
    total = sum(board) - 36
    
    board <- board[,2:9] / total
    #board_matrix <- data.matrix(board)
    #print(board)
    totals[p * 65 + r + 1] <- total
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
for (i in 0:11)
{
  print(paste("starting totals", 65 * i + 1))
  print(totals[65 * i + 1])
}
#print(totals)
path2 = "www//captures.csv"

captures <- read.csv(path2, sep=",")
captures <- data.matrix(captures)
#print(captures)
#heatmap3(apply(captures,2,rev), Rowv=NA, Colv=NA, labRow = c('R','N','B','Q','K','P'))

path3 = "www//captured_spots.csv"

cap_spots <- read.csv(path3, sep=",")
cap_spots_m <- data.matrix(cap_spots)
#print(cap_spots)

captures_norm = read.csv("www//captures_norm.csv")



space_R = read.csv("www//captured_spots_R.csv")

space_N = read.csv("www//captured_spots_N.csv")

space_B = read.csv("www//captured_spots_B.csv")

space_Q = read.csv("www//captured_spots_Q.csv")

space_K = read.csv("www//captured_spots_K.csv")

space_P = read.csv("www//captured_spots_P.csv")

wins_by_elo = c(28,
                76,
                248,
                531,
                623,
                634,
                404,
                224,
                173,
                198,
                122,
                185,
                89,
                43,
                19,
                11,
                14)
draws_by_elo = c(47,
                 83,
                 185,
                 304,
                 267,
                 180,
                 86,
                 50,
                 27,
                 23,
                 10,
                 12,
                 8,
                 1,
                 0,
                 0,
                 1)
losses_by_elo = c(20,
                  37,
                  95,
                  140,
                  144,
                  119,
                  71,
                  29,
                  23,
                  14,
                  5,
                  5,
                  2,
                  1,
                  1,
                  0,
                  0
)
all_by_elo = matrix(c(wins_by_elo, draws_by_elo, losses_by_elo), nrow = 3, ncol = 17, byrow = T)
#print(all_by_elo)
for(i in 1:17)
{
  temp = all_by_elo[1,i] + all_by_elo[2,i] + all_by_elo[3,i]
  for(j in 1:3)
    all_by_elo[j,i] = all_by_elo[j,i] / temp
}
length_by_elo = c(51.105263157894726,
                  50.75,
                  50.35037878787882,
                  46.6502564102564,
                  44.823017408123775,
                  42.47266881028933,
                  42.34402852049911,
                  40.745874587458744,
                  38.65022421524661,
                  38.3744680851064,
                  37.64963503649635,
                  35.6188118811881,
                  38.57575757575759,
                  38.333333333333336,
                  38.5,
                  37.09090909090909,
                  33.46666666666667)
elos = 0:16
elos = elos * 50
#print(elos)
plot(elos, length_by_elo, type = "p", main="Average length of game", xlab = "Difference of ELO rating", ylab = "Moves", ylim = c(0,55))
barplot(all_by_elo, main="Outcomes by ELO difference")