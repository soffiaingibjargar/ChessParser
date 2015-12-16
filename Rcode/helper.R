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
  boardSum <- 0:63
  for(t in 1:64)
    boardSum[t] = 0
  boardSum <- matrix(boardSum, nrow=8, ncol=8)
  for(r in 0:64)
  {
    path = paste("www//Results//distribution_p",p,"_r",r,".csv",sep = "")
    board <- read.csv(path, sep=",")
    #print(path)
    #print(class(board))
    total = sum(board) - 36
    
    board <- board[,2:9] / total
    #print(class(board))
    #board_matrix <- data.matrix(board)
    #print(board)
    totals[p * 65 + r + 1] <- total
    for(m in 1:8)
    {
      for(n in 1:8)
      {
        #print(board[m, n])
        all[ind(p,r,m,n)] <- board[m, n]
        boardSum[m,n] = boardSum[m,n] + board[m,n] * total
      }
    }
    
  }
  #print(paste("piece", p))
  #print(boardSum)
}
#for (i in 0:11)
#{
#  print(paste("starting totals", 65 * i + 1))
#  print(totals[65 * i + 1])
#}
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


allRooks = c(115053,  11797,	17292,	19710,	21853,	52972,	3923,	68675,
             3131,	3144,	3861,	4488,	3701,	3073,	1395,	1272,
             1985,	2086,	2438,	2889,	2221,	2134,	1151,	1180,
             1621,	1598,	2114,	2395,	2387,	1500,	1007,	1030,
             1731,	1913,	2386,	2366,	2138,	1629,	1005,	1083,
             2250,	2263,	2365,	2858,	2973,	2599,	1712,	1579,
             2549,	2540,	4048,	4911,	3931,	2993,	1400,	1201,
             109472,	10840,	17057,	28350,	25475,	49324,	3827,	69854
)
allKnights = c(263,  41330,	943,	981,	2662,	2630,	23766,	143,
               505,	887,	2521,	22576,	8497,	1477,	1254,	1245,
               2586,	6513,	29913,	3306,	3560,	55949,	3930,	1362,
               3589,	3195,	6927,	8671,	9686,	4365,	3679,	3154,
               3280,	2807,	5210,	11972,	8551,	3831,	3513,	2537,
               1645,	5516,	46267,	3597,	3615,	54135,	5367,	1142,
               571,	590,	2000,	13664,	9225,	1580,	655,	1204,
               96,	36836,	864,	1319,	1414,	1821,	25850,	132
)
allBishops = c(781,  460,	69467,	1563,	1759,	43448,	356,	417,
               1008,	16878,	1900,	11191,	23335,	1758,	33723,	977,
               3809,	3471,	4906,	9021,	10112,	5960,	3474,	3390,
               1140,	4962,	6573,	3729,	3629,	6497,	8261,	1756,
               1722,	5187,	9414,	4663,	3952,	8855,	4791,	2762,
               2168,	5391,	4126,	18453,	18109,	5072,	3440,	2173,
               1203,	11160,	4851,	8373,	14766,	2679,	28361,	817,
               565,	1696,	63417,	1140,	1216,	45256,	379,	320
)
allQueens = c(789,  1668,	2610,	75031,	2346,	1102,	246,	276,
              1202,	2414,	14797,	7155,	9446,	1857,	973,	541,
              1233,	6348,	2788,	4032,	2302,	3797,	1793,	1420,
              3964,	1447,	2102,	2506,	2117,	1629,	2074,	2347,
              2334,	1686,	2209,	3015,	2436,	2220,	2444,	2597,
              1354,	4982,	2905,	5829,	3599,	4911,	2166,	1374,
              1072,	1855,	12629,	11992,	11913,	2464,	853,	406,
              663,	1065,	2070,	71514,	1808,	631,	179,	309
)
allKings = c(402,  2017,	4406,	1183,	62857,	4819,	87475,	9620,
             347,	714,	1883,	2502,	4423,	5609,	10346,	5977,
             266,	685,	1097,	2153,	2702,	3000,	2071,	1286,
             220,	513,	676,	1099,	1340,	1251,	1114,	526,
             213,	573,	957,	1239,	1459,	1301,	993,	586,
             243,	717,	1341,	2084,	2680,	2596,	2342,	1314,
             459,	953,	1536,	2703,	3644,	5207,	9441,	6125,
             623,	4979,	7026,	1051,	61287,	4142,	87544,	10811
)
allPawns = c(0,  0,	0,	0,	0,	0,	0,	0,
             113468,	109690,	51400,	27161,	39363,	141576,	110198,	139449,
             46594,	31415,	41019,	48942,	63112,	19033,	70492,	38252,
             28191,	29292,	43667,	69164,	61064,	28693,	20224,	21990,
             28255,	27331,	54018,	70343,	67848,	33357,	19820,	20838,
             32095,	32340,	34153,	18180,	30062,	23026,	56888,	41516,
             129404,	125650,	60077,	21920,	32453,	133424,	123556,	136606,
             0,	0,	0,	0,	0,	0,	0,	0
)
allAll = allRooks + allKnights + allBishops + allQueens + allKings + allPawns
allDistributions = list(matrix(allRooks, nrow=8,ncol=8,byrow=T), matrix(allKnights, nrow=8,ncol=8,byrow=T), matrix(allBishops, nrow=8,ncol=8,byrow=T), matrix(allQueens, nrow=8,ncol=8,byrow=T), matrix(allKings, nrow=8,ncol=8,byrow=T), matrix(allPawns, nrow=8,ncol=8,byrow=T), matrix(allAll, nrow=8,ncol=8,byrow=T))
print(allDistributions[[6]])