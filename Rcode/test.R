my_palette <- colorRampPalette(c("white", "blue"))(n = 599)
pairs.breaks <- seq(from=0, to=1, length.out=600)

myBreaks <- pairs.breaks ^ (1/3)
myBreaks <- 1 - myBreaks

#print(pairs.breaks)
#print(myBreaks)

testData = 1:100
testData = testData / 100
testData2 = 1:100
testData2 = testData2 / 500
testData3 = 1:100
testData3 = testData3 / 1000
testData4 = 1:100 
testData4 = testData4 / 200
testData = c(testData, testData4, testData2, testData3)
#print(testData)
testData = matrix(testData, nrow = 100, ncol = 4)
print(testData)
chessHeatmap(testData, Rowv=NA, Colv=NA, col = my_palette, scale="none", margins=c(5,10),breaks = myBreaks)