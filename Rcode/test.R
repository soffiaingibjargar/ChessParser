my_palette <- colorRampPalette(c("white", "blue"))(n = 599)
pairs.breaks <- seq(from=0, to=1, length.out=600)

myBreaks <- pairs.breaks ^ (1/3)
myBreaks <- 1 - myBreaks

#print(pairs.breaks)
#print(myBreaks)

testData = 1:10
testData = testData / 10
testData2 = 1:10
testData2 = testData2 / 50
testData = c(testData, testData2)
testData = matrix(testData, nrow = 10, ncol = 2)
myHeatmap(testData, Rowv=NA, Colv=NA, col = my_palette, scale="none", margins=c(5,10),breaks = myBreaks)