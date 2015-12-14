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
testData = c(testData, testData2)
#print(testData)
testData = matrix(testData, nrow = 100, ncol = 2)
print(testData)
heatmap3(testData, Rowv=NA, Colv=NA, col = my_palette, scale="none", margins=c(5,10),breaks = myBreaks, add.expr = {abline(h=1); abline(h=10);abline(h=20);abline(h=30);abline(h=40);abline(h=50);abline(h=60);abline(h=70);abline(h=80); abline(h=90);abline(v=0.5);abline(v=1.5);abline(v=2.5);})