my_palette <- colorRampPalette(c("white", "blue"))(n = 599)
pairs.breaks <- seq(from=0, to=1, length.out=600)
myBreaks <- sqrt(sqrt(pairs.breaks))
myBreaks <- 1 - myBreaks

#print(my_palette)
z=matrix(1:100,nrow=1)
#z= sqrt(sqrt(z))
x=1
y=seq(0, 1,len=100) # supposing 3 and 2345 are the range of your data
image(x,y,z,col=my_palette,axes=FALSE,xlab="",ylab="")
axis(2)
