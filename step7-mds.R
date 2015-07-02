d <- as.dist(max(d1)-d1) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,3]

col1<-as.numeric(as.factor(memb.o))
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
  main="Metric	MDS",col=col1,	pch=16)

