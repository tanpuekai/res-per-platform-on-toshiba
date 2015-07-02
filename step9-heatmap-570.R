load("step9-570.RData")
library(gplots)
library(biclust)

###################################
d1<-cor(deg.q.570,method="spearman",use="complete.obs")
plot(apply(d1,1,sum,na.rm=T),apply(d1,1,sd,na.rm=T))
ind.out<- c(which(apply(d1,1,sum,na.rm=T)< 50 & apply(d1,1,sd,na.rm=T) < 0.15,
		which(is.na(tis.570))),1e6)

d2<-d1[-ind.out,-ind.out]

tis.all<-lab.570[-ind.out]
ind.filter<-which(tis.all!=""&tis.all%in%names(which(table(tis.all)>5)))
ord<-order(tis.all[ind.filter])
tab.o<-table(tis.all[ind.filter])


col.bar<-rep(sample(length(tab.o)),tab.o)
drawHeatmap(t(as.matrix(col.bar)))
abline(v=cumsum(tab.o)+0.5,col="white")

as.matrix((tab.o))

###################################

mat.core<-deg.q.570[,-ind.out][,ind.filter][,ord]

d3<-as.matrix(dist(t(mat.core)))

#d3<-d2[ord,ord]

drawHeatmap(d3)

###################################

col.1 <-seq(0.0,1,len=100)
my_palette <- colorRampPalette(c("green", "black", "red"))(n = 99)

#par(mai=c(1,1,1,1)+0.5) 

res.h<-heatmap.2(1-d3, col=my_palette, #margins = c(7, 15),
    breaks=col.1, Rowv = FALSE, Colv = FALSE,
	density.info="none", trace="none")
###################################

col.bar<-rep(sample(length(tab.o)),tab.o)
drawHeatmap(t(as.matrix(col.bar)))
abline(v=cumsum(tab.o)+0.5,col="white")

as.matrix((tab.o))
###################################
all.blood<-c( "blood","bone", "leukemia","leukocyte","lymph",
	 "lymphocyte","lymphoma","peripheral-blood")

tis.left<-tis.all
tab.1<-table(tis.left)
tab.2<-tab.1[tab.1>10]

ind.10<-which(tis.left%in%names(tab.2))# & !tis.left %in%all.blood)
tis.vis<-tis.left[ind.10]
tab.tis<-table(tis.vis)

#mydata<-deg.q[,-ind.out][,ind.10] #rbind(mat1,mat2)

dist.mat<- d2[ind.10,ind.10]
dist.mat<-(dist.mat-mean(dist.mat,na.rm=T))/sd(dist.mat,na.rm=T)
dist.mat<-max(dist.mat)-dist.mat
dist.1<-as.dist(dist.mat)
fit <- cmdscale(dist.1,eig=TRUE, k=3) # k is the number of dim


lab<-as.numeric(as.factor(tis.vis))

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
#z <- fit$points[,3]

lab[lab%%8==7]<-1

plot(x, y, col = lab, cex=.7,pch=lab%%20)


dev.new()
plot(1,type='n')
legend("bottomleft",legend =unique(tis.vis),ncol=4,
	col=unique(lab),pch=unique(lab)%%20,cex=0.8)





