source("G:/weiyun/my-func-lib/block-wise.R")
load("step9-570.RData")
library(gplots)
library(biclust)



########################################
d1<-cor(deg.q[,],method="spearman",use="complete.obs")
plot(apply(d1,1,sum,na.rm=T),apply(d1,1,sd,na.rm=T))
ind.out<- c(which(apply(d1,1,sum,na.rm=T)< 400 & apply(d1,1,sd,na.rm=T) < 0.15),1e6)

colors =seq(-0.25,1,len=100)
my_palette <- colorRampPalette(c("green", "black", "red"))(n = 99)

par(mai=c(1,1,1,1)+0.5) 

tab.o<-table(tis.570[ind.570])

d2<-d1[-ind.out,-ind.out]
ord<-order(tis.570[-ind.out])
drawHeatmap(d2[ord,ord])

res.h<-heatmap.2(d2[ord,ord], col=my_palette, #margins = c(7, 15),
    breaks=colors, Rowv = FALSE, Colv = FALSE,
	density.info="none", trace="none")


########################################
hc <- as.hclust( res.h$rowDendrogram )
memb.r<-cutree( hc, h=6 )
hc <- as.hclust( res.h$colDendrogram )
memb.c<-cutree( hc, k=2 )
table(tis.570[ind.570][ord],memb.r)

########################################

tis.570<-as.numeric(as.factor(tis.cat.2516[ind.col[ord]][res.h$rowInd]))

########################################
pl.570<-as.numeric(as.factor(platform.570[res.h$rowInd]))

dev.new()
col.bar<-tis.570#rep(sample(length(tab.o)),tab.o)
drawHeatmap(t(as.matrix(col.bar)))
abline(v=cumsum(tab.o)+0.5,col="white")

as.matrix((tab.o))






