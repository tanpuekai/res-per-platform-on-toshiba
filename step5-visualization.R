rm(list=ls())

load("AUCMAT-rand-FALSE-mean-only.RData")
##########################

library(biclust)


drawHeatmap(AUCMAT)

##########################


AUCMAT.24<-AUCMAT[1:26,]

for(i in 1:nrow(AUCMAT.24)){
	ord<-order(AUCMAT.24[i,])
	str1<-signif(tail(AUCMAT.24[i,][ord]),digits=2)
	str2<-tail(colnames(AUCMAT.24)[ord])
	str3<-paste(paste(str2,"(",str1,")",sep=""),collapse=", ")
	cat(c(rownames(AUCMAT.24)[i],":\t\t\t",str3,"\n"))
}
##########################
ind.na<-which(is.na(AUCMAT[,1]))
corMat<-cor(AUCMAT[-ind.na,])

d1<-1-corMat
d2<-as.dist(d1)
h1<-hclust(d2)
plot(h1)

