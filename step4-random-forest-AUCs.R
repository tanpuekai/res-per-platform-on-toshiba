load("step1-process.RData")
load("all-gene-lists.RData")
load("G:/weiyun/annot/refGene19.RData")
source("step8-funcs.R")
##############
library(ROCR)
library(randomForest)
##############
flag.rand<-F

len.feature.set<-length(L.ind.sel)

aucMat<-matrix(NA,len.all,len.feature.set)
aucMat.se<-aucMat


##############

attach(L.res.m)

L.ind.sel<-L.ind[sapply(L.ind,length)>20]
for(i in 1:length(L.all)){
	i<-25
	case.i<-L.all[[i]]
	back.i<-L.back[[ind.b[i]]]
	symb.i<-uniqGene[ind.row]

	for(j in 1:len.feature.set){
		ind.samp.j<-L.ind.sel[[j]]
		dat.5<-dat.4[,ind.samp.j]
		trainSet<-data.frame(symb.i,dat.5)
##############
		ind.b.i<-which(symb.i%in%back.i)
		ind.c.i<-which(symb.i%in%case.i)

##############
		if(length(ind.c.i)<10){
			cat(c("too few case genes",length(ind.c.i),"\n"))
			next
		}
		if(length(ind.c.i)>400){
			cat(c("too many case genes",length(ind.c.i),"\n"))
			next
		}
		if(ncol(trainSet)<10){
			cat(c("too few features",ncol(trainSet),"\n"))
			next
		}
##############
		lab<-rep(NA,nrow(trainSet))
		lab[ind.c.i]<-1
		lab[ind.b.i]<-0
		cat(c("\t",length(lab)," training genes,",sum(lab==1,na.rm=T), "cases and",sum(lab==0,na.rm=T),"controls\n"))
		cat(c("\tand,",ncol(trainSet), "features\n"))

		source("step8-iter-section.R")
		aucMat[i,j]<-mean(auc.i.j,na.rm=T)
		aucMat.se[i,j]<-sd(auc.i.j,na.rm=T)

		#print(aucMat)
		#print(aucMat.se)
	}
}
#save(aucMat,aucMat.se,res.4,dir.abs,dir1,L.all,file=fn.t)

names(L.ind.sel)[order(aucMat[i,])]
      



