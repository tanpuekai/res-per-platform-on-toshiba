attach(L.res.m)
load("G:/weiyun/annot/refGene19.RData")
source("G:/weiyun/my-func-lib/fisherDA.R")
###################################
symb.i<-uniqGene[ind.row]

ratioMat<-matrix(NA,24,len.feature.set)

for(i in 1:24){
#	i<-3
#	names(L.all[[i]])

	case.i<-L.all[[i]]
	back.i<-L.back[[ind.b[i]]]
##############
	ind.b.i<-which(symb.i%in%back.i)
	ind.c.i<-which(symb.i%in%case.i)

	lab<-rep(NA,nrow(dat.4))

	lab[ind.c.i]<-1
	lab[ind.b.i]<-0

	ind.i<-c(ind.c.i,sample(ind.b.i,length(ind.b.i)))
	y<-rep(seq(0,1),c(length(ind.c.i),length(ind.b.i)))	
	pvec<-rep(NA,len.feature.set)
	ratio<-pvec

	for(j in 1:len.feature.set){
		ind.samp.j<-L.ind.sel[[j]]
		dat.5<-dat.4[ind.i,ind.samp.j]

		#res.lda<-lda(y~dat.5)
		#res.pca<-princomp(t(dat.5))
		#plot(res.pca$scores[,1],res.pca$scores[,2])
		#points(res.pca$scores[1:2,lab==1],col="red")

		res.fisher<-fisherDA(dat.5,y+1)
		z<-res.fisher$z
		res.t<-aov(z~y)
		#pvec[j]<-res.t$p.value#res.fisher$ratio#
		ratio[j]<- var(res.t$fitted.values)/var(z)
	}
	ratioMat[i,]<-ratio
	plot(ratio,AUCMAT.24[i,])
	cat(c(names(L.all)[i],"\n"))
	cat(c(tail(names(L.ind.sel)[order(ratio)]),"\t"))
	
}
###################################
for(i in 1:24){
	cat(c(names(L.all)[i],":\t"))
	cat(c(tail(names(L.ind.sel)[order(ratioMat[i,])]),"\n"))
}

###################################
 auc.q<-normalize.quantiles(AUCMAT.24[1:24,])
 rat.q<-normalize.quantiles(ratioMat)



boxplot(AUCMAT.24,,las=3,par(cex.axis=.9))
###################################
		next
		dim(dat.5)
		mydata<-data.frame(y,dat.5[,])
		res.lda<-lda(y~.,data=mydata)
		plot(res.lda)

		mylogit <- glm(y~ ., data = mydata, family = "binomial")
		chisq.test(table(sign(mylogit$residuals),y))


		dat.0<-apply(dat.5,2,function(x)x-mean(x))
		svd1<-svd(dat.0)
		scores1<-dat.0%*%svd1$v

		boxplot(scores1[,1]~lab)
		plot(scores1[,1],scores1[,2])
		points(scores1[1:25,1],scores1[1:25,2],col="red")


		plot3d(res.pca$scores[1,],res.pca$scores[2,],res.pca$scores[3,])
	}

###################################
detach(L.res.m)
