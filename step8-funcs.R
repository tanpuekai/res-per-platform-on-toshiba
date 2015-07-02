library(ROCR)
library(randomForest)

############################
rocauc<-function(model,dattest,flag.plot=F){
	temp1<-predict(model,type="prob",newdata=as.matrix(dattest[,-1,drop=F]))

	heldout.rf.pr = temp1[,2]
	heldout.rf.pred = prediction(heldout.rf.pr, dattest[,1])
	heldout.rf.perf = performance(heldout.rf.pred,"tpr","fpr")

	if(flag.plot==T){
		plot(heldout.rf.perf,main=c(k,i),col=2,lwd=2)
		abline(a=0,b=1,lwd=2,lty=2,col="gray")
	}

 	perf <- performance(heldout.rf.pred,"auc")
	auc <-unlist(slot(perf , "y.values"))
	print(auc)
	return(auc)
}
############################
generate.set<-function(file.str){
	print(file.exists(file.str))
	load(file.str)

	s1<-apply(is.na(DegMat),1,sum)
	s2<-apply(is.na(MeanMat),1,sum)
	s3<-apply(is.na(VarMat),1,sum)

	ind.1<-which(s1< 1/2*ncol(MeanMat) & s2< 1/2*ncol(MeanMat) & s3< 1/2*ncol(MeanMat))
	
	temp.set<-data.frame(uniqGene,DegMat,MeanMat,VarMat)#[ind.1,]
	trainset<-temp.set[ind.1,]
	names(trainset)[1]<-"genes"	
	return(trainset)
}

############################
