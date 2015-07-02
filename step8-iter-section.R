

############################

numfold<-2

num.run<-10

auc.i.j<-matrix(NA,num.run,numfold)


############################

for(r in 1:num.run){
	if(flag.rand==T){
		z <-sample(lab)
	}else{z <-lab}

	ind1<-which(z >0)
	ind2<-sample(which(z == 0),length(ind1))

	tmp1<-as.matrix(trainSet[ind1,-1])
	tmp2<-as.matrix(trainSet[ind2,-1])

	y<-as.factor(rep(c(1,0),c(nrow(tmp1),nrow(tmp2))))

	x<-rbind(tmp1,tmp2)
	x[x==Inf]<-NA

	num.na<-sum(is.na(x))
	num.non.na<-sum(!is.na(x))
	dat<-data.frame(y,x[,])

	is.nan.data.frame <- function(x)
		do.call(cbind, lapply(x, is.nan))

#	print(sum(is.nan(dat)))
	dat[is.nan(dat)] <- NA 
#	print(sum(is.nan(dat)))

	if(num.na>0){
		cat(c("\timputing ",num.na," mis-vals with ",num.non.na," non-mis vals\n"))
		print(table(y))
		print(sum(is.na(y)))
		s1<-apply(is.na(dat),2,sum)
		ind.compl.na<-which(s1==nrow(dat))
		if(length(ind.compl.na)>0)
			dat<-dat[,-ind.compl.na]
		X.imputed <- rfImpute(y ~ .,dat)
		cat(c("\ttotal num of mis after imp:",sum(is.na(X.imputed)),"\n"))
	}else{
		X.imputed<-dat
		cat("\tno impute needed\n")
	}

	turns<-sample(1:numfold,nrow(X.imputed),replace=T)
	for(k in 1:numfold){
		cat(c("\t\trun=",r,"; fold=",k,"\n"))
		indtest<- which(turns==k)
		dattrain<-X.imputed[-indtest,]
		dattest<-X.imputed[indtest,]

		len.y1<-sum(table(dattrain[,1])>0)
		len.y2<-sum(table(dattest[,1])>0)
		cat(c("\t\tlen.y1=",table(dattrain[,1]),"; len.y2=",table(dattest[,1]),"\n"))
		if(len.y1<2 | len.y2<2){
			cat("\t\tskipping\n")
			next
		}
		model.rf <- randomForest(y ~ ., dattrain,#cutoff=table(y)/length(y),
			importance=TRUE,proximity=TRUE)
		print(model.rf)

		auctmp.1<-rocauc(model.rf,dattest,flag.plot=F)
		auc.i.j[r,k]<- auctmp.1

	}
	#print(AUC[k,])
}


print(c("numfold=",numfold, mean(auc.i.j), sd(auc.i.j)))

##############


