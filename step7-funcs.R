
process<-function(dat.unq){
	num.na.row<-apply(is.na(dat.unq),1,sum)
	num.zero.row<-apply(dat.unq==0,1,sum)

	ind.row<-which(num.na.row<ncol(dat.unq)/5 & num.zero.row< ncol(dat.unq)/5)
	cat(c("num of rows selected:",length(ind.row),"\n"))
	dat.1<-dat.unq[ind.row,]
	num.na.col<-apply((dat.1==0),2,sum)

	ind.col<-which(num.na.col<nrow(dat.1)/5)
	memb.o<-memb[ind.col]

	cat(c("num of cols selected:",length(ind.col),"\n"))
	dat.2<-dat.1[,ind.col]
	dat.3<-apply(dat.2, 2, function(y) y - median(y,na.rm=T))
	dat.3[is.nan(dat.3)] <- NA

#	platforms<-sapply(strsplit(platforms,"-"),function(x)x[3])
#########################
	cat(c("performing quant-norm ...\n"))
	dat.all.q<-normalize.quantiles(dat.3)
	dat.4<- dat.all.q-min(dat.all.q,na.rm=T)+1
	for(i in 1:nrow(dat.4)){
		vec.i<-dat.4[i,!is.na(dat.4[i,])]
		dat.4[i,is.na(dat.4[i,])]<-sample(vec.i,sum(is.na(dat.4[i,])))
		#dat.4[i,is.na(dat.4[i,])]<-median(vec.i,na.rm=T)
	}

#########################
	cat(c("cal pairwise cor ...\n"))
	d1<-cor(dat.4[,],method="spearman")

	cat(c("performing blockwise std ...\n"))

	L.res<-list(d1=d1,
		dat.4=dat.4,ind.row=ind.row,
		ind.col=ind.col,
		memb.o=memb.o)
	return(L.res)
}
