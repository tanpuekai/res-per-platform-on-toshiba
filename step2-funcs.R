BLKWSTD<-function(d1,platforms){
	d.std<-d1
	pls<-unique(platforms)
	for(i in 1:length(pls)){
		for(j in 1:length(pls)){
			ind.i<-which(platforms==pls[i])
			ind.j<-which(platforms==pls[j])
			block.ij<- d1[ind.i,ind.j]
			block.ij<-(block.ij-mean(block.ij))/sd(block.ij)
			d.std[ind.i,ind.j]<-block.ij
		}
	}
	return(d.std)
}

process<-function(dat.unq){
	num.na.row<-apply(is.na(dat.unq),1,sum)
	num.zero.row<-apply(dat.unq==0,1,sum)

	ind.row<-which(num.na.row<ncol(dat.unq)/5 & num.zero.row< ncol(dat.unq)/5)
	cat(c("num of rows selected:",length(ind.row),"\n"))
	dat.1<-dat.unq[ind.row,]
	num.na.col<-apply((dat.1==0),2,sum)

	ind.col<-which(num.na.col<nrow(dat.1)/5)

	cat(c("num of cols selected:",length(ind.col),"\n"))
	dat.2<-dat.1[,ind.col]
	dat.3<-apply(dat.2, 2, function(y) y - median(y,na.rm=T))
	dat.3[is.nan(dat.3)] <- NA

	sample.tis.sel<-sample.tis[ind.col,]
	memb.o<-as.character(sample.tis.sel$dis.cat[])

	cat(c("extracting tis labels ...\n"))
	L.label<-tis.label(sample.tis.sel[,-seq(4)])
	L.ind<-L.label$L.ind 
	mat.tis<-L.label$mat.tis


	tab.cat<-table(as.character(sample.tis.sel$dis.cat))

	platforms<-sapply(strsplit(as.character(sample.tis.sel[,1]),"/"),function(x)x[2])
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
	d.std<-BLKWSTD(d1,platforms)

	L.res<-list(sample.tis.sel=sample.tis.sel,
		L.ind=L.ind,mat.tis=mat.tis,
		platforms=platforms,d1=d1,
		dat.4=dat.4,ind.row=ind.row,
		ind.col=ind.col,
		d.std=d.std,memb.o=memb.o)
	return(L.res)
}


