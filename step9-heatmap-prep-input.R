load("step9-combined.RData")

########################################
type<-"d"
if(type=="m"){
	class(mat.mean)<-"numeric"
	mat.mean[is.na(mat.mean)]<-NA
	deg.log<-mat.mean
}else if(type=="d"){
	deg.log<-log(mat.deg.all+1)
}else if(type=="v"){
	deg.log<-mat.var
}

s1<-apply(is.na(deg.log)|deg.log==0,1,sum)
ind.row<-which(s1<ncol(deg.log)/5)
deg.log.2<-deg.log[ind.row,]

s2<-apply(is.na(deg.log.2)|deg.log.2==0,2,sum)
ind.col<-which(s2<nrow(deg.log.2)/5)
deg.log.3<-deg.log.2[,ind.col]

########################################
library(preprocessCore)
deg.q<-normalize.quantiles(deg.log.3)
gene.q<- uniqGene[ind.row]

tis.q<-tis.cat.2516[ind.col]
lab.q<-tis.lab.2516[ind.col]
platform.q<-pl.unq[ind.col]
ind.570<-which(platform.q=="set-007-GPL570")

tis.570<-tis.q[ind.570]
lab.570<-lab.q[ind.570]
deg.q.570<-deg.q[,ind.570]
unq.prefix.570<-unq.prefix[ind.col][ind.570]
platform.570<-platform.q[ind.570]

	

save(ind.col,ind.row,
	gene.q,deg.q,tis.q,platform.q,unq.prefix,lab.q,
	deg.q.570,tis.570,lab.570,ind.570,
	unq.prefix.570,platform.570,
	file="step9-570.RData")


