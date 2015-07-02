load("per.ind.annot.RData")
load("all.IDS.RData")

dir.per<-sapply( per.ind.annot[,2],function(x)strsplit(x,"/")[[1]][2])
unq.dir<-unique(dir.per)
L.ind.dir<-rep(list(NA),length(unq.dir))
for(i in 1:length(unq.dir)){
	L.ind.dir[[i]]<-which(dir.per%in%unq.dir[i])
}

dir.list<-sapply( list.1,function(x)strsplit(x,"/")[[1]][2])
ind.list<-match(dir.list,unq.dir)

###################################################
source("G:/weiyun/my-func-lib/tissue-label-search-terms.R")
sum.na<-rep(NA,length(L.IDs))

L.tab.2<-rep(list(NA),length(L.IDs))
tis.cat.2<-rep(NA,length(L.IDs))
for(i in 1:length(L.IDs)){
	if(i%%100==0)print(i)

	ind.k<-L.ind.dir[[ind.list[i]]]
	ind.i<-match( L.IDs[[i]], per.ind.annot[ind.k,3])
	sum.na[i]<-sum(is.na(ind.i))
	if(length(ind.i)==0)
		next
	annot.i<-per.ind.annot[ind.k,][ind.i,]
	str.i<-apply(annot.i[,-seq(3)],1,paste,collapse="::")
	t1<-sapply(L.pat,function(x)grepl(x[[1]],str.i,ignore.case =T))
	s.t1<-apply(t1,2,sum)
	s.t2<-s.t1[s.t1>length(L.IDs[[i]])*2/3]
	if(length(s.t2)>0){
		v.priori<-vec.priority[names(s.t2)]
		#print(cbind(s.t2,v.priori))
		#lab.i<-names(which.max(v.priori))
		lab.i<-paste(names(s.t2)[order(s.t2)],collapse="::")
		tis.cat.2[i]<-lab.i
		L.tab.2[[i]]<- s.t2
	}
}

###################################################
len.tab<-sapply(L.tab.2,length)
sort(table(tis.cat.2[len.tab>1]))

###################################################
ind.discrep<-which(tis.cat!=tis.cat.2)
tis.discrep<-data.frame(tis.cat.2,tis.cat)[ind.discrep,]
edit(tis.discrep)


