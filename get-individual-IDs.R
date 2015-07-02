
list.1<-list.files(pattern="^GSE.*.RData",recursive = T, full.names=T)
print(head(list.1))
print(length(list.1))

L.IDs<-rep(list(""),length(list.1))
L.Desvec<-L.IDs
L.tis<-L.IDs
for(i in 1:length(list.1)){
	if(i%%100==0)
		print(i)
	if(exists("dat.i"))
		rm(list=c("dat.i","des.vec","tis"))
	load(list.1[i])
	L.IDs[[i]]<-colnames(dat.i)
	L.Desvec[[i]]<-des.vec
	L.tis[[i]]<-tis
}

save(L.IDs,L.Desvec,L.tis,list.1,file="all.IDS.RData")
