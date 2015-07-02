load("all.IDS.RData")
load("step10.two.lev.lab.RData")

###################################################

dir.per<-two.lev.lab[,1]

tab.platform<-as.matrix(table(dir.per))
write.csv(tab.platform,file="step10.tab.platform.csv")


unq.dir<-unique(dir.per)
L.ind.dir<-rep(list(NA),length(unq.dir))
for(i in 1:length(unq.dir)){
	L.ind.dir[[i]]<-which(dir.per%in%unq.dir[i])
}


dir.list<-sapply( list.1,function(x)strsplit(x,"/")[[1]][2])
ind.list<-match(dir.list,unq.dir)

###################################################
ind.subsets<-match(unlist(L.IDs), two.lev.lab[,5])
print(table(is.na(ind.subsets)))

two.lev.lab.subsets<-two.lev.lab[ind.subsets,]

###################################################
lab.4059<-rep("",4059)
for(i in 1:length(L.IDs)){
	if(i%%100==0)print(i)
	
	ind.k<-L.ind.dir[[ind.list[i]]]
	ind.i<-match(L.IDs[[i]],two.lev.lab[ind.k,5])
	annot.i<-two.lev.lab[ind.k,,drop=F][ind.i,,drop=F]

	lab.vec<-paste(annot.i[,6],annot.i[,7],sep="::")
	tab.1<-table(lab.vec)

	lab.4059[i]<-names(which.max(tab.1))
}

lab.4059<-gsub("^::|::$","",lab.4059)

dir.4059<-sapply(list.1,function(x)strsplit(x,"/")[[1]][4])


save(lab.4059,dir.4059,list.1,file="step.10.info.all.rdata.RData")

###################################################
###################################################


load("info.all.rdata.RData")
mat.tab.old.new<-as.matrix(table(tis.cat,lab.4059))

net<-c("","","")
for(i in 1:(nrow(mat.tab.old.new))){
	for(j in 1:ncol(mat.tab.old.new)){
		if(mat.tab.old.new[i,j]==0)next
		tmp<-c(rownames(mat.tab.old.new)[i],
			colnames(mat.tab.old.new)[j],mat.tab.old.new[i,j])
		net<-rbind(net,tmp)
	}
}
net<-net[which(as.numeric(net[,3])>20),]

net[order(as.numeric(net[,3]),decreasing=T),][1:20,]
###################################################
ind.gpl570<-grepl("set-007-GPL570/",two.lev.lab[,4])
two.lev.lab.3<-two.lev.lab[ind.gpl570,]

org.tis.vec<-paste(two.lev.lab.3[,6],two.lev.lab.3[,7],sep="::")

table(two.lev.lab.3[,6]!="",two.lev.lab.3[,7]!="")


tab.vec<-table(org.tis.vec)
ind.20<-which(tab.vec>20)
names.20<-names(ind.20)

ind.tis.4<-which(org.tis.vec%in%names.20)
two.lev.lab.4<-two.lev.lab.3[ind.tis.4,]
tab.vec.4<-table(org.tis.vec[ind.tis.4])

save(two.lev.lab.4,file="two.lev.lab.570.RData")

###################################################
gse.4059<-sapply( list.1,function(x)strsplit(strsplit(x,"/")[[1]][4],"-")[[1]][1])
gse.all<-sapply(two.lev.lab[,4],function(x)strsplit(strsplit(strsplit(x,
	"/")[[1]][3],"-")[[1]][1],"_")[[1]][1])


###################################################
flag.0<-rep(1,length(L.IDs))
len.id<-sapply(L.IDs,length)

to<-cumsum(len.id)
from<-c(1,to[-length(to)]+1)


