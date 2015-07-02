load("per-individual-annot.RData")
source("G:/weiyun/my-func-lib/two-level-tissue-system.R")

###################################################
dir.per<-sapply(as.character(per.ind.annot[,3]),function(x)strsplit(x,"/")[[1]][2])
two.lev.lab<-cbind(dir.per,as.matrix(per.ind.annot[,1:4]),"","")

###################################################

for(i in seq(max(per.ind.annot[,1]))){
	if(i%%100==0)print(i)

	ind.i<-which(per.ind.annot[,1]==i)

	annot.i<-per.ind.annot[ind.i,]
	str.i<-as.character(annot.i[,5])

	o1<-sapply(basic.organs,function(x)grepl(x[[1]],str.i,ignore.case =T))
	o2<-sapply(not.basic.organs,function(x)grepl(x[[1]],str.i,ignore.case =T))
	o3<-matrix(o1& ! o2,ncol=length(basic.organs))

	org.ind<-apply(o3,1,function(x){
		paste(names(basic.organs[x==T]),collapse="/")
	})

	two.lev.lab[ind.i,6]<-org.ind

	t1<-sapply(basic.cell.types,function(x)grepl(x[[1]],str.i,ignore.case =T))
	t2<-sapply(not.cell.types,function(x)grepl(x[[1]],str.i,ignore.case =T))
	t3<-matrix(t1 & ! t2,ncol=length(basic.cell.types))

	cell.ind<-apply(t3,1,function(x){
		prio.x<-priority.cells[x==T]
		if(max(prio.x)<100)
			ind.x<-which(x==T)[which.max(prio.x)]
		else
			ind.x<-which(x==T)[prio.x==100]
		paste(names(basic.cell.types[ind.x]),collapse="/")
	})

	two.lev.lab[ind.i,7]<-cell.ind
}

save(two.lev.lab,file="step10.two.lev.lab.RData")

###################################################
sort(table(two.lev.lab[,6]))
sort(table(two.lev.lab[,7]))

ind.empt.org<-two.lev.lab[,6] ==""
ind.empt.cel<-two.lev.lab[,7] ==""

ind.bld<-grepl("blood",two.lev.lab[,6])
ind.bld.2<-sapply(blood.vec.cells,function(x)grepl(x[[1]],two.lev.lab[,7],ignore.case =T))

table(ind.bld[!ind.empt.cel],apply(ind.bld.2[!ind.empt.cel,],1,sum)>0 )

###################################################
org.all<-sort(table(two.lev.lab[!ind.empt.org,6]),decreasing=T)
write.csv(as.matrix(org.all),file="step10.org.all.csv")

cell.all<-sort(table(two.lev.lab[!ind.empt.cel,7]),decreasing=T)
write.csv(as.matrix(cell.all),file="step10.org.cell.csv")
###################################################
ind.nor.org<-which(ind.empt.org & ! ind.empt.cel)
without.org<-per.ind.annot[ind.nor.org,]
#edit(without.org)

ind.no.cell<-which(!ind.empt.org & ind.empt.cel)
without.cell<-per.ind.annot[ind.no.cell,]
#edit(without.cell)

two.lev.lab.no.cell<-two.lev.lab[ind.no.cell,]
table(two.lev.lab.no.cell[,6])

ind.colon<-which(two.lev.lab.no.cell[,6]=="colon")
colon.annot<-without.cell[ind.colon,]
###################################################
ind.1<-grepl("/",two.lev.lab[,6])
ind.2<-grepl("/",two.lev.lab[,7])
table(ind.1|ind.2)
ind.3<- two.lev.lab[,6] =="" | two.lev.lab[,7]==""

ind.4<-ind.3

two.lev.lab.2<-two.lev.lab[!ind.4,]

org.tis.tab<-table(two.lev.lab.2[,6],two.lev.lab.2[,7])

write.csv(org.tis.tab,file="step10.org.tis.tab.csv")

###################################################

net<-c("","","")
for(i in 1:(nrow(org.tis.tab))){
	for(j in 1:ncol(org.tis.tab)){
		if(org.tis.tab[i,j]==0)next
		tmp<-c(rownames(org.tis.tab)[i],
			colnames(org.tis.tab)[j],org.tis.tab[i,j])
		net<-rbind(net,tmp)
	}
}
net<-net[which(as.numeric(net[,3])>20),]
attributes<-rbind(cbind(unique(net[,1]),1),
	cbind(unique(net[,2]),2))
write.table(net,file="step10.org.tis.bipartite.txt",quote=F,
	row.names=F,col.names=F,sep="\t")
write.table(attributes,file="step10.org.tis.bipartite-attributes.txt",quote=F,
	row.names=F,col.names=F,sep="\t")
###################################################
tab.5<-table(two.lev.lab[,5])
tab.6<-tab.5[!grepl("/",names(tab.5))]

tab.7<-table(two.lev.lab[,4])
tab.8<-tab.7[grepl("/",names(tab.7))]

###################################################



