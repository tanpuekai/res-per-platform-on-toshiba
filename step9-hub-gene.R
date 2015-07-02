deg.log<-log(mat.deg.all+1)
s1<-apply(mat.deg.all==0,1,sum)
ind.row<-which(s1<ncol(mat.deg.all)/2)
deg.log.2<-deg.log[ind.row,]

s2<-apply(deg.log.2==0,2,sum)
ind.col<-which(s2<nrow(deg.log.2)/2)
deg.log.3<-deg.log.2[,ind.col]

library(preprocessCore)

deg.q<-normalize.quantiles(deg.log.3)

tis.cat.q<-tis.cat.2516[ind.col]
gene.q<- uniqGene[ind.row]
########################################
library(agricolae)

tab.tis<-table(tis.cat.q)
tab.tis.10<-tab.tis[tab.tis>10]
names.10<-names(tab.tis.10)
deg.q.10<-deg.q[,tis.cat.q%in%names.10]

tis.10<-tis.cat.q[tis.cat.q%in%names.10]
p.vec<-rep(NA,nrow(deg.q.10))
tis.unq<-unique(tis.10)
groups<-matrix(NA,nrow(deg.q.10),length(tis.unq))
colnames(groups)<-tis.unq
for(i in 1:nrow(deg.q.10)){
	if(i%%500==0){print(i)}
	vec.i<-deg.q.10[i,]
	ind.na<-which(is.na(vec.i))
	num.non.na<-sum(!is.na(vec.i))
	if(num.non.na<100)
		next
	vec.i.non.mis<-vec.i[-ind.na]
	tis.all.non.mis<-tis.10[-ind.na]

	tis.fac<-as.factor(tis.10)
	tmp<-kruskal.test(vec.i~tis.fac)
	p.vec[i]<-tmp$p.value
	if(p.vec[i]<1e-6){
		tmp.a<-aov(vec.i~tis.fac)
		out<-HSD.test(tmp.a,"tis.fac")
		g.out<-out$groups
		ind.a<-which(g.out$M=="a")
		str.a<-gsub(" ","",g.out$trt[ind.a])
		groups[i,str.a]<-1
	}
}
save(groups,gene.q,file="step9-tis-spec.hub.RData")


########################################
s1<-apply(groups==1,1,sum,na.rm=T)
ind.unq<-which(s1==1)
groups.unq<-groups#[ind.unq,]

L.hub.tis<-rep(list(0),ncol(groups.unq))
names(L.hub.tis)<-colnames(groups.unq)
for(i in 1:ncol(groups.unq)){
	hub.g<-gene.q[which(groups.unq[,i]==1)]
	L.hub.tis[[i]]<-hub.g
}

L.hub.tis<-L.hub.tis[sort(names(L.hub.tis))]
########################################
load("G:/weiyun/databases/wKGGseq/wKGGseq.combined.RData")

mat.overlap<-matrix(NA,length(L.combined),length(L.hub.tis))
colnames(mat.overlap)<-names(L.hub.tis)
rownames(mat.overlap)<-c(names(L.combined))

for(i in 1:length(L.combined)){
	L.1<-sapply(L.hub.tis,function(x) length(intersect(x,L.combined[[i]])))
	mat.overlap[i,]<-L.1#/sapply(L.hub.tis,length)/length(L.combined[[i]])
}

write.csv(mat.overlap,file="step9-hub.gwas.csv")

########################################






