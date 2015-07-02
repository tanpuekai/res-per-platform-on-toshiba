load("degree-combine.RData")
load("meanvar-combine.RData")

k<-sample(3017,1)
cor.k<-rep(NA,3017)
for(k in 1:3017){
	cor.k[k]<-cor(res.pos$Deg.mat[,k],res.neg$Deg.mat[,k],use="complete.obs")
}

plot(cor.k)

##############################

prefix.mv<-gsub("meanvar-","",prefix)
table(res.pos$prefix%in%prefix.mv)

tab.f<-table(res.pos$full.name)

plot(as.data.frame(res.pos$Deg.mat[,c(578, 2234, 2404)]))

unq.prefix<-unique(res.pos$prefix)
ind.unq<-match(unq.prefix,res.pos$prefix)
pl.unq<-sapply(res.pos$files,function(x)strsplit(x,"/")[[1]][1])[ind.unq]

ind.mv<-match(unq.prefix,prefix.mv)
##############################
load("info.all.rdata.RData")
load("step.10.info.all.rdata.RData")
info.prefix<-gsub(".RData","",as.character(dir.4059))
ind.info<-match(unq.prefix,info.prefix)
tis.lab.2516<-lab.4059[ind.info]
tis.cat.2516<-tis.cat[ind.info]

tab.2516<-table(tis.cat.2516)

mat.deg.all<-res.all$Deg.mat[,ind.unq]
mat.deg.pos<-res.pos$Deg.mat[,ind.unq]
mat.deg.neg<-res.neg$Deg.mat[,ind.unq]
mat.mean<-Mean.mat[,ind.mv]
mat.var<-Var.mat[,ind.mv]

save(mat.deg.all,mat.deg.pos,mat.deg.neg,
	mat.mean,mat.var,uniqGene,tab.2516,
	tis.cat.2516,tis.lab.2516,unq.prefix,pl.unq,
	file="step9-combined.RData")



