load("set-007-GPL570/degree/GPL570-degree.RData")
attach(res.pos)

load("info.570.1269.rdata.RData")

deg.prefix<-gsub("net-|.txt.gz.pos.degree.txt","",dir.deg,)
info.prefix<-gsub(".RData","",as.character(info.570[,1]))

ind.prefix<-match(deg.prefix,info.prefix)
deg.info.813<-info.570[ind.prefix,]
tis.cat.813<-tis.cat[ind.prefix]
#########################
library(preprocessCore)
source("step7-funcs.R")
source("step1-funcs.R")

#########################
#########################
ind.non.na<-which(!is.na(tis.cat.813))
memb<-as.character(tis.cat.813[ind.non.na])

Mat.deg<-MatDeg[,ind.non.na]
Mat.deg[is.na(Mat.deg)]<-0
Mat.deg.log<-log(Mat.deg+1)
L.res.d<- process(Mat.deg.log)

save(L.res.d,ind.non.na,file="step7-pos.RData")
attach(L.res.d)

#########################
d2<-as.dist(1-d1)
d3<-(abs(d.std))^(1/2)*sign(d.std)

d2<-as.dist(d2)

h1<-hclust(d2)
plot(h1)

memb.pred<-cutree(h1,k=60)
tab1<-table(memb.o,memb.pred)

d3<-apply(tab1,2,function(x)x/table(memb.o))
rownames(d3)<-rownames(tab1)
d3<-d3[table(memb.o[-ind.out])>0,]
d4<-dist(d3)

cor1<-cor(t(tab1),method="spearman")
ind.cor.na<-which(apply(is.na(cor1),1,sum)>5)
d4<-as.dist( 1-cor1[,])

h2<-hclust(d4)
plot(h2)

#########################
detach(L.res.d)
detach(res.all)





