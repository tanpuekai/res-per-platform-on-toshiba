load("tissue-categorization.RData")
tap1<-tapply(tis.sepc.annot$dis.cat,tis.sepc.annot$geo.acc,function(x)length(unique(x)))
tap2<-tap1[tap1==1]

tis.2<-tis.sepc.annot[tis.sepc.annot$geo.acc%in%names(tap2),]
tis.3<-tis.2[!tis.2$dis.cat%in%c(-1),]

source("step2-funcs.R")
source("step1-funcs.R")
#########################
load("combine.RData")

ind.acc<-which(geo.acc%in%tis.3$geo.acc)
table(ind.acc)
ind.tis<-match(geo.acc[ind.acc],tis.3$geo.acc)
sample.tis<-tis.3[ind.tis,]
#########################
library(preprocessCore)
is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))
#########################
dat.unq.mean<-Mean.mat[,ind.acc]
dat.unq.var<-Var.mat[,ind.acc]

L.res.m<- process(dat.unq.mean)
L.res.var<- process(dat.unq.var)

save(L.res.m,L.res.var,file="step1-process.RData")
##################################################
##################################################
load("step1-process.RData")
attach(L.res.m)
attach(L.res.var)
#########################

d2<-as.dist(1-d1)
d3<-(abs(d.std))^(1/2)*sign(d.std)

d2<-as.dist(max(d3)-d3)

h1<-hclust(d2)
plot(h1)

memb.pred<-cutree(h1,k=270)
table(platforms,memb.pred)
#########################
memb.o<-as.character(sample.tis.sel$dis.cat[])

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
detach(L.res.m)

detach(L.res.var)

