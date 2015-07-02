load("tis.spec.uniq.570.RData")
load("tis.spec.uniq.RData")

#tis<-tis.gpl570

source("step1-funcs.R")
####################
L.label<-tis.label(tis)
L.ind<-L.label$L.ind 
mat.tis<-L.label$mat.tis

sapply(L.ind,length)
#write.csv(mat.tis,file="mat.tis.gpl570.csv")

####################
dis.cat<-rep(-1,nrow(tis))
for(i in 1:length(L.ind)){
	dis.cat[L.ind[[i]]]<-cat.names[i]
}
table(dis.cat)

####################
str1<-sapply(strsplit(tis[,1],"/"),function(x)x[3])
prefix<-sapply(strsplit(str1,"_"),function(x)x[1])
geo.acc<-sapply(strsplit(prefix,"-"),function(x)x[1])

####################

tab.tis<-table(dis.cat)

gz.1<-gsub("matrix.txt.gz ","matrix.txt.gz",tis[,1])
tis.spec.annot<-data.frame(gz.1,dis.cat,geo.acc,prefix,tis)
tis.spec.annot<-tis.sepc.annot[order(tis.sepc.annot$dis.cat),][-1,]

write.csv(tis.spec.annot,file="tissue-categorization.csv")
save(tis.spec.annot,L.pat,file="tissue-categorization.RData")

####################
#tis.left<-tis[dis.cat==-1,]
#sort(table(tis.left[,3]),decreasing=T)[11:20]



