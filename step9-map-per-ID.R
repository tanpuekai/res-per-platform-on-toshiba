tis.all<-read.csv("tis-info-all-platforms-2.txt",skip=1,header=F,sep="\t")

L.gsm<-rep(list(NA),nrow(tis.all))
for(i in 1:length(L.gsm)){
	L.gsm[[i]]<-str.process(tis.all[i,2])
}
len1<-sapply(L.gsm,length)
len.sum<-sum(len1)

mat.annot<-matrix("",len.sum,5)
for(i in 1:nrow(tis.all)){
	if(i%%10==0)
		print(i)
	gsm<-str.process(tis.all[i,2])
	title<-str.process(tis.all[i,3])
	source<-str.process(tis.all[i,4])
	tis1<-str.process(tis.all[i,5])
	tis2<-str.process(tis.all[i,6])

	if(length(tis1)==0)
		tis1<-NA
	if(length(tis2)==0)
		tis2<-NA
	mat.tmp<-cbind(gsm,title,source,tis1,tis2)

	if(i==1)
		ind.seq<-seq(len1[i])
	else
		ind.seq<-seq(sum(len1[seq(i-1)])+1,sum(len1[seq(i)]))

	mat.annot[ind.seq,]<-mat.tmp
}




