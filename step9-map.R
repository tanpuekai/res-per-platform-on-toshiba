load("all.IDS.RData")
tis.all.0<-read.csv("tis-info-all-platforms.txt",skip=1,header=F,sep="\t")
tis.all<-read.csv("tis-info-all-platforms-2.txt",skip=1,header=F,sep="\t")


dir.all<-sapply(list.1,function(x)strsplit(x,"/")[[1]][4])
list.all<-sapply(strsplit(list.1,"/"),function(x)strsplit(x[4],"-")[[1]][1])
L.ind.map<-rep(list(NA),length(L.IDs))
platforms<-sapply(list.1,function(x)strsplit(x,"/")[[1]][2])
#ind.100<-sample(length(L.IDs),100)
for(i in 1:length(L.IDs)){
#for(i in ind.100){
	if(i%%10==0)
		print(i)
	mat.tmp<-sapply(L.IDs[[i]],function(x)grepl(x,as.character(tis.all[,2])))
	vec.tmp<-apply(mat.tmp,1,sum)
	ind.tmp<-which(vec.tmp==length(L.IDs[[i]]))
	L.ind.map[[i]]<-ind.tmp#as.numeric(n.tmp)
}
#save(L.ind.map,file="step9.RData")
###################################################
load("step9.RData")

ind.map<-rep(NA,length(L.ind.map))
for(i in 1:length(L.ind.map)){
	if(length(L.ind.map[[i]])==1)
		ind.map[i]<-L.ind.map[[i]]
	else{
		str1<-as.character(tis.all[L.ind.map[[i]],1])
		ind.str1<-which(grepl(list.all[i],str1)&grepl(platforms[i],str1))
		ind.map[i]<-L.ind.map[[i]][ind.str1]
	}
}

###################################################
source("G:/weiyun/my-func-lib/string-funcs.R")



###################################################
source("step1-funcs.R")

L.tab<-L.tis2<-L.tis1<-L.source<-L.title<-L.gsm<-rep(list(NA),length(ind.map))
tis.cat<-rep(NA,length(ind.map))
for(i in 1:length(ind.map)){
	if(i%%100==0)print(i)
	k<-ind.map[i]
	L.gsm[[i]]<-str.process(tis.all[k,2])

	ind.i<-which(L.gsm[[i]]%in%L.IDs[[i]])
	L.title[[i]]<-str.process(tis.all[k,3])[ind.i]	
	L.source[[i]]<-str.process(tis.all[k,4])[ind.i]
	L.tis1[[i]]<-str.process(tis.all[k,5])[ind.i]
	L.tis2[[i]]<-str.process(tis.all[k,6])[ind.i]

	longstr<-c(L.title[[i]],L.source[[i]],L.tis1[[i]],L.tis2[[i]])

	t1<-sapply(L.pat,function(x)grepl(x[[1]],longstr,ignore.case =T))
	s.t1<-apply(t1,2,sum)
	s.t2<-s.t1[s.t1>0]
	if(max(s.t1)>0){
		tis.cat[i]<-names(which.max(s.t1))
		L.tab[[i]]<- s.t2
	}
}

	
gsm1<-sapply(L.gsm,paste,collapse=":::")
tit<-sapply(L.title,paste,collapse=":::")
sourc<-sapply(L.source,paste,collapse=":::")
tis1<-sapply(L.tis1,paste,collapse=":::")
tis2<-sapply(L.tis2,paste,collapse=":::")

###################################################

rdata<-as.character(sapply(as.character(tis.all[,1]),
	function(x)strsplit(x,"/")[[1]][3])[ind.map])
rdata<-gsub("^ | $","",rdata)

info.all<-data.frame(dir.all,list.all,rdata,gsm1,tit,sourc,tis1,tis2)
rownames(info.all)<-NULL
###################################################
save(info.all,tis.cat,L.tab,platforms,list.1,file="info.all.rdata.RData")





