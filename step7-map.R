load("all.IDS.RData")
load("set-007-GPL570/degree/GPL570-degree.RData")
tis.570.1<-read.csv("tis-info-GPL570.txt",header=F,skip=1,sep="\t")
tis.570<-read.csv("tis-info-GPL570-2.txt",header=F,sep="\t")

flag.570<-grepl("set-007-GPL570",list.1)
L.IDs.570<-L.IDs[flag.570]

dir.570<-sapply(list.1[flag.570],function(x)strsplit(x,"/")[[1]][4])
list.570<-sapply(strsplit(list.1[flag.570],"/"),function(x)strsplit(x[4],"-")[[1]][1])
L.ind.map<-rep(list(NA),length(L.IDs.570))


for(i in 1:length(L.IDs.570)){
	if(i%%10==0)
		print(i)
	mat.tmp<-sapply(L.IDs.570[[i]],function(x)grepl(x,as.character(tis.570[,2])))
	vec.tmp<-apply(mat.tmp,1,sum)
	ind.tmp<-which(vec.tmp==length(L.IDs.570[[i]]))
	L.ind.map[[i]]<-ind.tmp#as.numeric(n.tmp)
}

ind.map<-rep(NA,length(L.ind.map))
for(i in 1:length(L.ind.map)){
	if(length(L.ind.map[[i]])==1)
		ind.map[i]<-L.ind.map[[i]]
	else{
		str1<-as.character(tis.570[L.ind.map[[i]],1])
		ind.str1<-which(grepl(list.570[i],str1))
		ind.map[i]<-L.ind.map[[i]][ind.str1]
	}
}

###################################################
str.process<-function(str.in){
	str.tmp1<-gsub("^ | $","",as.character(str.in))
	str.tmp2<-strsplit(str.tmp1,"@")[[1]]
	str.tmp2
}
str.cat<-function(str.in){
	str.tmp3<-str.process(str.in)
	str.out<-paste(str.tmp3,collapse=":::")
	str.out
}


###################################################
source("step1-funcs.R")

L.tab<-L.tis2<-L.tis1<-L.source<-L.title<-L.gsm<-rep(list(NA),length(ind.map))
tis.cat<-rep(NA,length(ind.map))
for(i in 1:length(ind.map)){
	if(i%%100==0)print(i)
	k<-ind.map[i]
	L.gsm[[i]]<-str.process(tis.570[k,2])
	L.title[[i]]<-str.process(tis.570[k,3])	
	L.source[[i]]<-str.process(tis.570[k,4])
	L.tis1[[i]]<-str.process(tis.570[k,5])
	L.tis2[[i]]<-str.process(tis.570[k,6])

	longstr<-c(L.title[[i]],L.source[[i]],L.tis1[[i]],L.tis2[[i]])

	t1<-sapply(L.pat,function(x)grepl(x[[1]],longstr,ignore.case =T))
	s.t1<-apply(t1,2,sum)
	if(max(s.t1)>0){
		tis.cat[i]<-names(which.max(s.t1))
		L.tab[[i]]<- s.t1[s.t1>0]
	}
}

	
gsm1<-sapply(L.gsm,paste,collapse=":::")
tit<-sapply(L.title,paste,collapse=":::")
sourc<-sapply(L.source,paste,collapse=":::")
tis1<-sapply(L.tis1,paste,collapse=":::")
tis2<-sapply(L.tis2,paste,collapse=":::")

###################################################

rdata<-as.character(sapply(as.character(tis.570[,1]),
	function(x)strsplit(x,"/")[[1]][3])[ind.map])
rdata<-gsub("^ | $","",rdata)

info.570<-data.frame(dir.570,list.570,rdata,gsm1,tit,sourc,tis1,tis2)
rownames(info.570)<-NULL
###################################################
save(info.570,tis.cat,L.tab,file="info.570.1269.rdata.RData")





