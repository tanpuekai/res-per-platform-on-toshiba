load("meanvar-combine.RData")

load("set-007-GPL570/degree/GPL570-degree.RData")
prefix<-gsub("meanvar-","",prefix)
ind.570<-grepl("set-007-",files)

load("info.570.1269.rdata.RData")
prefix.deg<-gsub("^net-|.txt.gz.all.degree.txt$","",res.all$dir.deg)

ind813<-match(prefix.deg,prefix[ind.570])
ind813.2<-which(ind.570)[ind813]

Mean.mat.813<-Mean.mat[,ind813.2]
Var.mat.813<-Var.mat[,ind813.2]

load("info.570.1269.rdata.RData")
info.prefix<-gsub(".RData","",as.character(info.570[,1]))
ind.info<-match(prefix.deg,info.prefix)
tis.cat.813<-tis.cat[ind.info]


###############
MatDeg.all<-res.all$MatDeg
MatDeg.pos<-res.pos$MatDeg
MatDeg.neg<-res.neg$MatDeg

save(MatDeg.all,
	MatDeg.pos,
	MatDeg.neg,
	Mean.mat.813,
	Var.mat.813,
	prefix.deg,
	tis.cat.813,
	file="step7-combine.570.RData")





