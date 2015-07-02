num.na<-apply(is.na(mat.q),2,sum)
ind.na<-which(num.na<2e3)
mat.q.2<-mat.q[,ind.na]
org.tis<-paste(two.lev.lab.4[,4],two.lev.lab.4[,5],sep="::")[ind.na]

s1<-apply(mat.q.2,1,sd)

dist.1<-dist(t(mat.q.2[s1>1.5,]))

#####################################

load("mds.570.RData")

pdf("step10.tmp.pdf")
unq.org<-unique(org.tis)
for(i in 1:length(unq.org)){
	org.i<-unq.org[i]
	ind.org.i<-which(org.tis%in%org.i)
	boxplot(fit$points[ind.org.i,1]~dataset.names[ind.org.i])
}

dev.off()