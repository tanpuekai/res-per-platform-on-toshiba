load("mds.570.RData")

#####################################
dist.mat<-as.matrix(dist.1)

lab.4309<-paste(two.lev.lab.5[,4],two.lev.lab.5[,5],sep="::")
tab.lab<-table(lab.4309)

dist.org<-matrix(NA,length(tab.lab),length(tab.lab))
rownames(dist.org)<-colnames(dist.org)<-names(tab.lab)

for(i in 1:nrow(dist.org)){
	ind.i<-which(lab.4309==names(tab.lab)[i])
	for(j in 1:length(tab.lab)){
		ind.j<-which(lab.4309==names(tab.lab)[j])
		dist.org[i,j]<-mean(dist.mat[ind.i,ind.j])
	}
}
d.org.1<-as.dist(dist.org)
hc<-hclust(d.org.1)
plot(hc)