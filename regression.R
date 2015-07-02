##############################
# before and after blockwise standardization (BWSTD)
##############################
d.std<-d1
pls<-unique(platforms)
for(i in 1:length(pls)){
	for(j in 1:length(pls)){
		ind.i<-which(platforms==pls[i])
		ind.j<-which(platforms==pls[j])
		block.ij<- d1[ind.i,ind.j]
		block.ij<-(block.ij-mean(block.ij))/sd(block.ij)
		d.std[ind.i,ind.j]<-block.ij
	}
}

drawHeatmap(d.std)

library(biclust)
####################
memb.o<-as.character(sample.tis.sel$dis.cat[])


##############################
# (1) before and after blockwise standardization (BWSTD)
# ordered by platform
##############################
ord<-order(platforms)
drawHeatmap(d1[ord,ord])
drawHeatmap(d.std[ord,ord])

d3<-(abs(d1))^(1/2)*sign(d1)
drawHeatmap(d3[ord,ord])

d3<-(abs(d.std))^(1/2)*sign(d.std)
drawHeatmap(d3[ord,ord])

##############################
tab.pl<-table(platforms)#[unique(platforms)]
col.bar<-rep(sample(length(tab.pl)),tab.pl)
drawHeatmap(t(as.matrix(col.bar)))
abline(v=cumsum(tab.pl),col="white")
##############################
# (2) before BWSTD
# ordered by tissue labels
##############################
d3<-(abs(d1))^(1/2)*sign(d1)
plot(apply(d3,1,sum),apply(d3,1,sd))

ind.out<- which(apply(d3,1,sum)< 1200 & apply(d3,1,sd) < 0.25)
tab.o<-table(memb.o[-ind.out])
ord<-order(memb.o[-ind.out])
d4<-d1[-ind.out,-ind.out][ord,ord]

drawHeatmap(d4)

##############################
# (3) after BWSTD
# ordered by tissue labels
##############################
d3<-(abs(d.std))^(1/2)*sign(d.std)
plot(apply(d3,1,sum),apply(d3,1,sd))

ind.out<- which(apply(d3,1,sum)< -1200 & apply(d3,1,sd) < 0.5)
tab.o<-table(memb.o[-ind.out])
ord<-order(memb.o[-ind.out])
d4<-d3[-ind.out,-ind.out][ord,ord]

drawHeatmap(d4)
##############################
# (4) before BWSTD, platform GPL 570 only
# ordered by tissue labels
##############################
ind.pl<-platforms=="GPL570"
d.gpl570<-d1[ind.pl,ind.pl]

d3<-(abs(d.gpl570))^(1/2)*sign(d.gpl570)
plot(apply(d3,1,sum),apply(d3,1,sd))
ind.out<- which(apply(d3,1,sum)< 600& apply(d3,1,sd) < 0.25)
tab.o<-table(memb.o[ind.pl][-ind.out])
ord<-order(memb.o[ind.pl][-ind.out])
d4<-d3[-ind.out,-ind.out][ord,ord]

drawHeatmap(d4)
##############################
# (5) after BWSTD, platform GPL 570 only
# ordered by tissue labels
##############################
ind.pl<-platforms=="GPL570"
d.gpl570<-d.std[ind.pl,ind.pl]

d3<-(abs(d.gpl570))^(1/2)*sign(d.gpl570)
plot(apply(d3,1,sum),apply(d3,1,sd))
ind.out<- which(apply(d3,1,sum)< 0& apply(d3,1,sd) < 0.6)
tab.o<-table(memb.o[ind.pl][-ind.out])
ord<-order(memb.o[ind.pl][-ind.out])
d4<-d3[-ind.out,-ind.out][ord,ord]

drawHeatmap(d4)
##############################
##############################

col.bar<-rep(sample(length(tab.o)),tab.o)
drawHeatmap(t(as.matrix(col.bar)))
abline(v=cumsum(tab.o),col="white")
##############################









