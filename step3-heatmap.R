##############################
# before and after blockwise standardization (BWSTD)
##############################
library(biclust)

drawHeatmap(d1)


drawHeatmap(d.std)

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
d3<-(abs(d1))^(1/1.2)*sign(d1)
plot(apply(d3,1,sum),apply(d3,1,sd))

ind.out<- c(which(apply(d3,1,sum)< 50 & apply(d3,1,sd) < 0.1),1e6)
ord<-order(memb.o[-ind.out])
d4<-d3[-ind.out,-ind.out][ord,ord]
tab.o<-table(sort(memb.o[-ind.out]))

drawHeatmap(d4)

##############################
# (3) after BWSTD
# ordered by tissue labels
##############################
d3<-(abs(d.std))^(1/2)*sign(d.std)
plot(apply(d3,1,sum),apply(d3,1,sd))

ind.out<- which(apply(d3,1,sum)< -0200 & apply(d3,1,sd) < 0.4)
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
ind.out<- which(apply(d3,1,sum)< 0& apply(d3,1,sd) < 0.2)
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
ind.out<- which(apply(d3,1,sum)< -400 & apply(d3,1,sd) < 0.4)
tab.o<-table(memb.o[ind.pl][-ind.out])
ord<-order(memb.o[ind.pl][-ind.out])
d4<-d3[-ind.out,-ind.out][ord,ord]

drawHeatmap(d4)
##############################
##############################

col.bar<-rep(sample(length(tab.o)),tab.o)
drawHeatmap(t(as.matrix(col.bar)))
abline(v=cumsum(tab.o)+0.5,col="white")

as.matrix((tab.o))

##############################









