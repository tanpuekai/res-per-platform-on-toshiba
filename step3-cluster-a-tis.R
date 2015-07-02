ind.tis<-which(sample.tis.sel$dis.cat%in%c("leukemia","brain","mental-diseases","brain.cerebellum","brain.cerebrum","brain.SFC","brain.hippocamp",
	"brain.hypotha","brain.parietal","brain.temporal","brain.occipital","brain.sub.nigra","brain.prefrontal"))


d2<-as.dist(1-d1[ind.tis,ind.tis])
d3<-(abs(d.std[ind.tis,ind.tis]))^(1/2)*sign(d.std[ind.tis,ind.tis])

n1<-as.character(sample.tis.sel$dis.cat[ind.tis])
n1[n1%in%c("leukemia")]<-1

rownames(d3)<-colnames(d3)<-n1

d2<-as.dist(max(d3)-d3)

h1<-hclust(d2)
plot(h1)


memb.pred<-cutree(h1,k=30)
table(as.character(sample.tis.sel$dis.cat[ind.tis]),memb.pred)
