gse.out<-c("GSE13849","GSE34205","GSE51835")
dataset.names<-sapply(two.lev.lab.5[,2],function(x)strsplit(x,"/")[[1]][3])
dataset.names<-gsub("_series_matrix.txt.gz","",dataset.names)

ind.out<-c(which(dataset.names%in%gse.out),1e6)



syms<-c(letters,LETTERS,0:9)
#####################################
ind.out<-which(fit$points[,1]>50&fit$points[,2]>50)
x <- fit$points[-ind.out,1]
y <- fit$points[-ind.out,2]

org.tis.clean<-org.tis[-ind.out]

num.lab<-as.numeric(as.factor(org.tis.clean))

col.lab<-rainbow(length(unique(num.lab)))[num.lab]
label<-syms[num.lab]

#####################################

pdf("step10.mds.570.pdf",height=16,width=16)

plot(x, y, col = col.lab, cex=0.6,pch=16,type='n')#,ylim=c(-40,60),xlim=c(-70,80))
text(x, y, labels=label, col = col.lab, cex=.99)

unq.id<-sort(unique(org.tis.clean))
legend("topright",legend =unq.id,ncol=3,
	col=col.lab[match(unq.id,org.tis.clean)],cex=1.0,
	pch=label[match(unq.id,org.tis.clean)])

dev.off()


#####################################

dev.new()
plot(1,type='n')
unq.id<-sort(unique(org.tis))
legend("left",legend =unq.id,ncol=3,
	col=col.lab[match(unq.id,org.tis)],cex=0.7,
	pch=let[match(unq.id,org.tis)])




