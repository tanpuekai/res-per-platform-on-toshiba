load("step7-all.RData")
L.res.d.all<-L.res.d
ind.all<-ind.non.na[L.res.d.all$ind.col]
load("step7-neg.RData")
L.res.d.neg<-L.res.d
ind.neg<-ind.non.na[L.res.d.neg$ind.col]

load("step7-pos.RData")
L.res.d.pos<-L.res.d
ind.pos<-ind.non.na[L.res.d.pos$ind.col]



load("set-007-GPL570/degree/GPL570-degree.RData")

plot(log(res.pos$MatDeg[,]),log(res.neg$MatDeg[,]),pch=".")
abline(0,1,col=2)