vec.tis<-as.vector(as.matrix(tis[,-1]))
tab1<-table(vec.tis)
tab.sort<-sort(tab1,decreasing=T)

write.csv(tab.sort,file="top.tissues.csv")

table(grepl("brain",vec.tis,ignore.case = T))
table(grepl("liver",vec.tis,ignore.case = T))
table(grepl("heart",vec.tis,ignore.case = T))
table(grepl("epithel",vec.tis,ignore.case = T))

tis[grepl("epithel",tis[,2],ignore.case = T),]


vec.tis[grepl("heart",vec.tis,ignore.case = T)]