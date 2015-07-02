
extract.key<-function(str1){
	str2<-sapply(as.list(str1),function(x)strsplit(x,"@")[[1]])
	str3<-unlist(str2)
	str4<-tolower(str3)

	str5<-gsub("^ ", "",str4)
	return(str5)
}

#############################

str1<-as.character(tis[,2])
str2<-as.character(tis[,3])
str3<-as.character(tis[,4])

str4<-c(str1,str2,str3)
str5<-extract.key(str4)
tab1<-sort(table(str5),decreasing=T)

write.csv(tab1,file="tissue-lib.csv")