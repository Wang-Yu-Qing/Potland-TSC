a<-paste('d:/s/accident18/',paste(as.character(c(1:18)),'/',sep=''),sep='')
for(j in 1:18){
dir<-paste(a[j],list.files(a[j]),sep = '')
acc<-read.csv(dir[1],stringsAsFactors = F)
tf<-read.csv(dir[2],stringsAsFactors = F)
d<-vector()
for(i in 1:nrow(acc)){
  d[i]<-strsplit(acc$Start.Time[i],' ')[[1]][1]
}
d<-factor(d)
d<-levels(d)
d<-as.character(d)
f_name<-chartr('/','_',d)
o_path<-paste(a[j],f_name,sep = '')
o_path<-paste(o_path,'.csv',sep = '')
for(i in 1:length(d)){
  d_tf<-subset(tf,date==d[i])
  write.csv(d_tf,o_path[i])
}
}