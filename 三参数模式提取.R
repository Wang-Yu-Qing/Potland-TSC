setwd('D:/data/station1049')
a=list.files('train')                      #c
dir=paste('D:/data/station1049/train/',a,sep="")        #c
n=length(dir)

train_list<-list()
for(i in 1:5){
  train_list[[i]]<-vector('numeric',4320)          #this is a vector but not a data frame!
}

for(i in 1:n){
  train_data<-read.xlsx(dir[i],'Sheet1')
  train_list[[i]]<-train_data[,3:5]}

  merge_data<-train_list[[1]]
  for (i in 2:5) {merge_data<-rbind(merge_data,train_list[[i]])}    #rbind

fa.parallel(merge_data,n.iter = 100)
pc<-principal(merge_data,nfactors = 1)


for (q in 1:5) {train_list[[q]]<-scale(train_list[[q]])
  train_list[[q]]<-data.frame(train_list[[q]])           #convert the vector into data frame!So the '$' can be used!
  train_list[[q]]$prin<-train_list[[q]]$volume*0.896+train_list[[q]]$occ*0.961+train_list[[q]]$speed*-0.821
  }                                                                                 #compute scores

raw_mode<-(train_list[[1]]$prin+train_list[[2]]$prin+train_list[[3]]$prin+train_list[[4]]$prin+train_list[[5]]$prin)/5
t<-strptime('0:00:00','%H:%M:%S')+(0:4319)*20
t_l<-c(1:4320)
l_mode<-loess(raw_mode~t_l,span=0.01)               #c
plot(t,raw_mode,pch=20,cex=0.5,xlab='time',ylab='prin',main='prin mode')
lines(t,l_mode$fitted,col='red',lwd=2)

valley<-l_mode$fitted[1:15]
norm<-l_mode$fitted[1761:1775] 
peak<-l_mode$fitted[1301:1315]                     #c
centroids<-list(valley,norm,peak)
plot(c(1:15),valley,type='l',xlab='time spot',ylab='score',main='valley')
plot(c(1:15),norm,type='l',xlab='time spot',ylab='score',main='norm')
plot(c(1:15),peak,type='l',xlab='time spot',ylab='score',main='peak')