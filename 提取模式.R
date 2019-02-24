setwd('D:/data/')
a=list.files('1015008/train')                      #c
dir=paste('D:/data/1015008/train/',a,sep="")        #c
n=length(dir)

train_list<-list()
for(i in 1:5){
  train_list[[i]]<-vector('numeric',2880)
}

for(i in 1:n){
train_data<-read.xlsx(dir[i],'Sheet1')

j<-2
while (j<2881) {
  (if(train_data$time[j]==train_data$time[j-1]) 
  {train_data<-train_data[-j,] 
  j<-j-1})
  j<-j+1
}

for(k in 1:2880){if(is.na(train_data$occ[k]))
  train_data$occ[k]<-train_data$occ[k-1]}


train_list[[i]]<-train_data$occ}
raw_mode<-(train_list[[1]]+train_list[[2]]+train_list[[3]]+train_list[[4]]+train_list[[5]])/5
t<-strptime('0:00:00','%H:%M:%S')+(0:2879)*30
t_l<-c(1:2880)
l_mode<-loess(raw_mode~t_l,span=0.01)               #c
l_mode$fitted[l_mode$fitted<0]<-0
plot(t,raw_mode,pch=20,cex=0.5,xlab='time',ylab='occ(%)',main='occupancy mode')
lines(t,l_mode$fitted,col='red',lwd=2)

valley<-l_mode$fitted[1:30]
norm<-l_mode$fitted[1730:1759] 
peak<-l_mode$fitted[874:903]                     #c
centroids<-list(valley,norm,peak)
plot(c(1:30),valley,type='l',xlab='time spot',ylab='occupancy(%)',main='valley')
plot(c(1:30),norm,type='l',xlab='time spot',ylab='occupancy(%)',main='norm')
plot(c(1:30),peak,type='l',xlab='time spot',ylab='occupancy(%)',main='peak')