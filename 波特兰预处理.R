a=list.files('d:/data/s1047')
dir=paste('d:/data/s1047/',a,sep="")
n=length(dir)
ndir=paste('d:/data/1047/',a,sep="")
for (k in 1:n) {
raw_data<-read.csv(dir[k],stringsAsFactors=FALSE)
detectorid<-rep(raw_data$detectorid[1],each=220320)
starttime<-rep(raw_data$starttime[1],each=220320)
volume<-vector('numeric',length=220320)
for (i in 1:220320) {volume[i]<-NA}
speed<-vector('numeric',length=220320)
for (i in 1:220320) {speed[i]<-NA}
occupancy<-vector('numeric',length=220320)
for (i in 1:220320) {occupancy[i]<-NA}
empty<-data.frame(detectorid,starttime,time,volume,speed,occupancy)

index<-0
j<-1
while (j<220320) {
  if(!(raw_data$time[j]==empty$time[j]))
  {raw_data<-rbind(rbind(raw_data[1:j-1,],empty[j,]),raw_data[j:length(raw_data$time),])
  index<-c(index,j)}
  
  j<-j+1
}
rownames(raw_data)<-c(1:220320)
raw_data$volume<-as.numeric(raw_data$volume)
raw_data$speed<-as.numeric(raw_data$speed)
raw_data$occupancy<-as.numeric(raw_data$occupancy)
for (i in 1:220320) {if(is.na(raw_data$volume[i]))
{raw_data$volume[i]<-raw_data$volume[i-1]}}
for (i in 1:220320) {if(is.na(raw_data$occupancy[i]))
{raw_data$occupancy[i]<-raw_data$occupancy[i-1]}}
for (i in 1:220320) {if(is.na(raw_data$speed[i]))
{raw_data$speed[i]<-round(runif(1,60,100),digits = 0)}}
for (i in 1:220320) {if(raw_data$volume[i]==0&raw_data$occupancy[i]==0&raw_data$speed[i]>59)
{raw_data$volume[i]<-round(runif(1,1,3),digits = 0)}}
for (i in 1:220320) {if(raw_data$volume[i]==0&raw_data$occupancy[i]<4&raw_data$speed[i]==0)
{raw_data$volume[i]<-round(runif(1,1,3),digits = 0)
 raw_data$speed[i]<-round(runif(1,60,100),digits = 0)}}

write.csv(raw_data,ndir[k],row.names = F)
}