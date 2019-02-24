raw_data<-data.frame(raw_data[with(raw_data,detectorid==1345|detectorid==1346|detectorid==1347|detectorid==1348),])   #subset detectors
date<-vector('character',nrow(raw_data))
time<-vector('character',nrow(raw_data))
for (i in 1:nrow(raw_data)) {
  a<-strsplit(as.character(raw_data$starttime[i]),' ')
  date[i]<-a[[1]][1]
  time[i]<-strsplit(strsplit(a[[1]][2],' ')[[1]][1],'-')[[1]][1]}
raw_data<-data.frame(raw_data$detectorid,date,time,raw_data[,3:5])
colnames(raw_data)<-c('detectorid','date','time','volume','speed','occupancy')               #split date and time
n<-length(levels(raw_data$date))                                                             #number of days
n<-n*4320*4
time<-levels(raw_data$time)
date<-levels(raw_data$date)
time<-rep(time,times=61*4)
date<-rep(date,each=4320)
date<-rep(date,times=4)
detectorid<-rep(c(1345,1346,1347,1348),each=61*4320)
volume<-vector('numeric',length=n)
for (i in 1:n) {volume[i]<-NA}
speed<-vector('numeric',length=n)
for (i in 1:n) {speed[i]<-NA}
occupancy<-vector('numeric',length=n)
for (i in 1:n) {occupancy[i]<-NA}
empty<-data.frame(detectorid,date,time,volume,speed,occupancy)
index<-0
for (i in 1:n) {
  print(i)
  if(!(raw_data$time[i]==empty$time[i]))
  {raw_data<-rbind(rbind(raw_data[1:i-1,],empty[i,]),raw_data[i:length(raw_data$time),])
  index<-c(index,i)}}
raw_data$volume<-as.numeric(raw_data$volume)
raw_data$speed<-as.numeric(raw_data$speed)
raw_data$occupancy<-as.numeric(raw_data$occupancy)
for (i in 1:n) {print(i)
  if(is.na(raw_data$volume[i]))
  {raw_data$volume[i]<-raw_data$volume[i-1]}}
for (i in 1:n) {print(i)
  if(is.na(raw_data$occupancy[i]))
  {raw_data$occupancy[i]<-raw_data$occupancy[i-1]}}
for (i in 1:n) {print(i)
  if(is.na(raw_data$speed[i]))
  {raw_data$speed[i]<-round(runif(1,60,100),digits = 0)}}
for (i in 1:n) {print(i)
  if(raw_data$volume[i]==0&raw_data$occupancy[i]==0&raw_data$speed[i]>59)
  {raw_data$volume[i]<-round(runif(1,1,3),digits = 0)}}
for (i in 1:n) {print(i)
  if(raw_data$volume[i]==0&raw_data$occupancy[i]<4&raw_data$speed[i]==0)
  {raw_data$volume[i]<-round(runif(1,1,3),digits = 0)
  raw_data$speed[i]<-round(runif(1,60,100),digits = 0)}}
for (i in 1:n) {print(i)
  if((!raw_data$volume[i]==0)&raw_data$occupancy[i]==0&raw_data$speed[i]>59)
  {raw_data$occupancy[i]<-round(runif(1,1,3),digits = 0)}}                                            #pretreatment

data1<-raw_data[raw_data$detectorid==1345,]
data2<-raw_data[raw_data$detectorid==1346,]
data3<-raw_data[raw_data$detectorid==1347,]
data4<-raw_data[raw_data$detectorid==1348,]


volume<-(data1$volume+data2$volume+data3$volume+data4$volume)
speed<-(data1$volume*data1$speed+data2$volume*data2$speed+data3$volume*data3$speed+data4$volume*data4$speed)/(data1$volume+data2$volume+data3$volume+data4$volume)
speed<-round(speed,digits = 0)
occ<-round((data1$occupancy+data2$occupancy+data3$occupancy+data4$occupancy)/4,digits = 0)
new_data<-data.frame(data1[,2:3],volume,speed,occ)
for (i in 1:nrow(new_data)) {if(is.na(new_data$speed[i]))                                          #volume=0 revise (speed/0)
{new_data$speed[i]<-round(runif(1,1,5),digits = 0)}}                                               #aggregation

rm(data1,data2,data3,data4,empty,raw_data)
day_data<-new_data[new_data$date=='2011-09-26',]                                     #date selection

volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(day_data$volume[(i*15-14):(i*15)])*12/4}
for (i in 1:288) {speed[i]<-mean(day_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(day_data$occ[(i*15-14):(i*15)])}
plot(volume,speed)
plot(occ,volume)