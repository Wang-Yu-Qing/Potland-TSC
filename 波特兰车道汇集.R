a=list.files('c:/R_projects/potland/1047/')
dir=paste('c:/R_projects/potland/1047/',a,sep="")

data1<-read.csv(dir[1],stringsAsFactors=FALSE)
data2<-read.csv(dir[2],stringsAsFactors=FALSE)
data3<-read.csv(dir[3],stringsAsFactors=FALSE)


volume<-(data1$volume+data2$volume+data3$volume)
speed<-(data1$volume*data1$speed+data2$volume*data2$speed+data3$volume*data3$speed)/(data1$volume+data2$volume+data3$volume)
speed<-round(speed,digits = 0)
occ<-round((data1$occupancy+data2$occupancy+data3$occupancy)/3,digits = 0)
new_data<-data.frame(data1[,2:3],volume,speed,occ)
for (i in 1:220320) {if(is.na(new_data$speed[i]))
{new_data$speed[i]<-round(runif(1,1,5),digits = 0)}}

write.csv(new_data,'c:/R_projects/potland/1046/train_1047.csv',row.names = F)