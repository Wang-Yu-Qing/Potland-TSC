# INITIALIZING CLUSTERING CENTERS:
# temporal aggregation into 5-min:
flow_5<-NULL
speed_5<-NULL
occ_5<-NULL
for(i in 1:288*61){
  flow_5<-c(flow_5,sum(new_data[(15*i-14):(15*i),3])) # veh/3ln/5min
  speed_5<-c(speed_5,mean(new_data[(15*i-14):(15*i),4]))
  occ_5<-c(occ_5,mean(new_data[(15*i-14):(15*i),5]))
}
train_sdata<-scale(data.frame(flow_5,speed_5,occ_5),scale = T)

# 1st clustering:
library('e1071')
fcm_5<-cmeans(train_sdata,3,method='cmeans')

# extract 3rd cluster observations
train_sdata<-data.frame(train_sdata,state=fcm_5$cluster)
train_sdata_3<-train_sdata[train_sdata$state==2,c(1:3)]

# 2nd clustering:
fcm_5_3<-cmeans(train_sdata_3,2,method='cmeans')

# merging clustering centers:
a<-fcm_5$centers[1,]
fcm_5$centers[1,]<-fcm_5$centers[3,]
fcm_5$centers[2,]<-a
fcm_5$centers[3,]<-fcm_5_3$centers[2,]
fcm_5$centers<-rbind(fcm_5$centers,fcm_5_3$centers[1,])
rownames(fcm_5$centers)[4]<-'4'

his_mean_f<-mean(flow_5)
his_sd_f<-sd(flow_5)
his_mean_o<-mean(occ_5)
his_sd_o<-sd(occ_5)
his_mean_s<-mean(speed_5)
his_sd_s<-sd(speed_5)


## KEEP 'fcm_5' and REMOVE other objects:
rm(list=ls()[!(ls()=='fcm_5'|ls()=='his_mean_f'|ls()=='his_sd_f'|ls()=='his_mean_o'|ls()=='his_sd_o'|ls()=='his_mean_s'|ls()=='his_sd_s')])


## convert scale:
# flow:
(fcm_5$centers[,1]*sd(flow_5-mean(flow_5))+mean(flow_5))*4
# speed:
fcm_5$centers[,2]*sd(speed_5)+mean(speed_5)
# occ:
fcm_5$centers[,3]*sd(occ_5-mean(occ_5))+mean(occ_5)



## mean and sd of 20-sec :
# flow
mean(new_data[,3])
sd(new_data[,3])
# speed
mean(new_data[,4])
sd(new_data[,4])
# occ
mean(new_data[,5])
sd(new_data[,5])






