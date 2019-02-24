train_list<-list()
train_data<-new_data
occ<-train_data$occ
for (i in 1:(288*length(levels(new_data$date)))) {train_list[[i]]<-occ[(i*15-14):(i*15)]}
clusters_occ<-dtwclust(train_list,'fuzzy',k=3,distance = 'dtw',centroid='mean',seed=100)


train_list<-list()
train_data<-data.frame(scale(new_data[,3:5],scale = F))
fm20<-mean(new_data$volume)
sm20<-mean(new_data$speed)
om20<-mean(new_data$occ)
fsd20<-sd(new_data$volume)
ssd20<-sd(new_data$speed)
osd20<-sd(new_data$occ)
for (i in 1:(288*61)) {train_list[[i]]<-cbind(train_data[(i*15-14):(i*15),1],train_data[(i*15-14):(i*15),2],train_data[(i*15-14):(i*15),3])}
multi_cluster<-dtwclust(train_list,'partitional',k=3,distance = 'dtw',centroid='mean',seed=100)
rm(list=ls()[!(ls()=='fm20'|ls()=='sm20'|ls()=='om20'|ls()=='fsd20'|ls()=='ssd20'|ls()=='osd20')])





m_volume<-mean(all_data$volume)
m_speed<-mean(all_data$speed)
m_occ<-mean(all_data$occ)
centers<-data.frame()
for (i in 1:4) {centers[(i*15-14):(i*15),1]<-round((multi_cluster@centers[[i]][,1]+m_volume)*60,digits = 0) #flow rate???????????????volume
centers[(i*15-14):(i*15),2]<-round(multi_cluster@centers[[i]][,2]+m_speed,digits = 2)
centers[(i*15-14):(i*15),3]<-round(multi_cluster@centers[[i]][,3]+m_occ,digits = 2)}    #??????,????????????????????????
centers<-t(centers)
write.csv(centers,'d:/data/centers.csv',row.names = F,col.names = NA) # unit conversion       

m_volume<-mean(data_s3$volume)
m_speed<-mean(data_s3$speed)
m_occ<-mean(data_s3$occ)
centers<-data.frame()
for (i in 1:2) {centers[(i*15-14):(i*15),1]<-(multi_cluster_s3@centers[[i]][,1]+m_volume)*60
centers[(i*15-14):(i*15),2]<-multi_cluster_s3@centers[[i]][,2]+m_speed
centers[(i*15-14):(i*15),3]<-multi_cluster_s3@centers[[i]][,3]+m_occ}

clusters_occ@centers[[2]]<-clusters_s3_occ2@centers[[1]]
clusters_occ@centers[[4]]<-clusters_s3_occ2@centers[[2]]
multi_cluster@centers[[2]]<-clusters_s3_multi@centers[[2]]
multi_cluster@centers[[4]]<-clusters_s3_multi@centers[[1]]
rm(a,date,detectorid,new_data,day_data,speed,volume,occ,occupancy,time,n,index,train_data,i,train_list,agg_m,agg_v,all_data,data_s3,five_data,p_matrix,s_data,target_data,d,d_value,dir,j,k,m,multi_series,multi_state,multi_state_merge,occ_series,occ_state,occ_state_merge,occ_window,p,p_value,state,t)
