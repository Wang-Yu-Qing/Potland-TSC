a=list.files('d:/data/station1050/day/')
dir=paste('d:/data/station1050/day/',a,sep="")
train_list<-list()
load(dir[1])
all_data<-day_data
all_s_data<-data.frame(scale(all_data[,3:5],scale = F))
multi_series<-list()
for (i in 1:288) {multi_series[[i]]<-vector('numeric',15)}
for (i in 1:288) {multi_series[[i]]<-cbind(all_s_data[(i*15-14):(i*15),1],all_s_data[(i*15-14):(i*15),2],all_s_data[(i*15-14):(i*15),3])}
multi_state<-vector('numeric',288)
p<-predict(multi_cluster,multi_series[1])
p<-c(p[1],p[2],p[3])                                    
multi_state[1]<-which.max(p)  
for (i in 2:288) {p<-predict(multi_cluster,multi_series[i])
p<-c(p[1],p[2],p[3])   
if (abs(p[multi_state[i-1]]-max(p))<0.3) (multi_state[i]<-multi_state[i-1])       # Here is max()!
else (multi_state[i]<-which.max(p))}
multi_state_merge<-multi_state                                                                                    #multi data NO.1


for (k in 2:(length(dir)-1)) {
  load(dir[k])
  all_data<-rbind(all_data,day_data)
  s_data<-data.frame(scale(day_data[,3:5],scale = F))
  all_s_data<-rbind(all_s_data,s_data)
  multi_series<-list()
  for (i in 1:288) {multi_series[[i]]<-vector('numeric',15)}
  for (i in 1:288) {multi_series[[i]]<-cbind(s_data[(i*15-14):(i*15),1],s_data[(i*15-14):(i*15),2],s_data[(i*15-14):(i*15),3])}
  multi_state<-vector('numeric',288)
  p<-predict(multi_cluster,multi_series[1])
  p<-c(p[1],p[2],p[3])                                    
  multi_state[1]<-which.max(p)  
  for (i in 2:288) {p<-predict(multi_cluster,multi_series[i])
  p<-c(p[1],p[2],p[3])   
  if (abs(p[multi_state[i-1]]-max(p))<0.3) (multi_state[i]<-multi_state[i-1])       # Here is max()!
  else (multi_state[i]<-which.max(p))}
  
  multi_state_merge<-c(multi_state_merge,multi_state)}                                     #all data

m<-(length(dir)-1)*288                                                               #number of days
volume<-vector('numeric',m)
speed<-vector('numeric',m)
occ<-vector('numeric',m)
for (i in 1:m) {volume[i]<-sum(all_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:m) {speed[i]<-mean(all_data$speed[(i*15-14):(i*15)])}
for (i in 1:m) {occ[i]<-mean(all_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ,multi_state_merge)
colnames(five_data)[4]<-'multi_state' 
state<-multi_state_merge
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
qplot(occ,volume,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange"))
qplot(volume,speed,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue","lightblue","orange"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)
agg_m<-aggregate(five_data,by=list(cluster=state),FUN=mean)
agg_v<-aggregate(five_data,by=list(cluster=state),FUN=sd)                          # 3 states plot

all_s_data<-scale(all_data[,3:5],scale = F)
all_s_data<-data.frame(all_s_data,rep(multi_state_merge,each=15))
colnames(all_s_data)[4]<-'state'
data_s3<-all_s_data[all_s_data$state==3,]                                           #DO NOT SCALE WITHIN STATE 3 !!
n<-nrow(data_s3)/15
for (i in 1:n) {train_list[[i]]<-cbind(data_s3[(i*15-14):(i*15),1],data_s3[(i*15-14):(i*15),2],data_s3[(i*15-14):(i*15),3])}
clusters_s3_multi<-dtwclust(train_list,'fuzzy',k=2,distance = 'dtw',centroid = "mean",seed=1)
data_s3<-data.frame(data_s3[,1:3],rep(clusters_s3_multi@cluster,each=15))
colnames(data_s3)[4]<-'state'
data_s3$state[data_s3$state==1]<-4
data_s3$state[data_s3$state==2]<-3
all_data<-data.frame(all_data,rep(multi_state_merge,each=15))
colnames(all_data)[6]<-'state'
all_data[all_data$state==3,6]<-data_s3$state                                         # add state 



m<-(length(dir)-1)*288
state<-vector('numeric',m)
for (i in 1:m) {state[i]<-mean(all_data$state[(i*15-14):(i*15)])}                  # 4320 to 288 !!
volume<-vector('numeric',m)
speed<-vector('numeric',m)
occ<-vector('numeric',m)
for (i in 1:m) {volume[i]<-sum(all_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:m) {speed[i]<-mean(all_data$speed[(i*15-14):(i*15)])}
for (i in 1:m) {occ[i]<-mean(all_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ,state)
colnames(five_data)[4]<-'multi_state' 
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
qplot(occ,volume,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))
qplot(volume,speed,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)
agg_m<-aggregate(five_data,by=list(cluster=state),FUN=mean)
agg_v<-aggregate(five_data,by=list(cluster=state),FUN=sd)  
multi_cluster@centers[[3]]<-clusters_s3_multi@centers[[2]]
multi_cluster@centers[[4]]<-clusters_s3_multi@centers[[1]]
rm(all_s_data,a,date,detectorid,new_data,day_data,speed,volume,occ,occupancy,time,n,index,train_data,i,train_list,agg_m,agg_v,data_s3,five_data,p_matrix,s_data,target_data,d,d_value,dir,j,k,m,multi_series,multi_state,multi_state_merge,occ_series,occ_state,occ_state_merge,occ_window,p,p_value,state,t)
