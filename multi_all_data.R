library('dtwclust')
load("D:/data/station1141/multi_3.RData")
multi_cluster<- as(multi_cluster, "TSClusters")
multi_cluster@call$centroid <- "fcm"

a=list.files('d:/data/station1141/day/')
dir=paste('d:/data/station1141/day/',a,sep="")
train_list<-list()
load(dir[1])
all_data<-day_data
all_s_data<-data.frame(scale(all_data[,3:5],scale = F))
multi_series<-list()
for (i in 1:288) {multi_series[[i]]<-vector('numeric',15)}
for (i in 1:288) {multi_series[[i]]<-cbind(all_s_data[(i*15-14):(i*15),1],all_s_data[(i*15-14):(i*15),2],all_s_data[(i*15-14):(i*15),3])}
multi_state<-vector('numeric',288)
multi_state[1]<-predict(multi_cluster,multi_series[1])  
for (i in 2:288) {d<-proxy::dist(multi_series[i],multi_cluster@centroids,method='dtw')
if (abs(d[multi_state[i-1]]-min(d))<30) (multi_state[i]<-multi_state[i-1])
else (multi_state[i]<-which.min(d))}
multi_state[multi_state==1]<-5
multi_state[multi_state==2]<-1
multi_state[multi_state==3]<-3
multi_state[multi_state==5]<-2
multi_state_merge<-multi_state                                                                                    #multi data NO.1

d_value<-rep(30,times=length(dir))
for (k in 2:(length(dir)-1)) {
  load(dir[k])
  all_data<-rbind(all_data,day_data)
  s_data<-scale(day_data[,3:5],scale = F)
  all_s_data<-rbind(all_s_data,s_data)
  multi_series<-list()
  for (i in 1:288) {multi_series[[i]]<-vector('numeric',15)}
  for (i in 1:288) {multi_series[[i]]<-cbind(s_data[(i*15-14):(i*15),1],s_data[(i*15-14):(i*15),2],s_data[(i*15-14):(i*15),3])}
  multi_state<-vector('numeric',288)
  multi_state[1]<-predict(multi_cluster,multi_series[1])  
  for (i in 2:288) {d<-proxy::dist(multi_series[i],multi_cluster@centroids,method='dtw')
  if (abs(d[multi_state[i-1]]-min(d))<d_value[k]) (multi_state[i]<-multi_state[i-1])
  else (multi_state[i]<-which.min(d))}
  multi_state[multi_state==1]<-5
  multi_state[multi_state==2]<-1
  multi_state[multi_state==3]<-3
  multi_state[multi_state==5]<-2
  
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
clusters_s3_multi<-dtwclust(train_list,'partitional',k=2,distance = 'dtw',centroid = "mean",seed=1)
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


#multi_cluster@centroids[[3]]<-clusters_s3_multi@centroids[[1]]
#multi_cluster@centroids[[4]]<-clusters_s3_multi@centroids[[2]]
#rm(all_data,all_s_data,a,date,detectorid,new_data,day_data,speed,volume,occ,occupancy,time,n,index,train_data,i,train_list,agg_m,agg_v,data_s3,five_data,p_matrix,s_data,target_data,d,d_value,dir,j,k,m,multi_series,multi_state,multi_state_merge,occ_series,occ_state,occ_state_merge,occ_window,p,p_value,state,t)


#qplot(occ,volume,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))
#qplot(volume,speed,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)





five_data_merge<-NULL
station<-NULL
five_data_merge<-rbind(five_data_merge,five_data)
station<-c(station,rep('d',times=nrow(five_data)))

five_data<-data.frame(five_data_merge,station)


ggplot(data=five_data,aes(x=occ,y=volume))+geom_point(aes(col=factor(multi_state)),alpha=0.5,size=0.7)+
  xlab('占有率（%）')+ylab('流量（辆/小时/车道）')+scale_colour_manual(values=c("blue","lightblue","orange", "red"),labels = c('状态一','状态二','状态三','状态四'))+
  guides(col=guide_legend(title=" "))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+
  facet_wrap(~station,nrow=2)
  
ggplot(data=five_data,aes(x=volume,y=speed))+geom_point(aes(col=factor(multi_state)),alpha=0.5,size=0.7)+
  xlab('流量（辆/小时/车道）')+ylab('速度（英里/小时）')+scale_colour_manual(values=c("blue","lightblue","orange", "red"),labels = c('状态一','状态二','状态三','状态四'))+
  guides(col=guide_legend(title=" "))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+
  facet_wrap(~station,nrow=2)

colnames(five_data)[4]<-'state'
five_data$state<-as.character(five_data$state)
five_data$state[five_data$state=='state1']<-'1'
five_data$state[five_data$state=='state2']<-'2'
five_data$state[five_data$state=='state3']<-'3'
five_data$state[five_data$state=='state4']<-'4'

ggplot(data=five_data,aes(x=state,y=speed))+geom_jitter(alpha=I(1/8),size=0.7)+ylab('速度（英里/小时）')+xlab('状态')+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+
  facet_wrap(~station,nrow=2)
ggplot(data=five_data,aes(x=state,y=volume))+geom_jitter(alpha=I(1/8),size=0.7)+ylab('流量（辆/小时/车道）')+xlab('状态')+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+
  facet_wrap(~station,nrow=2)
ggplot(data=five_data,aes(x=state,y=occ))+geom_jitter(alpha=I(1/8),size=0.7)+ylab('占有率（%）')+xlab('状态')+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+
  facet_wrap(~station,nrow=2)


rm(list=ls()[!(ls()=='five_data')])
rf<-randomForest(x=five_data[,1:3],y=factor(five_data$state),importance = T)
varImpPlot(rf)
