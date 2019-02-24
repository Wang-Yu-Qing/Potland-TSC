a=list.files('d:/data/station1140/day/tar/')
dir=paste('d:/data/station1140/day/tar/',a,sep="")
load(dir[1])
target_data<-day_data
s_target_data<-data.frame(scale(target_data[,3:5],scale = F))
multi_series<-list()
for (i in 1:288) {multi_series[[i]]<-cbind(s_target_data[(i*15-14):(i*15),1],s_target_data[(i*15-14):(i*15),2],s_target_data[(i*15-14):(i*15),3])}
multi_state<-vector('numeric',288)
multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centers,method='dtw'))
for (i in 2:288) {d_matrix<-proxy::dist(multi_series[i],multi_cluster@centers,method='dtw')
if (abs(d_matrix[multi_state[i-1]]-min(d_matrix))<50) (multi_state[i]<-multi_state[i-1])
else (multi_state[i]<-which.min(d_matrix))}
multi_state[multi_state==1]<-5
multi_state[multi_state==2]<-1
multi_state[multi_state==3]<-3
multi_state[multi_state==5]<-2
t<-c(1:288)
volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ)
state<-c(multi_state)
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
five_data_all<-data.frame(volume,speed,occ,state)
for (k in 2:length(dir)) {
  load(dir[k])
  target_data<-day_data
  s_target_data<-data.frame(scale(target_data[,3:5],scale = F))
  multi_series<-list()
  for (i in 1:288) {multi_series[[i]]<-cbind(s_target_data[(i*15-14):(i*15),1],s_target_data[(i*15-14):(i*15),2],s_target_data[(i*15-14):(i*15),3])}
  multi_state<-vector('numeric',288)
  multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centers,method='dtw'))
  for (i in 2:288) {d_matrix<-proxy::dist(multi_series[i],multi_cluster@centers,method='dtw')
  if (abs(d_matrix[multi_state[i-1]]-min(d_matrix))<50) (multi_state[i]<-multi_state[i-1])
  else (multi_state[i]<-which.min(d_matrix))}
  multi_state[multi_state==1]<-5
  multi_state[multi_state==2]<-1
  multi_state[multi_state==3]<-3
  multi_state[multi_state==5]<-2
  t<-c(1:288)
  volume<-vector('numeric',288)
  speed<-vector('numeric',288)
  occ<-vector('numeric',288)
  for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
  for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
  for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
  five_data<-data.frame(volume,speed,occ)
  state<-c(multi_state)
  state[state==1]<-'state1'
  state[state==2]<-'state2'
  state[state==3]<-'state3'
  state[state==4]<-'state4'
  five_data_all<-rbind(five_data_all,data.frame(volume,speed,occ,state))
}
agg_m<-aggregate(five_data_all,by=list(cluster=five_data_all$state),FUN=mean)             #state is factor or character vectors, aggregate() will coerce it into numeric vector which results in NA vectors
agg_v<-aggregate(five_data_all,by=list(cluster=five_data_all$state),FUN=sd)
qplot(volume,speed,data = five_data_all,geom=c('path','point'),group=1,color=factor(five_data_all$state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue", "lightblue", "orange","red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)