a=list.files('d:/data/station1049/day/')
dir=paste('d:/data/station1049/day/',a,sep="")
train_list<-list()
load(dir[1])
all_data<-day_data
occ_series<-all_data$occ                                  #occ series 
occ_window<-list()
for (i in 1:288) {occ_window[[i]]<-vector('numeric',15)}
j<-1
i<-1
while (i<4307){
  occ_window[[j]]<-occ_series[i:(i+14)]
  i<-i+15
  j<-j+1
}
occ_state_merge<-vector('numeric',288)
p<-predict(clusters_occ,occ_window[[1]])
p<-c(p[1],p[3],p[2])  
p_matrix<-p
occ_state_merge[1]<-which.max(p)                                    # which.max() NOT max() !!
for (i in 2:288) {p<-predict(clusters_occ,occ_window[[i]])
p<-c(p[1],p[3],p[2])  
p_matrix<-rbind(p_matrix,p)
if (abs(p[occ_state_merge[i-1]]-max(p))<0.2) (occ_state_merge[i]<-occ_state_merge[i-1])       # Here is max()!
else (occ_state_merge[i]<-which.max(p))
}
row.names(p_matrix)<-NULL

p_value<-rep(0.3,times=length(dir))
for (k in 2:(length(dir)-1)) {
load(dir[k])
all_data<-rbind(all_data,day_data)
occ_series<-day_data$occ                                  #occ series 
occ_window<-list()
for (i in 1:288) {occ_window[[i]]<-vector('numeric',15)}
j<-1
i<-1
while (i<4307){
  occ_window[[j]]<-occ_series[i:(i+14)]
  i<-i+15
  j<-j+1
}
occ_state<-vector('numeric',288)
p<-predict(clusters_occ,occ_window[[1]])
p<-c(p[1],p[3],p[2]) 
p_matrix<-p
occ_state[1]<-which.max(p)                                    # which.max() NOT max() !!
for (i in 2:288) {p<-predict(clusters_occ,occ_window[[i]])
p<-c(p[1],p[3],p[2]) 
p_matrix<-rbind(p_matrix,p)
if (abs(p[occ_state[i-1]]-max(p))<p_value[k]) (occ_state[i]<-occ_state[i-1])       # Here is max()!
else (occ_state[i]<-which.max(p))
}
row.names(p_matrix)<-NULL
occ_state_merge<-c(occ_state_merge,occ_state)}                                     #all data

m<-(length(dir)-1)*288
volume<-vector('numeric',m)
speed<-vector('numeric',m)
occ<-vector('numeric',m)
for (i in 1:m) {volume[i]<-sum(all_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:m) {speed[i]<-mean(all_data$speed[(i*15-14):(i*15)])}
for (i in 1:m) {occ[i]<-mean(all_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ,occ_state_merge)
colnames(five_data)[4]<-'occ_state' 
state<-occ_state_merge
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
qplot(occ,volume,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue", "lightblue", "orange"))
qplot(volume,speed,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue", "lightblue", "orange"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)
agg_m<-aggregate(five_data,by=list(cluster=state),FUN=mean)
agg_v<-aggregate(five_data,by=list(cluster=state),FUN=sd)                          # 3 states plot

all_data<-data.frame(all_data,rep(occ_state_merge,each=15))
colnames(all_data)[6]<-'state'
data_s3<-all_data[all_data$state==3,]
n<-nrow(data_s3)/15
for (i in 1:n) {train_list[[i]]<-data_s3$occ[(i*15-14):(i*15)]}
clusters_s3_occ2<-dtwclust(train_list,'fuzzy',k=2,distance = 'dtw',seed=100)
data_s3<-data.frame(data_s3[,-6],rep(clusters_s3_occ2@cluster,each=15))
colnames(data_s3)[6]<-'state'
data_s3$state[data_s3$state==1]<-3
data_s3$state[data_s3$state==2]<-4
all_data[all_data$state==3,6]<-data_s3$state                                         # add state 




state<-vector('numeric',m)
for (i in 1:m) {state[i]<-mean(all_data$state[(i*15-14):(i*15)])}                  # 4320 to 288 !!
volume<-vector('numeric',m)
speed<-vector('numeric',m)
occ<-vector('numeric',m)
for (i in 1:m) {volume[i]<-sum(all_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:m) {speed[i]<-mean(all_data$speed[(i*15-14):(i*15)])}
for (i in 1:m) {occ[i]<-mean(all_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ,state)
colnames(five_data)[4]<-'occ_state' 
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
qplot(occ,volume,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))
qplot(volume,speed,data = five_data,geom=c('point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)
agg_m<-aggregate(five_data,by=list(cluster=state),FUN=mean)
agg_v<-aggregate(five_data,by=list(cluster=state),FUN=sd)
clusters_occ@centers[[3]]<-clusters_s3_occ2@centers[[1]]
clusters_occ@centers[[4]]<-clusters_s3_occ2@centers[[2]]
rm(all_data,all_s_data,a,date,detectorid,new_data,day_data,speed,volume,occ,occupancy,time,n,index,train_data,i,train_list,agg_m,agg_v,data_s3,five_data,p_matrix,s_data,target_data,d,d_value,dir,j,k,m,multi_series,multi_state,multi_state_merge,occ_series,occ_state,occ_state_merge,occ_window,p,p_value,state,t)