a=list.files('d:/data/station1141/day/')
dir=paste('d:/data/station1141/day/',a,sep="")
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
p<-c(p[1],p[2],p[3])  
p_matrix<-p
occ_state_merge[1]<-which.max(p)                                    # which.max() NOT max() !!
for (i in 2:288) {p<-predict(clusters_occ,occ_window[[i]])
p<-c(p[1],p[2],p[3])  
p_matrix<-rbind(p_matrix,p)
if (abs(p[occ_state_merge[i-1]]-max(p))<0.3) (occ_state_merge[i]<-occ_state_merge[i-1])       # Here is max()!
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
  p<-c(p[1],p[2],p[3]) 
  p_matrix<-p
  occ_state[1]<-which.max(p)                                    # which.max() NOT max() !!
  for (i in 2:288) {p<-predict(clusters_occ,occ_window[[i]])
  p<-c(p[1],p[2],p[3]) 
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
colnames(five_data)[4]<-'state' 
state<-occ_state_merge
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
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
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
five_data<-data.frame(volume,speed,occ,state)
colnames(five_data)[4]<-'state'
occ_data<-five_data
rm(five_data)


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
if (abs(d[multi_state[i-1]]-min(d))<50) (multi_state[i]<-multi_state[i-1])
else (multi_state[i]<-which.min(d))}
multi_state[multi_state==1]<-1
multi_state[multi_state==2]<-2
multi_state[multi_state==3]<-3
multi_state[multi_state==3]<-3
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
  multi_state[multi_state==1]<-1
  multi_state[multi_state==2]<-2
  multi_state[multi_state==3]<-3
  multi_state[multi_state==3]<-3
  
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
all_s_data<-scale(all_data[,3:5],scale = F)
all_s_data<-data.frame(all_s_data,rep(multi_state_merge,each=15))
colnames(all_s_data)[4]<-'state'
data_s3<-all_s_data[all_s_data$state==3,]                                           #DO NOT SCALE WITHIN STATE 3 !!
n<-nrow(data_s3)/15
for (i in 1:n) {train_list[[i]]<-cbind(data_s3[(i*15-14):(i*15),1],data_s3[(i*15-14):(i*15),2],data_s3[(i*15-14):(i*15),3])}
clusters_s3_multi<-dtwclust(train_list,'partitional',k=2,distance = 'dtw',centroid = "mean",seed=1)
data_s3<-data.frame(data_s3[,1:3],rep(clusters_s3_multi@cluster,each=15))
colnames(data_s3)[4]<-'state'
data_s3$state[data_s3$state==1]<-3
data_s3$state[data_s3$state==2]<-4
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
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
five_data<-data.frame(volume,speed,occ,state)
colnames(five_data)[4]<-'state'
method<-c(rep('Occupancy TSC',nrow(occ_data)),rep('Multivariable TSC',nrow(five_data)))
p_data<-cbind(rbind(occ_data,five_data),method)    #???????????????p_data$method?????????????????????,??????method???????????????,?????????????????????????????????????????????
grid.arrange(qplot(occ,volume,data = p_data,geom='point',size=I(0.8),color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~method,nrow = 1),
qplot(volume,speed,data = p_data,geom='point',size=I(0.8),color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'speed(mph)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~method,nrow=1)+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1),
ncol=1)






station<-rep('(d)',nrow(p_data))
a_data<-rbind(a_data,cbind(p_data,station))
a_data$method<-as.character(a_data$method)
a_data$method[a_data$method=='Multivariable TSC']<-'(A)'
a_data$method[a_data$method=='Occupancy TSC']<-'(B)'
qplot(occ,volume,data = a_data,geom='point',size=I(0.8),color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_grid(station~method)+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))
qplot(volume,speed,data = a_data,geom='point',size=I(0.8),color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mph)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_grid(station~method)+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))