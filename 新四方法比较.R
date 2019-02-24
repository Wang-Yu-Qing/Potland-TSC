clusters_occ<-as(clusters_occ,"TSClusters")
multi_cluster<-as(multi_cluster,"TSClusters")
load('d:/data/station1049/day/10.14.RData')

## LOS:
target_data<-day_data
volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ)
Density<-volume/speed
LOS_state<-vector('character',288)
for (i in 1:288) {if(Density[i]<18) (LOS_state[i]<-'state1')
  else(if(Density[i]<35) (LOS_state[i]<-'state2')
       else(if(Density[i]<45) (LOS_state[i]<-'state3')
            else (LOS_state[i]<-'state4')))}


## 5-MIN FUZZY K-MEANS:
target_data<-day_data
# 5-min aggregation
flow_5<-NULL
speed_5<-NULL
occ_5<-NULL
for(i in 1:288){
  flow_5<-c(flow_5,sum(target_data[(15*i-14):(15*i),3])) # veh/3ln/5min
  speed_5<-c(speed_5,mean(target_data[(15*i-14):(15*i),4]))
  occ_5<-c(occ_5,mean(target_data[(15*i-14):(15*i),5]))
}
target_data_5<-data.frame(flow_5,speed_5,occ_5)
flow<-(target_data_5[,1]-his_mean_f)/his_sd_f
speed<-(target_data_5[,2]-his_mean_s)/his_sd_s
occ<-(target_data_5[,3]-his_mean_o)/his_sd_o
target_sdata_5<-data.frame(flow,speed,occ)
# state identification:
cc<-fcm_5$centers
x<-target_sdata_5
dm <- sapply(seq_len(nrow(x)),
             function(i) apply(cc, 1, function(v) sqrt(sum((x[i, ]-v)^2))))

m <- 2
## compute cluster membership values
ms <- t(apply(dm, 2,
              function(x) {
                tmp <- 1/((x/sum(x))^(2/(m-1)))  # formula above
                tmp/sum(tmp)  # normalization
              }))

# convert to state:
state_5<-vector()
state_5[1]<-which.max(ms[1,])
for(i in 2:nrow(ms)){
  p.max<-max(ms[i,])
  if (p.max-ms[i,state_5[i-1]]<0.3) (state_5[i]<-state_5[i-1]
  ) else (state_5[i]<-which.max(ms[i,]))
  
}



## MULTI TSC:
target_data<-day_data
#scale:
flow<-(target_data[,3]-fm20)
speed<-(target_data[,4]-sm20)
occ<-(target_data[,5]-om20)

s_target_data<-data.frame(flow,speed,occ)
multi_series<-list()
for (i in 1:288) {multi_series[[i]]<-cbind(s_target_data[(i*15-14):(i*15),1],s_target_data[(i*15-14):(i*15),2],s_target_data[(i*15-14):(i*15),3])}
multi_state<-vector('numeric',288)
multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centroids,method='dtw'))
for (i in 2:288) {d_matrix<-proxy::dist(multi_series[i],multi_cluster@centroids,method='dtw')
if (abs(d_matrix[multi_state[i-1]]-min(d_matrix))<50) (multi_state[i]<-multi_state[i-1])
else (multi_state[i]<-which.min(d_matrix))}


## OCC TSC:
target_data<-day_data
occ_series<-target_data$occ                                  #occ series 
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
p<-c(p[1],p[2],p[3],p[4])                                    
p_matrix<-p
occ_state[1]<-which.max(p)                                    # which.max() NOT max() !!
for (i in 2:288) {p<-predict(clusters_occ,occ_window[[i]])
p<-c(p[1],p[2],p[3],p[4])   
p_matrix<-rbind(p_matrix,p)
if (abs(p[occ_state[i-1]]-max(p))<0.3) (occ_state[i]<-occ_state[i-1])       # Here is max()!
else (occ_state[i]<-which.max(p))
}


# STATE PLOT:
t = seq(as.POSIXct("2016-01-01 00:05:00"), as.POSIXct("2016-01-02 00:00:00"), by = '5 min')
Time<-c(t,t,t,t)
method<-rep(c('normal FCM','occupancy TSC','multivariate TSC','LOS'),each=288)
State<-c(state_5,occ_state,multi_state,LOS_state)
State[State==1]<-'state1'
State[State==2]<-'state2'
State[State==3]<-'state3'
State[State==4]<-'state4'
state_matrix<-data.frame(Time,State,method)
library('scales')
qplot(Time,State,data=state_matrix,geom='crossbar',fill=factor(State),colour=factor(State),fatten=0,ymin=0,ymax=5)+facet_grid(method~.)+scale_fill_manual(values=c("blue","lightblue","orange", "red"))+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('2 hours'))+guides(fill=guide_legend(title="State"),col=guide_legend(title="State"))


# PARAMETER PLOT:
volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*4}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ)
five_data<-rbind(five_data,five_data,five_data,five_data)
state<-c(occ_state,multi_state,LOS_state,state_5)
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
method<-c(rep('occupancy TSC',each=288),rep('multivariable TSC',each=288),rep('LOS',each=288),rep('normal FCM',each=288))
five_data<-data.frame(five_data,state,method)
five_data$method<-as.character(five_data$method)
five_data$method[five_data$method=='LOS']<-'(a)'
five_data$method[five_data$method=='multivariable TSC']<-'(b)'
five_data$method[five_data$method=='occupancy TSC']<-'(c)'
five_data$method[five_data$method=='normal FCM']<-'(d)'
ggplot(data=five_data,aes(occ,volume))+geom_path(size=0.5,group=1,aes(color=state))+geom_point(size=0.9,aes(color=state))+xlab('Occupancy(%)')+ylab('Volume(veh/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~method,scales = 'free')+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))
ggplot(data=five_data,aes(volume,speed))+geom_path(size=0.5,group=1,aes(color=state))+geom_point(size=0.9,aes(color=state))+xlab('Volume(veh/h/ln)')+ylab('Speed(mph)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~method,scales = 'free')+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))