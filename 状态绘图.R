load('d:/data/station1141/day/10.26.RData')
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

target_data<-day_data
speed_series<-target_data$speed                                  #speed series 
speed_window<-list()
for (i in 1:288) {speed_window[[i]]<-vector('numeric',15)}        #(5,5) 
i<-1
j<-1
while (i<4307){
  speed_window[[j]]<-speed_series[i:(i+14)]
  i<-i+15
  j<-j+1
}                                                         #(5,5)window

mean_speed_series<-vector('numeric',288)
speed_only_state<-vector('numeric',288)
for (i  in 1:288) {
  mean_speed_series[i]<-mean(speed_window[[i]])  
}                                                          #compute mean speed series

for (i in 1:288) {
  if (mean_speed_series[i]<20) (speed_only_state[i]<-4)
  else (if (mean_speed_series[i]<40) (speed_only_state[i]<-3)
        else (if (mean_speed_series[i]<60) (speed_only_state[i]<-2)
              else (speed_only_state[i]<-1)))
}                                                                   #state judging
t<-c(1:288)
volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ)
state<-c(speed_only_state)
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'

target_data<-day_data
s_target_data<-data.frame(scale(target_data[,3:5],scale = F))
multi_series<-list()
for (i in 1:288) {multi_series[[i]]<-cbind(s_target_data[(i*15-14):(i*15),1],s_target_data[(i*15-14):(i*15),2],s_target_data[(i*15-14):(i*15),3])}
multi_state<-vector('numeric',288)
multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centroids,method='dtw'))
for (i in 2:288) {d_matrix<-proxy::dist(multi_series[i],multi_cluster@centroids,method='dtw')
if (abs(d_matrix[multi_state[i-1]]-min(d_matrix))<50) (multi_state[i]<-multi_state[i-1])
else (multi_state[i]<-which.min(d_matrix))}
multi_state[multi_state==1]<-1
multi_state[multi_state==2]<-2
multi_state[multi_state==3]<-3
multi_state[multi_state==4]<-4
t<-c(1:288)


volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
state<-c(multi_state)
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
five_data<-data.frame(volume,speed,occ,state)

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
row.names(p_matrix)<-NULL
t<-c(1:288)


volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
state<-c(occ_state)
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
five_data<-data.frame(volume,speed,occ,state)

t = seq(as.POSIXct("2016-01-01 00:05:00"), as.POSIXct("2016-01-02 00:00:00"), by = '5 min')
Time<-c(t,t,t,t)
method<-rep(c('speed threshold','occupancy TSC','multivariate TSC','LOS'),each=288)
State<-c(speed_only_state,occ_state,multi_state,LOS_state)
State[State==1]<-'state1'
State[State==2]<-'state2'
State[State==3]<-'state3'
State[State==4]<-'state4'
state_matrix<-data.frame(Time,State,method)

state_matrix$method<-as.character(state_matrix$method)
state_matrix$method[state_matrix$method=='LOS']<-'(a)'
state_matrix$method[state_matrix$method=='multivariate TSC']<-'(b)'
state_matrix$method[state_matrix$method=='occupancy TSC']<-'(c)'
state_matrix$method[state_matrix$method=='speed threshold']<-'(d)'
state_matrix<-cbind(state_matrix,rep('(D)',1152))
colnames(state_matrix)[4]<-'station'
sm<-rbind(sm,state_matrix)
library('scales')
qplot(Time,State,data=sm,geom='crossbar',fill=factor(State),colour=factor(State),fatten=0,ymin=0,ymax=5)+facet_grid(method~station)+scale_fill_manual(values=c("blue","lightblue","orange", "red"))+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('4 hours'))+guides(fill=guide_legend(title="State"),col=guide_legend(title="State"))+theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'))