load('d:/data/station1050/day/10.13.RData')
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
if (abs(p[occ_state[i-1]]-max(p))<0) (occ_state[i]<-occ_state[i-1])       # Here is max()!
else (occ_state[i]<-which.max(p))
}
row.names(p_matrix)<-NULL


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
five_data_0<-data.frame(volume,speed,occ,state)


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
if (abs(p[occ_state[i-1]]-max(p))<0.15) (occ_state[i]<-occ_state[i-1])       # Here is max()!
else (occ_state[i]<-which.max(p))
}
row.names(p_matrix)<-NULL


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
five_data_0.15<-data.frame(volume,speed,occ,state)


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
five_data_0.3<-data.frame(volume,speed,occ,state)


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
if (abs(p[occ_state[i-1]]-max(p))<0.6) (occ_state[i]<-occ_state[i-1])       # Here is max()!
else (occ_state[i]<-which.max(p))
}
row.names(p_matrix)<-NULL


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
five_data_0.6<-data.frame(volume,speed,occ,state)

p<-rep(c('P=0','P=0.15','P=0.3','P=0.6'),each=288)
five_data<-data.frame(rbind(five_data_0,five_data_0.15,five_data_0.3,five_data_0.6),p)
t = seq(as.POSIXct("2016-01-01 00:05:00"), as.POSIXct("2016-01-02 00:00:00"), by = '5 min')
Time<-c(t,t,t,t)
qplot(Time,state,data=five_data,geom='crossbar',fill=factor(state),colour=factor(state),fatten=0,ymin=0,ymax=5)+facet_grid(p~.)+scale_fill_manual(values=c("blue","lightblue","orange", "red"))+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('2 hours'))+guides(fill=guide_legend(title="State"),col=guide_legend(title="State"))
ggplot(data=five_data,aes(occ,volume))+geom_path(size=0.5,group=1,aes(color=state))+geom_point(size=0.9,aes(color=state))+xlab('Occupancy(%)')+ylab('Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~p,scales = 'free')
ggplot(data=five_data,aes(volume,speed))+geom_path(size=0.5,group=1,aes(color=state))+geom_point(size=0.9,aes(color=state))+xlab('Occupancy(%)')+ylab('Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~p,scales = 'free')