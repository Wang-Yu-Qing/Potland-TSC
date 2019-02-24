
volume<-vector('numeric',288)
speed<-vector('numeric',288)
occ<-vector('numeric',288)
for (i in 1:288) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*4}
for (i in 1:288) {speed[i]<-mean(target_data$speed[(i*15-14):(i*15)])}
for (i in 1:288) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
five_data<-data.frame(volume,speed,occ)
five_data<-rbind(five_data,five_data,five_data,five_data)
state<-c(occ_state,multi_state,LOS_state,speed_only_state)
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
method<-c(rep('occupancy TSC',each=288),rep('multivariable TSC',each=288),rep('LOS',each=288),rep('speed threshold',each=288))
five_data<-data.frame(five_data,state,method)
qplot(occ,volume,data = five_data,geom=c('path','point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~method,scales = 'free')
qplot(volume,speed,data = five_data,geom=c('path','point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mph)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+facet_wrap(~method,scales = 'free')
