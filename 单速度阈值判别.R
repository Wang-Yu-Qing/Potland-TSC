load('d:/data/station1049/day/10.14.RData')
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
qplot(occ,volume,data = five_data,geom=c('path','point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))
qplot(volume,speed,data = five_data,geom=c('path','point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)
qplot(t,state,geom='crossbar',ymin=1,ymax=4,ylab = 'state',xlab = 'time spot',color=state,fill=state)+scale_fill_manual(values=c("blue","lightblue","orange","red"))+scale_colour_manual(values=c("blue","lightblue","orange","red"))  #state ploting  

agg_m<-aggregate(five_data,by=list(cluster=state),FUN=mean)
agg_v<-aggregate(five_data,by=list(cluster=state),FUN=sd)