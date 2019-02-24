load('d:/data/station1049/day/10.14.RData')
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
for (i in 1:288) {if(Density[i]<11) (LOS_state[i]<-'state1')
            else(if(Density[i]<35) (LOS_state[i]<-'state2')
                 else(if(Density[i]<45) (LOS_state[i]<-'state3')
                      else (LOS_state[i]<-'state4')))}
t<-c(1:288)
qplot(t,LOS_state,geom='crossbar',ylab = 'state',xlab = 'time spot',color=LOS_state,fill=LOS_state,ymin=1,ymax=6)+scale_fill_manual(values=c("blue","lightblue","orange","red"))+scale_colour_manual(values=c("blue","lightblue","orange","red"))  #state ploting
qplot(occ,volume,data = five_data,geom=c('path','point'),group=1,color=factor(LOS_state),xlab = 'Occupancy',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue","lightblue","orange","red"))
qplot(volume,speed,data = five_data,geom=c('path','point'),group=1,color=factor(LOS_state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue","lightblue","orange","red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)
