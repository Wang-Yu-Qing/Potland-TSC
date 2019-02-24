load('d:/data/station1140/day/11.12.RData')
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
qplot(t,occ_state,geom='crossbar',ylab = 'state',xlab = 'time spot',color=occ_state,fill=occ_state,ymin=1,ymax=3)  #state ploting



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
qplot(occ,volume,data = five_data,geom=c('path','point'),group=1,color=factor(state),xlab = 'Occupancy(%)',ylab = 'Volume(pc/h/ln)')+scale_colour_manual(values=c("blue", "lightblue", "orange","red"))
qplot(volume,speed,data = five_data,geom=c('path','point'),group=1,color=factor(state),xlab = 'Volume(pc/h/ln)',ylab = 'Speed(mi/h)')+scale_colour_manual(values=c("blue", "lightblue", "orange","red"))+geom_abline(intercept=0,slope=1/11,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/18,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/26,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/35,linetype = 2,size=1)+geom_abline(intercept=0,slope=1/45,linetype = 2,size=1)

agg_m<-aggregate(five_data,by=list(cluster=state),FUN=mean)
agg_v<-aggregate(five_data,by=list(cluster=state),FUN=sd)