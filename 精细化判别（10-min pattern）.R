library('dtwclust')
clusters_occ<-as(clusters_occ,"TSClusters")
multi_cluster<-as(multi_cluster,"TSClusters")
load('d:/data/station1049/day/09.19.RData')

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
pre_state<-vector()

multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centroids,method='dtw'))
pre_state[1]<-multi_state[1]
for (i in 2:288) {d_matrix<-proxy::dist(multi_series[i],multi_cluster@centroids,method='dtw')


if (abs(d_matrix[pre_state[i-1]]-min(d_matrix))<50) (pre_state[i]<-pre_state[i-1]
) else (pre_state[i]<-which.min(d_matrix))

dis<-proxy::dist(list(rbind(multi_series[[i]][,c(2,3)],multi_series[[i-1]][,c(2,3)])),dtwc@centroids,method='dtw') # no flow and convert to list!
# second identification:
if ((abs(dis[1,2]-dis[1,1])/sum(dis)>0.2)) (multi_state[i]<-which.min(dis)+4
) else (multi_state[i]<-pre_state[i])
}
state<-multi_state
state[state==1]<-'state1'
state[state==2]<-'state2'
state[state==3]<-'state3'
state[state==4]<-'state4'
state[state==5]<-'state with explosion'
state[state==6]<-'state with slump'

t = seq(as.POSIXct("2016-01-01 00:05:00"), as.POSIXct("2016-01-02 00:00:00"), by = '5 min')
state_data<-data.frame(t,state)
library('scales')
qplot(t,state,data=state_data,geom='crossbar',fill=factor(state),colour=factor(state),fatten=0,ymin=0,ymax=7)+
  scale_fill_manual(values=c('yellow','black','blue','lightblue','orange', 'red'))+
  scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('2 hours'))+
  guides(fill=guide_legend(title="State"),col=guide_legend(title="State"))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'))
## state and series comparison:
t<-seq(as.POSIXct("2016-01-01 00:00:00"), as.POSIXct("2016-01-01 23:59:40"), by = '20 sec')
star.t<-as.POSIXct('2016-01-01 14:00:00')
end.t<-as.POSIXct('2016-01-01 20:00:00')
star<-which(t==star.t)
end<-which(t==end.t)
s_series<-data.frame(x=t[star:end],y=day_data$speed[star:end],state=factor(rep(state,each=15)[star:end]))
o_series<-data.frame(x=t[star:end],y=day_data$occ[star:end],state=factor(rep(state,each=15)[star:end]))
ggplot(data=s_series,aes(x=x,y=y,col=state,group=1))+geom_point(size=1.2)+
  geom_line(size=0.5)+scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'))+
  theme(panel.background = element_rect(fill='white', colour='white'))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('1 hours'))+
  xlab('Time')+ylab('Speed(mph)')
