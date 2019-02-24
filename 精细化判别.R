load('d:/data/station1049/loadings.RData')
library('dtwclust')
load('d:/data/station1049/day/10.18.RData')
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

dis<-proxy::dist(list(multi_series[[i]][,c(2,3)]),dtwc@centroids,method='dtw') # no flow and convert to list!
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
library('ggplot2')
#状态-时间(5分钟)：
qplot(t,state,data=state_data,geom='crossbar',fill=factor(state),colour=factor(state),fatten=0,ymin=0,ymax=7)+
  scale_fill_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('3 hours'))+
  guides(fill=guide_legend(title=" "),col=guide_legend(title=" "))+
  scale_y_discrete(breaks=NULL,labels=NULL,name=NULL)+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=0.1),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 0.1))+xlab('时间')


#参数关系图(5分钟)：
#20s to 5min:
s5<-vector()
f5<-vector()
o5<-vector()
for(i in 1:288){
  f5[i]<-sum(day_data$volume[(i*15-14):(i*15)])*12/3
  o5[i]<-mean(day_data$occ[(i*15-14):(i*15)])
  s5[i]<-sum(day_data$speed[(i*15-14):(i*15)]*day_data$volume[(i*15-14):(i*15)])/sum(day_data$volume[(i*15-14):(i*15)]) #不要除以f[i]，单位不一！
}
five_data<-data.frame(f5,s5,o5,state)
#速度-流量：
ggplot(data=five_data,aes(x=f5,y=s5,col=state))+geom_point(size=1.5,alpha=I(0.5))+
  geom_path(group=1,arrow = arrow(length = unit(0.07,'inches'),type='closed'))+scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  xlab('流量(辆/小时/车道)')+ylab('速度(英里/小时)')
#流量-占有率：
ggplot(data=five_data,aes(x=o5,y=f5,col=state))+geom_point(size=1.5,alpha=I(0.5))+
  geom_path(group=1,arrow = arrow(length = unit(0.07,'inches'),type='closed'))+scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  xlab('占有率(%)')+ylab('流量(辆/小时/车道)')



## state and series comparison:
t<-seq(as.POSIXct("2016-01-01 00:00:00"), as.POSIXct("2016-01-01 23:59:40"), by = '20 sec')
star.t<-as.POSIXct('2016-01-01 13:30:00')
end.t<-as.POSIXct('2016-01-01 16:00:00')
star<-which(t==star.t)
end<-which(t==end.t)
s_series<-data.frame(x=t[star:end],y=day_data$speed[star:end],state=factor(rep(state,each=15)[star:end]))
o_series<-data.frame(x=t[star:end],y=day_data$occ[star:end],state=factor(rep(state,each=15)[star:end]))
f_series<-data.frame(x=t[star:end],y=day_data$volume[star:end],state=factor(rep(state,each=15)[star:end]))
#速度：
ggplot(data=s_series,aes(x=x,y=y,col=state,group=1))+geom_point(size=1.2)+
  geom_line(size=0.5)+scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('1 hours'))+
  xlab('时间')+ylab('速度(英里/小时)')
#占有率：
ggplot(data=o_series,aes(x=x,y=y,col=state,group=1))+geom_point(size=1.2)+
  geom_line(size=0.5)+scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('1 hours'))+
  xlab('时间')+ylab('占有率(%)')
#流量：
ggplot(data=f_series,aes(x=x,y=y*180/3,col=state,group=1))+geom_point(size=1.2)+
  geom_line(size=0.5)+scale_colour_manual(values=c('yellow','black','blue','lightblue','orange', 'red'),labels = c('模式一','模式二','状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('1 hours'))+
  xlab('时间')+ylab('流量(辆/小时/车道)')






## explosion and slump series DTW visualization:
state_20s<-rep(multi_state,each=15)
explo<-day_data[state_20s==5,c(4,5)]
slump<-day_data[state_20s==6,c(4,5)]
nes<-nrow(explo)/15 
nss<-nrow(slump)/15   #number of series

library('dtw')
## explo DTW:
for(i in 1:nes){
  speed_series<-explo[(i*15-14):(i*15),1]
  occ_series<-explo[(i*15-14):(i*15),2]
  speed_center<-dtwc@centroids[[1]][,1]+sm20
  occ_center<-dtwc@centroids[[1]][,2]+om20
  plot(dtw(speed_series,speed_center,k=TRUE),type="two",offset=0,match.indices=100)
  plot(dtw(speed_series,speed_center,k=TRUE),type="threeway",match.indices=100)
  plot(dtw(occ_series,occ_center,k=TRUE),type="two",offset=0,match.indices=100)
  plot(dtw(occ_series,occ_center,k=TRUE),type="threeway",match.indices=100)
}

for(i in 1:nss){
  speed_series<-slump[(i*15-14):(i*15),1]
  occ_series<-slump[(i*15-14):(i*15),2]
  speed_center<-dtwc@centroids[[2]][,1]+sm20
  occ_center<-dtwc@centroids[[2]][,2]+om20
  plot(dtw(speed_series,speed_center,k=TRUE),type="two",off=1,match.lty=2,match.indices=100)
  plot(dtw(speed_series,speed_center,k=TRUE),type="threeway",off=1,match.lty=2,match.indices=100)
  plot(dtw(occ_series,occ_center,k=TRUE),type="two",off=1,match.lty=2,match.indices=100)
  plot(dtw(occ_series,occ_center,k=TRUE),type="threeway",off=1,match.lty=2,match.indices=100)
  
}



