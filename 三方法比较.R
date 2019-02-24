load('c:/R_projects/potland/station1049/loadings.RData')
load('c:/R_projects/potland/station1049/day/10.26.RData')
t = seq(as.POSIXct("2016-01-01 00:05:00"), as.POSIXct("2016-01-02 00:00:00"), by = '5 min')
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
LOS_state<-vector()
for (i in 1:288) {if(Density[i]<18) (LOS_state[i]<-'1')
  else(if(Density[i]<35) (LOS_state[i]<-'2')
       else(if(Density[i]<45) (LOS_state[i]<-'3')
            else (LOS_state[i]<-'4')))}
LOS<-data.frame(t,five_data,state=LOS_state)
library('scales')
library('ggplot2')
#状态-时间(5分钟)：
qplot(t,state,data=LOS,geom='crossbar',fill=factor(state),colour=factor(state),fatten=0,ymin=0,ymax=7)+
  scale_fill_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('3 hours'))+
  guides(fill=guide_legend(title=" "),col=guide_legend(title=" "))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=0.1),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 0.1))+xlab('时间')
#速度-流量：
ggplot(data=LOS,aes(x=volume,y=speed,col=factor(state)))+geom_point(size=1.5,alpha=I(0.5))+
  geom_path(group=1,arrow = arrow(length = unit(0.07,'inches'),type='closed'))+scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  xlab('流量(辆/小时/车道)')+ylab('速度(英里/小时)')
#流量-占有率：
ggplot(data=LOS,aes(x=occ,y=volume,col=factor(state)))+geom_point(size=1.5,alpha=I(0.5))+
  geom_path(group=1,arrow = arrow(length = unit(0.07,'inches'),type='closed'))+scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  xlab('占有率(%)')+ylab('流量(辆/小时/车道)')

##speed:
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
  if (mean_speed_series[i]<25) (speed_only_state[i]<-4)
  else (if (mean_speed_series[i]<40) (speed_only_state[i]<-3)
        else (if (mean_speed_series[i]<65) (speed_only_state[i]<-2)
              else (speed_only_state[i]<-1)))
} #state judging
SPEED<-data.frame(t,five_data,state=speed_only_state)
#状态-时间(5分钟)：
qplot(t,state,data=SPEED,geom='crossbar',fill=factor(state),colour=factor(state),fatten=0,ymin=0,ymax=7)+
  scale_fill_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('3 hours'))+
  guides(fill=guide_legend(title=" "),col=guide_legend(title=" "))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=0.1),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 0.1))+xlab('时间')
#速度-流量：
ggplot(data=SPEED,aes(x=volume,y=speed,col=factor(state)))+geom_point(size=1.5,alpha=I(0.5))+
  geom_path(group=1,arrow = arrow(length = unit(0.07,'inches'),type='closed'))+scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  xlab('流量(辆/小时/车道)')+ylab('速度(英里/小时)')
#流量-占有率：
ggplot(data=SPEED,aes(x=occ,y=volume,col=factor(state)))+geom_point(size=1.5,alpha=I(0.5))+
  geom_path(group=1,arrow = arrow(length = unit(0.07,'inches'),type='closed'))+scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  xlab('占有率(%)')+ylab('流量(辆/小时/车道)')



## MULTI TSC:
library('dtwclust')
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




