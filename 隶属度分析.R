load('c:/data/station1049/loadings.RData')
library('dtwclust')
load('c:/data/station1049/day/09.16.RData')
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
md1<-p[1,1]
md2<-p[1,2]
md3<-p[1,3]
md4<-p[1,4]
occ_state[1]<-which.max(p)                                    # which.max() NOT max() !!
for (i in 2:288) {p<-predict(clusters_occ,occ_window[[i]])
if (abs(p[occ_state[i-1]]-max(p))<0.3) (occ_state[i]<-occ_state[i-1]       # Here is max()!
) else (occ_state[i]<-which.max(p))
md1<-c(md1,p[1,1])
md2<-c(md2,p[1,2])
md3<-c(md3,p[1,3])
md4<-c(md4,p[1,4])
}

t = seq(as.POSIXct("2016-01-01 00:05:00"), as.POSIXct("2016-01-02 00:00:00"), by = '5 min')
state_data<-data.frame(t=t,state=occ_state)
tt<-rep(t,times=4)
bar<-data.frame(tt,md=c(md1,md2,md3,md4),state=rep(c(1,2,3,4),each=288))
library('scales')

ggplot(data=bar,aes(x=tt,y=md))+geom_bar(stat="identity",aes(fill=factor(state)),width = 300)+
  scale_fill_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  ylab('隶属度')+xlab('时间')+scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('3 hours'))

qplot(t,state,data=state_data,geom='crossbar',fill=factor(state),colour=factor(state),fatten=0,ymin=0,ymax=7)+
  scale_fill_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  scale_colour_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('3 hours'))+
  guides(fill=guide_legend(title=" "),col=guide_legend(title=" "))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=0.1),
        axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 0.1))+xlab('时间')







ggplot(data=bar,aes(x=tt,y=md))+geom_bar(stat="identity",aes(fill=factor(state)),width =700)+
  scale_fill_manual(values=c('blue','lightblue','orange', 'red'),labels = c('状态一','状态二','状态三','状态四'))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
  legend.text = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
  axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  ylab('隶属度')+xlab('时间')+scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('3 hours'))+coord_polar()