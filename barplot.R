tym<-read.csv('d:/test.csv')
t<-seq(as.POSIXct("2016-01-01 07:15:00"), as.POSIXct("2016-01-01 09:00:00"), by = '15 min')
tym$t<-t
ggplot(tym,aes(x=t,y=c))+geom_bar(aes(fill=factor(con)),stat="identity")+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('15 min'))+
  guides(fill=guide_legend(title="拥挤度"),col=guide_legend(title="拥挤度"))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'))+
  ylab('饱和度')+xlab('时间')+scale_fill_manual(values=c("blue","lightblue","orange", "red"))