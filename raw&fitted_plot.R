f<-list.files('d:/data/station1049/day/tar/')
d<-paste('d:/data/station1049/day/tar/',f,sep = '') # file direction
all_data<-NULL
for (i in 1:length(d)){
  load(d[i])
  all_data<-rbind(all_data,day_data)
}
t = seq(as.POSIXct("2011-09-16 00:00:00"), as.POSIXct("2011-09-23 23:59:40"), by = '20 sec') # 8 days
## raw plot:
y<-c(all_data$volume,all_data$speed,all_data$occ)
t<-rep(t,times=3)
parameter<-rep(c('a','b','c'),each=nrow(all_data))
raw_plot<-data.frame(t,y,parameter)
library('ggplot2')
library('scales')
ggplot(data=raw_plot,aes(x=t,y=y))+geom_line()+facet_grid(parameter~.)+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('12 hours'))+
  ylab('')+xlab('时间')+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),
        strip.text.y = element_text(angle = 0))

## fitted plot:
t = seq(as.POSIXct("2011-09-16 00:00:00"), as.POSIXct("2011-09-23 23:59:40"), by = '20 sec')
fm<-loess(all_data$volume~as.numeric(t),span = 0.005)
sm<-loess(all_data$speed~as.numeric(t),span = 0.005)
om<-loess(all_data$occ~as.numeric(t),span = 0.005)               ## convert t to numeric!!
t<-rep(t,times=3)
y<-c(fm$fitted,sm$fitted,om$fitted)
parameter<-rep(c('a','b','c'),each=nrow(all_data))
fitted_plot<-data.frame(t,y,parameter)
library('ggplot2')
library('scales')
ggplot(data=fitted_plot,aes(x=t,y=y))+geom_line()+facet_grid(parameter~.)+
  scale_x_datetime(labels = date_format('%H:%M', tz = "Asia/Taipei"), breaks = date_breaks('12 hours'))+
  ylab('')+xlab('时间')+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),
        strip.text.y = element_text(angle = 0))
