o_c<-read.csv('d:/data/1.csv')
o_c$state<-factor(o_c$state)
ggplot(data=o_c,aes(t,occ,group=state))+geom_line(size=0.5,aes(colour=state))+geom_point(size=0.9,aes(color=state))+scale_colour_manual(values=c("blue","lightblue","orange", "red"))+xlab('Time indexes')+ylab('Occupancy(%)')+scale_x_continuous(breaks=c(1:15))+scale_y_continuous(breaks=seq(0,45,5))+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+facet_wrap(~station,nrow=2)

m_c<-read.csv('d:/data/2.csv')
m_c$state<-factor(m_c$state)
ggplot(data=m_c,aes(t,o,group=state))+geom_line(size=0.5,aes(colour=state))+geom_point(size=0.9,aes(color=state))+scale_colour_manual(values=c("blue","lightblue","orange", "red"),labels = c('状态一','状态二','状态三','状态四'))+xlab('时间索引')+ylab('占有率（%）')+scale_x_continuous(breaks=c(1:15))+scale_y_continuous(breaks=seq(0,45,5))+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+facet_wrap(~station,nrow=2)+guides(col=guide_legend(title=" "))
ggplot(data=m_c,aes(t,v,group=state))+geom_line(size=0.5,aes(colour=state))+geom_point(size=0.9,aes(color=state))+scale_colour_manual(values=c("blue","lightblue","orange", "red"),labels = c('状态一','状态二','状态三','状态四'))+xlab('时间索引')+ylab('流量（辆/小时/车道）')+scale_x_continuous(breaks=c(1:15))+scale_y_continuous(breaks=seq(400,1600,100))+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+facet_wrap(~station,nrow=2)+guides(col=guide_legend(title=" "))
ggplot(data=m_c,aes(t,s,group=state))+geom_line(size=0.5,aes(colour=state))+geom_point(size=0.9,aes(color=state))+scale_colour_manual(values=c("blue","lightblue","orange", "red"),labels = c('状态一','状态二','状态三','状态四'))+xlab('时间索引')+ylab('速度（英里/小时）')+scale_x_continuous(breaks=c(1:15))+scale_y_continuous(breaks=seq(10,75,5))+theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+facet_wrap(~station,nrow=2)+guides(col=guide_legend(title=" "))

#series&center plot:
load("D:/data/1049_o$m_allplot.RData")
m_c<-read.csv('d:/data/2.csv')
data_1<-all_data[all_data$state==1,]
data_1$x<-1:15
data_1$group<-rep(c(1:(nrow(data_1)/15)),each=15)


ggplot()+geom_line(data=data_1,aes(x=x,y=speed,group=group),alpha=I(1/15),size=1)+
  geom_line(data=m_c[m_c$state==1&m_c$station=='(a)',],aes(x=c(1:15),y=s),size=2,col='blue')+
  geom_point(data=m_c[m_c$state==1&m_c$station=='(a)',],aes(x=c(1:15),y=s),size=4,col='blue')+
  theme(panel.background = element_rect(fill='white', colour='black'),strip.background=element_rect(fill='white', colour='white'))+
  xlab('时间索引')+ylab('速度（英里/小时）')+scale_x_continuous(breaks=c(1:15))



data<-data.frame(y=rnorm(95040))
data$x <- 1:15
data$group <- (1:nrow(data)) %/% 15
p<-ggplot(data=data,aes(x=x,y=y,group=group))+geom_line(alpha=I(1/7),size=1)