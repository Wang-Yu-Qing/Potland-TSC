load("c:/data/portland/initial_raw_data.RData")
detector<-c(1377,1378,1379,1385,1386,1387,1945,1946,1947,1941,1942,1943)
i<-4
#????????????????????
data1<-raw_data[raw_data$detectorid==detector[i*3-2],]
data2<-raw_data[raw_data$detectorid==detector[i*3-1],]
data3<-raw_data[raw_data$detectorid==detector[i*3],]
# remove '-07'
s.list<-strsplit(as.character(data1$starttime),'-')
time<-sapply(seq(1,nrow(data1)),function(i) paste(s.list[[i]][1],s.list[[i]][2],s.list[[i]][3],sep='-'))

# replace data:
# convert to time format:
data1<-data.frame(time,data1[,c(1,3:5)],stringsAsFactors = F)
data1$time<-as.POSIXct(data1$time,format='%Y-%m-%d %H:%M:%S')

# complete data:
## complete time index:
fulltime<-seq(as.POSIXct('2011-09-15 00:00:00'),as.POSIXct('2011-11-14 23:59:40'),by='20 s')
# left join:
library(dplyr)
data1<-left_join(data.frame(time=fulltime), data1)


# remove '-07'
s.list<-strsplit(as.character(data2$starttime),'-')
time<-sapply(seq(1,nrow(data2)),function(i) paste(s.list[[i]][1],s.list[[i]][2],s.list[[i]][3],sep='-'))

# replace data:
# convert to time format:
data2<-data.frame(time,data2[,c(1,3:5)],stringsAsFactors = F)
data2$time<-as.POSIXct(data2$time,format='%Y-%m-%d %H:%M:%S')

# complete data:
## complete time index:
fulltime<-seq(as.POSIXct('2011-09-15 00:00:00'),as.POSIXct('2011-11-14 23:59:40'),by='20 s')
# left join:
library(dplyr)
data2<-left_join(data.frame(time=fulltime), data2)


# remove '-07'
s.list<-strsplit(as.character(data3$starttime),'-')
time<-sapply(seq(1,nrow(data3)),function(i) paste(s.list[[i]][1],s.list[[i]][2],s.list[[i]][3],sep='-'))

# replace data:
# convert to time format:
data3<-data.frame(time,data3[,c(1,3:5)],stringsAsFactors = F)
data3$time<-as.POSIXct(data3$time,format='%Y-%m-%d %H:%M:%S')

# complete data:
## complete time index:
fulltime<-seq(as.POSIXct('2011-09-15 00:00:00'),as.POSIXct('2011-11-14 23:59:40'),by='20 s')
# left join:
library(dplyr)
data3<-left_join(data.frame(time=fulltime), data3)
station<-rbind(data1,data2,data3)

##????????????:
flag<-vector()
for (i in 1:nrow(station)) {
  if(is.na(station$detectorid[i])) (flag[i]<-1
) else (if(is.na(station$volume[i])) (flag[i]<-2
) else (if(is.na(station$occupancy[i])) (flag[i]<-3
) else (if(!station$volume[i]==0&!station$occupancy[i]==0&is.na(station$speed[i])) (flag[i]<-4
) else (if(station$volume[i]==0&station$occupancy[i]==0&is.na(station$speed[i])) (flag[i]<-5
) else (flag[i]<-6)))))
  print(i)}
flag[flag==1]<-'??????????????'
flag[flag==2]<-'????????????'
flag[flag==3]<-'??????????'
flag[flag==4]<-'????????'
flag[flag==5]<-'??????????????'
flag[flag==6]<-'??完????'
station<-cbind(station,flag)

##??station
{#????????????????????
t1<-as.POSIXct('2011-10-01 00:00:00')
t2<-as.POSIXct('2011-11-01 00:00:00')

station$time<-as.character(station$time)
station$time[as.POSIXct(station$time,format='%Y-%m-%d %H:%M:%S')<t1]<-'????'
station$time[as.POSIXct(station$time,format='%Y-%m-%d %H:%M:%S')>=t1&as.POSIXct(station$time,format='%Y-%m-%d %H:%M:%S')<t2]<-'????'
station$time[as.POSIXct(station$time,format='%Y-%m-%d %H:%M:%S')>=t2]<-'??????'
cirplot<-data.frame(table(station$flag,station$time))

library(circlize)
gap<-c(15,15,15,15,1,1,15)
circos.par(gap.after = gap)
col<-c(????="grey",????="grey",??????="grey",??????????????="red",????????????="blue",??????????????="black",??完????="green")
chordDiagram(cirplot, grid.col = col,transparency = 0.3)
circos.clear()}

##all station:
{load("C:/data/??完????????.RData")
  #????????????????????
  t1<-as.POSIXct('2011-10-01 00:00:00')
  t2<-as.POSIXct('2011-11-01 00:00:00')
  d1<-which(all_station$time==t1)
  d2<-which(all_station$time==t2)
  
  all_station$time<-as.character(all_station$time)
  all_station$time[as.POSIXct(all_station$time,format='%Y-%m-%d %H:%M:%S')<t1]<-'????'
  all_station$time[as.POSIXct(all_station$time,format='%Y-%m-%d %H:%M:%S')>=t1&as.POSIXct(all_station$time,format='%Y-%m-%d %H:%M:%S')<t2]<-'????'
  all_station$time[as.POSIXct(all_station$time,format='%Y-%m-%d %H:%M:%S')>=t2]<-'??????'
  cirplot<-data.frame(table(all_station$flag,all_station$time))
  
  library(circlize)
  gap<-c(1,1,20,20,20,20,20)
  circos.par(gap.after = gap)
  col<-c(????="black",????="black",??????="black",??????????????="red",????????????="purple",??????????????="black",??完????="light green")
  #????????????????????
  pdf("NAs.pdf",family="GB1")
  chordDiagram(cirplot, grid.col = col,transparency = 0.3,order = c("????", "????", "??????", "????????????", "??????????????", "??完????", "??????????????"))
  dev.off()
  circos.clear()
  }





library(ggplot2)
library(scales)
ggplot(data=all_data,aes(x=time,y=flag,color= factor(flag),fill=factor(flag)))+geom_crossbar(fatten = 0,ymin=1,ymax=3)+
  xlab('????')+
  scale_x_datetime(labels = date_format('%Y-%m-%d', tz = "Asia/Taipei"), breaks = date_breaks('10 days'))+
  guides(fill=guide_legend(title="????????"),col=guide_legend(title="????????"))+
  scale_colour_grey(labels=c("????????????","????????","??完????"))+
  scale_fill_grey(labels=c("????????????","????????","??完????"))+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.key.size = unit(1.0, "cm"),
        legend.text = element_text(size=15),axis.text.x = element_text(size=15),
        axis.title.x = element_text(size = 15))+
  facet_grid(lane~.)

#crossbar??????????????????????fill??colour??????????????????????????????



flag<-c(flag,flag1,flag2,flag3)
station<-rep(c('station 1049','station 1050','station 1140','station 1141'),each=length(flag1)*3)
pieplot<-data.frame(flag,station)
rm(list=ls()[!(ls()=='pieplot')])

##load pieplot.Rdata
p1<-pieplot[pieplot$station=='station 1049',]
p2<-pieplot[pieplot$station=='station 1050',]
p3<-pieplot[pieplot$station=='station 1140',]
p4<-pieplot[pieplot$station=='station 1141',]
y<-c(sum(p1$flag==1),sum(p1$flag==2),sum(p1$flag==3),sum(p2$flag==1),sum(p2$flag==2),sum(p2$flag==3),
     sum(p3$flag==1),sum(p3$flag==2),sum(p3$flag==3),sum(p4$flag==1),sum(p4$flag==2),sum(p4$flag==3))
fill<-rep(c(1,2,3),times=4)
station<-rep(c('a','b','c','d'),each=3)
pdata<-data.frame(y,fill,station)
ratio<-NULL
i<-1
while (i<11) {
  for(j in 0:2){ratio<-c(ratio,y[i+j]/sum(y[i],y[i+1],y[i+2]))};
  i<-i+3}
pdata$ratio<-ratio #add ratio to data frame directly
position<-cumsum(pdata$y)-0.5*pdata$y # text position
pdata$position<-position 
ggplot(data = pdata) + 
  geom_bar(aes(x = "", y = y, fill = factor(fill)), stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = rep('',12)),x=1,y=1)+
  facet_grid(station ~ ., scales = "free")+
  theme(panel.background = element_rect(fill='white', colour='white'),strip.background=element_rect(fill='white', colour='white'),legend.text = element_text(size=15))+
  guides(fill=guide_legend(title="????????"),col=guide_legend(title="????????"))+
  scale_fill_grey(labels=c("????????????","????????","??完????"))
                              

