a<-c('s1049.RData','s1050.RData','s1140.RData','s1141.RData')
#数据路径：
b<-paste('c:/data/portland/',a,sep='')
#聚类中心路径：
c<-paste('c:/data/station',c('1049/loadings.RData','1050/loadings.RData','1140/loadings.RData','1141/loadings.RData'),sep='')
all_state<-NULL
dat<-NULL
##两月全样本基础状态判别：
#注意路径变量名不可以跟读取文件中的变量重名！读取的文件里也不能有k！
for(k in 1:4){
  load(b[k])
  load(c[k])
  library('dtwclust')
  target_data<-new_data
  #标准化:
  flow<-(target_data[,3]-fm20)
  speed<-(target_data[,4]-sm20)
  occ<-(target_data[,5]-om20)
  s_target_data<-data.frame(flow,speed,occ)
  multi_series<-list()
  #抽取序列：
  nday<-nrow(new_data)/4320
  for (j in 1:(nday*288)) {multi_series[[j]]<-cbind(s_target_data[(j*15-14):(j*15),1],s_target_data[(j*15-14):(j*15),2],s_target_data[(j*15-14):(j*15),3])}
  #状态判别：
  multi_state<-vector()
  pre_state<-vector()
  multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centroids,method='dtw'))
  pre_state[1]<-multi_state[1]
  for (j in 2:(nday*288)) {d_matrix<-proxy::dist(multi_series[j],multi_cluster@centroids,method='dtw')
  if (abs(d_matrix[pre_state[j-1]]-min(d_matrix))<50) (pre_state[j]<-pre_state[j-1]
  ) else (pre_state[j]<-which.min(d_matrix))
  #显示做到哪一步：
  print(paste(k,j,sep='-'))}
  all_state<-c(all_state,pre_state)
  #将20秒的时间变为5分钟(因为以五分钟划分日期都一样，所以以15条为间隔，取第15个日期)：
  #因为是先NULL后载入，因此不可以与载入变量date重名！
  #返回的是因子，要转化为字符后再赋值，不然赋的是因子水平编号！
  dat<-c(dat,as.character(sapply(c(1:(nday*288)),function(x){new_data$date[x*15]})))
  }

##注意：跑完后，dat和all_state的长度必须是一样的！

#构造日期-状态数据框：
date_state<-data.frame(date=rep(dat,times=4),state=all_state)
#转化为周几及汉字状态：
date_state$date<-weekdays(as.POSIXct(date_state$date))
date_state$state<-factor(date_state$state,levels=c(1,2,3,4),labels=c('状态一','状态二','状态三','状态四'))
#频数统计表并转化为数据框：
cirplot<-data.frame(table(date_state$state,date_state$date))
round(prop.table(table(date_state$state,date_state$date),2),5)
#绘制弦图(存为PDF会更清晰)：
library(circlize)
gap<-c(1,1,1,1,1,1,15,15,15,15,15)
circos.par(gap.after = gap)
col<-c(星期一="black",星期二="black",星期三="black",星期四="black",星期五="black",星期六="black",
星期日="black",状态一="blue",状态二="light blue",状态三="orange",状态四="red")
#保存在工作目录下
pdf("s-w.pdf",family="GB1")
chordDiagram(cirplot, grid.col = col,transparency = 0.3,
order=c('星期一','星期二','星期三','星期四','星期五','星期六','星期日',
'状态一','状态二','状态三','状态四'))
dev.off()
circos.clear()
