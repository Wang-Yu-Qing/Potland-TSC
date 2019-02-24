a<-c('s1049.RData','s1050.RData','s1140.RData','s1141.RData')
#数据路径：
b<-paste('c:/data/portland/',a,sep='')
#聚类中心路径：
c<-paste('c:/data/station',c('1049/loadings.RData','1050/loadings.RData','1140/loadings.RData','1141/loadings.RData'),sep='')
TSC_state<-NULL
LOS_State<-NULL
##两月全样本基础状态判别：
#注意上面创建的所有变量名及循环变量名不可以跟读取文件中的变量重名！
for(k in 1:4){
  load(b[k])
  load(c[k])
  # TSC:
  {library('dtwclust')
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
  TSC_state<-c(TSC_state,pre_state)}
  # LOS:
  {
    target_data<-new_data
    #时间汇集为五分钟：
    volume<-vector()
    speed<-vector()
    occ<-vector()
    for (i in 1:(nday*288)) {volume[i]<-sum(target_data$volume[(i*15-14):(i*15)])*12/3}
    for (i in 1:(nday*288)) {speed[i]<-sum(target_data$speed[(i*15-14):(i*15)]*target_data$volume[(i*15-14):(i*15)])/sum(target_data$volume[(i*15-14):(i*15)])}
    for (i in 1:(nday*288)) {occ[i]<-mean(target_data$occ[(i*15-14):(i*15)])}
    #LOS判别：
    Density<-volume/speed
    los_state<-vector()
    for (i in 1:(nday*288)) {if(Density[i]<11) (los_state[i]<-'LOS-A'
) else(if(Density[i]<18) (los_state[i]<-'LOS-B'
) else(if(Density[i]<26) (los_state[i]<-'LOS-C'
) else(if(Density[i]<35) (los_state[i]<-'LOS-D'
) else(if(Density[i]<45) (los_state[i]<-'LOS-E'
) else(los_state[i]<-'LOS-F')))))}
  LOS_State<-c(LOS_State,los_state)
  }
}
l_t<-data.frame(LOS=LOS_State,TSC=TSC_state)
l_t$TSC<-factor(l_t$TSC,levels=c(1,2,3,4),labels=c('状态一','状态二','状态三','状态四'))
lt<-data.frame(table(l_t$LOS,l_t$TSC))
#绘制弦图(存为PDF会更清晰)：
library(circlize)
gap<-c(1,1,1,15,15,15,1,15,15,15)
circos.par(gap.after = gap)
#变量名可以加引号也可以不加
col<-c('LOS-A'="purple",'LOS-B'="light blue",'LOS-C'="light green",'LOS-D'="orange",'LOS-E'="brown",'LOS-F'="red",
状态一="blue",状态二="light blue",状态三="orange",状态四="red")
#保存在工作目录下
pdf("l-t.pdf",family="GB1")
chordDiagram(lt, grid.col = col,transparency = 0.3,
order=c('LOS-A','LOS-B','LOS-C','LOS-D','LOS-E','LOS-F','状态一','状态二','状态三','状态四'))
dev.off()
circos.clear()