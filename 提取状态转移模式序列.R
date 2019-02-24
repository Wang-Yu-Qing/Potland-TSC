library('dtwclust')
clusters_occ<-as(clusters_occ,"TSClusters")
multi_cluster<-as(multi_cluster,"TSClusters")
f<-list.files('d:/data/station1049/day/tar/')
d<-paste('d:/data/station1049/day/tar/',f,sep = '') # file direction
trans_data<-NULL
trans_series<-list()
for(j in 1:length(f)){
  load(d[j])
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
  multi_state[1]<-which.min(proxy::dist(multi_series[1],multi_cluster@centroids,method='dtw'))
  for (i in 2:288) {d_matrix<-proxy::dist(multi_series[i],multi_cluster@centroids,method='dtw')
  if (abs(d_matrix[multi_state[i-1]]-min(d_matrix))<50) (multi_state[i]<-multi_state[i-1])
  else (multi_state[i]<-which.min(d_matrix))}
  
  # extract state==4 median +- 1.5h (scaled):
  state<-rep(multi_state,each=15)
  me<-median(which(state==4))
  trans_data<-s_target_data[(me-270):(me+269),2:3]
  # denoising:
  slm<-loess(trans_data$speed~c(1:nrow(trans_data)),span = 0.6)
  olm<-loess(trans_data$occ~c(1:nrow(trans_data)),span = 0.6)
  plot(c(1:nrow(trans_data)),slm$fitted,'l',main=)
  plot(c(1:nrow(trans_data)),olm$fitted,'l')
  # two converting series:
  s_down<-slm$fitted[1:which.max(olm$fitted)]
  s_up<-slm$fitted[which.max(olm$fitted):length(slm$fitted)]
  o_up<-olm$fitted[1:which.max(olm$fitted)]
  o_down<-olm$fitted[which.max(olm$fitted):length(olm$fitted)]
  trans_series[[j*2-1]]<-cbind(speed=s_down,occ=o_up)
  trans_series[[j*2]]<-cbind(speed=s_up,occ=o_down)
}                                                      # 取得是占有率取最大值


for(i in 1:length(trans_series)){
  plot(c(1:length(trans_series[[i]][,1])),trans_series[[i]][,1],'l')
  plot(c(1:length(trans_series[[i]][,1])),trans_series[[i]][,2],'l')
}

dtwc<-dtwclust(trans_series,'partitional',k=2,distance = 'dtw')
plot(dtwc)


#plot series and centers (series length differ so automaically add group and cluster):
gg<-NULL
for(i in 1:length(trans_series)){
  gg<-rbind(gg,data.frame(trans_series[[i]],group=rep(i,nrow(trans_series[[i]])),
                          cluster=rep(dtwc@cluster[i],nrow(trans_series[[i]])),
                          tindex=c(1:nrow(trans_series[[i]]))))
}
gg[,1]<-gg[,1]+sm20
gg[,2]<-gg[,2]+om20
library('scales')
# clus1 speed:
ggplot()+geom_line(data=gg[gg$cluster==1,],aes(x=tindex,y=speed,group=group,colour=factor(group)))+
  geom_line(aes(x=c(1:nrow(dtwc@centroids[[1]])),y=(dtwc@centroids[[1]][,1]+sm20)),size=2,lty=2)+
  theme(panel.background = element_rect(fill='white', colour='white'))+xlim(c(0,400))+ylim(0,80)

# clus1 occ:
ggplot()+geom_line(data=gg[gg$cluster==1,],aes(x=tindex,y=occ,group=group,colour=factor(group)))+
  geom_line(aes(x=c(1:nrow(dtwc@centroids[[1]])),y=(dtwc@centroids[[1]][,2]+om20)),size=2,lty=2)+
  theme(panel.background = element_rect(fill='white', colour='white'))+xlim(c(0,400))+ylim(0,65)

# clus2 speed:
ggplot()+geom_line(data=gg[gg$cluster==2,],aes(x=tindex,y=speed,group=group,colour=factor(group)))+
  geom_line(aes(x=c(1:nrow(dtwc@centroids[[2]])),y=(dtwc@centroids[[2]][,1]+sm20)),size=2,lty=2)+
  theme(panel.background = element_rect(fill='white', colour='white'))+xlim(c(0,400))+ylim(0,80)

# clus1 occ:
ggplot()+geom_line(data=gg[gg$cluster==2,],aes(x=tindex,y=occ,group=group,colour=factor(group)))+
  geom_line(aes(x=c(1:nrow(dtwc@centroids[[2]])),y=(dtwc@centroids[[2]][,2]+om20)),size=2,lty=2)+
  theme(panel.background = element_rect(fill='white', colour='white'))+xlim(c(0,400))+ylim(0,65)


# test:
# real-time series with denoise:
test_series<-list()
re_state<-NULL
plot(c(1:nrow(trans_data)),trans_data$speed,'l')
plot(c(1:nrow(trans_data)),trans_data$occ,'l')
dis_matrix<-NULL
for(i in 1:(nrow(trans_data)/15)){
  sml<-loess(trans_data[(i*15-14):(i*15),1]~c(1:15),span = 0.7)
  oml<-loess(trans_data[(i*15-14):(i*15),2]~c(1:15),span = 0.7)
  plot(c(1:15),sml$fitted,'l')
  plot(c(1:15),oml$fitted,'l')
  test_series[[i]]<-cbind(sml$fitted,oml$fitted)
  dis_matrix<-rbind(dis_matrix,proxy::dist(test_series[i],dtwc@centroids,method='dtw'))
  re_state[i]<-predict(dtwc,test_series[i])   #it must be a list of matrices so use [] not [[]]
  }

# real-time series without denoise:
for(i in 1:(nrow(trans_data)/15)){
  test_series[[i]]<-as.matrix(trans_data[(i*15-14):(i*15),])
  dis_matrix<-rbind(dis_matrix,proxy::dist(test_series[i],dtwc@centroids,method='dtw'))
  re_state[i]<-predict(dtwc,test_series[i])   #it must be a list of matrices so use [] not [[]]
}
rm(list=ls()[!(ls()=='dtwc')])


