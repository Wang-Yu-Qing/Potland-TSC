train_data<-read.csv('d:/data/station1046/train/train_1046.csv')
train_data[,3:5]<-scale(train_data[,3:5],scale = F)
train_list<-list()
for (i in 1:288) {train_list[[i]]<-cbind(train_data$volume[(i*15-14):(i*15)],train_data$speed[(i*15-14):(i*15)],train_data$occ[(i*15-14):(i*15)])}
multi_cluster<-dtwclust(train_list,type='partitional',k=3,distance = 'dtw','pam',seed=100)
