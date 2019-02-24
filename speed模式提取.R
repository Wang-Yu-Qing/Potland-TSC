train_data<-read.csv('d:/data/station1046/train/train_1046.csv',stringsAsFactors=FALSE)
train_list<-list()
for (i in 1:14688) {train_list[[i]]<-vector('numeric',15)}
occ_train_data<-train_data$speed
for (i in 1:14688) {train_list[[i]]<-occ_train_data[(i*15-14):(i*15)]}

clusters_speed_5<-dtwclust(train_list,'fuzzy',k=5,distance = 'dtw',seed=100)