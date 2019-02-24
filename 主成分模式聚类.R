train_data<-read.csv('d:/data/station1046/train/train_1046.csv',stringsAsFactors=FALSE)
train_list<-list()
for (i in 1:14688) {train_list[[i]]<-vector('numeric',15)}
train_data<-scale(train_data[,3:5],scale=F)
principal(train_data,nfactors = 1,scores = T)
prin_train_data<-train_data$volume*0.828+train_data$speed*-0.866+train_data$occ*0.956
for (i in 1:14688) {train_list[[i]]<-prin_train_data[(i*15-14):(i*15)]}
cr3<-dtwclust(train_list,'fuzzy',k=4,distance = 'dtw',seed=100)