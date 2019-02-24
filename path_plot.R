



each_state<-rep(speed_only_state,each=15)                    #c
path_data<-data.frame(target_data,each_state)
qplot(volume,speed,data = path_data,geom=c('path','point'),color=factor(each_state))+scale_colour_manual(values=c("blue", "orange", "red"))
qplot(occ,volume,data = path_data,geom=c('path','point'),color=factor(each_state))+scale_colour_manual(values=c("blue", "orange", "red"))



peak_data<-path_data[path_data$each_state==3,]
norm_data<-path_data[path_data$each_state==2,]
valley_data<-path_data[path_data$each_state==1,]
qplot(volume,speed,data = peak_data)+geom_path(colour='red')+geom_point(colour='red')
qplot(volume,speed,data = norm_data)+geom_path(colour='orange')+geom_point(colour='orange')
qplot(volume,speed,data = valley_data)+geom_path(colour='blue')+geom_point(colour='blue')

state<-factor(path_data$each_state[1621:1640])
qplot(path_data$occ[1621:1640],path_data$volume[1621:1640],ylab='volume(veh)',xlab='occ(%)',geom=c('path','point'),color=state)+scale_colour_manual(values=c("blue", "red"))

qplot(path_data$occ[1631:1640],path_data$volume[1631:1640])+geom_path(colour='blue')+geom_point(colour='blue')