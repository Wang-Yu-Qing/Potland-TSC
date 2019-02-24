o_state<-rep(occ_state,each=15)
s_state<-rep(speed_state,each=15)
p_state<-rep(prin_state,each=15)
s_o_state<-rep(speed_only_state,each=15)
state<-c(o_state,p_state,s_state,s_o_state)
method<-c(rep('occ',each=4320),rep('PCA',each=4320),rep('speed',each=4320),rep('speed threshold',each=4320))
all_data<-rbind(target_data,target_data,target_data,target_data)
all_data<-data.frame(all_data,state,method)

qplot(occ,volume,data = all_data,geom=c('path','point'),group=1,color=factor(state))+scale_colour_manual(values=c("blue", "orange", "red"))+facet_wrap(~method,scales = 'free')

qplot(occ,volume,data = all_data,geom='point',group=1,color=factor(state))+scale_colour_manual(values=c("blue", "orange", "red"))+facet_wrap(~method,scales = 'free')

qplot(volume,speed,data = all_data,geom=c('path','point'),group=1,color=factor(state))+scale_colour_manual(values=c("blue", "orange", "red"))+facet_wrap(~method,scales = 'free')

qplot(volume,speed,data = all_data,geom='point',group=1,color=factor(state))+scale_colour_manual(values=c("blue", "orange", "red"))+facet_wrap(~method,scales = 'free')