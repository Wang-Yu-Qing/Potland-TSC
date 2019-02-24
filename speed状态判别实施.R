target_data<-read.xlsx(file='d:/data/station1049/11.11.xlsx','Sheet1')    #c

speed_series<-target_data$speed                                  #speed series 
speed_window<-list()
for (i in 1:288) {speed_window[[i]]<-vector('numeric',15)}        #(5,5) 
i<-1
j<-1
while (i<4307){
  speed_window[[j]]<-speed_series[i:(i+14)]
  i<-i+15
  j<-j+1
}                                                         #(5,5)window

speed_state<-apply(proxy::dist(speed_window,centroids,method='dtw'),1,which.min)  #state judging

t<-c(1:288)                                                       #(5,5)
qplot(t,speed_state,geom='crossbar',ymin=1,ymax=3,ylab = 'state',xlab = 'time spot',fill=speed_state,color=speed_state)  #state ploting   