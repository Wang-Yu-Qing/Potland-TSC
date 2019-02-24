#预测值：
pre<-c(1:10)
#真实值：
real<-c(2:11)
# MAE:
MAE<-sum(abs(pre-real))/length(real)
# MAPE:
MAPE<-sum(sapply(seq_len(length(real)),function(i) abs((real[i]-pre[i]))/real[i]))/length(real)
#RMSE:
RMSE<-sqrt(sum(sapply(seq_len(length(real)),function(i) (real[i]-pre[i])^2))/length(real))