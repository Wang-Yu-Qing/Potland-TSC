vol_o<-0
for (i in 1:287) {vol_o<-vol_o+abs(occ_state[i]-occ_state[i+1])}


vol_p<-0
for (i in 1:287) {vol_p<-vol_p+abs(prin_state[i]-prin_state[i+1])}


vol_s<-0
for (i in 1:287) {vol_s<-vol_s+abs(speed_state[i]-speed_state[i+1])}


vol_s_o<-0
for (i in 1:287) {vol_s_o<-vol_s_o+abs(speed_only_state[i]-speed_only_state[i+1])}

vol_o
vol_p
vol_s
vol_s_o