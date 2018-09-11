rm(list=ls())
setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
#setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#load("iid_logreg_1206.RData")
#load("IID sim results/sim06-18.RData")
load("From RDC/IIDsims/iid_logreg_1215.RData")
#load("IID sim results/iid_linreg_1209.RData")
TIME = (0:(TimeLimit*1))/1
y.bt = rep(0,length(TIME));
y.blb = matrix(0,length(B),length(TIME));
y.ws = matrix(0,length(B),length(TIME));

for (isim in 1:n.sim){
x = as.vector(na.omit(cum.t.bt[,isim]))
y = as.vector(na.omit(err.bt[,isim]))
for (i in 1:length(TIME)){
time = TIME[i]
# the next line has to be done because for iid sims, we did not record time 
# for the iteration that breaches TimeLimit, so for time == 59 or 60
# index will just be an array of zeros
if (time >= x[length(x)]) {y.bt[i] = y.bt[i]+y[length(x)]}
if (time < x[length(x)]){
index<-as.numeric(x>time)
cut<-which.max(index)-1
if (cut==0) {
y.bt[i]=y.bt[i]+1}
if (cut>0){
y.bt[i] = y.bt[i]+y[cut]}
}}}

for (irow in 1:length(B)){
  for (isim in 1:n.sim){
x = as.vector(na.omit(cum.t.blb[(s.blb*(irow-1)+1:s.blb),isim]))
y = as.vector(na.omit(err.blb[(s.blb*(irow-1)+1:s.blb),isim]))
for (i in 1:length(TIME)){
time = TIME[i]
# the next line has to be done because for iid sims, we did not record time 
# for the iteration that breaches TimeLimit, so for time == 59 or 60
# index will just be an array of zeros
if (time >= x[length(x)]) {y.blb[irow,i] = y.blb[irow,i]+y[length(x)]}
if (time < x[length(x)]){
index<-as.numeric(x>time)
cut<-which.max(index)-1
if (cut==0) {
y.blb[irow,i]=y.blb[irow,i]+1}
if (cut>0){
y.blb[irow,i] = y.blb[irow,i]+y[cut]}
}}}
}	# irow

for (irow in 1:length(B)){
for (isim in 1:n.sim){
x = as.vector(na.omit(cum.t.ws[(s.ws*(irow-1)+1:s.ws),isim]))
y = as.vector(na.omit(err.ws[(s.ws*(irow-1)+1:s.ws),isim]))
for (i in 1:length(TIME)){
time = TIME[i]
# the next line has to be done because for iid sims, we did not record time 
# for the iteration that breaches TimeLimit, so for time == 59 or 60
# index will just be an array of zeros
if (time >= x[length(x)]) {y.ws[irow,i] = y.ws[irow,i]+y[length(x)]}
if (time < x[length(x)]){
index<-as.numeric(x>time)
cut<-which.max(index)-1
if (cut==0) {
y.ws[irow,i]=y.ws[irow,i]+1}
if (cut>0){
y.ws[irow,i] = y.ws[irow,i]+y[cut]}
}}}
}	# irow

##### plots #####
ymax = max(y.bt,y.blb,y.ws)/n.sim
size = 3
plot(TIME,y.bt/n.sim,type="l",ylim=c(0,ymax),ylab="",xlab="")
#title(main = expression("b = " ~ "n"^{0.6}))	# CHANGE THIS MANUALLY
#title(xlab="Time(sec)",ylab="error rate")  # CHANGE THIS MANUALLY
points(TIME,y.blb[size,]/n.sim,type="l", lwd = 1, lty = 5)
points(TIME,y.ws[size,]/n.sim,type="l", lwd = 2, lty = 3)
# Plot legend where you want
# legend("topright",
#        legend = c("Bootstrap","BLB","FDB"),
#        lty = 1, col = c(1,"red","blue"),cex=1)



















##### PLOT 2. All methods, single subset size
# Expand south side of clipping rect to make room for the legend
#par(xpd=T, mar=par()$mar+c(6,1,0,1))
par(mfrow=c(1,3))
plot(cum.t.bt.avg,err.bt.avg,xlim=c(0,xmax),ylim=c(0,ymax),type="l",xlab="",ylab="")
#title(xlab="time", ylab="error")
#points(cum.t.bt.avg,err.bt.avg)
for (size in 1:1){
  x = rowMeans(cum.t.blb[(s.blb*(size-1)+1:s.blb),],na.rm=TRUE)
  x = na.omit(x)
  y = err.blb.avg[s.blb*(size-1)+1:s.blb]
  y = na.omit(y)
  points(x,y,col="red",type="l")
  points(x,y,pch=20)
}
for (size in 1:1){
  x = rowMeans(cum.t.ws[(s.ws*(size-1)+1:s.ws),],na.rm=TRUE)
  x = na.omit(x)
  y = err.ws.avg[s.ws*(size-1)+1:s.ws]
  y = na.omit(y)
  points(x,y,col="blue",type="l",lty=2)}
points(0:TimeLimit,rep(0,61),type="l",lty=2)
# Plot legend where you want
legend(0,-0.1,
       legend = c("Bootstrap","BLB(b=n^0.8)", "BLB-WS(b=n^0.8)"),
       lty = c(1,(3:3),(3:3),(3:3),(3:3)),
       pch = c(1,(3:3),rep(NA,2),(3:3)), 
       col = c(1,rep(30,1),rep(36,1),rep(51,1),rep(12,1))
       ,ncol=2,cex=0.8)