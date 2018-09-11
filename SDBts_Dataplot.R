rm(list=ls())
setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
load("From RDC/Dataset/CET_MBBT600L50_1221.RData")
TimeLimit1 <- TimeLimit
load("From RDC/Dataset/CET_BLBT300L50B10000_1221.RData")
TimeLimit2 <- TimeLimit
load("From RDC/Dataset/CET_SDBT300L50B10000_1221.RData")
TimeLimit3 <- TimeLimit
Time <- c(TimeLimit1,TimeLimit2,TimeLimit3)
rm(list=setdiff(ls(),c("l","b","cum.t.bt","cum.t.blb","cum.t.ws","q.bt","q.blb","q.ws","Time","r.bt.act","s.blb.act","s.ws.act")))
t.bt = cum.t.bt;t.blb = cum.t.blb;t.ws = cum.t.ws

TIME1 = (0:((Time[1])*1))/1
y.bt = rep(0,length(TIME1))
for (i in 1:length(TIME1)){
time = TIME1[i]
index<-as.numeric(t.bt[[1]]>time)
cut<-which.max(index)-1
if (cut==0) {
y.bt[i]=0}
if (cut>0){
y.bt[i] = q.bt[[1]][[cut]]}
}
TIME2 = (0:((Time[2])*1))/1
y.blb = rep(0,length(TIME2))
for (i in 1:length(TIME2)){
  time = TIME2[i]
  index<-as.numeric(t.blb[[1]]>time)
cut<-which.max(index)-1
if (length(cut)>0) {
if (cut==0) {
y.blb[i]=0}
if (cut>0){
y.blb[i] = q.blb[[1]][[cut]]}}
}
TIME3 = (0:((Time[3])*1))/1
y.ws = rep(0,length(TIME3))
for (i in 1:length(TIME3)){
  time = TIME3[i]
  index<-as.numeric(t.ws[[1]]>time)
cut<-which.max(index)-1
if (length(cut)>0) {
  if (cut==0) {
y.ws[i]=0}
if (cut>0){
y.ws[i] = q.ws[[1]][[cut]]}}
}

ymax = max(y.bt,y.blb,y.ws)/10  # since the data is in unit if 10times degree C
ymax = 30
##### PLOT 2. All methods, single subset size
# Expand south side of clipping rect to make room for the legend
#par(xpd=T, mar=par()$mar+c(6,1,0,1))
plot(TIME1,y.bt/10,type="l",ylim=c(0,ymax),ylab="",xlab="")
#title(xlab="time", ylab="error",main=c(bquote(q==.(q)~ ","~l==.(l[block])~ "," ~b==.(b[sub]))))
#title(xlab="time", ylab="error",main=c(bquote(l==.(l[block])~ "," ~b==.(b[sub]))))
#title(xlab="time", ylab="error")
points(TIME2,y.blb/10,type="l", lty=5)
points(TIME3,y.ws/10,type="l", lwd=2,lty=3)
# Plot legend where you want
#legend(0,-0.5,
#       legend = c("Bootstrap","BLB","BLB-WS"),lty = c(1,3,3), 
#	col = c(1,rep(30,1),rep(36,1)),ncol=3,cex=0.8)
l;b
