rm(list=ls())
setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
#load("TS results from desktop/CI/ts_Msimsrho3q2_1103.RData")
#setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#load("From RDC/TSsims/ts_regsimqrho3_1216.RData")
load("From RDC/TSsims/ts_Msimsrho3q2_1217.RData")
q
rho
(l=c(l1,l2,l3))
#(l=c(l1,l2,l3))
(b=c(b1,b2))
###############################
foo = sim32
block=3;sub=2
# for Sample median
if (l[block]==10) {TIME = (0:(TimeLimit2*1))/1}
if (l[block]>10) {TIME = (0:(TimeLimit*1))/1}
# for TS regression
if (l[block]==10) {TIME = (0:(TimeLimit1*1))/1}
if (l[block]==20) {TIME = (0:(TimeLimit2*1))/1}
if (l[block]==50) {TIME = (0:(TimeLimit3*1))/1}

t.bt = foo$cum.t.bt;t.blb = foo$cum.t.blb;t.ws = foo$cum.t.ws
err.bt=foo$err.bt;err.blb=foo$err.blb;err.ws=foo$err.ws
y.bt = rep(0,length(TIME));y.blb = rep(0,length(TIME));y.ws = rep(0,length(TIME))
for (i in 1:length(TIME)){
time = TIME[i]
for (isim in 1:n.sim){
index<-as.numeric(t.bt[[isim]]>time)
cut<-which.max(index)-1
if (cut==0) {
y.bt[i]=y.bt[i]+1}
if (cut>0){
y.bt[i] = y.bt[i]+err.bt[[isim]][[cut]]}

index<-as.numeric(t.blb[[isim]]>time)
cut<-which.max(index)-1
if (length(cut)>0) {
if (cut==0) {
y.blb[i]=y.blb[i]+1}
if (cut>0){
y.blb[i] = y.blb[i]+err.blb[[isim]][[cut]]}}
if (foo$s.blb.act[isim]==0) {y.blb[i]=y.blb[i]+1}

index<-as.numeric(t.ws[[isim]]>time)
cut<-which.max(index)-1
if (cut==0) {
y.ws[i]=y.ws[i]+1}
if (cut>0){
y.ws[i] = y.ws[i]+err.ws[[isim]][[cut]]}
}}

ymax = max(y.bt,y.blb,y.ws)/n.sim
##### PLOT 2. All methods, single subset size
# Expand south side of clipping rect to make room for the legend
#par(xpd=T, mar=par()$mar+c(6,1,0,1))
plot(TIME,y.bt/n.sim,type="l",ylim=c(0,ymax),ylab="",xlab="")
#title(xlab="time", ylab="error",main=c(bquote(q==.(q)~ ","~l==.(l[block])~ "," ~b==.(b[sub]))))
#title(xlab="time", ylab="error",main=c(bquote(l==.(l[block])~ "," ~b==.(b[sub]))))
#title(xlab="time", ylab="error")
points(TIME,y.blb/n.sim,type="l",lwd = 1, lty = 5)
points(TIME,y.ws/n.sim,type="l",lwd = 2, lty = 3)
# Plot legend where you want
#legend(0,-0.5,
#       legend = c("Bootstrap","BLB","BLB-WS"),lty = c(1,3,3), 
#	col = c(1,rep(30,1),rep(36,1)),ncol=3,cex=0.8)
l[block]
b[sub]



