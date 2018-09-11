rm(list=ls())
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
ptm1<-proc.time()

t1<-function(n,d,df){	# generate regression data
x<-matrix(0,n,d)		# n=sample size, d =dim
y<-rep(0,n)			# vessel for response variable
for (j in 1:d) {x[,j] = rt(n, df, ncp=0)}	# x generated from t distribution
for (i in 1:n){
              p = exp(sum(x[i,]))/(1+exp(sum(x[i,])))
              y[i] = rbinom(1, 1, p)}
list(x=x,y=y)}

######## Declare/ initialize variables and calculate true quantiles ################################
n<-100000
d<-10
true.sim <- 10000
b.true <- rep(1,d) 
Rn = rep(NA, true.sim)
ptm<-proc.time()
for (isim in 1:true.sim){
  data<-t1(n,d,3)  # data generating function
  x=data$x;y=data$y
  b.est <- coef(glm(y~x, family = "binomial",control = list(maxit = 1000)))[1+1:d]
  foo <- exp(x%*%b.est)
  s <- foo/(1+foo)^2     # elements of covariance matrix
  Rn[isim] <- sum(s*((x%*%(b.est-b.true))^2))  # calculate root function
}
proc.time()-ptm
E.true <- quantile(Rn, probs=0.95, na.rm=TRUE)

#E.true=qchisq(0.95,df=d)

n.sim<-20
TimeLimit = 20   # time limit for all algorithms

r.bt = 100				              # max no. of bootstrap resamples
r.bt.act = rep(0,n.sim)         # actual no. of bootstrap resamples
t.bt<-matrix(NA,r.bt,n.sim)		  # vessel for time taken for bootstrap resamples
cum.t.bt<-matrix(NA,r.bt,n.sim)	# vessel for time taken for bootstrap resamples
Rn.bt<-matrix(NA,r.bt,n.sim)		# vessel for bootstrap estimates of Rn
E.bt<- matrix(NA,r.bt,n.sim)		# vessel for cumulative bootstrap estimates of CI of Rn
err.bt<-matrix(NA,r.bt,n.sim)		# vessel for cumulative error for bootstrap

g = c(0.6,0.7,0.8)
B = ceiling(n^g)

s.blb = 100			  # max no. of BLB subsets
s.blb.act = matrix(0,nrow=length(B),ncol=n.sim)	  # actual no. of BLB subsets
r.blb = 100					# no. of BLB resamples for each subset
t.blb<-matrix(NA,s.blb,n.sim)	  	# vessel for time taken for BLB subsets
cum.t.blb<-matrix(NA,s.blb*length(B),n.sim)	# vessel for time taken for BLB subsets
Rn.blb<-matrix(NA,r.blb,n.sim)  	# vessel for BLB estimates of Rn
E.blb<- matrix(NA,s.blb,n.sim)  	# vessel for cumulative BLB estimates of CI of Rn
mean.E.blb<- matrix(NA,s.blb,n.sim)	# vessel for cumulative means of BLB estimates of CI of Rn
err.blb<-matrix(NA,s.blb*length(B),n.sim)	# vessel for cumulative error for BLB

s.ws = 5000				              # no. of BLB-WS subsets
s.ws.act = matrix(0,nrow=length(B),ncol=n.sim)  	  # actual no. of BLB-WS subsets
t.ws<-matrix(NA,s.ws,n.sim)		  # vessel for time taken for BLB-WS subsets
cum.t.ws<-matrix(NA,s.ws*length(B),n.sim)	# vessel for cumulative time taken for BLB-WS subsets
Rn.ws<-matrix(NA,s.ws,n.sim)		# vessel for BLB-WS estimates of Rn
E.ws<- matrix(NA,s.ws,n.sim)		# vessel for cumulative BLB-WS estimates of CI of Rn
err.ws<-matrix(NA,s.ws*length(B),n.sim)		# vessel for cumulative error for BLB

##### Run Simulations #####
for (sim in 1:n.sim) {
set.seed(1729+sim)
data<-t1(n,d,3)	# data generating function
x=data$x;y=data$y

ptm<-proc.time()  # time taken in calculating b.est will count for bootstrap
b.est <- coef(glm(y~x, family = "binomial",control = list(maxit = 1000)))[1+1:d]
t.est<-proc.time()-ptm

##### Bootstrap code #####
for (i in 1:r.bt){
  ptm<-proc.time()          # start stopwatch
  index = sample(n, size=n, replace = TRUE)	# bootstrap resample
x1=x[index,]; y1=y[index] # construct resample
b.bt<-coef(glm(y1~x1, family = "binomial",control = list(maxit = 1000)))[1+1:d] # resample estimate
foo <- exp(x1%*%b.bt)
s <- foo/(1+foo)^2     # elements of covariance matrix
Rn.bt[i,sim] <- sum(s*((x1%*%(b.est-b.bt))^2))	# calculate root function
t<-proc.time()-ptm				# stop the clock
t.bt[i,sim]<-t[3]         # record time for current resample
if (i == 1) {t.bt[i,sim] = t[3]+t.est[3]}
if (sum(t.bt[(1:i),sim]) > TimeLimit) {
  r.bt.act[sim] = i-1  # check if time limit is breached
  break}
  # Obtain quantiles from bootstrap ensemble so far, and calculate error
E.bt[i,sim] = quantile(Rn.bt[1:i,sim],prob=0.95, na.rm = TRUE)
err.bt[i,sim]<-abs(E.bt[i,sim]/E.true-1)
}	# next bootstrap resample
if (r.bt.act[sim]>0){
cum.t.bt[1:r.bt.act[sim],sim]<-cumsum(t.bt[1:r.bt.act[sim],sim])}		# cumulative time taken for bootstrap iterations
print("Bootstrap"); print(sim)

##### BLB code #####
for (size in 1:length(B)){
  b = B[size]             # pick subset size
for (i in 1:s.blb){				# loop for subsets
  ptm<-proc.time()
  subset=sample(n, size=b, replace = TRUE)	# select subsample
x.sub=x[subset,];y.sub=y[subset]
b.sub <- coef(glm(y.sub~x.sub, family = "binomial",control = list(maxit = 1000)))[1+1:d]  # calculate subset estimate
for (j in 1:r.blb){		# r.blb resamples from each subsample
w<-as.vector(rmultinom(n=1, size=n, prob=rep(1,b)/b))	# resample wts
b.blb <- coef(glm(y.sub~x.sub, family = "binomial", weights=w,control = list(maxit = 1000)))[1+1:d] # calculate subset estimate
foo <- exp(x.sub%*%b.blb)   # elements of covariance matrix
s <-  w*(foo/(1+foo)^2) # incorporate resample wts into s
Rn.blb[j,sim] <- sum(s*((x.sub%*%(b.sub-b.blb))^2))  # calculate root function
}	# next resample
t<-proc.time()-ptm				# record time
t.blb[i,sim]<-t[3]
if (sum(t.blb[(1:i),sim]) > TimeLimit) {
  s.blb.act[size,sim] = i-1  # check if time limit is breached
  break}
# Obtain quantiles from resamples, and calculate error
E.blb[i,sim] = quantile(Rn.blb[,sim],prob=0.95, na.rm = TRUE)
# take average across subsets so far
mean.E.blb[i,sim] = mean(E.blb[1:i,sim])
# calculate error
err.blb[s.blb*(size-1)+i,sim]<-abs(mean.E.blb[i,sim]/E.true-1)
}	# next subset
if (s.blb.act[size,sim]>0){
  foo = s.blb.act[size,sim]
cum.t.blb[s.blb*(size-1)+1:foo,sim]<-cumsum(t.blb[1:foo,sim])}		# cumulative time taken for blb subsets
} # next subset size
print("BLB"); print(sim)

##### BLB-WS #####
for (size in 1:length(B)){
  b = B[size]             # pick subset size
  for (i in 1:s.ws){			# loop for subset
    ptm<-proc.time()
    subset=sample(n, size=b, replace = TRUE)	# select subsample
x.sub=x[subset,];y.sub=y[subset]
b.sub <- coef(glm(y.sub~x.sub, family = "binomial", control = list(maxit = 1000)))[1+1:d] # calculate subset estimate
w<-as.vector(rmultinom(n=1, size=n, prob=rep(1,b)/b))	# resample wts
b.ws <- coef(glm(y.sub~x.sub, family = "binomial", weights=w,control = list(maxit = 1000)))[1+1:d]  # calculate resample estimate
foo <- exp(x.sub%*%b.ws)   # elements of covariance matrix
s <-  w*(foo/(1+foo)^2) # incorporate resample wts into s
Rn.ws[i,sim] <- sum(s*((x.sub%*%(b.sub-b.ws))^2))  # calculate root function
t<-proc.time()-ptm				# record time
t.ws[i,sim]<-t[3]
if (sum(t.ws[(1:i),sim]) > TimeLimit) {
  s.ws.act[size,sim] = i-1  # check if time limit is breached
  break}
# Obtain quantiles from blb-ws ensemble so far, and calculate error
E.ws[i,sim] = quantile(Rn.ws[1:i,sim],prob=0.95, na.rm=TRUE)
err.ws[s.ws*(size-1)+i,sim]<-abs(E.ws[i,sim]/E.true-1)
}	# next blb-ws subset
if (s.ws.act[size,sim]>0){
  foo = s.ws.act[size,sim]
cum.t.ws[s.ws*(size-1)+1:foo,sim]<-cumsum(t.ws[1:foo,sim])}		# cumulative time taken for ws iterations
} # next subset size
print("SDB"); print(sim)
}	# next MC sample

proc.time() - ptm1   # time taken

##### Save output
save.image("iid_logreg_1215.RData")