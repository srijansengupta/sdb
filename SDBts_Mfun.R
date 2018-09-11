Msimv <- function(Var.true,n,rho,l,b,n.sim,TimeLimit){
k1 = ceiling(n/l)			# no. of blocks in resample
N = n-l+1	# number of blocks in sample
N1 = b-l+1	# number of blocks in subset

##### Declare variables #####
r.bt = 1000000	# max no. of bootstrap resamples
r.bt.act = rep(0,n.sim)       # actual no. of bootstrap resamples
cum.t.bt<-NULL	# vessel for cumulative time taken for bootstrap resamples
Var.bt<- NULL	# vessel for cumulative bootstrap estimates of Var
err.bt<-NULL		# vessel for cumulative error for bootstrap

s.blb = 1000000	# max no. of BLB subsets
s.blb.act = rep(0,n.sim)       # actual no. of BLB subsets
r.blb = 100		# no. of BLB resamples for each subset
cum.t.blb<-NULL	# vessel for cumulative time taken for BLB subsets
Var.blb<- NULL	# vessel for cumulative means of BLB estimates of Var
err.blb<-NULL		# vessel for cumulative error for BLB

s.ws = s.blb*r.blb/2	# max no. of BLB-WS subsets
s.ws.act = rep(0,n.sim)       # actual no. of BLB-WS subsets
cum.t.ws<-NULL	# vessel for cumulative time taken for BLB-WS subsets
Var.ws<- NULL	# vessel for cumulative BLB-WS estimates of Var
err.ws<-NULL		# vessel for cumulative error for BLB-WS

##### Run Simulations #####
for (sim in 1:n.sim) {
set.seed(1729+sim)
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")

ptm<-proc.time()	# including block mean calculation time in time taken


##### 1. MBB #####
VAR <- NULL			# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
ptm<-proc.time()  # time taken in calculating m will count for MBB
orderts<-sort(X)
m<-median(X)		# calculate this once for the ensemble
t.est<-proc.time()-ptm
for (i in 1:r.bt){	# loop for resamples
ptm<-proc.time()		# start stopwatch
wt <- rep(0,n)		# freq density of sample elements in current resample
foo = sample(1:N, size=k1,replace=TRUE)	# select blocks randomly
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N-foo[j]))}
wt <- wt/(k1*l)
cumwt<-cumsum(wt[order(X)])	# cumulative freq of subset elements in current resample
index<-as.numeric(cumwt>0.5)
cut<-which.max(index)		# calculate resample quantile by linear interpolation
BootMed<-(orderts[cut]*(0.5-cumwt[cut-1])+orderts[cut-1]*(cumwt[cut]-0.5))/(cumwt[cut]-cumwt[cut-1])
Rn = c(Rn, sqrt(n)*(BootMed-m))	# add centered median of current resample to ensemble for current subset
t<-proc.time()-ptm  			# record time
VAR <- c(VAR, var(Rn))		# Obtain variance estimate from ensemble of resamples	
TIME<-c(TIME,ifelse(i==1,t[3]+t.est[3],t[3])) #estimation time for original sample goes to first resample
if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
  r.bt.act[sim] = i-1  		# record actual number of subsets completed
	print("MBB sim")
	print(sim)
  break}

VAR[1] <- 0	# the formula calculates variance of a single term as NA
err<-abs(VAR/Var.true-1)	# calculate error
} 				# end loop for resamples

if (r.bt.act[sim]>0){		# write output into respective lists
cum.t.bt = c(cum.t.bt,list(cumsum(TIME)))
Var.bt = c(Var.bt, list(VAR))
err.bt = c(err.bt, list(err))		# cumulative time taken for bootstrap iterations
}	# close if (r.bt.act[sim]>0) loop

##### 2.BLB #####
mVAR <- NULL		# initialize for current sim
VAR <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
for (i in 1:s.blb){	# loop for subsets
Rn <- NULL			# initialize for current subset
ptm<-proc.time()		# start stopwatch
X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
orderts<-sort(X.sub)	# ordered subset
m<-median(X.sub)		# calculate this once for the ensemble
for (irep in 1:r.blb){	# loop for resamples
wt <- rep(0,b)		# freq density of subset elements in current resample
foo = sample(1:N1, size=k1,replace=TRUE)	# select blocks randomly
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
wt <- wt/(k1*l)
cumwt<-cumsum(wt[order(X.sub)])	# cumulative freq of subset elements in current resample
index<-as.numeric(cumwt>0.5)
cut<-which.max(index)			# calculate resample quantile by linear interpolation
BootMed<-(orderts[cut]*(0.5-cumwt[cut-1])+orderts[cut-1]*(cumwt[cut]-0.5))/(cumwt[cut]-cumwt[cut-1])
Rn = c(Rn, sqrt(n)*(BootMed-m))		# add centered median of current resample to ensemble for current subset
}				# end loop for resamples from current subset
t<-proc.time()-ptm  	# record time
VAR <- c(VAR, var(Rn))		# Obtain variance estimate from ensemble of resamples
mVAR <- c(mVAR, mean(VAR))	# take average across subsets so far

TIME<-c(TIME,t[3])
if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
  s.blb.act[sim] = i-1  	# record actual number of subsets completed
  print("BLB sim")
	print(sim)
break}
err<-abs(mVAR/Var.true-1)	# calculate error
} 					# end loop for subset

if (s.blb.act[sim]>0){		# write output into respective lists
cum.t.blb = c(cum.t.blb,list(cumsum(TIME)))
Var.blb = c(Var.blb, list(mVAR))
err.blb = c(err.blb, list(err))		
}	# close if (s.blb.act[sim]>0) loop


##### 3. BLB-WS #####
VAR <- NULL			# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim

for (i in 1:s.ws){	# loop for subset-resamples
ptm<-proc.time()		# start stopwatch
X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
orderts<-sort(X.sub)	# ordered subset
wt <- rep(0,b)		# freq density of subset elements in single resample
foo = sample(1:N1, size=k1,replace=TRUE)	# select blocks randomly to construct resample
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
wt <- wt/(k1*l)
cumwt<-cumsum(wt[order(X.sub)])	# cumulative freq of subset elements in single resample
index<-as.numeric(cumwt>0.5)
cut<-which.max(index)			# calculate resample quantile by linear interpolation
BootMed<-(orderts[cut]*(0.5-cumwt[cut-1])+orderts[cut-1]*(cumwt[cut]-0.5))/(cumwt[cut]-cumwt[cut-1])
Rn = c(Rn, sqrt(n)*(BootMed-median(X.sub)))		# add centered median of current resample to BLB-WS ensemble
t<-proc.time()-ptm  		# record time
VAR <- c(VAR, var(Rn))			# variance estimate from cumulative ensemble of subset-resamples
TIME<-c(TIME,t[3])
if (sum(TIME) > TimeLimit) {		# STOP if time limit is breached
  s.ws.act[sim] = i-1 			# record actual number of subsets completed
 print("BLB-WS sim")
	print(sim)
  break}

VAR[1] <- 0	# the formula calculates variance of a single term as NA
err<-abs(VAR/Var.true-1)		# calculate error
} 					# end loop for subset-resamples

if (s.ws.act[sim]>0){			# write output into respective lists
cum.t.ws = c(cum.t.ws,list(cumsum(TIME)))
Var.ws = c(Var.ws, list(VAR))
err.ws = c(err.ws, list(err))		# cumulative time taken for bootstrap iterations
}	# close if (s.ws.act[sim]>0) loop

print(sim)
} # next sim

list(cum.t.bt=cum.t.bt,err.bt=err.bt,r.bt.act=r.bt.act,
cum.t.blb=cum.t.blb,err.blb=err.blb,s.blb.act=s.blb.act,
cum.t.ws=cum.t.ws,err.ws=err.ws,s.ws.act=s.ws.act)}


#####################################################
Msimq <- function(q,q.true,n,rho,l,b,n.sim,TimeLimit){
k1 = ceiling(n/l)			# no. of blocks in resample
N = n-l+1	# number of blocks in sample
N1 = b-l+1	# number of blocks in subset

##### Declare variables #####
r.bt = 1000000	# max no. of bootstrap resamples
r.bt.act = rep(0,n.sim)       # actual no. of bootstrap resamples
cum.t.bt<-NULL	# vessel for cumulative time taken for bootstrap resamples
q.bt<- NULL	# vessel for cumulative bootstrap estimates of quantile
err.bt<-NULL		# vessel for cumulative error for bootstrap

s.blb = 1000000	# max no. of BLB subsets
s.blb.act = rep(0,n.sim)       # actual no. of BLB subsets
r.blb = 100		# no. of BLB resamples for each subset
cum.t.blb<-NULL	# vessel for cumulative time taken for BLB subsets
q.blb<- NULL	# vessel for cumulative BLB estimates of q
err.blb<-NULL		# vessel for cumulative error for BLB

s.ws = s.blb*r.blb/2	# max no. of BLB-WS subsets
s.ws.act = rep(0,n.sim)       # actual no. of BLB-WS subsets
cum.t.ws<-NULL	# vessel for cumulative time taken for BLB-WS subsets
q.ws<- NULL	# vessel for cumulative BLB-WS estimates of q
err.ws<-NULL		# vessel for cumulative error for BLB-WS

##### Run Simulations #####
for (sim in 1:n.sim) {
set.seed(1729+sim)
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")

ptm<-proc.time()	# including block mean calculation time in time taken


##### 1. MBB #####
Q <- NULL			# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
ptm<-proc.time()  	# start stopwatch
orderts<-sort(X)
m<-median(X)		# calculate this once for the ensemble
t.est<-proc.time()-ptm # time taken in calculating b.est will count for MBB
for (i in 1:r.bt){	# loop for resamples
  ptm<-proc.time()  	# start stopwatch
  wt <- rep(0,n)		# freq density of sample elements in current resample
foo = sample(1:N, size=k1,replace=TRUE)	# select blocks randomly
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N-foo[j]))}
wt <- wt/(k1*l)
cumwt<-cumsum(wt[order(X)])	# cumulative freq of subset elements in current resample
index<-as.numeric(cumwt>0.5)
cut<-which.max(index)		# calculate resample quantile by linear interpolation
BootMed<-(orderts[cut]*(0.5-cumwt[cut-1])+orderts[cut-1]*(cumwt[cut]-0.5))/(cumwt[cut]-cumwt[cut-1])
Rn = c(Rn, sqrt(n)*(BootMed-m))	# add centered median of current resample to ensemble for current subset
t<-proc.time()-ptm  			# record time
Q <- c(Q, quantile(Rn,probs=q))		# Obtain quantile from ensemble of resamples	
TIME<-c(TIME,ifelse(i==1,t[3]+t.est[3],t[3])) #estimation time for original sample goes to first resample
if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
  r.bt.act[sim] = i-1  		# record actual number of subsets completed
	print("MBB sim")
	print(sim)
  break}

err<-abs(Q/q.true-1)	# calculate error
} 				# end loop for resamples

if (r.bt.act[sim]>0){		# write output into respective lists
cum.t.bt = c(cum.t.bt,list(cumsum(TIME)))
q.bt = c(q.bt, list(Q))
err.bt = c(err.bt, list(err))		# cumulative time taken for bootstrap iterations
}	# close if (r.bt.act[sim]>0) loop

##### 2.BLB #####
mQ <- NULL			# initialize for current sim
Q <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
for (i in 1:s.blb){	# loop for subsets
Rn <- NULL			# initialize for current subset
ptm<-proc.time()		# start stopwatch
X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
orderts<-sort(X.sub)	# ordered subset
m<-median(X.sub)		# calculate this once for the ensemble
for (irep in 1:r.blb){	# loop for resamples
wt <- rep(0,b)		# freq density of subset elements in current resample
foo = sample(1:N1, size=k1,replace=TRUE)	# select blocks randomly
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
wt <- wt/(k1*l)
cumwt<-cumsum(wt[order(X.sub)])	# cumulative freq of subset elements in current resample
index<-as.numeric(cumwt>0.5)
cut<-which.max(index)			# calculate resample quantile by linear interpolation
BootMed<-(orderts[cut]*(0.5-cumwt[cut-1])+orderts[cut-1]*(cumwt[cut]-0.5))/(cumwt[cut]-cumwt[cut-1])
Rn = c(Rn, sqrt(n)*(BootMed-m))		# add centered median of current resample to ensemble for current subset
}				# end loop for resamples from current subset
t<-proc.time()-ptm  	# record time
Q <- c(Q, quantile(Rn,probs=q))		# Obtain quantile from ensemble of resamples
mQ <- c(mQ, mean(Q))	# take average across subsets so far

TIME<-c(TIME,t[3])
if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
  s.blb.act[sim] = i-1  	# record actual number of subsets completed
  print("BLB sim")
	print(sim)
break}
err<-abs(mQ/q.true-1)	# calculate error
} 					# end loop for subset

if (s.blb.act[sim]>0){		# write output into respective lists
cum.t.blb = c(cum.t.blb,list(cumsum(TIME)))
q.blb = c(q.blb, list(mQ))
err.blb = c(err.blb, list(err))		
}	# close if (s.blb.act[sim]>0) loop


##### 3. BLB-WS #####
Q <- NULL			# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim

for (i in 1:s.ws){	# loop for subset-resamples
ptm<-proc.time()		# start stopwatch
X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
orderts<-sort(X.sub)	# ordered subset
wt <- rep(0,b)		# freq density of subset elements in single resample
foo = sample(1:N1, size=k1,replace=TRUE)	# select blocks randomly to construct resample
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
wt <- wt/(k1*l)
cumwt<-cumsum(wt[order(X.sub)])	# cumulative freq of subset elements in single resample
index<-as.numeric(cumwt>0.5)
cut<-which.max(index)			# calculate resample quantile by linear interpolation
BootMed<-(orderts[cut]*(0.5-cumwt[cut-1])+orderts[cut-1]*(cumwt[cut]-0.5))/(cumwt[cut]-cumwt[cut-1])
Rn = c(Rn, sqrt(n)*(BootMed-median(X.sub)))		# add centered median of current resample to BLB-WS ensemble
t<-proc.time()-ptm  		# record time
Q <- c(Q, quantile(Rn,probs=q))			# quantile from cumulative ensemble of subset-resamples
TIME<-c(TIME,t[3])
if (sum(TIME) > TimeLimit) {		# STOP if time limit is breached
  s.ws.act[sim] = i-1 			# record actual number of subsets completed
 print("BLB-WS sim")
	print(sim)
  break}

err<-abs(Q/q.true-1)		# calculate error
} 					# end loop for subset-resamples

if (s.ws.act[sim]>0){			# write output into respective lists
cum.t.ws = c(cum.t.ws,list(cumsum(TIME)))
q.ws = c(q.ws, list(Q))
err.ws = c(err.ws, list(err))		# cumulative time taken for bootstrap iterations
}	# close if (s.ws.act[sim]>0) loop

print(sim)
} # next sim

list(cum.t.bt=cum.t.bt,err.bt=err.bt,r.bt.act=r.bt.act,
cum.t.blb=cum.t.blb,err.blb=err.blb,s.blb.act=s.blb.act,
cum.t.ws=cum.t.ws,err.ws=err.ws,s.ws.act=s.ws.act)}