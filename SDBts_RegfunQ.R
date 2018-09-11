Regsimq <- function(q,q.true,n,d, rho,l,b,n.sim,TimeLimit){
k1 = ceiling(n/l)			# no. of blocks in resample
N = n-l+1	# number of blocks in sample
N1 = b-l+1	# number of blocks in subset

##### Declare variables #####
r.bt = 1000000	# max no. of bootstrap resamples
r.bt.act = rep(0,n.sim)       # actual no. of bootstrap resamples
cum.t.bt<-NULL	# vessel for cumulative time taken for bootstrap resamples
q.bt<- NULL	# vessel for cumulative bootstrap estimates of Var
err.bt<-NULL		# vessel for cumulative error for bootstrap

s.blb = 10000	# max no. of BLB subsets
s.blb.act = rep(0,n.sim)       # actual no. of BLB subsets
r.blb = 100		# no. of BLB resamples for each subset
cum.t.blb<-NULL	# vessel for cumulative time taken for BLB subsets
q.blb<- NULL	# vessel for cumulative means of BLB estimates of Var
err.blb<-NULL		# vessel for cumulative error for BLB

s.ws = s.blb*r.blb/2	# max no. of BLB-WS subsets
s.ws.act = rep(0,n.sim)       # actual no. of BLB-WS subsets
cum.t.ws<-NULL	# vessel for cumulative time taken for BLB-WS subsets
q.ws<- NULL	# vessel for cumulative BLB-WS estimates of Var
err.ws<-NULL		# vessel for cumulative error for BLB-WS

##### Run Simulations #####
for (sim in 1:n.sim) {
set.seed(1729+sim)
E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2)))   	# AR(1) data generation
U<-filter(E, rho, method = "recursive")       # HOM AR(1) errors
X<-matrix(0,n,d)    # n=sample size, d =dim
#X[,1] <- 1          # intercept
for (j in 1:d) {  E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2)))   	# AR(1) data generation
                  foo<-filter(E, rho, method = "recursive")       # AR(1) regressors
                  X[,j] = foo}
y<-matrix(U,n,1)                              # yt = Xt'b + ut where b = 0

##### 1. MBB #####
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
ptm<-proc.time()  # time taken in calculating b.est will count for MBB
bhat <- coef(lm(y~X))[1+1:d]    # calculate bhat calculate this once for the ensemble
t.est<-proc.time()-ptm

for (i in 1:r.bt){	# loop for resamples
ptm<-proc.time()		# start stopwatch
wt <- rep(0,n)		  # vessel for frequency in current resample
foo = sample(1:N, size=k1,replace=TRUE)	# select blocks randomly
for (j in 1:k1){    # frequency of sample elements in current resample
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N-foo[j]))}
fit<-lm(y~X,weights=wt)
bhat.bt <- fit$coef[1+1:d]
num <- sum(wt*(X%*%(bhat.bt-bhat))^2)/d  	# MSM
den <- sum(wt*(fit$res)^2)/(n-d-1)		# MSE
Rn = c(Rn, (num/den))	# add Rn from current resample to ensemble
t<-proc.time()-ptm				# record time for current resample
Q <- c(Q, quantile(Rn,probs=q))    # Obtain quantile from ensemble of resamples
TIME<-c(TIME,ifelse(i==1,t[3]+t.est[3],t[3])) #estimation time for original sample goes to first resample
if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
  r.bt.act[sim] = i-1  		# record actual number of subsets completed
	print("MBB sim")
	print(sim)
  break}
       err<-abs(Q/q.true-1)  # calculate error
} 				# end loop for resamples
if (r.bt.act[sim]>0){		# write output into respective lists
cum.t.bt = c(cum.t.bt,list(cumsum(TIME)))
q.bt = c(q.bt, list(Q))
err.bt = c(err.bt, list(err))		# cumulative time taken for bootstrap iterations
}	# close if (r.bt.act[sim]>0) loop

##### 2.BLB #####
mQ <- NULL  		# initialize for current sim
Q <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
for (i in 1:s.blb){	# loop for subsets
Rn <- NULL			# initialize for current subset
ptm<-proc.time()		# start stopwatch
sub <- sample(1:(n-b+1),1)+0:(b-1)  # select subset as continuous stretch
X.sub <- X[sub,]
y.sub <- y[sub,]
fit<-lm(y.sub~X.sub)
bhat.sub <- fit$coef[1+1:d] # calculate bhat for subset
for (irep in 1:r.blb){	# loop for resamples
wt <- rep(0,b)		# freq density of subset elements in current resample
foo = sample(1:N1, size=k1,replace=TRUE)	# select blocks randomly
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
fit<-lm(y.sub~X.sub,weights=wt)
bhat.blb <- fit$coef[1+1:d]
num <- sum(wt*(X.sub%*%(bhat.sub-bhat.blb))^2)/d  # weighted MSM
den <- sum(wt*(fit$res)^2)/(n-d-1)		# weighted MSE
Rn = c(Rn, (num/den))  # add Rn from current resample to ensemble
}				# end loop for resamples from current subset
t<-proc.time()-ptm  	# record time
Q <- c(Q, quantile(Rn,probs=q))  	# Obtain quantile from ensemble of resamples
mQ <- c(mQ, mean(Q))	# take average across subsets so far
TIME<-c(TIME,t[3])
if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
  s.blb.act[sim] = i-1  	# record actual number of subsets completed
  print("BLB sim")
	print(sim)
break}
err<-abs(mQ/q.true-1)  # calculate error
} 					# end loop for subset
if (s.blb.act[sim]>0){		# write output into respective lists
cum.t.blb = c(cum.t.blb,list(cumsum(TIME)))
q.blb = c(q.blb, list(mQ))
err.blb = c(err.blb, list(err))		
}	# close if (s.blb.act[sim]>0) loop

##### 3. BLB-WS #####
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
 for (i in 1:s.ws){	# loop for subset-resamples
ptm<-proc.time()		# start stopwatch
sub <- sample(1:(n-b+1),1)+0:(b-1)  # select subset as continuous stretch
X.sub <- X[sub,]
y.sub <- y[sub,]
fit<-lm(y.sub~X.sub)
bhat.sub <- fit$coef[1+1:d] # calculate bhat for subset
wt <- rep(0,b)		# freq density of subset elements in single resample
foo = sample(1:N1, size=k1,replace=TRUE)	# select blocks randomly to construct resample
for (j in 1:k1){
wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
fit<-lm(y.sub~X.sub,weights=wt)
bhat.ws <- fit$coef[1+1:d]
num <- sum(wt*(X.sub%*%(bhat.sub-bhat.ws))^2)/d  # weighted MSM
den <- sum(wt*(fit$res)^2)/(n-d-1)  	# weighted MSE
Rn = c(Rn, (num/den))  # add Rn from current resample to ensemble
t<-proc.time()-ptm  		# record time
Q <- c(Q, quantile(Rn,probs=q))  		# quantile from cumulative ensemble of subset-resamples
TIME<-c(TIME,t[3])
if (sum(TIME) > TimeLimit) {		# STOP if time limit is breached
  s.ws.act[sim] = i-1 			# record actual number of subsets completed
 print("BLB-WS sim")
	print(sim)
  break}
err<-abs(Q/q.true-1)  	# calculate error
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