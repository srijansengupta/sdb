rm(list=ls())
data <- read.table("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R/Dataset/CentralEnglandTemperatures.txt", quote="\"")
dim(data) # 14 columns: col 1 for year, col2 for date, col3-14 for 12 months
sum(data==-999) # missing values

yr <- 228
day <- c(31,28,31,30,31,30,31,31,30,31,30,31) # days on each month
day.start <- cumsum(day)-day  # day at the start of each month
n <- 228*365
X <- rep(NA, n)
for (i in 1:yr){
  for (month in 1:12){
    foo1 <- (i-1)*365+day.start[month]+1:day[month]  # cells of X for current yr and month
    foo2 <- (i-1)*31+1:day[month]                    # rows of data for current yr
    X[foo1]  <- data[foo2,month+2]                   # populate X from data
  }
 }
length(X)
sum(X==-999) # missing values
sum(X)
max(X);min(X)

daily.avg <- rep(NA, n)
for (j in 1:365){
  foo3 <- j + 365*(0:(yr-1))
  daily.avg[foo3] <- mean(X[foo3])
  }

X <- X - daily.avg # remove seasonality
#cor(X[-length(X)],X[-1])
save.image("temp_data_proc.RData")



##### CI for population mean #####
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 600

##### 1. MBB
r.bt <- 1000
r.bt.act <- 0
n <- length(X)
l <- 50
Rn <- NULL
Q <- NULL
TIME <- NULL
N <- n-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
block.mean <- rep(NA,N)
ptm<-proc.time()
m<-mean(X)    # calculate this once for the ensemble
t<-proc.time()-ptm  # estimation time for original sample will count towards MBB runtime
TIME<-c(TIME,t[3])
Q<-c(Q,0)     # we do not have any estimate yet, so use 0 as a dummy value
for (i in 1:r.bt){  # loop for resamples
  ptm<-proc.time()		# start stopwatch
  wt <- rep(0,n)		# freq density of sample elements in current resample
  foo = sample(1:N, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X)-m))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  print("MBB"); print(i)
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    r.bt.act= i-1  		# record actual number of subsets completed
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])  	# Obtain quantile from ensemble of resamples	
} 				# end loop for resamples

if (r.bt.act>0){		# write output into respective lists
  cum.t.bt = list(cumsum(TIME))
  q.bt = list(Q)
}	# close if (r.bt.act[sim]>0) loop
save.image("CET_MBBT600L50_1221.RData")
plot(q.bt[[1]])
foo <- ceiling(r.bt.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
r.bt.act;Q[length(Q)]

##### CI for population mean #####
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 600

##### 1. MBB
r.bt <- 1000
r.bt.act <- 0
n <- length(X)
l <- 20
Rn <- NULL
Q <- NULL
TIME <- NULL
N <- n-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
block.mean <- rep(NA,N)
ptm<-proc.time()
m<-mean(X)    # calculate this once for the ensemble
t<-proc.time()-ptm # estimation time for original sample will count towards MBB runtime
TIME<-c(TIME,t[3])
Q<-c(Q,0)     # we do not have any estimate yet, so use 0 as a dummy value
for (i in 1:r.bt){  # loop for resamples
  ptm<-proc.time()		# start stopwatch
  wt <- rep(0,n)		# freq density of sample elements in current resample
  foo = sample(1:N, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X)-m))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  print("MBB"); print(i)
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    r.bt.act= i-1  		# record actual number of subsets completed
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])  	# Obtain quantile from ensemble of resamples	
} 				# end loop for resamples

if (r.bt.act>0){		# write output into respective lists
  cum.t.bt = list(cumsum(TIME))
  q.bt = list(Q)
}	# close if (r.bt.act[sim]>0) loop
save.image("CET_MBBT600L20_1221.RData")
plot(q.bt[[1]])
foo <- ceiling(r.bt.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
r.bt.act;Q[length(Q)]


##### CI for population mean #####
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 600

##### 1. MBB
r.bt <- 1000
r.bt.act <- 0
n <- length(X)
l <- 10
Rn <- NULL
Q <- NULL
TIME <- NULL
N <- n-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
block.mean <- rep(NA,N)
ptm<-proc.time()
m<-mean(X)    # calculate this once for the ensemble
t<-proc.time()-ptm # estimation time for original sample will count towards MBB runtime
TIME<-c(TIME,t[3])
Q<-c(Q,0)     # we do not have any estimate yet, so use 0 as a dummy value
for (i in 1:r.bt){  # loop for resamples
  ptm<-proc.time()  	# start stopwatch
  wt <- rep(0,n)		# freq density of sample elements in current resample
  foo = sample(1:N, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X)-m))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  print("MBB"); print(i)
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    r.bt.act= i-1  		# record actual number of subsets completed
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])  	# Obtain quantile from ensemble of resamples	
} 				# end loop for resamples

if (r.bt.act>0){		# write output into respective lists
  cum.t.bt = list(cumsum(TIME))
  q.bt = list(Q)
}	# close if (r.bt.act[sim]>0) loop
save.image("CET_MBBT600L10_1221.RData")
plot(q.bt[[1]])
foo <- ceiling(r.bt.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
r.bt.act;Q[length(Q)]







##### 2. BLB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 10000
r.blb <- 100
s.blb <- 1000
s.blb.act <- 0
l <- 50
Q <- NULL
mQ <- NULL  		
TIME <- NULL
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.blb){  # loop for subsets
  Rn <- NULL			# initialize for current subset
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  m<-mean(X.sub)		# calculate this once for the ensemble
  for (irep in 1:r.blb){	# loop for resamples
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-m))
  }				# end loop for resamples from current subset
  t<-proc.time()-ptm  	# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    s.blb.act = i-1  	# record actual number of subsets completed
    print("BLB"); print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  mQ <- c(mQ, mean(Q))	# take average across subsets so far
  
} 					# end loop for subsets

if (s.blb.act>0){		# write output into respective lists
  cum.t.blb = list(cumsum(TIME))
  q.blb = list(mQ)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_BLBT300L50B10000_1221.RData")
plot(q.blb[[1]])
foo <- ceiling(s.blb.act/5)
plot(mQ[length(mQ)-foo + 1:foo]/mQ[length(mQ)])
s.blb.act;mQ[length(mQ)]


##### 2. BLB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 10000
r.blb <- 100
s.blb <- 1000
s.blb.act <- 0
l <- 20
Q <- NULL
mQ <- NULL    	
TIME <- NULL
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.blb){  # loop for subsets
  Rn <- NULL			# initialize for current subset
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  m<-mean(X.sub)		# calculate this once for the ensemble
  for (irep in 1:r.blb){	# loop for resamples
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-m))
  }				# end loop for resamples from current subset
  t<-proc.time()-ptm  	# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    s.blb.act = i-1  	# record actual number of subsets completed
    print("BLB"); print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  mQ <- c(mQ, mean(Q))	# take average across subsets so far
  
} 					# end loop for subsets

if (s.blb.act>0){		# write output into respective lists
  cum.t.blb = list(cumsum(TIME))
  q.blb = list(mQ)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_BLBT300L20B10000_1221.RData")
plot(q.blb[[1]])
foo <- ceiling(s.blb.act/5)
plot(mQ[length(mQ)-foo + 1:foo]/mQ[length(mQ)])
s.blb.act;mQ[length(mQ)]



##### 2. BLB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 10000
r.blb <- 100
s.blb <- 1000
s.blb.act <- 0
l <- 10
Q <- NULL
mQ <- NULL    	
TIME <- NULL
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.blb){  # loop for subsets
  Rn <- NULL			# initialize for current subset
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  m<-mean(X.sub)		# calculate this once for the ensemble
  for (irep in 1:r.blb){	# loop for resamples
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-m))
  }				# end loop for resamples from current subset
  t<-proc.time()-ptm  	# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    s.blb.act = i-1  	# record actual number of subsets completed
    print("BLB"); print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  mQ <- c(mQ, mean(Q))	# take average across subsets so far
  
} 					# end loop for subsets

if (s.blb.act>0){		# write output into respective lists
  cum.t.blb = list(cumsum(TIME))
  q.blb = list(mQ)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_BLBT300L10B10000_1221.RData")
plot(q.blb[[1]])
foo <- ceiling(s.blb.act/5)
plot(mQ[length(mQ)-foo + 1:foo]/mQ[length(mQ)])
s.blb.act;mQ[length(mQ)]







##### 2. BLB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 5000
r.blb <- 100
s.blb <- 1000
s.blb.act <- 0
l <- 50
Q <- NULL
mQ <- NULL    	
TIME <- NULL
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.blb){  # loop for subsets
  Rn <- NULL			# initialize for current subset
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  m<-mean(X.sub)		# calculate this once for the ensemble
  for (irep in 1:r.blb){	# loop for resamples
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-m))
  }				# end loop for resamples from current subset
  t<-proc.time()-ptm  	# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    s.blb.act = i-1  	# record actual number of subsets completed
    print("BLB"); print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  mQ <- c(mQ, mean(Q))	# take average across subsets so far
  
} 					# end loop for subsets

if (s.blb.act>0){		# write output into respective lists
  cum.t.blb = list(cumsum(TIME))
  q.blb = list(mQ)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_BLBT300L50B5000_1221.RData")
plot(q.blb[[1]])
foo <- ceiling(s.blb.act/5)
plot(mQ[length(mQ)-foo + 1:foo]/mQ[length(mQ)])
s.blb.act;mQ[length(mQ)]


##### 2. BLB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 5000
r.blb <- 100
s.blb <- 1000
s.blb.act <- 0
l <- 20
Q <- NULL
mQ <- NULL    	
TIME <- NULL
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.blb){  # loop for subsets
  Rn <- NULL			# initialize for current subset
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  m<-mean(X.sub)		# calculate this once for the ensemble
  for (irep in 1:r.blb){	# loop for resamples
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-m))
  }				# end loop for resamples from current subset
  t<-proc.time()-ptm  	# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    s.blb.act = i-1  	# record actual number of subsets completed
    print("BLB"); print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  mQ <- c(mQ, mean(Q))	# take average across subsets so far
  
} 					# end loop for subsets

if (s.blb.act>0){		# write output into respective lists
  cum.t.blb = list(cumsum(TIME))
  q.blb = list(mQ)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_BLBT300L20B5000_1221.RData")
plot(q.blb[[1]])
foo <- ceiling(s.blb.act/5)
plot(mQ[length(mQ)-foo + 1:foo]/mQ[length(mQ)])
s.blb.act;mQ[length(mQ)]



##### 2. BLB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 5000
r.blb <- 100
s.blb <- 1000
s.blb.act <- 0
l <- 10
Q <- NULL
mQ <- NULL    	
TIME <- NULL
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.blb){  # loop for subsets
  Rn <- NULL			# initialize for current subset
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  m<-mean(X.sub)		# calculate this once for the ensemble
  for (irep in 1:r.blb){	# loop for resamples
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-m))
  }				# end loop for resamples from current subset
  t<-proc.time()-ptm  	# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {	# STOP if time limit is breached
    s.blb.act = i-1  	# record actual number of subsets completed
    print("BLB"); print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  mQ <- c(mQ, mean(Q))	# take average across subsets so far
  
} 					# end loop for subsets

if (s.blb.act>0){		# write output into respective lists
  cum.t.blb = list(cumsum(TIME))
  q.blb = list(mQ)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_BLBT300L10B5000_1221.RData")
plot(q.blb[[1]])
foo <- ceiling(s.blb.act/5)
plot(mQ[length(mQ)-foo + 1:foo]/mQ[length(mQ)])
s.blb.act;mQ[length(mQ)]









##### 3. SDB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 5000
s.ws <- 100000
s.ws.act <- 0
n <- length(X)
l <- 50
Q <- NULL    	# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.ws){  # loop for subsets
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  wt <- rep(0,b)		# freq density of subset elements in current resample
  foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-mean(X.sub)))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {  	# STOP if time limit is breached
    s.ws.act = i-1 			# record actual number of subsets completed
    print("BLB-WS");print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples	
}				# end loop for subset-resamples

if (s.ws.act>0){  	# write output into respective lists
  cum.t.ws = list(cumsum(TIME))
  q.ws = list(Q)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_SDBT300L50B5000_1221.RData")
plot(q.ws[[1]])
foo <- ceiling(s.ws.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
s.ws.act;Q[length(Q)]

##### 3. SDB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 5000
s.ws <- 100000
s.ws.act <- 0
n <- length(X)
l <- 20
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.ws){  # loop for subsets
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  wt <- rep(0,b)		# freq density of subset elements in current resample
  foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-mean(X.sub)))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {  	# STOP if time limit is breached
    s.ws.act = i-1 			# record actual number of subsets completed
    print("BLB-WS");print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
}				# end loop for subset-resamples

if (s.ws.act>0){  	# write output into respective lists
  cum.t.ws = list(cumsum(TIME))
  q.ws = list(Q)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_SDBT300L20B5000_1221.RData")
plot(q.ws[[1]])
foo <- ceiling(s.ws.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
s.ws.act;Q[length(Q)]

##### 3. SDB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 5000
s.ws <- 100000
s.ws.act <- 0
n <- length(X)
l <- 10
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.ws){  # loop for subsets
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  wt <- rep(0,b)		# freq density of subset elements in current resample
  foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-mean(X.sub)))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {  	# STOP if time limit is breached
    s.ws.act = i-1 			# record actual number of subsets completed
    print("BLB-WS");print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples	
}				# end loop for subset-resamples

if (s.ws.act>0){  	# write output into respective lists
  cum.t.ws = list(cumsum(TIME))
  q.ws = list(Q)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_SDBT300L10B5000_1221.RData")
plot(q.ws[[1]])
foo <- ceiling(s.ws.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
s.ws.act;Q[length(Q)]










##### 3. SDB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 10000
s.ws <- 100000
s.ws.act <- 0
n <- length(X)
l <- 50
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.ws){  # loop for subsets
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  wt <- rep(0,b)		# freq density of subset elements in current resample
  foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-mean(X.sub)))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {  	# STOP if time limit is breached
    s.ws.act = i-1 			# record actual number of subsets completed
    print("BLB-WS");print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples	
}				# end loop for subset-resamples

if (s.ws.act>0){  	# write output into respective lists
  cum.t.ws = list(cumsum(TIME))
  q.ws = list(Q)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_SDBT300L50B10000_1221.RData")
plot(q.ws[[1]])
foo <- ceiling(s.ws.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
s.ws.act;Q[length(Q)]

##### 3. SDB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 10000
s.ws <- 100000
s.ws.act <- 0
n <- length(X)
l <- 20
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.ws){  # loop for subsets
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
    wt <- rep(0,b)		# freq density of subset elements in current resample
    foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
    for (j in 1:k){
      wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
    if (k>k1) {   # trim out the extra part of the last block
      wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
    wt <- wt/n
    Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-mean(X.sub)))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {  	# STOP if time limit is breached
    s.ws.act = i-1 			# record actual number of subsets completed
    print("BLB-WS");print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples  
  }				# end loop for subset-resamples
  
if (s.ws.act>0){  	# write output into respective lists
  cum.t.ws = list(cumsum(TIME))
  q.ws = list(Q)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_SDBT300L20B10000_1221.RData")
plot(q.ws[[1]])
foo <- ceiling(s.ws.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
s.ws.act;Q[length(Q)]

##### 3. SDB
rm(list=ls())
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("Dataset/temp_data_proc.RData")
rm(list=setdiff(ls(),c("X")))
q = c(0.05, 0.95)
TimeLimit <- 300
n <- length(X)
b <- 10000
s.ws <- 100000
s.ws.act <- 0
n <- length(X)
l <- 10
Q <- NULL  		# initialize for current sim
Rn <- NULL			# initialize for current sim
TIME <- NULL		# initialize for current sim
N1 <- b-l+1
k <- ceiling(n/l)    # what to do when this non-integer?
k1 <- (n/l)
for (i in 1:s.ws){  # loop for subsets
  ptm<-proc.time()		# start stopwatch
  X.sub = X[sample(1:(n-b+1),1)+0:(b-1)]	# select subset as continuous stretch
  wt <- rep(0,b)		# freq density of subset elements in current resample
  foo = sample(1:N1, size=k,replace=TRUE)	# select blocks randomly
  for (j in 1:k){
    wt <- wt + c(rep(0,foo[j]-1),rep(1,l),rep(0,N1-foo[j]))}
  if (k>k1) {   # trim out the extra part of the last block
    wt<-wt-c(rep(0,foo[k]-1+n%%l),rep(1,l-n%%l),rep(0,N1-foo[j]))}
  wt <- wt/n
  Rn <- c(Rn, sqrt(n)*(sum(wt*X.sub)-mean(X.sub)))
  t<-proc.time()-ptm  			# record time
  TIME<-c(TIME,t[3])
  if (sum(TIME) > TimeLimit) {  	# STOP if time limit is breached
    s.ws.act = i-1 			# record actual number of subsets completed
    print("BLB-WS");print(i)
    break}
  foo <- quantile(Rn,probs=q)
  Q <- c(Q, foo[2]-foo[1])    # Obtain quantile from ensemble of resamples	
}				# end loop for subset-resamples

if (s.ws.act>0){  	# write output into respective lists
  cum.t.ws = list(cumsum(TIME))
  q.ws = list(Q)
}	# close if (s.blb.act[sim]>0) loop
save.image("CET_SDBT300L10B10000_1221.RData")
plot(q.ws[[1]])
foo <- ceiling(s.ws.act/5)
plot(Q[length(Q)-foo + 1:foo]/Q[length(Q)])
s.ws.act;Q[length(Q)]