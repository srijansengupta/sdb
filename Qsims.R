############### 12-17-2014 ###############
# 1. l=10,20,50; rho = -0.8; q=95%
rm(list=ls())
# true quantile: read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_Msimsrho1q1_1027.RData")
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q1_1217.RData")


# 2. l=10,20,50; rho = -0.8; q=5%
rm(list=ls())
# : true quantile: read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_Msimsrho1q2_1027.RData")
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150  # for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q2_1217.RData")


# 3. l=10,20,50; rho = 0.5; q=95%
rm(list=ls())
# true quantile: read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_Msimsrho2q1_1101.RData")
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150  # for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q1_1217.RData")


# 4. l=10,20,50; rho = 0.5; q=5%
rm(list=ls())
# true quantile: read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_Msimsrho2q2_1102.RData")
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150  # for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q2_1217.RData")


# 5. l=10,20,50; rho = 0.9; q=95%
rm(list=ls())
# true quantile: read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_Msimsrho3q1_1102.RData")
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150  # for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q1_1217.RData")


# 6. l=10,20,50; rho = 0.9; q=5%
rm(list=ls())
# true quantile: read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_Msimsrho3q2_1103.RData")
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150  # for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q2_1217.RData")






































############### 11-03-2014 ###############
# 6. l=10,20,50; rho = 0.9; q=5%
rm(list=ls())
##### true quantile #####
n = 100000
rho = 0.9
nsim = 10000	
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.05
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q2_1103.RData")




############### 11-02-2014 ###############
# 5. l=10,20,50; rho = 0.9; q=95%
rm(list=ls())
##### true quantile #####
n = 100000
rho = 0.9
nsim = 10000	
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.95
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q1_1102.RData")




# 4. l=10,20,50; rho = 0.5; q=5%
rm(list=ls())
##### true quantile #####
n = 100000
rho = 0.5
nsim = 10000	
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.05
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q2_1102.RData")


############### 11-01-2014 ###############

# 3. l=10,20,50; rho = 0.5; q=95%
rm(list=ls())
##### true quantile #####
n = 100000
rho = 0.5
nsim = 10000	
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.95
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q1_1101.RData")






############### 10-27-2014 ###############

# 1. l=10,20,50; rho = -0.8; q=95%
rm(list=ls())
##### true quantile #####
n = 100000
rho = -0.8
nsim = 10000	
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.95
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q1_1027.RData")

# 2. l=10,20,50; rho = -0.8; q=5%
rm(list=ls())
##### true quantile #####
n = 100000
rho = -0.8
nsim = 10000	
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
E<-rnorm(n) 		# AR(1) data generation
X<-filter(E, rho, method = "recursive")
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.05
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q2_1027.RData")




















################ 10-02 ####################
# 6. l=10,20,50; rho = 0.9; q=0.05
rm(list=ls())
##### true quantile #####
rho = 0.9
q = 0.05
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q2_0210.RData")




#  5. l=10,20,50; rho = 0.9; q=0.95
rm(list=ls())
##### true quantile #####
rho = 0.9
q = 0.95
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q1_0210.RData")





# 4. l=10,20,50; rho = 0.5; q=0.05
rm(list=ls())
##### true quantile #####
rho = 0.5
q = 0.05
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q2_0210.RData")



# 3. l=10,20,50; rho = 0.5; q=0.95
rm(list=ls())
##### true quantile #####
rho = 0.5
q = 0.95
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q1_0210.RData")



# 2. l=10,20,50; rho = -0.8; q=5%
rm(list=ls())
##### true quantile #####
rho = -0.8
q = 0.05
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q2_0210.RData")


# 1. l=10,20,50; rho = -0.8; q=95%
rm(list=ls())
##### true quantile #####
rho = -0.8
q = 0.95
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 150	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q1_0210.RData")






































################ 09-27 ####################
# Server: 6. l=10,20,50; rho = 0.9; q=0.05
rm(list=ls())
##### true quantile #####
rho = 0.9
q = 0.05
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("/home/ssengpt2/BLB")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 120	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q2_2709.RData")




# Server: 5. l=10,20,50; rho = 0.9; q=0.95
rm(list=ls())
##### true quantile #####
rho = 0.9
q = 0.95
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("/home/ssengpt2/BLB")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 120	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho3q1_2709.RData")





# Server: 4. l=10,20,50; rho = 0.5; q=0.05
rm(list=ls())
##### true quantile #####
rho = 0.5
q = 0.05
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("/home/ssengpt2/BLB")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 120	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q2_2709.RData")



# Server: 3. l=10,20,50; rho = 0.5; q=0.95
rm(list=ls())
##### true quantile #####
rho = 0.5
q = 0.95
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("/home/ssengpt2/BLB")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 120	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho2q1_2709.RData")



# Server: 2. l=10,20,50; rho = -0.8; q=5%
rm(list=ls())
##### true quantile #####
rho = -0.8
q = 0.05
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("/home/ssengpt2/BLB")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 120	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q2_2709.RData")


# Server: 1. l=10,20,50; rho = -0.8; q=95%
rm(list=ls())
##### true quantile #####
rho = -0.8
q = 0.95
n = 100000
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("/home/ssengpt2/BLB")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
TimeLimit2 = 120	# for l=10 BLB may not complete any subset by 60 secs
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit2)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
sim12 = Msimq(q,q.true,n,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit2)
sim22 = Msimq(q,q.true,n,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit)
sim32 = Msimq(q,q.true,n,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsrho1q1_2709.RData")


################ 09-24 ####################
rm(list=ls())
##### true quantile #####
n = 100000
rho = 0.8
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
q = 0.95
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)

rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
l1 = 20
l2 = 50
l3 = 100
b1 = 5000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsq1_2409.RData")


################ 09-24 ####################
rm(list=ls())
##### true quantile #####
n = 100000
rho = 0.8
burn = 1000
nsim = 10000	# 10000 sims ~ 6 mins
q = 0.05
M = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
X<-rnorm(n+burn) 
X[-1]<-X[-1]+rho*X[-n]	# AR(1) 
X<-X[burn + 1:n]
M[sim] = sqrt(n)*median(X)
if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q.true = quantile(M,probs=q)

rm(list=setdiff(ls(),c("q","q.true","n","rho")))
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_Mfun.R")
n.sim = 20
TimeLimit = 60
l1 = 20
l2 = 50
l3 = 100
b1 = 5000
sim11 = Msimq(q,q.true,n,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit)
sim21 = Msimq(q,q.true,n,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit)
sim31 = Msimq(q,q.true,n,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit)
save.image("ts_Msimsq2_2409.RData")
