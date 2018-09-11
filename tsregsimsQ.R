################ 12-16 ####################
# Do all runs new from the same machine

#1. Whole set l = 10, 20,50 for rho = -0.8
rm(list=ls())
##### true variance #####
# read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_regsimqrho1_1210.RData")  
rm(list=setdiff(ls(),c("q","q.true","n","d","rho")))

setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
source("BLBts_RegfunQ.R")
n.sim = 20
TimeLimit1 = 150  # for l=10
TimeLimit2 = 90	  # for l=20, BLB might have 0 iterations at t=60
TimeLimit3 = 60   # for l=50
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Regsimq(q,q.true,n,d,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit1)
sim21 = Regsimq(q,q.true,n,d,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit2)  
sim31 = Regsimq(q,q.true,n,d,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit3)  
sim12 = Regsimq(q,q.true,n,d,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit1)
sim22 = Regsimq(q,q.true,n,d,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit2)
sim32 = Regsimq(q,q.true,n,d,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit3)
save.image("ts_regsimqrho1_1216.RData")	

#2. Whole set l = 10, 20,50 for rho = 0.5
rm(list=ls())
##### true variance #####
# read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_regsimqrho2_1210.RData")  
rm(list=setdiff(ls(),c("q","q.true","n","d","rho")))

setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
source("BLBts_RegfunQ.R")
n.sim = 20
TimeLimit1 = 150  # for l=10
TimeLimit2 = 90	  # for l=20, BLB might have 0 iterations at t=60
TimeLimit3 = 60   # for l=50
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Regsimq(q,q.true,n,d,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit1)
sim21 = Regsimq(q,q.true,n,d,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit2)  
sim31 = Regsimq(q,q.true,n,d,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit3)  
sim12 = Regsimq(q,q.true,n,d,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit1)
sim22 = Regsimq(q,q.true,n,d,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit2)
sim32 = Regsimq(q,q.true,n,d,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit3)
save.image("ts_regsimqrho2_1216.RData")	


#3. Whole set l = 10, 20,50 for rho = 0.9
rm(list=ls())
##### true variance #####
# read from older run
setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
load("ts_regsimqrho3_1210.RData")  
rm(list=setdiff(ls(),c("q","q.true","n","d","rho")))

setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
source("BLBts_RegfunQ.R")
n.sim = 20
TimeLimit1 = 150  # for l=10
TimeLimit2 = 90    # for l=20, BLB might have 0 iterations at t=60
TimeLimit3 = 60   # for l=50
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Regsimq(q,q.true,n,d,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit1)
sim21 = Regsimq(q,q.true,n,d,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit2)  
sim31 = Regsimq(q,q.true,n,d,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit3)  
sim12 = Regsimq(q,q.true,n,d,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit1)
sim22 = Regsimq(q,q.true,n,d,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit2)
sim32 = Regsimq(q,q.true,n,d,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit3)
save.image("ts_regsimqrho3_1216.RData")	


































################ 12-10 ####################
# Do all runs new from the same machine

#1. Whole set l = 10, 20,50 for rho = -0.8
rm(list=ls())
##### true variance #####
n = 100000
d = 10
X<-matrix(0,n,d)    # n=sample size, d =dim
#X[,1] <- 1  # intercept
rho = -0.8
nsim = 10000
R = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
  E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2)))   	# AR(1) data generation
  U<-filter(E, rho, method = "recursive")       # HOM AR(1) errors
  for (j in 1:d) {  E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2)))   	# AR(1) data generation
                    foo<-filter(E, rho, method = "recursive")       # AR(1) regressors
                    X[,j] = foo}
  y<-matrix(U,n,1)                              # yt = Xt'b + ut where b = 0
  fit<-lm(y~X)
  bhat <- fit$coef[1+1:d]
  num <- sum((X%*%(bhat))^2)/d  # MSM
  den <- sum((fit$res)^2)/(n-d-1)    # MSE
  R[sim] = (num/den)                     # Root function
  if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.95
q.true = quantile(R,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","d","rho")))

setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
source("BLBts_RegfunQ.R")
n.sim = 20
TimeLimit1 = 150  # for l=10
TimeLimit2 = 90	  # for l=20, BLB might have 0 iterations at t=60
TimeLimit3 = 60   # for l=50
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Regsimq(q,q.true,n,d,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit1)
sim21 = Regsimq(q,q.true,n,d,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit2)  
sim31 = Regsimq(q,q.true,n,d,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit3)  
sim12 = Regsimq(q,q.true,n,d,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit1)
sim22 = Regsimq(q,q.true,n,d,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit2)
sim32 = Regsimq(q,q.true,n,d,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit3)
save.image("ts_regsimqrho1_1210.RData")	

#2. Whole set l = 10, 20,50 for rho = 0.5
rm(list=ls())
##### true variance #####
n = 100000
d = 10
X<-matrix(0,n,d)    # n=sample size, d =dim
#X[,1] <- 1  # intercept
rho = 0.5
nsim = 10000
R = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
  E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2))) 		# AR(1) data generation
  U<-filter(E, rho, method = "recursive")       # HOM AR(1) errors
  for (j in 1:d) {  E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2)))   	# AR(1) data generation
                    foo<-filter(E, rho, method = "recursive")       # AR(1) regressors
                    X[,j] = foo}
  y<-matrix(U,n,1)                              # yt = Xt'b + ut where b = 0
  fit<-lm(y~X)
  bhat <- fit$coef[1+1:d]
  num <- sum((X%*%(bhat))^2)/d  # MSM
  den <- sum((fit$res)^2)/(n-d-1)    # MSE
  R[sim] = (num/den)                     # Root function
  if (sim %% 100 == 0) {print(sim)}
}
proc.time()-ptm
q = 0.95
q.true = quantile(R,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","d","rho")))

setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
source("BLBts_RegfunQ.R")
n.sim = 20
TimeLimit1 = 150  # for l=10
TimeLimit2 = 90	  # for l=20, BLB might have 0 iterations at t=60
TimeLimit3 = 60   # for l=50
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Regsimq(q,q.true,n,d,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit1)
sim21 = Regsimq(q,q.true,n,d,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit2)  
sim31 = Regsimq(q,q.true,n,d,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit3)  
sim12 = Regsimq(q,q.true,n,d,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit1)
sim22 = Regsimq(q,q.true,n,d,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit2)
sim32 = Regsimq(q,q.true,n,d,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit3)
save.image("ts_regsimqrho2_1210.RData")	


#3. Whole set l = 10, 20,50 for rho = 0.9
rm(list=ls())
##### true variance #####
n = 100000
d = 10
X<-matrix(0,n,d)  	# n=sample size, d =dim
#X[,1] <- 1  # intercept
rho = 0.9
nsim = 10000
R = rep(NA, nsim)
ptm<-proc.time()
for (sim in 1:nsim){
    E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2))) 		# AR(1) data generation
    U<-filter(E, rho, method = "recursive")       # HOM AR(1) errors
    for (j in 1:d) {  E<-rnorm(n,mean = 0, sd = sqrt((1-rho^2)))   	# AR(1) data generation
                    foo<-filter(E, rho, method = "recursive")       # AR(1) regressors
                    X[,j] = foo}
    y<-matrix(U,n,1)                              # yt = Xt'b + ut where b = 0
    fit<-lm(y~X)
    bhat <- fit$coef[1+1:d]
    num <- sum((X%*%(bhat))^2)/d  # MSM
    den <- sum((fit$res)^2)/(n-d-1)    # MSE
    R[sim] = (num/den)                     # Root function
    if (sim %% 100 == 0) {print(sim)}
                    }
proc.time()-ptm
q = 0.95
q.true = quantile(R,probs=q)
rm(list=setdiff(ls(),c("q","q.true","n","d","rho")))

setwd("C:/Users/ssengpt2/Downloads/work/BLB/09-19")
#setwd("C:/Users/pureblood/Box Sync/Dr Shao/current/BLB/R")
source("BLBts_RegfunQ.R")
n.sim = 20
TimeLimit1 = 150  # for l=10
TimeLimit2 = 90	  # for l=20, BLB might have 0 iterations at t=60
TimeLimit3 = 60   # for l=50
l1 = 10
l2 = 20
l3 = 50
b1 = 5000
b2 = 10000
sim11 = Regsimq(q,q.true,n,d,rho,l=l1,b=b1,n.sim,TimeLimit=TimeLimit1)
sim21 = Regsimq(q,q.true,n,d,rho,l=l2,b=b1,n.sim,TimeLimit=TimeLimit2)  
sim31 = Regsimq(q,q.true,n,d,rho,l=l3,b=b1,n.sim,TimeLimit=TimeLimit3)  
sim12 = Regsimq(q,q.true,n,d,rho,l=l1,b=b2,n.sim,TimeLimit=TimeLimit1)
sim22 = Regsimq(q,q.true,n,d,rho,l=l2,b=b2,n.sim,TimeLimit=TimeLimit2)
sim32 = Regsimq(q,q.true,n,d,rho,l=l3,b=b2,n.sim,TimeLimit=TimeLimit3)
save.image("ts_regsimqrho3_1210.RData")	