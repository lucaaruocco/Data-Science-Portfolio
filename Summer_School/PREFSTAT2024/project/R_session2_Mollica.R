### R session 2: Model-based preference learning in R
### PREFSTAT 2024 - Day 2

rm(list=ls())

## 1. Simulation from Plackett-Luce model ----

?sample

# Single ranked observation of 5 items from the uniform model (PL with equal support parameters)
n_items=5
set.seed(123)
sample(x=1:n_items,size=n_items,replace=FALSE,prob=rep(1/n_items,n_items))
# Is it a ranking or an ordering?
# Thus equivalent to (note both x and prob arguments)
set.seed(123)
sample(x=paste0("Item",1:n_items),size=n_items,prob=rep(1,n_items))

# Shortcut
set.seed(123)
sample(x=n_items,prob=rep(1/n_items,n_items))

# Single ranked observation of 5 items from the PL with given support parameters
p_par=1:n_items
set.seed(123)
sample(x=1:n_items,size=n_items,prob=p_par)

# Shortcut
set.seed(123)
obs_seq5=sample(n_items,prob=p_par)
obs_seq5

## 2. Likelihood vs sampling distribution ----

# Let us compute the probability of observing the sampled sequence 
# under the postulated PL by hand
p_par
prop.table(p_par) # normalized support parameters
sum(prop.table(p_par))

obs_seq5
numerators=prop.table(p_par[obs_seq5])
numerators

denominators=rev(cumsum(rev(numerators)))
denominators

numerators/denominators

# P(\pi^(-1)=c(5,2,4,1,3)|p_par)
prod(numerators/denominators) 

# Note that the above probability is the likelihood of the observed ordering
library(PLMIX)
?likPLMIX
likPLMIX(p=t(p_par),ref_order=t(1:n_items),weights=1,
         pi_inv=t(obs_seq5))

# Compute the probabilities of all possible 5!=120 orderings
all_order5=e1071::permutations(n=n_items)
all_order5
probs5=apply(all_order5,1,
            FUN=likPLMIX,p=t(p_par),ref_order=t(1:n_items),weights=1)
sum(probs5)

ord_distr=cbind(all_order5,probs5)
ord_distr

# What is the meaning of this plot?
# Is this the likelihood function or the sampling distribution?
n_fact=factorial(n_items)
plot(1:n_fact,y=probs5,type="h",xlab="",ylab="probability",axes=FALSE)
axis(2,at=seq(0,0.05,by=0.01),labels=seq(0,0.05,by=0.01))
axis(1,at=1:n_fact,labels=apply(all_order5,1,paste,collapse=" "),las=3,cex.axis=0.5)

# Plot with sorted probabilities
plot(1:n_fact,y=sort(probs5),type="h",xlab="",ylab="probability",
     main="PL (sampling) distribution",axes=FALSE)
axis(2,at=seq(0,0.05,by=0.01),labels=seq(0,0.05,by=0.01))
axis(1,at=1:n_fact,labels=apply(all_order5,1,paste,collapse=" ")[order(probs5)],las=3,cex.axis=0.5)

# Let us sort the orderings for increasing probability values
ord_distr[order(probs5),]

# Modal ordering
all_order5[which.max(probs5),]
order(-p_par)

# Determine the PL model on the ranking space, rather than on the ordering space
all_rank5=t(apply(all_order5,1,order))
rank_distr=cbind(all_rank5,probs5)
rank_distr[order(probs5),]

## 3. First-order marginal distributions of the PL ----

# Which are the marginal item distributions by each rank?
# for rank 1
r1=colSums(probs5*(all_order5[,1]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
r1
prop.table(p_par)
# for rank 2
r2=colSums(probs5*(all_order5[,2]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
r2
# for rank 3
r3=colSums(probs5*(all_order5[,3]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
r3
# for rank 4
r4=colSums(probs5*(all_order5[,4]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
r4
# for rank 5
r5=colSums(probs5*(all_order5[,5]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
r5
prop.table(1-prop.table(p_par)) # attention, they do not coincide!

cbind(rank1=r1,rank2=r2,rank3=r3,rank4=r4,rank5=r5)
heatmap(rbind(r1,r2,r3,r4,r5),Rowv = NA,Colv = NA)

# Which are the marginal rank distributions by each item?
# for item 1
i1=colSums(probs5*(all_rank5[,1]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
i1
# for item 2
i2=colSums(probs5*(all_rank5[,2]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
i2
# for item 3
i3=colSums(probs5*(all_rank5[,3]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
i3
# for item 4
i4=colSums(probs5*(all_rank5[,4]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
i4
# for item 5
i5=colSums(probs5*(all_rank5[,5]==matrix(1:n_items,nrow=n_fact,ncol=n_items,byrow=TRUE)))
i5

cbind(item1=i1,item2=i2,item3=i3,item4=i4,item5=i5)
heatmap(rbind(i1,i2,i3,i4,i5),Rowv = NA,Colv = NA)

# Switch from rank marginals to item marginals and viceversa
apply(cbind(i1,i2,i3,i4,i5),1,prop.table)
cbind(r1,r2,r3,r4,r5)
apply(cbind(r1,r2,r3,r4,r5),1,prop.table)
cbind(item1=i1,item2=i2,item3=i3,item4=i4,item5=i5)

## 4. Stochastic dominance of rank marginals of the PL with distinct support parameters ----

plot(1:n_items,cumsum(i1),type="s",col=1,lwd=2,xlab="rank",ylab="prob",
     main="Ecdf of the marginal rank distributions\nof each item")
lines(1:n_items,cumsum(i2),type="s",col=2,lwd=2)
lines(1:n_items,cumsum(i3),type="s",col=3,lwd=2)
lines(1:n_items,cumsum(i4),type="s",col=4,lwd=2)
lines(1:n_items,cumsum(i5),type="s",col=n_items,lwd=2)
legend("topleft",legend=paste0("Item",1:n_items),col=1:n_items,text.col=1:n_items,lwd=2,bty="n")


## 5. Mean rank vector ----

# Simply compute the expectation of each marginal rank distribution  
mean_rank_vec5=colSums(matrix(1:n_items,nrow=n_items,ncol=n_items)*cbind(i1,i2,i3,i4,i5))
mean_rank_vec5

# Simulate a large sample from the postulated PL
set.seed(123)
# obs_seq5=sample(n_items,prob=p_par)
n_sample=10000
sample_seq5=t(sapply(rep(n_items,n_sample),sample,prob=p_par))
head(sample_seq5)

# Compute the empirical mean rank vector
colMeans(t(apply(sample_seq5,1,order)))

# and compare with the theoretical one
mean_rank_vec5

## 6. Identifiability of the PL model ----

# Simulate a large sample from a new PL for 10 items, but with the rPLMIX from the PLMIX package
set.seed(123)
p_par10=runif(10)
prop.table(p_par10)

set.seed(123)
sample_ord10=rPLMIX(n = 1000, K=10, G=1, p = t(p_par10),ref_order = t(1:10),
                    weights = 1, format_output = "ordering")

# Maximum log-likelihood value
loglikPLMIX(p=t(p_par10),ref_order=t(1:n_items),weights=1,pi_inv=sample_ord10)

# Identifiability up to multiplication for positive constants
loglikPLMIX(p=1.2*t(p_par10),ref_order=t(1:n_items),weights=1,pi_inv=sample_ord10)

## 7. MLE of the PL model ----

# MLE of the PL on the simulated data
?mapPLMIX_multistart

fit_plmix10 <- mapPLMIX_multistart(pi_inv=sample_ord10, K=ncol(sample_ord10), G=1,
                                       n_start=50,n_iter=100)
str(fit_plmix10)

# MLE of the support parameters vs true values 
fit_plmix10$mod$P_map
prop.table(p_par10)

# Maximized log-likelihood vs likelihood of the true parameter values
max_loglik_plmix10=max(fit_plmix10$mod$log_lik)
max_loglik_plmix10

## 8. Real data application #1: Carconf dataset ----
## PLMIX ##
# To use parallelization
library(doParallel)

detectCores()
registerDoParallel(cores=detectCores())
getDoParWorkers()


# Fit mixtures of PL to the Carconf (partial) ranking dataset
# MAP estimates
?d_carconf
head(d_carconf)
n <- 6
G_seq=1:4
bic_vec_plmix=rep(NA,length(G_seq))
fit_plmix=vector(length=length(G_seq),mode="list")

for(GG in G_seq){
  set.seed(paste0(GG,"4374973"))
  fit_plmix[[GG]] <- mapPLMIX_multistart(pi_inv=d_carconf, K=ncol(d_carconf), G=GG,
                                         n_start=50*GG,n_iter=100,parallel=TRUE)
  bic_vec_plmix[GG]=fit_plmix[[GG]]$mod$bic
}


# Gibbs sampling
# It takes some minutes!
gibbs_plmix=vector(length=length(G_seq),mode="list")
for(GG in G_seq){
  set.seed(paste0(GG,"98632690"))
  gibbs_plmix[[GG]] <- gibbsPLMIX(pi_inv=d_carconf, K=n, G=GG,
                        init=list(p=fit_plmix[[GG]]$mod$P_map,
                        z=binary_group_ind(fit_plmix[[GG]]$mod$class_map,G=GG)),
                        n_iter=15000, n_burn=5000)
}


# Model seletion
select_carconf <- selectPLMIX(pi_inv=d_carconf, seq_G=G_seq,parallel=TRUE,
            MAPestP=list(fit_plmix[[1]]$mod$P_map,fit_plmix[[2]]$mod$P_map,fit_plmix[[3]]$mod$P_map,
                         fit_plmix[[4]]$mod$P_map),
            MAPestW=list(fit_plmix[[1]]$mod$W_map,fit_plmix[[2]]$mod$W_map,fit_plmix[[3]]$mod$W_map,
                         fit_plmix[[4]]$mod$W_map),
            deviance=list(gibbs_plmix[[1]]$deviance,gibbs_plmix[[2]]$deviance,gibbs_plmix[[3]]$deviance,
                          gibbs_plmix[[4]]$deviance))
select_carconf
criteria_carconf=cbind(select_carconf$criteria,BIC=bic_vec_plmix)
apply(criteria_carconf,2,which.min)

# Model check
check_carconf <- ppcheckPLMIX(pi_inv=d_carconf, seq_G=G_seq,parallel=TRUE,
                              MCMCsampleP=list(gibbs_plmix[[1]]$P,gibbs_plmix[[2]]$P,
                                               gibbs_plmix[[3]]$P,gibbs_plmix[[4]]$P),
                              MCMCsampleW=list(gibbs_plmix[[1]]$W,gibbs_plmix[[2]]$W,
                                               gibbs_plmix[[3]]$W,gibbs_plmix[[4]]$W))
check_carconf

# Model check (conditionally on the length of partial sequences)
check_cond_carconf <- ppcheckPLMIX_cond(pi_inv=d_carconf, seq_G=G_seq,parallel=TRUE,
                              MCMCsampleP=list(gibbs_plmix[[1]]$P,gibbs_plmix[[2]]$P,
                                               gibbs_plmix[[3]]$P,gibbs_plmix[[4]]$P),
                              MCMCsampleW=list(gibbs_plmix[[1]]$W,gibbs_plmix[[2]]$W,
                                               gibbs_plmix[[3]]$W,gibbs_plmix[[4]]$W))
check_cond_carconf

# Let us select 2 groups...
final_gibbs <- gibbs_plmix[[2]]
summary(final_gibbs) # recall coda package

# We have to check for possible occurrence of label switching...
plot(final_gibbs,plot="traceplot") # recall ggmcmc package

# If label switching occured in the posterior sample
# run the following code
final_map <- fit_plmix[[2]]
LS_postproc <- label_switchPLMIX(pi_inv=d_carconf,seq_G=2,
                                 MCMCsampleP=list(final_gibbs$P),
                                 MCMCsampleW=list(final_gibbs$W),
                                 MAPestP=list(final_map$mod$P_map),
                                 MAPestW=list(final_map$mod$W_map))
str(LS_postproc)
plot(LS_postproc$final_sampleW$G_2[,1],type="l",ylim=c(0,1),ylab="W")
lines(LS_postproc$final_sampleW$G_2[,2],type="l",col=2)


## 9. Real data application #2: Gaming dataset ----
## PLMIX ##
# Fit mixtures of PL to the Game (full) ranking dataset

G_seq=1:6
bic_vec_plmix=rep(NA,length(G_seq))
fit_plmix=vector(length=length(G_seq),mode="list")

for(GG in G_seq){
  set.seed(paste0(GG,"53648238"))
  fit_plmix[[GG]] <- mapPLMIX_multistart(pi_inv=d_gaming, K=ncol(d_gaming), G=GG,
                                         n_start=50*GG,n_iter=100,parallel=TRUE)
  bic_vec_plmix[GG]=fit_plmix[[GG]]$mod$bic
}

bic_vec_plmix
which.min(bic_vec_plmix)
summary(fit_plmix[[which.min(bic_vec_plmix)]])
plot(fit_plmix[[which.min(bic_vec_plmix)]])


# MSmix - Fit mixtures of MS to the Game (full) ranking dataset
library(MSmix)

ranks_gaming <- data_conversion(d_gaming)
colnames(ranks_gaming) <- c("Xbox", "PlayStation", "PSPortable",
                            "GameCube", "GameBoy", "PC")

log_lik_vec_mms=bic_vec_mms=rep(NA,length(G_seq))
fit_mms=vector(length=length(G_seq),mode="list")
for(GG in G_seq){
  set.seed(paste0(GG,"73846372"))
  fit_mms[[GG]] <- fitMSmix(rankings=ranks_gaming,n_clust=GG,
                            n_start=50*GG,n_iter=100,parallel=TRUE)
  log_lik_vec_mms[GG]=max(fit_mms[[GG]]$max_log_lik)
  bic_vec_mms[GG]=fit_mms[[GG]]$mod$bic
}

plot(G_seq,log_lik_vec_mms, type="b", xlab="G", ylab="Maximized log-likelihood")
bic_vec_mms
which.min(bic_vec_mms)
plot(bic_vec_mms,type="b", ylab="BIC", ylim=c(1050,1150), lwd=2)
lines(bic_vec_plmix, type="b",col="purple", lwd=2)
legend("topleft", legend=c("MSmix","PLMIX"), lwd=2, col=c("black","purple"),
       text.col=c("black","purple"), bty="n")

summary(fit_mms[[which.min(bic_vec_mms)]])
plot(fit_mms[[which.min(bic_vec_mms)]])

