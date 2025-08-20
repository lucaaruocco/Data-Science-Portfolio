###################################################################
#### PREFSTAT 2024 -- R SESSION DAY 2 -- DISTANCE-BASED MODELS ####
####                      VALERIA VITELLI                      ####
###################################################################

rm(list=objects())
setwd("C:/Users/valeriv/UiO Dropbox/Valeria Vitelli/Conferences/PrefStat_summer_school/lectures/labs")

##---------------------------------------------------------------------
## packages for model-based preference learning (distance-based models)
##---------------------------------------------------------------------
library(rankdist)     # fits several Kendall-based models
                      # ML estimation, handles clustering & partial data
library(BayesMallows) # fits distance-based models for several distances
                      # Bayes estimation, handles clustering & partial data
## OTHER INTERESTING PACKAGES -> NOT FOR TODAY!
#library(Rankcluster)  # includes many nice datasets
#library(PerMallows)   # computationally advanced functions for sampling

##---------------------------------
## example datasets
##---------------------------------

## example 1, complete data: POTATO DATASET
# (Example dataset from Liu et al. 2019, Section 4)
# Experiment on ranking potatoes in terms of weight, where
# N = 12 assessors gave complete rankings of n = 20 potatoes.
# VISUAL experiment: potatoes were only inspected visually
# WEIGHING experiment: assessors were allowed to lift the potatoes
potato_visual
potato_weighing
potato_true_ranking

## example 2, pairwise preference data: BEACH DATASET
# (Example dataset from Vitelli et al. 2018, Section 6.2)
# N = 60 assessors give (at most) 25 pariwise comparisons
# of 15 beaches; intransitive patterns have been removed
# An object of class data.frame with 1442 rows and 3 columns
head(beach_preferences)


##-----------------
## Bayes estimation
##-----------------

## EXAMPLE 1 ##
##-----------##

## we focus on the potato data (visual)
## for fitting a 1-component Complete data example
##------------------------------------------------
complete_data <- setup_rank_data(rankings = potato_visual)
complete_data

# fitting a Bayesian Mallows model with 1 component
# and exploring the results
bmm_test <- compute_mallows(data = complete_data)
assess_convergence(bmm_test)
assess_convergence(bmm_test, parameter = "rho", items = 1:5) # try: change items 

## the MCMC algorithm seems to have converged after around 1,000 iterations
## we can thus discard the first 1,000 MCMC samples as burn-in

# => rerun the algorithm to get 20,000 samples after burn-in;
bmm_visual <- compute_mallows(
  data = complete_data, 
  compute_options = set_compute_options(nmc = 21000, burnin = 1000)
)

# "bmm_visual" is an S3 class BayesMallows object, so we 
# can directly use the plot functions (plot.BayesMallows)

## we first plot the posterior distribution of \alpha (scale param)
plot(bmm_visual)

# We can also get posterior credible intervals for \alpha
# using compute_posterior_intervals, 
# -> returns highest posterior density intervals (HPDI) & central intervals
#    (data.frame format)
compute_posterior_intervals(bmm_visual, decimals = 1L)

## we now study the posterior distribution of \rho
## (if the argument 'items' is not provided, and the number of items exceeds
## five, then five items are picked at random for plotting. 
## To show all potatoes, we explicitly set this argument) 
# marginal posterior distribution of the consensus ranking
plot(bmm_visual, parameter = "rho", items = 1:20)

## JUMPING OVER THE SCALE PARAMETER:
# Updating \alpha in every step of the MCMC algorithm may not be necessary,
# as the number of posterior samples typically is more than large enough 
# to obtain good estimates of its posterior distribution. Moreover,
# being \rho a vector, letting \rho vary more can help the MCMC mixing 

# With the 'alpha_jump' argument, we can tell the MCMC algorithm to update 
# \alpha only every alpha_jump-th iteration with \rho updates 
# for example, to update every 10th iteration, we fix alpha_jump = 10
bmm_visual <- compute_mallows(
  data = complete_data, 
  compute_options = 
    set_compute_options(nmc = 21000, burnin = 1000, alpha_jump = 10)
)

## OTHER DISTANCE METRICS:
# By default, 'compute_mallows' uses the footrule distance,
# but the user can also choose to use other distances, in particular:
# Cayley, Kendall, Hamming, Spearman, and Ulam

# Running the same analysis of the potato data with Spearman distance:
bmm <- compute_mallows(
  data = complete_data, 
  model_options = set_model_options(metric = "spearman"),
  compute_options = set_compute_options(nmc = 21000, burnin = 1000)
)

## NOTE:
# For the particular case of Spearman distance, integer sequences for computing
# the exact partition function with 14 or fewer items.
# BayesMallows includes a precomputed importance sampling estimate,
# which is automatically used in this case.

## EXAMPLE 2 ##
##-----------##
## replicate the same analysis on the BEACH DATA
## -> now data are in the form of pairwise preferences!

# These data should be provided in a dataframe with 3 columns,
# AND with 1 row per pairwise comparison:
# column 1: 'assessor' -> identifier for the assessor;
#           numeric (character) vector containing the assessor index (name)
# column 2: 'bottom_item' is a numeric vector containing the index of the item
#           that was disfavored in each pairwise comparison.
# column 3: 'top_item' is a numeric vector containing the index of the item
#           that was preferred in each pairwise comparison.
head(beach_preferences)

## NOTE:
# Unless the argument error_model to set_model_options is set,
# pairwise preference data are assumed to be consistent within each assessor.


# A dataframe with this structure can be given in the preferences argument
# to setup_rank_data, which will generate the full set of IMPLIED COMPARISONS
# for each assessor, as well as an initial ranking matrix consistent with
# the pairwise preferences.
beach_data <- setup_rank_data(preferences = beach_preferences)

## IMPLIED RANKINGS -> what does this mean??
# Let's understand this by comparing the computed transitive closure
# to the stated preferences. Let???s do this for assessor 1, and
# for preferences involving beach 2.

# raw preferences:
subset(beach_preferences, assessor == 1 & (bottom_item == 2 | top_item == 2))
# transitive closure:
tc <- get_transitive_closure(beach_data)
subset(tc, assessor == 1 & (bottom_item == 2 | top_item == 2))

## what happened?
# In addition to the statement that beach 15 is preferred to beach 2, 
# all the other orderings stated by assessor 1 imply that this assessor
# prefers beach 6 to beach 2.

## Convergence diagnostics
# As with the potato data, let us do a test run to assess the convergence
# of the MCMC algorithm. This time we use the beach_data object. 
# We also set save_aug = TRUE to save the augmented rankings in each MCMC step,
# hence letting us assess the convergence of the augmented rankings.
bmm_test <- compute_mallows(
  data = beach_data,
  compute_options = set_compute_options(save_aug = TRUE))
assess_convergence(bmm_test)
assess_convergence(bmm_test, parameter = "rho", items = 1:6)

# To check the convergence of the data augmentation scheme, we need to set
# parameter = "Rtilde", and also specify which items and assessors to plot.
# Let us start by considering items 2, 6, and 15 for assessor 1, which we studied above.
assess_convergence(
  bmm_test, parameter = "Rtilde", items = c(2, 6, 15), assessors = 1)

## NOTE:
# The convergence plot illustrates how the augmented rankings vary,
# while also obeying their implied ordering.

# What if there are no orderings implied between two beaches for a given assessor?
# Let us try with beaches 1 and 15 for assessor 2:
subset(tc, assessor == 2 & bottom_item %in% c(1, 15) & top_item %in% c(1, 15))
assess_convergence(
  bmm_test, parameter = "Rtilde", items = c(1, 15), assessors = 2)
# no ordering!!


# Ideally, we should look at trace plots for augmented ranks for more assessors
# to be sure that the algorithm is close to convergence. 
assess_convergence(
  bmm_test, parameter = "Rtilde", items = 13:15, assessors = 1:8)

## FINAL POSTERIOR DISTRIBUTION ESTIMATES:
# Based on the convergence diagnostics, and being fairly conservative,
# we discard the first 2,000 MCMC iterations as burn-in,
# and take 20,000 additional samples 
bmm_beaches <- compute_mallows(
  data = beach_data,
  compute_options = 
    set_compute_options(nmc = 22000, burnin = 2000, save_aug = TRUE)
)
# Posterior distributions of \alpha and \rho can be studied as for potatoes
plot(bmm_beaches) # smaller \alpha than for potatoes! more uncertainty..
plot(bmm_beaches, parameter = "rho", items = 1:15)

# In this case, since we have partial data, we can also compute the
# posterior intervals for the latent rankings of each beach.
# we use the function 'compute_posterior_intervals':
compute_posterior_intervals(bmm_beaches, parameter = "rho")

# We can also rank the beaches according to the posterior summary of the 
# consensus ranking: two notable approaches
# 1. the Maximum A Posterior (MAP) ranking 
# 2. the Cumulative Probability (CP) consensus (Vitelli et al. 2018) 
# we use the function 'compute_consensus':
compute_consensus(bmm_beaches, type = "CP")
compute_consensus(bmm_beaches, type = "MAP")


# In case of pairwise preferences, we can also ask:
# which is the probability that beach i is ranked top-k
# by assessor j?
# we use the function 'plot_top_k' to plot these probabilities:
plot_top_k(bmm_beaches)
# By default, it sets k = 3, so a heatplot of the probability
# of being ranked top-3 is obtained with the call

# The function 'predict_top_k' returns a dataframe with all these
# underlying probabilities.
# For example, in order to find all the beaches that are among the top-3
# of assessors 1-5 with more than 90 % probability, we would do:
subset(predict_top_k(bmm_beaches), prob > .9 & assessor %in% 1:5)
## NOTE:
# assessor 2 does not appear in this table, i.e., there are no beaches
# for which we are at least 90 % certain that the beach is among
# assessor 2's top-3 (we might say that assessor 2 is quite 'uncertain')



##-----------------
## ML estimation
##-----------------

## the package rankdist provides functions for ML inference
## for a wide class of distance-based models (ALL based on Kendall!)
## it based on handling the ranking data in a S4 class 'RankData'
## so the first step is always creating the RankData object

## data preprocessing: 
# functionalities to deal with/build ranking dataset

# toy example (deafult in rankdist): create a dataset with
# N = 2000 and n = 5 (note: n! = 120 in this case, so ALL rankings observed)
gen1 <- GenerateExample(ranking = TRUE)
tail(gen1$ranking)
dim(gen1$ranking)
gen1$count
dat1 <- new("RankData", ranking = gen1$ranking, count = gen1$count)
dat1

# creating a more useful data set with only complete rankings
# (same dimension as potatoes)
rankmat <- t(replicate(12,sample(1:20,20), simplify = "array") )
rankmat
rankdat <- new("RankData",ranking=rankmat) 
rankdat

## we focus on the potato data (visual)
## for fitting a 1-component Mallows' phi model
## for a Complete data example
##------------------------------------------------
rankdata_potato <- new("RankData", ranking = potato_visual)
rankdata_potato

## model choices:

## the 'RankControl' class specifies the type of model and controls
## the behavior of parameter estimation. 

# The objects 'ctrlK' and 'ctrlWK' specify the desired model class
# to be the Mallows' phi model and the weighted Kendall model respectively. 
# (we focus on the Mallows' phi model for brevity, but the other version is equivalent!)
ctrlK <- new("RankControlKendall", SearchPi0_show_message = FALSE)
ctrlWK <- new("RankControlWeightedKendall", SearchPi0_show_message = FALSE)

# NOTE: SearchPi0_show_message = FALSE removes the message output
# when searching for the central ranking

## model initialization:

## the 'RankInit' class controls the initialization of the model,
## both parameters and modeling choices.
# 1 component model, with central ranking initialized to Borda count
# clu argument: number of mixture components; if clu>1, the user must provide 
#               initial central ranking and weights for each component
avg_rank <- rankdata_potato@count %*% rankdata_potato@ranking
modal_ranking.init <- OrderingToRanking(order(avg_rank))
init1 <- new("RankInit", param.init = list(),
             modal_ranking.init = list(modal_ranking.init), clu = 1L)

# NOTE: when fitting the weighted Kendall model, the initialization of the
# weights is ALSO needed! It can be done in the following way:
# MomentsEst: creates feasible initial values for the weights (only)
str1 <- MomentsEst(rankdata_potato, 500)
init1 <- new("RankInit", param.init = list(str1),
             modal_ranking.init = list(modal_ranking.init), clu = 1L)

## model fitting:
# once we have created the objects 'RankData', 'RankControl' and 'RankInit'
# the generic function RankDistanceModel can be used for model fitting 
model1K <- RankDistanceModel(rankdata_potato, init1, ctrlK) # 1 component phi model


## model summary:
# Two common approaches to assess the goodness of fit are
# 1. BIC (Bayesian information criterion)
# 2. sum of squares of Pearson residuals (SSR)
ModelSummary(model1K)
names(model1K)

model1K$p # only 1 cluster!

model1K$BIC # model BIC (useful when fitting mixtures)

model1K$modal_ranking.est # consensus ranking

model1K$w.est # this is the unique dispersion parameter!
model1K$param.est # equivalent in this case

# NOTE: remember that the general notation is theta=alpha/n,
# so in my notation from this morning the estimate of alpha is:
n <- dim(potato_visual)[2]
n * model1K$param.est[[1]]
