### R session 1: Model-based preference learning in R
### PREFSTAT 2024 - Day 2

rm(list=ls())

## 1. Factorial n! -------

# With the base package...
n <- 5
factorial(n)

# or by exploiting the relationship with the gamma function
gamma(n+1)

# But there is the limit!
n_seq=1:171
sapply(n_seq,factorial)
sapply(n_seq+1,gamma)

# The gmp package provides arithmetics for large integer and rationale numbers
library(gmp)
factorialZ(171)
# virtually no limit
sapply(170:250,factorialZ)

## 2. Ranking space -------
library(combinat)
permn(n)
perms <- permn(paste0("Items",1:n))
perms
matrix(unlist(perms),nrow=gamma(n+1),ncol=n,byrow=TRUE)

?gtools::permutations
gtools::permutations(n=n,r=n)
gtools::permutations(n=10,r=10)

e1071::permutations(n=n)
e1071::permutations(n=10)

arrangements::permutations(x=1:n)
arrangements::permutations(x=paste0("Items",1:n))
arrangements::permutations(x=1:10)
arrangements::permutations(x=1:11)
arrangements::permutations(x=1:12) # critical threshold for memory!
# Check the GB currently occupied!!!

# Let us compare the computational efficiency
microbenchmark::microbenchmark(gtools::permutations(n=8,r=8),
                               e1071::permutations(n=8),
                               arrangements::permutations(x=1:8),
                               times=10)


# 3. Ranking polytope -------
# For rankings of 3 items (hexagon)
library(pmr)
help(package=pmr)

Item1 <- c(1,1,2,2,3,3)
Item2 <- c(2,3,1,3,1,2)
Item3 <- c(3,2,3,1,2,1)
freq <- c(8,2,15,4,20,5)
fake_data <- data.frame(Item1,Item2,Item3,freq) # some package require aggregate data
fake_data
?rankplot
rankplot(fake_data, label.type="ranking", circle.col="purple", circle.bg="purple")
rankplot(fake_data, circle.col="purple", circle.bg="purple")

# For rankings of 4 items (truncated octahedron)
data(big4)
?big4
rankplot(big4, circle.col="orange", circle.bg="orange")
rankplot(big4,label.type="ranking", circle.col="orange", circle.bg="orange")


## 4. Data format and manipulation -------
# Intuition
c(3,2,5,1,4) # ranking
order(c(3,2,5,1,4)) # ordering

# with the function from the PLMIX package
library(PLMIX)
head(d_occup)
?rank_ord_switch
head(rank_ord_switch(d_occup, format_input="ordering"))

library(MSmix)
head(ranks_beers[,1:20])
?data_conversion
head(data_conversion(ranks_beers[,1:20]))


## 5. Descriptive statistics -------
# For full ranking data
data(idea)
?idea

# with pmr package
destat(idea)

# with PLMIX package
idea_no_aggr=freq_to_unit(idea)

# with MSmix package
descr_idea=data_description(idea_no_aggr)
class(descr_idea)
descr_idea
descr_idea=data_description(idea_no_aggr,item_names=paste0("Item",1:n))
print(descr_idea)
plot(descr_idea)
sum(descr_idea$mean_rank)==n*(n+1)/2

r_sports <- ranks_sports[, 1:8]
desc_m <- data_description(rankings = r_sports, subset = (ranks_sports$Gender == "Male"))
desc_m
plot(desc_m)

desc_f <- data_description(rankings = r_sports, subset = (ranks_sports$Gender == "Female"))
plot(desc_f)

# For partial ranking data
str(ranks_read_genres)
head(ranks_read_genres[,1:11]) # top-5 rankings
plot(data_description(ranks_read_genres[,1:11], borda_ord=TRUE))

?d_dublinwest
head(d_dublinwest)
mode(d_dublinwest)
class(d_dublinwest)
d_dublinwest[d_dublinwest==0] <- NA # pay attention to the missing entry codification!!!
ranks_dublinwest <- data_conversion(data=d_dublinwest)

plot(data_description(ranks_dublinwest,
                      item_names=c("Bonnie", "Burton", "Doherty", "Higgins",
                                   "Lenihan", "Donald", "Morrissey", "Smyth", "Terry")))

