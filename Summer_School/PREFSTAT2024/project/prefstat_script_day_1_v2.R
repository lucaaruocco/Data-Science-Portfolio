
##-------------PREFSTAT DAY 1, examples shown in the teaching handout
##-------------load the packges, and load the data set in the working directory

#--we need the PerMallows package to sample rankings 
require(PerMallows)
set.seed(134) #for reproducibility
R <- rmm(10,c(1,2,3,4),0.8)
#R is a rank data matrix
colnames(R) <- c("Coke","Fanta","Pepsi","7up")
require(ConsRank)
#O is the ordering matrix
#the rank2order function tramsform rankings into ordering
#?rank2order
O <- rank2order(R,items=colnames(R))
#-----------------------------------------
##leisure time data
#set the race
race <- c(rep("blacks",2),"whites",
          rep("blacks",5),rep("whites",7),
          rep("whites",6),rep("blacks",6)
)
#set the ranks
ranks <- matrix(c(c(1,2,3),c(1,3,2),c(2,1,3),rep(c(2,3,1),5),
                  rep(c(3,1,2),7),rep(c(3,2,1),12)),nrow=27,byrow=TRUE)
leisure <- data.frame(race,ranks)
colnames(leisure) <- c("race","M","F","B")
leisure$race <- factor(leisure$race)
#frequency table off all rankings
leisureall <- tabulaterows(leisure[,-1])$tabfreq
colnames(leisureall) <- c("M","F","B","weight")
#frequency table partitioned by race
leisureby <- by(leisure,race,function(x) tabulaterows(x[,-1])$tabfreq)
colnames(leisureby$blacks) <- colnames(leisureby$whites) <- c("M","F","B","weight")
#--------------------------plot polytopes
#1 all rankings
polyplot(leisureall[,-4],Wk=leisureall[,4],L=colnames(leisureall)[c(1:3)])
#2 black females
polyplot(leisureby$blacks[,-4],Wk=leisureby$blacks[,4],L=colnames(leisureby$blacks)[c(1:3)])
#3white females
polyplot(leisureby$whites[,-4],Wk=leisureby$whites[,4],L=colnames(leisureby$whites)[c(1:3)])
##
#---------------------------------political goals
#data(German)
#?German
load("polgoals.Rd")
#see the data
polgoals
polyplot(polgoals[,1:4],L=c(colnames(polgoals)[1:4]),Wk=polgoals[,5]*100,nobj=4)
#-------------------sports data set
data("sports")
##-----PCA-like
m <- ncol(sports)
cm <- (m+1)/2
csports <- (sports-cm) #center data as in Marden
W <- crossprod(csports/sqrt(130))
g <- eigen(W) #compute eigenvalues and eigenvectors
G <- g$vectors[,1:2] #isolate two dimensions
row.names(G) <- colnames(W)
coo <- G%*%diag(sqrt(g$values[1:2])) #variables coordinates
z <- csports%*%G #individuals coordinates
#plot PCA-like sports
par(pty="s")
plot(z[,1],z[,2], cex=0.43,axes=FALSE, xlab="" ,ylab="" )
for (j in 1:nrow(coo)){
  segments(x0=0,y0=0,x1=coo[j,1],y1=coo[j,2])
}
text(coo[,1],coo[,2],rownames(coo),cex=0.5)
points(z[,1],z[,2], cex=0.7)
dev.off()
#-------------------unfolding
require(smacof)
plot(unfolding(sports,type="ordinal",omega=10,lambda=0.5),
     cex=1,xlim=c(-0.5,0.5),ylim=c(-1,1), xlab="",ylab="", main="")
#
##------------------------ distances for rankings
#set two rankings as example
A <- matrix(c(1,2,3,4,5,6),nrow=1)
B <- matrix(c(2,5,3,1,4,6),nrow=1)
#----------------------footrule distance
require(proxy) #the "dist"function in proxy allows for two matrices
footr <- dist(A,B,method="minkowski",p=1)
footr
#---------------------------------spearman distance
sp <- dist(A,B,method="minkowski",p=2)^2
sp
#spearman rho
#original formulation
1-(6*sp/(6^3-6))
#define the reverse ranking
C <- 7-A 
#maximum distance
maxsp <- dist(A,C,method="minkowski",p=2)^2
#spearman rho by hand
1-2*sp/maxsp
#again, using the "cor" function
cor(t(A),t(B),method = "spearman")
#---------------------------Kendall distance
#require(ConsRank)
#require(PerMallows)
kd <- distance(A,B,dist.name="kendall")
kd
#tau
a <- scorematrix(A) #score matrix ranking A 
b <- scorematrix(B) #score matrix ranking B
tau <- sum(a*b)/sqrt(sum(a^2)*sum(b^2)) #tau
tau
#maximum distance
maxk <- 6*5/2 #n(n-1)/2
#Kendall tau by hand
1-2*kd/maxk
#Kendall tau by using the "cor" function
cor(t(A),t(B),method = "kendall")
#--------------------------------------Kemeny distance
#require(ConsRank)
kd <- kemenyd(A,B)
kd
#Tau_x
#by hand
mkd <- 6*5 #maximum Kemeny distance
1-2*kd/mkd
#by using the function "tau_x"
tau_x(A,B)
#--------------------------------------Cayley distance
#require(PerMallows)
dc <- distance(A,B,dist.name="cayley")
dc
#---------------------------------------Hamming distance
#require(PerMallows)
hd <- distance(A,B,dist.name="hamming")
hd
#--------------------------------------Ulam distance
#require(PerMallows)
ud <- distance(A,B,dist.name="ulam")
ud
##-----compute distances in the space of permutations of 4 items
require(gtools) #for function permutations
prmt <- permutations(4,4) #all rankings with n=4
colnames(prmt)=c("a","b","c","d")
require(PerMallows)
require(ConsRank)
refr <- prmt[1,] # reference ranking (1,2,3,4)
footr <- dist(prmt,matrix(refr,1,4),method="minkowski",p=1)
spear <- dist(prmt,matrix(refr,1,4),method="minkowski",p=2)^2
kend <- cayl <- hamm <- ulam <- matrix(0,24,1)
for (j in 1:24){
  kend[j,1]=distance(prmt[j,],refr,dist.name="kendall")
  cayl[j,1]=distance(prmt[j,],refr,dist.name="cayley")
  hamm[j,1]=distance(prmt[j,],refr,dist.name="hamming")
  ulam[j,1]=distance(prmt[j,],refr,dist.name="ulam")
}
kemen=kemenyd(prmt,refr)
orderings=labels(prmt,4,c("a","b","c","d"),labs=1)
cbind(orderings,footr,spear,kend,cayl,hamm,ulam,kemen)
#-------------------Rank Aggregation Problem
#----------------------Branch-and-bound
#sports data set
data("sports")
crs <- consrank(sports)
crs
rank2order(crs$Consensus,items=colnames(crs$Consensus))
##
#breakfast data set
data("breakfast")
br <- consrank(breakfast)
br
#use rank2order to transform ranking into ordering
rank2order(br$Consensus,items=colnames(br$Consensus))
#APAfull data set
data("APAFULL")
#visualize the first 15 rows of the frequencies using tabulaterows function
tabulaterows(APAFULL)$tabfreq[1:15,]
crAPA <- consrank(APAFULL)
rank2order(crAPA$Consensus,items=colnames(crAPA$Consensus))
#all the unranked candidates are in a tie at position 5
APA2 <- APAFULL
APA2[which(is.na(APA2))] <- 5
crAPA2 <- consrank(APA2)
crAPA2
rank2order(crAPA2$Consensus,items=colnames(crAPA2$Consensus))
#combined input matrix
cijapa2 <- combinpmatr(APA2) 
cijapa2
##University rankings data set
require(ConsRankClass) #necessary to load the university rankings
data(Univranks)
crUN <- consrank(Univranks$rankings)
crUN
rank2order(crUN$Consensus,items=colnames(crUN$Consensus))
#option full rankings
crUNfull <- consrank(Univranks$rankings, full=TRUE)
crUNfull
rank2order(crUNfull$Consensus,items=colnames(crUNfull$Consensus))
##------------------QUICK algorithm
#emond and mason data set
data(EMD)
EMD #the last column is the frequency of each ranking
cremd <- consrank(EMD[,1:15],wk=EMD[,16],algorithm="quick")
cremd
rank2order(cremd$Consensus,items=colnames(cremd$Consensus))
#----------------FAST algorithm
set.seed(2)
cremd2 <- consrank(EMD[,1:15],wk=EMD[,16],algorithm="fast",itermax=60)
cremd2
rank2order(cremd2$Consensus,items=colnames(cremd2$Consensus))
#---------------DECoR algorithm
set.seed(4)
cremd3 <- consrank(EMD[,1:15],wk=EMD[,16],algorithm="decor",
                   itermax=5, gl=100, ff=0.4, cr=0.9)
cremd3
rank2order(cremd3$Consensus,items=colnames(cremd3$Consensus))
#USAranks
data(USAranks)
set.seed(10)
crusa <- consrank(USAranks,algorithm="decor",
                  itermax=15, gl=100, ff=0.4, cr=0.9, np=10)
crusa
rank2order(crusa$Consensus,items=colnames(crusa$Consensus))
#about 160 seconds
#
#-----------------------------------FUR and SIgFUR
require(RankAggSIgFUR)
load("cities.Rd")
#FUR
#This example runs in about 490 seconds
set.seed(10)
a <-proc.time()
furcity <- fur(t(cities),subit_len_list=2,search_radius=30)  #questo pacchetto vuole ordinamento diverso percio faccio la trasposta 
proc.time()-a
#about 490 seconds
# 
# SIgFUR
set.seed(20)
a <-proc.time()
sigfurcity <- sigfur(t(cities),omega_sbi=5, subit_len_list_sbi=2,
                      search_radius=1,subit_len_list_fur=2)
proc.time()-a


