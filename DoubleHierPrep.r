
# Code for double-hierarchical model
library(rstan)

#Prepare data

#Non-constructed (autonomous) cases
dAut <- subset(d, d$CombNC=="N")
datAut <- as.list(NA)
datAut$N <- length(dAut$Grad.linear.value)
datAut$N_Study <- length(unique(dAut$Subset))
datAut$Grad_obs <- dAut$Grad.linear.value
datAut$Grad_sd <- dAut$Grad.linear.StErr
datAut$Study <- dAut$Subset
datAut$Study <- sapply(datAut$Study, function(x) which(unique(datAut$Study)==x))

#Constructed cases
dNC <- subset(d, d$CombNC=="Y")
datNC <- as.list(NA)
datNC$N <- length(dNC$Grad.linear.value)
datNC$N_Study <- length(unique(dNC$Subset))
datNC$Grad_obs <- dNC$Grad.linear.value
datNC$Grad_sd <- dNC$Grad.linear.StErr
datNC$Study <- dNC$Subset
datNC$Study <- sapply(datNC$Study, function(x) which(unique(datNC$Study)==x))


#Run stan models
mAut <- stan( file="DoubleHier.stan" , data=datAut , chains=1, iter = 120000, cores=1 ,
              control=list(adapt_delta=0.998, max_treedepth=15))

mNC <- stan( file="DoubleHier.stan" , data=datNC , chains=1, iter = 120000, cores=1, 
             control=list(adapt_delta=0.998, max_treedepth=15))

