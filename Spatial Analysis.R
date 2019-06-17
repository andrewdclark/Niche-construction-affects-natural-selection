#Initial setup
{
  #Libraries
  {
    library(MCMCglmm)
    library(readxl)
  }
  
  #Dataset creation
  {
    d<-read_excel("")
    d<-subset(d,d$Subset!="NA")
    d<-subset(d,d$Borderline.ambiguous.case!=1)
    
    #Calculation of sampling variance from reported standard errors
    d$SE<-d$Grad.linear.StErr^2
    
    #Subgroups for analysis
    {
      dPert<- subset(d, d$Physical.Artifact!="0")
      dChoice<- subset(d, d$Physical.Artifact!="1")
      dBirds<-subset(d, d$Birds=="1")
      dOther<-subset(d, d$Birds=="0")
      dSame<-subset(d, d$Same.Study=="1")
    }
  }
  
  #Folded normal distribution
  {
    mu.fnorm<-function(mu,sigma){ sigma*sqrt(2/pi)*exp((-1*mu^2)/(2*sigma^2))+mu*(1-2*pnorm(-1*mu/sigma,0,1))} 
  }
  
  #Priors
  {
    priorComb <- list(G = list(G1 = list(V = diag(2),nu = 0.002)),R = list(V = diag(2),nu = 0.002))
    priorSep <- list(G = list(G1 = list(V = diag(3),nu = 0.002)),R = list(V = diag(3),nu = 0.002))
  }
}

#Spatial variation model with combined niche construction categories
{
  #Primary analysis
  mSpatComb <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                        mev = d$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = d)
  
  #Expected differences between two locations from the same subset#
  ExpdiffSpatCombN <-(2/sqrt(pi))*sqrt(mSpatComb$VCV[,4])
  ExpdiffSpatCombNC<-(2/sqrt(pi))*sqrt(mSpatComb$VCV[,5])
  
  #Consistency#
  ConsistencySpatCombN<-mSpatComb$VCV[,1]/(mSpatComb$VCV[,1]+mSpatComb$VCV[,4])
  ConsistencySpatCombNC<-mSpatComb$VCV[,2]/(mSpatComb$VCV[,2]+mSpatComb$VCV[,5])
}

#Spatial variation model with separate niche construction categories
{
  mSpatSep <- MCMCglmm(Grad.linear.value ~ NC, random = ~idh(NC):Subset, rcov = ~idh(NC):units,
                       mev = d$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorSep, data = d)
  
  ExpdiffSpatSepM<- (2/sqrt(pi))*sqrt(mSpatSep$VCV[,5])
  ExpdiffSpatSepN<- (2/sqrt(pi))*sqrt(mSpatSep$VCV[,6])
  ExpdiffSpatSepY<- (2/sqrt(pi))*sqrt(mSpatSep$VCV[,7])
  
  ConsistencySpatSepM<-mSpatSep$VCV[,1]/(mSpatSep$VCV[,1]+mSpatSep$VCV[,5])
  ConsistencySpatSepN<-mSpatSep$VCV[,2]/(mSpatSep$VCV[,2]+mSpatSep$VCV[,6])
  ConsistencySpatSepY<-mSpatSep$VCV[,3]/(mSpatSep$VCV[,3]+mSpatSep$VCV[,7])
}

#Spatial variation model with physical artifacts only and combined NC categories
{
  mSpatCombPert <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dPert$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dPert)
  
  ExpdiffSpatCombPertN <-(2/sqrt(pi))*sqrt(mSpatCombPert$VCV[,4])
  ExpdiffSpatCombPertNC<-(2/sqrt(pi))*sqrt(mSpatCombPert$VCV[,5])
  
  ConsistencySpatCombPertN<-mSpatCombPert$VCV[,1]/(mSpatCombPert$VCV[,1]+mSpatCombPert$VCV[,4])
  ConsistencySpatCombPertNC<-mSpatCombPert$VCV[,2]/(mSpatCombPert$VCV[,2]+mSpatCombPert$VCV[,5])
}

#Spatial variation model separately for non-birds with combined NC categories
{
  mSpatCombOther <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                             mev = dOther$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dOther)
  
  ExpdiffSpatCombOtherN <-(2/sqrt(pi))*sqrt(mSpatCombOther$VCV[,4])
  ExpdiffSpatCombOtherNC<-(2/sqrt(pi))*sqrt(mSpatCombOther$VCV[,5])
  
  ConsistencySpatCombOtherN<-mSpatCombOther$VCV[,1]/(mSpatCombOther$VCV[,1]+mSpatCombOther$VCV[,4])
  ConsistencySpatCombOtherNC<-mSpatCombOther$VCV[,2]/(mSpatCombOther$VCV[,2]+mSpatCombOther$VCV[,5])
}

#Spatial variation model with estimates from same study with combined NC categories
{
  mSpatCombSame <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dSame$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dSame)
  
  ExpdiffSpatCombSameN <-(2/sqrt(pi))*sqrt(mSpatCombSame$VCV[,4])
  ExpdiffSpatCombSameNC<-(2/sqrt(pi))*sqrt(mSpatCombSame$VCV[,5])
  
  ConsistencySpatCombSameN<-mSpatCombSame$VCV[,1]/(mSpatCombSame$VCV[,1]+mSpatCombSame$VCV[,4])
  ConsistencySpatCombSameNC<-mSpatCombSame$VCV[,2]/(mSpatCombSame$VCV[,2]+mSpatCombSame$VCV[,5])
}