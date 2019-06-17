#Initial Setup
{
  #Libraries
  {
    library(MCMCglmm)
    library(readxl)
  }
  
  #Dataset creation
  {
    d<-read_excel("")
    d<-subset(d,d$Borderline.ambiguous.case!=1)
    
    #Calculation of sampling variance from reported standard errors
    d$SE<-d$Grad.linear.StErr^2
    
    #Subgroups for analysis
    {
      dPert<- subset(d, d$Physical.Artifact!="0")
      dChoice<- subset(d, d$Physical.Artifact!="1")
      dBirds<-subset(d, d$Birds=="1")
      dOther<-subset(d, d$Birds=="0")
      dNoInvert<-subset(d,d$Taxon.group!='I')
      dSame<-subset(d, d$Same.Study=="1")
      dPhen<-subset(d,d$Trait.Class!='Phen')
      dS<-subset(d,d$Other.Org.NC!=1)
      dO<-subset(d,d$Other.Org.NC!=0)
      dSing<-subset(d,d$Other.Org.NC!=1)
      dMult<-subset(d,d$Other.Org.NC!=0)
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

#Temporal variation model with combined niche construction categories
{
  #Primary analysis
  mTempComb <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                        mev = d$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = d)
 
  #Expected differences between two years from the same subset
  ExpdiffTempCombN <-(2/sqrt(pi))*sqrt(mTempComb$VCV[,4])
  ExpdiffTempCombNC<-(2/sqrt(pi))*sqrt(mTempComb$VCV[,5])
  
  #Consistency
  ConsistencyTempCombN<-mTempComb$VCV[,1]/(mTempComb$VCV[,1]+mTempComb$VCV[,4])
  ConsistencyTempCombNC<-mTempComb$VCV[,2]/(mTempComb$VCV[,2]+mTempComb$VCV[,5])
}

#Temporal variation model with separate NC categories
{
  mTempSep <- MCMCglmm(Grad.linear.value ~ NC, random = ~idh(NC):Subset, rcov = ~idh(NC):units,
                       mev = d$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorSep, data = d)
  
  ExpdiffTempSepM<- (2/sqrt(pi))*sqrt(mTempSep$VCV[,5])
  ExpdiffTempSepN<- (2/sqrt(pi))*sqrt(mTempSep$VCV[,6])
  ExpdiffTempSepY<- (2/sqrt(pi))*sqrt(mTempSep$VCV[,7])
  
  ConsistencyTempSepM<-mTempSep$VCV[,1]/(mTempSep$VCV[,1]+mTempSep$VCV[,5])
  ConsistencyTempSepN<-mTempSep$VCV[,2]/(mTempSep$VCV[,2]+mTempSep$VCV[,6])
  ConsistencyTempSepY<-mTempSep$VCV[,3]/(mTempSep$VCV[,3]+mTempSep$VCV[,7])
}

#Temporal variation model with combined NC categories and physical artifacts only
{
  mTempCombPert <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dPert$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dPert)
  
  ExpdiffTempCombPertN <-(2/sqrt(pi))*sqrt(mTempCombPert$VCV[,4])
  ExpdiffTempCombPertNC<-(2/sqrt(pi))*sqrt(mTempCombPert$VCV[,5])
  
  ConsistencyTempCombPertN<-mTempCombPert$VCV[,1]/(mTempCombPert$VCV[,1]+mTempCombPert$VCV[,4])
  ConsistencyTempCombPertNC<-mTempCombPert$VCV[,2]/(mTempCombPert$VCV[,2]+mTempCombPert$VCV[,5])
}

#Temporal variation model with separate NC categories and physical artifacts only
{
  mTempSepPert <- MCMCglmm(Grad.linear.value ~ NC, random = ~idh(NC):Subset, rcov = ~idh(NC):units,
                           mev = dPert$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorSep, data = dPert)
 
  ExpdiffTempSepPertM<- (2/sqrt(pi))*sqrt(mTempSepPert$VCV[,5])
  ExpdiffTempSepPertN<- (2/sqrt(pi))*sqrt(mTempSepPert$VCV[,6])
  ExpdiffTempSepPertY<- (2/sqrt(pi))*sqrt(mTempSepPert$VCV[,7])
  
  ConsistencyTempSepPertM<-mTempSepPert$VCV[,1]/(mTempSepPert$VCV[,1]+mTempSepPert$VCV[,5])
  ConsistencyTempSepPertN<-mTempSepPert$VCV[,2]/(mTempSepPert$VCV[,2]+mTempSepPert$VCV[,6])
  ConsistencyTempSepPertY<-mTempSepPert$VCV[,3]/(mTempSepPert$VCV[,3]+mTempSepPert$VCV[,7])
}

#Temporal variation model separately for birds and other groups with combined NC categories
{
  mTempCombBirds <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                             mev = dBirds$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dBirds)
 
  mTempCombOther <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                             mev = dOther$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dOther)
 
  ExpdiffTempCombBirdsN <-(2/sqrt(pi))*sqrt(mTempCombBirds$VCV[,4])
  ExpdiffTempCombBirdsNC<-(2/sqrt(pi))*sqrt(mTempCombBirds$VCV[,5])
  ExpdiffTempCombOtherN <-(2/sqrt(pi))*sqrt(mTempCombOther$VCV[,4])
  ExpdiffTempCombOtherNC<-(2/sqrt(pi))*sqrt(mTempCombOther$VCV[,5])
  
  ConsistencyTempCombBirdsN<-mTempCombBirds$VCV[,1]/(mTempCombBirds$VCV[,1]+mTempCombBirds$VCV[,4])
  ConsistencyTempCombBirdsNC<-mTempCombBirds$VCV[,2]/(mTempCombBirds$VCV[,2]+mTempCombBirds$VCV[,5])
  ConsistencyTempCombOtherN<-mTempCombOther$VCV[,1]/(mTempCombOther$VCV[,1]+mTempCombOther$VCV[,4])
  ConsistencyTempCombOtherNC<-mTempCombOther$VCV[,2]/(mTempCombOther$VCV[,2]+mTempCombOther$VCV[,5])
}

#Temporal variation model separately for non-birds with separate NC categories
{
  mTempSepOther <- MCMCglmm(Grad.linear.value ~ NC, random = ~idh(NC):Subset, rcov = ~idh(NC):units,
                            mev = dOther$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorSep, data = dOther)
  ExpdiffTempSepOtherM<- (2/sqrt(pi))*sqrt(mTempSepOther$VCV[,5])
  ExpdiffTempSepOtherN<- (2/sqrt(pi))*sqrt(mTempSepOther$VCV[,6])
  ExpdiffTempSepOtherY<- (2/sqrt(pi))*sqrt(mTempSepOther$VCV[,7])
  
  ConsistencyTempSepOtherM<-mTempSepOther$VCV[,1]/(mTempSepOther$VCV[,1]+mTempSepOther$VCV[,5])
  ConsistencyTempSepOtherN<-mTempSepOther$VCV[,2]/(mTempSepOther$VCV[,2]+mTempSepOther$VCV[,6])
  ConsistencyTempSepOtherY<-mTempSepOther$VCV[,3]/(mTempSepOther$VCV[,3]+mTempSepOther$VCV[,7])
}

#Temporal variation model for no invertebrates for combined NC categories
{
  mTempCombIn <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                          mev = dNoInvert$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dNoInvert)
  
  ExpdiffTempCombInN <-(2/sqrt(pi))*sqrt(mTempCombIn$VCV[,4])
  ExpdiffTempCombInNC<-(2/sqrt(pi))*sqrt(mTempCombIn$VCV[,5])

  ConsistencyTempCombInN<-mTempCombIn$VCV[,1]/(mTempCombIn$VCV[,1]+mTempCombIn$VCV[,4])
  ConsistencyTempCombInNC<-mTempCombIn$VCV[,2]/(mTempCombIn$VCV[,2]+mTempCombIn$VCV[,5])
}

#Temporal variation model for no invertebrates for separate NC categories
{
  mTempSepIn <- MCMCglmm(Grad.linear.value ~ NC, random = ~idh(NC):Subset, rcov = ~idh(NC):units,
                         mev = dNoInvert$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorSep, data = dNoInvert)
  
  ExpdiffTempSepInM<- (2/sqrt(pi))*sqrt(mTempSepIn$VCV[,5])
  ExpdiffTempSepInN<- (2/sqrt(pi))*sqrt(mTempSepIn$VCV[,6])
  ExpdiffTempSepInY<- (2/sqrt(pi))*sqrt(mTempSepIn$VCV[,7])
  
  ConsistencyTempSepInM<-mTempSepIn$VCV[,1]/(mTempSepIn$VCV[,1]+mTempSepIn$VCV[,5])
  ConsistencyTempSepInN<-mTempSepIn$VCV[,2]/(mTempSepIn$VCV[,2]+mTempSepIn$VCV[,6])
  ConsistencyTempSepInY<-mTempSepIn$VCV[,3]/(mTempSepIn$VCV[,3]+mTempSepIn$VCV[,7])
}

#Temporal variation model with estimates from same study with combined NC categories
{
  mTempCombSame <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dSame$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dSame)
  
  ExpdiffTempCombSameN <-(2/sqrt(pi))*sqrt(mTempCombSame$VCV[,4])
  ExpdiffTempCombSameNC<-(2/sqrt(pi))*sqrt(mTempCombSame$VCV[,5])
  
  ConsistencyTempCombSameN<-mTempCombSame$VCV[,1]/(mTempCombSame$VCV[,1]+mTempCombSame$VCV[,4])
  ConsistencyTempCombSameNC<-mTempCombSame$VCV[,2]/(mTempCombSame$VCV[,2]+mTempCombSame$VCV[,5])
}

#Without phenology analysis for combined NC categories
{
  mTempCombPhen <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dPhen$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dPhen)
 
  ExpdiffTempCombPhenN <-(2/sqrt(pi))*sqrt(mTempCombPhen$VCV[,4])
  ExpdiffTempCombPhenNC<-(2/sqrt(pi))*sqrt(mTempCombPhen$VCV[,5])

  ConsistencyTempCombPhenN<-mTempCombPhen$VCV[,1]/(mTempCombPhen$VCV[,1]+mTempCombPhen$VCV[,4])
  ConsistencyTempCombPhenNC<-mTempCombPhen$VCV[,2]/(mTempCombPhen$VCV[,2]+mTempCombPhen$VCV[,5])
}

#Without phenology analysis for separate NC categories
{
  mTempSepPhen <- MCMCglmm(Grad.linear.value ~ NC, random = ~idh(NC):Subset, rcov = ~idh(NC):units,
                           mev = dPhen$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorSep, data = dPhen)
 
  ExpdiffTempSepPhenM<- (2/sqrt(pi))*sqrt(mTempSepPhen$VCV[,5])
  ExpdiffTempSepPhenN<- (2/sqrt(pi))*sqrt(mTempSepPhen$VCV[,6])
  ExpdiffTempSepPhenY<- (2/sqrt(pi))*sqrt(mTempSepPhen$VCV[,7])
  
  ConsistencyTempSepPhenM<-mTempSepPhen$VCV[,1]/(mTempSepPhen$VCV[,1]+mTempSepPhen$VCV[,5])
  ConsistencyTempSepPhenN<-mTempSepPhen$VCV[,2]/(mTempSepPhen$VCV[,2]+mTempSepPhen$VCV[,6])
  ConsistencyTempSepPhenY<-mTempSepPhen$VCV[,3]/(mTempSepPhen$VCV[,3]+mTempSepPhen$VCV[,7])
}

#Temporal variation model with combined NC categories and choice only
{
  mTempCombChoice <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                              mev = dChoice$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dChoice)
  
  ExpdiffTempCombChoiceN <-(2/sqrt(pi))*sqrt(mTempCombChoice$VCV[,4])
  ExpdiffTempCombChoiceNC<-(2/sqrt(pi))*sqrt(mTempCombChoice$VCV[,5])
  
  ConsistencyTempCombChoiceN<-mTempCombChoice$VCV[,1]/(mTempCombChoice$VCV[,1]+mTempCombChoice$VCV[,4])
  ConsistencyTempCombChoiceNC<-mTempCombChoice$VCV[,2]/(mTempCombChoice$VCV[,2]+mTempCombChoice$VCV[,5])
}

#Temporal variation model with combined niche construction categories and self vs. other
{
  mTempCombO <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                         mev = dO$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dO)
  
  mTempCombS <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                         mev = dS$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dS)
  
  OExpdiffTempCombN <-(2/sqrt(pi))*sqrt(mTempCombO$VCV[,4])
  OExpdiffTempCombNC<-(2/sqrt(pi))*sqrt(mTempCombO$VCV[,5])
  SExpdiffTempCombN <-(2/sqrt(pi))*sqrt(mTempCombS$VCV[,4])
  SExpdiffTempCombNC<-(2/sqrt(pi))*sqrt(mTempCombS$VCV[,5])

  OConsistencyTempCombN<-mTempCombO$VCV[,1]/(mTempCombO$VCV[,1]+mTempCombO$VCV[,4])
  OConsistencyTempCombNC<-mTempCombO$VCV[,2]/(mTempCombO$VCV[,2]+mTempCombO$VCV[,5])
  SConsistencyTempCombN<-mTempCombS$VCV[,1]/(mTempCombS$VCV[,1]+mTempCombS$VCV[,4])
  SConsistencyTempCombNC<-mTempCombS$VCV[,2]/(mTempCombS$VCV[,2]+mTempCombS$VCV[,5])
}

#Temporal variation model with combined niche construction categories and single vs. multiple
{
  mTempCombSing <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dSing$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dSing)
  
  mTempCombMult <- MCMCglmm(Grad.linear.value ~ CombNC, random = ~idh(CombNC):Subset, rcov = ~idh(CombNC):units,
                            mev = dMult$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorComb, data = dMult)
  
  SingExpdiffTempCombN <-(2/sqrt(pi))*sqrt(mTempCombSing$VCV[,4])
  SingExpdiffTempCombNC<-(2/sqrt(pi))*sqrt(mTempCombSing$VCV[,5])
  MultExpdiffTempCombN <-(2/sqrt(pi))*sqrt(mTempCombMult$VCV[,4])
  MultExpdiffTempCombNC<-(2/sqrt(pi))*sqrt(mTempCombMult$VCV[,5])

  SingConsistencyTempCombN<-mTempCombSing$VCV[,1]/(mTempCombSing$VCV[,1]+mTempCombSing$VCV[,4])
  SingConsistencyTempCombNC<-mTempCombSing$VCV[,2]/(mTempCombSing$VCV[,2]+mTempCombSing$VCV[,5])
  MultConsistencyTempCombN<-mTempCombMult$VCV[,1]/(mTempCombMult$VCV[,1]+mTempCombMult$VCV[,4])
  MultConsistencyTempCombNC<-mTempCombMult$VCV[,2]/(mTempCombMult$VCV[,2]+mTempCombMult$VCV[,5])
}
