#Data import and preparation#
{
  #Libraries
  {
    library(MCMCglmm)
    library(readxl)
  }
  
  #Dataset creation
  {
    d<-read_excel("")
    d<-subset(d,d$Spatial.Subset!="NA")
    d<-subset(d,d$Temporal.Subset!="NA")
    d<-subset(d,d$Borderline.ambiguous.case!=1)
    
    #Calculation of sampling variance from reported standard errors
    d$SE<-d$Grad.linear.StErr^2
    
    #Subgroups for analysis
    {
    dM<-subset(d, d$NC=="M")
    dN<-subset(d, d$NC=="N")
    dY<-subset(d, d$NC=="Y")
    dComb<-subset(d, d$CombNC=="Y")
    
    dNoPhen<-subset(d,d$Trait.Class!='Phen')
    dNPN<-subset(dNoPhen,dNoPhen$NC=="N")
    dNPM<-subset(dNoPhen,dNoPhen$NC=="M")
    dNPY<-subset(dNoPhen,dNoPhen$NC=="Y")
    dNPComb<-subset(dNoPhen,dNoPhen$CombNC=="Y")
    }
  }
  
  #Folded normal distribution#
  {
    mu.fnorm<-function(mu,sigma){ sigma*sqrt(2/pi)*exp((-1*mu^2)/(2*sigma^2))+mu*(1-2*pnorm(-1*mu/sigma,0,1))}
  }
  
  #Priors
  {
    priorUsed <- list(G = list(G1 = list(V = diag(1),nu = 0.002), G2 = list(V = diag(1),nu = 0.002), G3 = list(V = diag(1),nu = 0.002), G4 = list(V = diag(1),nu = 0.002)),R = list(V = diag(1),nu = 0.002))
  }
}

#Strength of selection analysis
{
  #MCMCglmm
  mStrengthM10<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dM$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorUsed,  data = dM)
  mStrengthN10<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dN$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorUsed,  data = dN)
  mStrengthY10<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dY$SE, nitt = 120000, thin = 10, burnin = 20000,  prior = priorUsed, data = dY)
  mStrengthComb10<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dComb$SE, nitt = 120000, thin = 10, burnin = 20000,  prior = priorUsed, data = dComb)
  
  #Folded normal distributions defined by model parameter
  post.meanabsComb10<-array(dim=c(1000,2))
  for(i in 1:1000){
    post.meanabsComb10[i,1]<-mu.fnorm(mu=mStrengthN10$Sol[i,1],sigma=sqrt(mStrengthN10$VCV[i,6]))
    post.meanabsComb10[i,2]<-mu.fnorm(mu=mStrengthComb10$Sol[i,1],sigma=sqrt(mStrengthComb10$VCV[i,6]))
  }
  
  post.meanabsSep10<-array(dim=c(1000,3))
  for(i in 1:1000){
    post.meanabsSep10[i,1]<-mu.fnorm(mu=mStrengthN10$Sol[i,1],sigma=sqrt(mStrengthN10$VCV[i,6]))
    post.meanabsSep10[i,2]<-mu.fnorm(mu=mStrengthM10$Sol[i,1],sigma=sqrt(mStrengthM10$VCV[i,6]))
    post.meanabsSep10[i,3]<-mu.fnorm(mu=mStrengthY10$Sol[i,1],sigma=sqrt(mStrengthY10$VCV[i,6]))
  }
}

#Strength of selection analysis with no phenology
{
  mStrengthM11<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dNPM$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorUsed,  data = dNPM)
  mStrengthN11<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dNPN$SE, nitt = 120000, thin = 10, burnin = 20000, prior = priorUsed,  data = dNPN)
  mStrengthY11<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dNPY$SE, nitt = 120000, thin = 10, burnin = 20000,  prior = priorUsed, data = dNPY)
  mStrengthComb11<- MCMCglmm(Grad.linear.value~1, random=~Study.ID + Species + Fitness.Measure.Defined + Trait.Name, mev = dNPComb$SE, nitt = 120000, thin = 10, burnin = 20000,  prior = priorUsed, data = dNPComb)
  
  post.meanabsComb11<-array(dim=c(1000,2))
  for(i in 1:1000){
    post.meanabsComb11[i,1]<-mu.fnorm(mu=mStrengthN11$Sol[i,1],sigma=sqrt(mStrengthN11$VCV[i,6]))
    post.meanabsComb11[i,2]<-mu.fnorm(mu=mStrengthComb11$Sol[i,1],sigma=sqrt(mStrengthComb11$VCV[i,6]))
  }
  
  post.meanabsSep11<-array(dim=c(1000,3))
  for(i in 1:1000){
    post.meanabsSep11[i,1]<-mu.fnorm(mu=mStrengthN11$Sol[i,1],sigma=sqrt(mStrengthN11$VCV[i,6]))
    post.meanabsSep11[i,2]<-mu.fnorm(mu=mStrengthM11$Sol[i,1],sigma=sqrt(mStrengthM11$VCV[i,6]))
    post.meanabsSep11[i,3]<-mu.fnorm(mu=mStrengthY11$Sol[i,1],sigma=sqrt(mStrengthY11$VCV[i,6]))
  }
}
