#Libraries
{
  library('rethinking')
  library(scales)
}

#Temporal plotting functions
{
  Plot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.015)
    yr <- c(0,1950)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "", xlab="", ylab="", cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}
#Temporal expected difference plotting functions
{
  ExpPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.15)
    yr <- c(0,110)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "", xlab="", ylab="", cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}
#Temporal consistency plotting function
{
  ConPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,1)
    yr <- c(0,20)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "",xlab="",cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}

#Spatial plotting function
{
  SpatPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.015)
    yr <- c(0,630)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "",cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}
#Spatial expected difference plotting function
{
  ExpSpatPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.15)
    yr <- c(0,50)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "",cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}
#Spatial consistency plotting function
{
  SpatConPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,1)
    yr <- c(0,3)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "",cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}

#Magnitude plotting function
{
  Newplot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.35)
    yr <- range(junk.y)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "")
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}

#Narrow plotting function
{
  RobNarPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.015)
    yr <- c(0,2000)
    plot(density(s[[1]]), xlim = xr, ylim = yr, xlab = "", ylab = "", main = "",cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}
#Broad plotting function
{
  RobBroPlot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s))
    {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(0,.015)
    yr <- c(0,650)
    plot(density(s[[1]]), xlim = xr, ylim = yr, xlab = "", ylab = "", main = "",cex.axis=1.2)
    
    if(length(s)==2)
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "red", lty = 2,lwd=2.5)
      }
    }
    else
    {
      for(i in 1)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = 'black',lwd=2.5)
      }
      for(i in 2)
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "cornflowerblue", lty = 3,lwd=2.5)
      }
      for(i in 3:length(s))
      {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = "green", lty = 5,lwd=2.5)
      }
    }
  }
}

#Temporal graph
{
  dev.new(width=9, height=7.5, noRStudioGD = TRUE)
  par(mfrow = c(3,3),
      oma = c(1,4,0,0) + 0.5,
      mar = c(4,0,2,2) + 0.8)
  
  #Plots A-F
  {
    Plot.multi.dens(list(mTempComb$VCV[,4],mTempComb$VCV[,5]))
    segments(mean(mTempComb$VCV[,4]),1725,mean(mTempComb$VCV[,5]),1725)
    text((mean(mTempComb$VCV[,4])+mean(mTempComb$VCV[,5]))/2,1800,"a",bty = "n",cex = 1.2)
    title(xlab="Temporal variation",cex.lab=1.7)
    title(main="A",adj=0,cex.main=1.5,font.main=1,line=.6)
    ExpPlot.multi.dens(list(ExpdiffTempCombN,ExpdiffTempCombNC))
    segments(mean(ExpdiffTempCombN),100,mean(ExpdiffTempCombNC),100)
    text((mean(ExpdiffTempCombN)+mean(ExpdiffTempCombNC))/2,104,"a",bty = "n",cex = 1.2)
    title(xlab="Exp. diff. btw. random years",cex.lab=1.7)
    title(main="B",adj=0,cex.main=1.5,font.main=1,line=.6)
    ConPlot.multi.dens(list(ConsistencyTempCombN,ConsistencyTempCombNC))
    title(xlab="Consistency",cex.lab=1.7)
    title(main="C",adj=0,cex.main=1.5,font.main=1,line=.6)
    
    Plot.multi.dens(list(mTempSep$VCV[,6],mTempSep$VCV[,5],mTempSep$VCV[,7]))
    segments(mean(mTempSep$VCV[,5])-.0001,1710,mean(mTempSep$VCV[,7])-.0001,1710)
    segments((mean(mTempSep$VCV[,5])+mean(mTempSep$VCV[,7]))/2-.0001,1710,(mean(mTempSep$VCV[,5])+mean(mTempSep$VCV[,7]))/2-.0001,1760)
    segments((mean(mTempSep$VCV[,5])+mean(mTempSep$VCV[,7]))/2-.0001,1760,mean(mTempSep$VCV[,6]),1760)
    text(((mean(mTempSep$VCV[,5])+mean(mTempSep$VCV[,7]))/2+mean(mTempSep$VCV[,6]))/2,1835,"a",bty = "n",cex = 1.2)
    title(xlab="Temporal variation",cex.lab=1.7)
    title(main="D",adj=0,cex.main=1.5,font.main=1,line=.6)
    ExpPlot.multi.dens(list(ExpdiffTempSepN,ExpdiffTempSepM,ExpdiffTempSepY))
    segments(mean(ExpdiffTempSepM)-.001,80,mean(ExpdiffTempSepY)-.0001,80)
    segments((mean(ExpdiffTempSepM)+mean(ExpdiffTempSepY))/2-.0001,80,(mean(ExpdiffTempSepM)+mean(ExpdiffTempSepY))/2-.0001,83)
    segments((mean(ExpdiffTempSepM)+mean(ExpdiffTempSepY))/2-.0001,83,mean(ExpdiffTempSepN),83)
    text(((mean(ExpdiffTempSepM)+mean(ExpdiffTempSepY))/2+mean(ExpdiffTempSepN))/2,87,"a",bty = "n",cex = 1.2)
    title(xlab="Exp. diff. bet. random years",cex.lab=1.7)
    title(main="E",adj=0,cex.main=1.5,font.main=1,line=.6)
    ConPlot.multi.dens(list(ConsistencyTempSepN,ConsistencyTempSepM,ConsistencyTempSepY))
    segments(mean(ConsistencyTempSepY)+.03,17,mean(ConsistencyTempSepM)+.01,17)
    text((mean(ConsistencyTempSepY)+mean(ConsistencyTempSepM))/2+.015,18,"b",bty = "n",cex = 1.2)
    title(xlab="Consistency",cex.lab=1.7)
    title(main="F",adj=0,cex.main=1.5,font.main=1,line=.6)
    mtext(text="Posterior density",side=2,line=2,outer=TRUE,cex=1.5)
  }
  
  #Plots G-I
  {
    # Extract samples from posterior
    SamplesAut <- extract.samples(mAut)
    SamplesNC <- extract.samples(mNC)
    
    # Calculate densities
    DensNC<- density((1/SamplesNC$lambda)^2)
    DensAut<- density((1/SamplesAut$lambda)^2)
    
    # Calculate 99% CIs
    NC_99<-c(quantile(1/SamplesNC$lambda,c(.005,.999)))
    Aut_99<-c(quantile(1/SamplesAut$lambda,c(.005,.999)))
    
    NC_99_2<-c(quantile((1/SamplesNC$lambda)^2,c(.005,.999)))
    Aut_99_2<-c(quantile((1/SamplesAut$lambda)^2,c(.005,.999)))
    
    #G.
    plot(DensNC, xlim=c(0,0.015), ylim=c(0,1900),col="red",lty=2, lwd=2.5,main="",xlab="",ylab="",cex.axis=1.2)
    par(new=TRUE)
    lines(DensAut, col="black",lwd=2.5,main="",xlab="",ylab="",cex.axis=1.2)
    segments(.000634,1750,.0057,1750)
    text(.003167,1825,"a",bty = "n",cex = 1.2)
    title(xlab="Exp. temporal variation",cex.lab=1.7)
    title(main="G",adj=0,cex.main=1.5,font.main=1,line=.6)
    
    #H.
    plot(0, xlim=c(0,0.5),ylim=c(0,100), col="red", lwd=3, ylab="", xlab="",type="n",cex.axis=1.2)
    for (x in 1:ncol(SamplesNC$b)) {
      lines(density(SamplesNC$b[,x]),col=alpha("red",alpha=.2),ldw=1)
    }
    title(xlab="Subset-specific variation",cex.lab=1.7)
    title(main="H",adj=0,cex.main=1.5,font.main=1,line=.6)
    
    #I.
    plot(0, xlim=c(0,0.5),ylim=c(0,100), col="red", lwd=3, ylab="", xlab="",type="n",cex.axis=1.2)
    for (x in 1:ncol(SamplesAut$b)) {
      lines(density(SamplesAut$b[,x]), col=alpha("black", alpha=0.2),ldw=1)
      par(new=TRUE)
    }
    
    #Misc. labels
    title(xlab="Subset-specific variation",cex.lab=1.7)
    title(main="I",family="Monaco",adj=0,cex.main=1.5,font.main=1,line=.6)
    legend("topright",legend=c("Non-constructed","Combined","Mixed","Constructed"), bty = "n",col=c('black','red', 'cornflowerblue','green'), lty=c(1,2,3,5),cex = 1.2,lwd=2)
  }
}

#Spatial graph
{
  dev.new(width=9, height=6, noRStudioGD = TRUE)
  par(mfrow = c(2,3),
      oma = c(5,4,0,0) + 0.5,
      mar = c(0,0,3,3) + 0.8)
  
  SpatPlot.multi.dens(list(mSpatComb$VCV[,4],mSpatComb$VCV[,5]))
  title(main="A",adj=0,cex.main=1.5,font.main=1,line=.6)
  legend("topright",legend=c("Non-constructed","Combined","Mixed","Constructed"), bty = "n",col=c('black','red', 'cornflowerblue','green'), lty=c(1,2,3,5),cex = 1.2,lwd=2)
  ExpSpatPlot.multi.dens(list(ExpdiffSpatCombN,ExpdiffSpatCombNC))
  title(main="B",adj=0,cex.main=1.5,font.main=1,line=.6)
  SpatConPlot.multi.dens(list(ConsistencySpatCombN,ConsistencySpatCombNC))
  title(main="C",adj=0,cex.main=1.5,font.main=1,line=.6)
  SpatPlot.multi.dens(list(mSpatSep$VCV[,6],mSpatSep$VCV[,5],mSpatSep$VCV[,7]))
  title(main="D",adj=0,cex.main=1.5,font.main=1,line=.6)
  ExpSpatPlot.multi.dens(list(ExpdiffSpatSepN,ExpdiffSpatSepM,ExpdiffSpatSepY))
  title(main="E",adj=0,cex.main=1.5,font.main=1,line=.6)
  SpatConPlot.multi.dens(list(ConsistencySpatSepN,ConsistencySpatSepM,ConsistencySpatSepY))
  title(main="F",adj=0,cex.main=1.5,font.main=1,line=.6)
  mtext(text="Posterior density",side=2,line=2,outer=TRUE,cex=1.2)
  mtext(text="Spatial variation",side=1,line=2,outer=TRUE,at=.15,cex=1.2)
  mtext(text="Exp. diff. btw. random locations",side=1,line=2,outer=TRUE,at=.495,cex=1.2)
  mtext(text="Consistency",side=1,line=2,outer=TRUE,at=.825,cex=1.2)
}

#Magnitude full v No phenology
{
  dev.new(width=7, height=7, noRStudioGD = TRUE)
  par(mfrow = c(2,2),
      oma = c(5,4,0,0) + 0.5,
      mar = c(0,0,3,3) + 0.8)
  Newplot.multi.dens(list(post.meanabsComb10[,1],post.meanabsComb10[,2]))
  title(main="A",adj=0,cex.main=1.5,font.main=1,line=.6)
  legend("topright",legend=c("Non-constructed","Combined","Mixed","Constructed"), bty = "n",col=c('black','red', 'cornflowerblue','green'), lty=c(1,2,3,5),cex = .85,lwd=2)
  Newplot.multi.dens(list(post.meanabsSep10[,1],post.meanabsSep10[,2],post.meanabsSep10[,3]))
  title(main="B",adj=0,cex.main=1.5,font.main=1,line=.6)
  Newplot.multi.dens(list(post.meanabsComb11[,1],post.meanabsComb11[,2]))
  title(main="C",adj=0,cex.main=1.5,font.main=1,line=.6)
  Newplot.multi.dens(list(post.meanabsSep11[,1],post.meanabsSep11[,2],post.meanabsSep11[,3]))
  title(main="D",adj=0,cex.main=1.5,font.main=1,line=.6)
  mtext(text="Posterior density",cex=1.2,side=2,line=2,outer=TRUE)
  mtext(text="Mean magnitude of selection",cex=1.2,side=1,line=2,outer=TRUE)
}

#Broad v narrow graph
{
  dev.new(width=9, height=6, noRStudioGD = TRUE)
  par(mfrow = c(2,3),
      oma = c(5,4,0,0) + 0.5,
      mar = c(0,0,2.75,4.5) + 0.8)
  
  RobNarPlot.multi.dens(list(mTempCombPert$VCV[,4],mTempCombPert$VCV[,5]))
  title(main="A",adj=0,cex.main=1.5,font.main=1,line=.6)
  title(main="(i)",adj=0.07,cex.main=1.2,font.main=1,line=.6)
  legend(.001,2150,"Physical Artifacts",text.font=2,bty = "n",cex = 1.2)
  segments(mean(mTempCombPert$VCV[,4]),1650,mean(mTempCombPert$VCV[,5]),1650)
  text((mean(mTempCombPert$VCV[,4])+mean(mTempCombPert$VCV[,5]))/2,1725,"a",bty = "n",cex = 1.2)
  RobNarPlot.multi.dens(list(mTempCombS$VCV[,4],mTempCombS$VCV[,5]))
  title(main="B",adj=0,cex.main=1.5,font.main=1,line=.6)
  title(main="(i)",adj=0.07,cex.main=1.2,font.main=1,line=.6)
  legend(.0012,2150,"Self-constructed",text.font=2,bty = "n",cex = 1.2)
  segments(mean(mTempCombS$VCV[,4]),1665,mean(mTempCombS$VCV[,5]),1665)
  text((mean(mTempCombS$VCV[,4])+mean(mTempCombS$VCV[,5]))/2,1740,"a",bty = "n",cex = 1.2)
  RobNarPlot.multi.dens(list(mTempCombSing$VCV[,4],mTempCombSing$VCV[,5]))
  title(main="C",adj=0,cex.main=1.5,font.main=1,line=.6)
  title(main="(i)",adj=0.07,cex.main=1.2,font.main=1,line=.6)
  legend(.0018,2150,"Single species",text.font=2,bty = "n",cex = 1.2)
  segments(mean(mTempCombSing$VCV[,4]),1665,mean(mTempCombSing$VCV[,5]),1665)
  text((mean(mTempCombSing$VCV[,4])+mean(mTempCombSing$VCV[,5]))/2,1740,"a",bty = "n",cex = 1.2)
  legend("right",legend=c("Non-constructed","Combined"), bty = "n",col=c('black','red'), lty=c(1,2),cex = 1.2,lwd=2)
  RobBroPlot.multi.dens(list(mTempCombChoice$VCV[,4],mTempCombChoice$VCV[,5]))
  title(main="(ii)",adj=0.07,cex.main=1.2,font.main=1,line=.6)
  segments(mean(mTempCombChoice$VCV[,4]),550,mean(mTempCombChoice$VCV[,5]-.0007),550)
  text((mean(mTempCombChoice$VCV[,4])+mean(mTempCombChoice$VCV[,5]))/2-.00035,575,"a",bty = "n",cex = 1.2)
  legend(.0037,695,"Choice",text.font=2,bty = "n",cex = 1.2)
  RobBroPlot.multi.dens(list(mTempCombO$VCV[,4],mTempCombO$VCV[,5]))
  title(main="(ii)",adj=0.07,cex.main=1.2,font.main=1,line=.6)
  segments(mean(mTempCombO$VCV[,4]),550,mean(mTempCombO$VCV[,5]-.0005),550)
  text((mean(mTempCombO$VCV[,4])+mean(mTempCombO$VCV[,5]))/2,575,"a",bty = "n",cex = 1.2)
  legend(.001,695,"Other-constructed",text.font=2,bty = "n",cex = 1.2)
  RobBroPlot.multi.dens(list(mTempCombMult$VCV[,4],mTempCombMult$VCV[,5]))
  title(main="(ii)",adj=0.07,cex.main=1.2,font.main=1,line=.6)
  segments(mean(mTempCombMult$VCV[,4]),555,mean(mTempCombMult$VCV[,5]-.0005),555)
  text((mean(mTempCombMult$VCV[,4])+mean(mTempCombMult$VCV[,5]))/2-.00025,580,"a",bty = "n",cex = 1.2)
  legend(.0014,695,"Multiple species",text.font=2,bty = "n",cex = 1.2)
  mtext(text="Posterior density",side=2,line=2,outer=TRUE,cex=1.2)
  mtext(text="Temporal variation",side=1,line=2,outer=TRUE,cex=1.2)
}
