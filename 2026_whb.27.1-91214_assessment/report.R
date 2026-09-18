## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(stockassessment)
library(tidyverse)

mkdir("report")

# Function to open sAM data files from boot/data folder
read.ices.taf <- function(...) {
  read.ices(taf.data.path(...))
}

# Load survey data
surveys             <-read.ices.taf("survey.dat")
surveys$IBWSS       <- surveys$IBWSS[-nrow(surveys$IBWSS),]
attributes(surveys$IBWSS)$time <- attributes(surveys$IBWSS)$time #add again attribute
attributes(surveys$IBWSS)$twofirst <- attributes(surveys$IBWSS)$twofirst #add again attribute

# Load model runs and forecast results
load("model/model_base.RData")
load("model/model_forecast.RData")
load("forecast/forecast.RData")

# Load data for forecast
load("data/data_forecast.RData")

# Load reporting functions
source("common.R")
source("MV_functions.R")

# Set assessment year and TAC year
assessmentYear      <- fit.base$data$years[length(fit.base$data$years)]
TACyear             <- assessmentYear + 1


############################################################################################################-
# Base model run ----

# Set base run as 'main' fit
fit <- fit.base

# Run script that makes first large bunch of plots and tables for the report
source("plotscript.R")

# Table parameter estimates
CommonThings(fit.base)
fit$data$surveys  <- surveys  # MV addition
tp<-tableParameters(fit.base)
print(round(tp,2),na.print = "")
xtab(tp, caption=paste('Table 11. Parameter estimates','.',sep=''), cornername='Parameter \ Year', 
     file=file.path("report",paste(stamp,'_tab11a.html',sep='')), dec=rep(2,ncol(ftab)))

# Table parameter estimates last 5 years based on retrospective analysis
a<-cbind(
  tableParameters(RETRO[[4]],doCV=FALSE),
  tableParameters(RETRO[[3]],doCV=FALSE),
  tableParameters(RETRO[[2]],doCV=FALSE),
  tableParameters(RETRO[[1]],doCV=FALSE),
  tableParameters(fit,doCV=FALSE))

colnames(a)<-as.character((assessmentYear-4):assessmentYear)
xtab(a, caption=paste('Table 11. Parameter estimates, retrospective','.',sep=''), cornername='Parameter \ Year', 
     file=file.path("report",paste(stamp,'_tab11.html',sep='')), dec=rep(2,ncol(ftab)))



############################################################################################################-
# Forecast results and catch advice ----

# Just to check: first catch advice according to long-term management plan
FC[[1]]

# Check N at age
FbarAges<-c(3,7)
test<-FC[[1]]
n1<-exp(test[[1]]$sim) # Stock at the beginning of the TAC year
n2<-exp(test[[2]]$sim) # Stock at the beginning at next year
n<-rbind(n1[1,],n2[1,]) #what are the different columns
N<-n[,1:10]
cat(paste0("Stock N at the beginning of the TAC year(",TACyear,") and the following year(",TACyear+1,")\n:"));round(N)

# Check F at age and exploitation pattern
FF<-cbind(n[,11:19],n[,19])
round(FF,3)
expl<-FF[2,]
expl<-expl/mean(expl[FbarAges[1]:FbarAges[2]])
expl

# Get mean stock weight, proportion mature and natural mortality used in the forecast
mw<-attr(test,"fit")[['data']]$stockMeanWeight
mw[as.character(TACyear),]
pm<-attr(test,"fit")[['data']]$propMat
nm<-attr(test,"fit")[['data']]$natMor

# Table with input to short-term forecast
tab<-t(rbind(mw[as.character(TACyear-1),],gem$sw ,dat.forecast$propMat[as.character(TACyear-1),],dat.forecast$natMor[as.character(TACyear-1),],expl,N[1,]))
rownames(tab)<-paste('Age',attr(test,"fit")[['conf']]$minAge:attr(test,"fit")[['conf']]$maxAge)
colnames(tab)<-c(paste0('Mean weight in the stock and catch (kg) in ',as.character(TACyear-1)),paste0('Mean weight in the stock and catch (kg) in ', as.character(TACyear),'+'),'Proportion mature','Natural mortality','Exploitation pattern',paste0('Stock number(',TACyear,') (thousands)'))

stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))

xtab(tab, caption=paste('Table 20. Input to short term forecast','.',sep=''), cornername='Age', 
     file=file.path('report',paste(stamp,'_tab20.html',sep='')), dec=c(3,3,2,2,3,0))

xtab(tab, caption=paste('UNROUNDED Table 21. Input to short term forecast','.',sep=''), cornername='Age', 
     file=file.path('report',paste(stamp,'_tab21.html',sep='')), dec=c(5,5,5,5,5,2))

# Get SSB and catch in intermediate year
ft<-attr(FC[[1]],"tab")
refSSB<-ft[as.character(TACyear),'ssb:median']
refCatch<-catchtable(fit.forecast,obs.show = TRUE)[as.character(TACyear-1),'sop.catch']

# Table with intermediate year assumptions
ft<-rbind(Fsq, 
          refSSB,
          rec.last*my.nscale[2],
          ft[as.character(TACyear),'rec:median'],
          ft[as.character(TACyear+1),'rec:median'],
          refCatch)

rownames(ft)<-c(paste0('F ages ',FbarAges[1],'-',FbarAges[2],' (',TACyear-1,')'),
                paste0('SSB (',TACyear,')'),
                paste0('R age 1 (',TACyear-1,')'),
                paste0('R age 1 (',TACyear,')'),     
                paste0('R age 1 (',TACyear+1,')'),
                paste0('Total catch (',TACyear-1,')'))  
ft<-cbind(ft,rep(NA,dim(ft)[[1]]))
colnames(ft)<-c('Value','Notes')

stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))

xtab(ft, caption=paste('Table 19. Intermediate year input to short term forecast','.',sep=''), cornername='Values', 
     file=file.path('report',paste(stamp,'_tab19.html',sep='')), dec=c(3,0))

# Calculate change in SSB, catch and advice
SSByear<-TACyear+1
if(exists("FC")){  
  res<-lapply(FC, function(f){
    tf<-attr(f,"tab");
    c(tf[as.character(TACyear),'catch:median'],tf[as.character(TACyear),'fbar:median'],tf[as.character(SSByear),'ssb:median'])
  })
  
  res<-cbind(matrix(unlist(res),ncol=3,byrow=TRUE),0,0,0)
  res[,4]<-(res[,3]-refSSB)/refSSB*100   
  res[,5]<-(res[,1]-refCatch)/refCatch*100 
  res[,6]<-(res[,1]-last.advice)/last.advice*100 
  head(res)
}

# Table with catch options
rownames(res)<-names(targetF)
colnames(res)<-c(paste0('Catch(',TACyear,')'), paste0('F(',TACyear,')'),paste0('SSB(',SSByear,')'),'% SSB change*', '% Catch change**','% Advice change***')
head(res,14)
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))
xtab(res, caption=paste('Table 22.  short term forecast','.',sep=''), cornername='Basis', 
     file=file.path('report',paste(stamp,'_tab22.html',sep='')), dec=c(0,3,0,1,1,1))
#res

# Check the catch proportions by age for the next year for the relevant scenario (long-term management strategy)
### NOTE!! In 2026, this was not the first scenario, but scenario 14 due to advice cap of +25% change!
FC[[14]]
catchAge <-  FC[[14]][[1]][["catchByAge"]][,1]
catchProp <- catchAge / sum(catchAge)
ctprop <- matrix(round(catchAge,0))
ctprop <- cbind(ctprop, round(catchProp*100,1))
colnames(ctprop) <- c("Catch at age (tonnes)","% of catch")
rownames <- c(fit.forecast$data$minAgePerFleet[1]:fit.forecast$data$maxAgePerFleet[1])
ctprop

stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))
xtab(ctprop, caption=paste('Table 23.  short term forecast catch proportions by age for TAC year','.',sep=''), cornername='Age', 
     file=file.path('report',paste(stamp,'_tab23.html',sep='')), dec=c(0,0,1))


# Re-calculate SSB for 2026 based on replaced recruitment estimate for 2026 used in forecast

## Set year index for extracting data from SAM output
yearIdx <- as.character(assessmentYear)

## Get weight at age and maturity
wtab             <- as.data.frame(fit.base$data$catchMeanWeight)[yearIdx,]
wtab             <- as.numeric(wtab[1,])
propmat          <- as.numeric(fit.base$data$propMat[yearIdx,])


## Approach 1

# Get N at age, select 2026 and replace N age 1 by GP
ntab             <- ntable(fit.base)[yearIdx,]
ntab[1]          <- GP

# Re-calculate SSB for 2026
ssb.old          <- ssbtable(fit.base)[yearIdx,1]
ssb.new          <- sum(ntab * wtab * propmat)
ssb.old; ssb.new

# Get current confidence intervals for SSB and recruitment 2026
confR.old         <- rectable(fit.base)[yearIdx,c(2:3)]
confSSB.old       <- ssbtable(fit.base)[yearIdx,c(2:3)]
confR.old; confSSB.old

# Substract from the CI of SSB the contribution from the old recruitment value
confSSB.new      <- c(confSSB.old[1] - (confR.old[1] * wtab[1] * propmat[1]),
                      confSSB.old[2] - (confR.old[2] * wtab[1] * propmat[1]))
confSSB.new

# Add to the CI of SSB the contribution from the new recruitment value
confSSB.new      <- c(confSSB.new[1] + (GP * wtab[1] * propmat[1]),
                      confSSB.new[2] + (GP * wtab[1] * propmat[1]))
confSSB.new


## Approach 2 - FINAL APPROACH CHOSEN

# Calculate difference between new and old recruitment
rec.diff         <- as.numeric(GP) - rectable(fit.base)[yearIdx,1]

# Re-calculate SSB based on difference in recruitment values
ssb.old2        <- ssbtable(fit.base)[yearIdx,1]
ssb.new2        <- ssb.old2 + (rec.diff * wtab[1] * propmat[1])

# Check whether different from approach 1
ssb.old; ssb.new; ssb.new2 #no, they're the same

# Re-calculate confidence intervals
rescale_lognormal <- function(median, lower, upper, new_median,
                              coverage = 0.95) {
  # z-score bij de opgegeven (tweezijdige) dekking
  z <- qnorm(1 - (1 - coverage) / 2)
  
  # naar log-space
  mu    <- log(median)
  mu_lo <- log(lower)
  mu_hi <- log(upper)
  
  # SD uit de volledige intervalbreedte (robuust bij lichte asymmetrie)
  sd_log <- (mu_hi - mu_lo) / (2 * z)
  
  # nieuwe mediaan + limieten met dezelfde SD
  mu_new <- log(new_median)
  list(
    median   = new_median,
    lower    = exp(mu_new - z * sd_log),
    upper    = exp(mu_new + z * sd_log),
    sd_log   = sd_log,
    coverage = coverage
  )
}
conf.calc      <- rescale_lognormal(ssb.old, confSSB.old[1], confSSB.old[2], ssb.new2)
confSSB.new2   <- c(conf.calc$lower, conf.calc$upper)

# Compare CI for the approaches
confSSB.old; confSSB.new; confSSB.new2
