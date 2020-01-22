#Created by Lynn Waterhouse
#Modified from code by Brice X. Semmens

#Take video counts and tag counts
#Use one model to estimate abundance through time
#In a Bayesian Framework

#Estimates: Population size each year as well as overall population growth

###################################################################################
###################################################################################
#install the libraries
library(R2jags)
library(gtools)
library(gdata)
library(picante)
library(plyr)


###################################################################
# Bring in the raw data:
###################################################################
###################################################################

#set working directory
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2018_bothislands_withvideo")
###################################################################
###################################################################

#### Little Cayman First ###
#load data
tagdat <- read.csv("MasterTagSurveys_2018_onlyLC.csv", header = TRUE)
head(tagdat) #check where most recent data is
tail(tagdat)

###################################################################
#some summary data
library(plyr)
tag_summary<-ddply(tagdat,.(date,time_cat,surveyor_code,TAG_NUM,SURVEY_NUM,Year),summarize,
                   tagscountedby1diver=sum(TAG_CNT),perc_tags=mean(TAG_CNT/SURVEY_NUM),
                   pop_est=median(TAG_NUM/((TAG_CNT/SURVEY_NUM))))
tag_summary
table(tag_summary$TAG_NUM<tag_summary$tagscountedby1diver)
#we want to check that a diver never counts more tags than are out during 1 dive (this means they
#have counted the same fish multiple times if they count more tags than are out)
#so we want this table to be all FALSE

#find out if any surveyors did <5 counts
tag_person_counts<-ddply(tagdat,.(Year,surveyor_code),summarize,
                         surveysdone=length(surveyor_code))
tag_person_counts

tag_person_counts_allyr<-ddply(tagdat,.(surveyor_code),summarize,
                               surveysdone=length(surveyor_code))
tag_person_counts_allyr
#need to remove
#surveyor 14 from 2008, 21 from 2011, 3 from 2015, 42 from 2018 (if <5 per year)
# surveyor 43 if (<5 overall)
#surveyor 43, 18 (<=5 overall)
#surveyor 43, 19, 8, 23, 37, 39 (if <10 overall) 
# 43 has 2, 19 has 5, 8 has 6, 37 has 8, 39 has 9, 23 has 9, 25 has 10, 31 has 10

plot(tag_summary$Year,tag_summary$tagscountedby1diver/tag_summary$TAG_NUM)

#Let's go ahead and remove surveyor 43 and surveyor 19 from the data
tagdat_new<-tagdat[which(tagdat$surveyor_code!=43),] #remove 43
tagdat_new2<-tagdat_new[which(tagdat_new$surveyor_code!=19),] #remove 19

tagdat<-tagdat_new2

#####################################################################
##
#create stand-alone variable names
Year<-tagdat$Year
Person<-tagdat$surveyor_code
date<-tagdat$date
myyears<-unique(Year) #vector of years in data
nyears<-length(myyears)  #how many years do we have data for?

#make unique ID of survey per dive for each diver
person.surveys.perday<-ddply(tagdat,.(date,surveyor_code),summarise,no.survey=sum(surveyor_code>0))
tagdat$surveyno<-rep(NA,length(date))
tagdat$date_time<-paste(tagdat$date,tagdat$time_cat,sep="_")
date_time<-tagdat$date_time
for (day in 1:length(unique(date_time))){
  for (who in 1:length(unique(Person))){
    mycount<-person.surveys.perday$no.survey[which(person.surveys.perday$date_time==unique(date_time)[day]&person.surveys.perday$surveyor_code==unique(Person)[who])]
    if(!identical(mycount, integer(0))){tagdat$surveyno[which(tagdat$surveyor_code==unique(Person)[who]&tagdat$date_time==unique(date_time)[day])]<-seq(1,mycount,1)}
    
  }
}
head(tagdat)
tail(tagdat)
TAG_NUM<-tagdat$TAG_NUM  #how many tags were put out

number.tagged<-unique(TAG_NUM)  #unique values for batches of tags out

TAG_CNT<-tagdat$TAG_CNT      #tags counted in each survey
SURVEY_NUM<-tagdat$SURVEY_NUM  #how many sides of fish were counted in each survey

N<-rep(NA,length(number.tagged))  #how many unique surveys for year, number tags out combos
for (i in 1:length(number.tagged)){
  N[i]<-table(TAG_NUM[TAG_NUM==number.tagged[i]])
}

#Need list of years for each unique set of # of tags out, will use in Surveyor effect Model
rep.year<-rep(NA,length(N))
dumdum<-0
for (i in 1:length(N)){
  dumdum<-dumdum+N[i]
  rep.year[i]<-Year[dumdum]
}

t<-tagdat$TAG_CNT  #tags counted in that survey
n<-tagdat$SURVEY_NUM #number of fish counted per survey
dummyvec<-seq(1,length(number.tagged),1) #dummy vector based on unique values of batches of tags out 
#note- some years tag counts were done before all tagging completed, and other years some tags were found
#on the ground, so the number of tags out can change during the tag counting period


###################
## Read in CB data
#load data
tagdat.CB <- read.csv("2008.2018_BracTagCounts.csv", header = TRUE)
head(tagdat.CB) #check where most recent data is
tail(tagdat.CB)

##keep only ones with quality code 1 or 0
temp<-tagdat.CB[which(tagdat.CB$quality.code<2),]
tagdat.CB<-temp #replace file

#create stand-alone variable names
Year.CB<-tagdat.CB$Year
myyears.CB<-unique(Year.CB)    #vector of years in data
nyears.CB<-length(myyears.CB)  #how many years do we have data for?

Person.CB<-tagdat.CB$surveyor_code      #unique surveyor code
date.CB<-tagdat.CB$date.CB              #date
TAG_NUM.CB<-tagdat.CB$TAG_NUM        #how many tags were put out
TAG_CNT.CB<-tagdat.CB$TAG_CNT        #tags counted in each survey
SURVEY_NUM.CB<-tagdat.CB$SURVEY_NUM  #how many sides of fish were counted in each survey
  


####################
#bring in video pan counts
vid.counts<-read.csv("MasterList_allcounts_videopans.csv",sep="\t")
vid.counts0<-vid.counts[which(vid.counts$Year!=2016),] #remove 2016
#vid.counts1<-vid.counts0[which(vid.counts0$Year.Counted>2007),]  #only keep 2008 on, match tag model
vid.countsLC<-vid.counts0[which(vid.counts0$Island=="LC"),]  #only use LC
vid.LC.counter<-vid.countsLC$ï..Name   #person doing video counts
vid.LC.number<-vid.countsLC$Number     #count estimate
vid.LC.year<-vid.countsLC$Year.Counted #year of video pan

vid.countsCB<-vid.counts0[which(vid.counts0$Island=="CB"),]  #only use CB
vid.CB.counter<-vid.countsCB$ï..Name   #person doing video counts
vid.CB.number<-vid.countsCB$Number     #count estimate
vid.CB.year<-vid.countsCB$Year.Counted #year of video pan

vid.summ<-ddply(vid.counts0,.(Year.Counted,Island),summarize,
                vid.yr.rep=length(Number),yr.avg=mean(Number),yr.sd=sd(Number))
vid.summ.LC<-vid.summ[which(vid.summ$Island=="LC"),] #all years LC
vid.summ.CB<-vid.summ[which(vid.summ$Island=="CB"),] #all years CB
vid.summ.LC
vid.summ.CB

yr.vid.startLC<-min(vid.summ.LC$Year.Counted)
yr.vid.startCB<-min(vid.summ.CB$Year.Counted)
yr.vid.endLC<-max(vid.summ.LC$Year.Counted)
yr.vid.endCB<-max(vid.summ.CB$Year.Counted)
############################################################################
############################################################################
# State space model
###################################################################
###################################################################
#run state space model
num.Person<-length(unique(Person))
library(R2jags)
#########################################################################
#########################################################################
# Try with just Little Cayman  - then use median vid.prop for CB
###################################################################
#run state space model
num.Person<-length(unique(Person))
library(R2jags)
sink("ssm.tags.jags.tagvid.Lconly")
cat("
    model { 
    # Priors and constraints for LC
    N.est[1] ~ dunif(log(1),log(5000))
    mean.mu ~ dunif(-10, 10)          # Prior for mean rate of change
    sigma.proc ~ dunif(0, 10)           # Prior for sd of state process
    sigma2.proc <- pow(sigma.proc, 2)
    tau.proc <- pow(sigma.proc, -2)
    percep.sigma ~ dunif(0,5)
    percep.sigma2 <- pow(percep.sigma,2)
    percep.tau <- pow(percep.sigma, -2)
    
    #Prior for variance and SD of video proportion LC
    for(mo in uni.vid.year){
    sigma.vid[mo] ~ dunif(0, 100)           # Prior for sd of video prop
    sigma2.vid[mo] <- pow(sigma.vid[mo], 2)
    tau.vid[mo] <- pow(sigma.vid[mo], -2)
    } 
    
    #priors to video params estimated from LC 
    vid.prop~dunif(0,1)
    
    ###########################################
    # Likelihood
    ###########################################
    # State process for number of fish in LC
    for (t in 1:((yr.vid.endLC-yr.vid.startLC))){
    mu[t] ~ dnorm(mean.mu, tau.proc) 
    N.est[t+1] <- N.est[t]+mu[t] #is +mu becuase in log space
    }
    
    #Surveyor error
    for (z in Person.no){
    percep[z] ~ dnorm(0, percep.tau)
    }
    
    # Do mark-resight model here
    for (i in 1:length(TAG_CNT)){
    TAG_CNT[i] ~ dbin(prob.tagged[i], SURVEY_NUM[i]) #tag counts come from binomial distribution
    logit.probs[i]<-log(probs[i]/(1-probs[i]))
    dumdum[i]<-percep[Person[i]]+logit.probs[i]
    prob.tagged[i]<-exp(dumdum[i])/(1+exp(dumdum[i])) #turn into logistic
    probs[i]<-TAG_NUM[i]/(exp(N.est[Year[i]-(yr.vid.startLC-1)])*2) #proportion tagged, based on number tags out and sides of fish that year
    }
    
    ############################################
    ### Tie in Video
    #match video observations on LC
    for (nn in 1:length(vid.number)){
    vid.number[nn]~dnorm(vid.prop*exp(N.est[vid.year[nn]-(yr.vid.startLC-1)]),
    tau.vid[vid.year[nn]-(yr.vid.startLC-1)])
    }
    
    
    }
    ",fill = TRUE)
sink()

###################################################################
# build data, parameters and MCMC settings for JAGS:
###################################################################
jags.data.tagvid.Lconly <- list(TAG_CNT=TAG_CNT,TAG_NUM=TAG_NUM,Year=Year,
                                SURVEY_NUM=SURVEY_NUM,Person=Person, 
                                Person.no=unique(Person),
                                vid.year=vid.LC.year,vid.number=vid.LC.number,
                                uni.vid.year=unique(vid.LC.year)-(yr.vid.startLC-1),
                                #counting params for LC 
                                yr.vid.startLC=yr.vid.startLC,
                                yr.vid.endLC=yr.vid.endLC)

# What should JAGS keep track of
jags.params.tagvid.Lconly = c("N.est","mu","mean.mu","sigma2.proc","percep.sigma2","percep",
                              "vid.prop","sigma2.vid")

#MCMC settings
mcmc.chainLength <- as.integer(200000)  #Chainlength 100,000
mcmc.burn <- as.integer (40000)         #number samples to burn 40,000
mcmc.thin = 240                       #thinning interval 120
mcmc.chains =8                        #number of MCMC chains 4
(mcmc.chainLength-mcmc.burn)*mcmc.chains/mcmc.thin

############################### run the model in JAGS########################################
ssm_grouper_tagvid.Lconly<- jags(jags.data.tagvid.Lconly, inits=NULL, 
                                 parameters.to.save= jags.params.tagvid.Lconly, 
                                 "ssm.tags.jags.tagvid.Lconly", n.chains = mcmc.chains, 
                                 n.thin = mcmc.thin, n.iter = mcmc.chainLength, 
                                 n.burnin = mcmc.burn, working.directory = getwd(),
                                 DIC=TRUE)
attach.jags(ssm_grouper_tagvid.Lconly)
ssm_grouper_tagvid.Lconly
DIC_model_surveyor<-ssm_grouper_tagvid.Lconly$BUGSoutput$DIC

save.image(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),"Model.tagvid.Lconly", "RData", sep = "."))

stat<-matrix(NA,ncol=3,nrow=dim(N.est)[2])
for(i in 1:dim(N.est)[2]){
  stat[i,]<-quantile(N.est[,i],c(0.5,0.02,0.975))
}

sink(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),
           "posterior.est.ssm_grouper_tagvid.Lconly.csv",sep=""))
print(stat)
sink()

######################################################################
######################################################################
#Gelman-rubin statistics
#want this to be below 1.1
# citation: https://blog.stata.com/2016/05/26/gelman-rubin-convergence-diagnostic-using-multiple-chains/
#setwd(mywd)
func.gelman<-function(jags.file){
  mcmcjags.file<-as.mcmc(jags.file)
  n.var <- coda::nvar(mcmcjags.file)
  gelman <- matrix(NA, nrow=n.var, ncol=2)
  for (v in 1:n.var) {gelman[v,] <- coda::gelman.diag(mcmcjags.file[,v])$psrf }
  gelman.all <- gelman[which(!is.nan(gelman[,1])),] # Remove dummy variables (show up as NA) 
  gelman_short <- gelman[order(gelman[,1],decreasing=T),] 
  if(n.var>10) gelman_short <- gelman_short[1:10,] 
  gelman_fail <- c(length(which(gelman[,1]>1.01)), length(which(gelman[,1]>1.05)), 
                   length(which(gelman[,1]>1.1))) 
  gelman_fail_CI <- c(length(which(gelman[,2]>1.01)), length(which(gelman[,2]>1.05)), 
                   length(which(gelman[,2]>1.1)))
  #failing is >1.05, so middle number
  use.DIC=jags.file$BUGSoutput$DIC
  return(list("gelman_fail"=gelman_fail,"gelman_fail_CI"=gelman_fail_CI,
              "gelman_short"=gelman_short,
              "gelman.all"=gelman.all,"numbervar"=n.var,"DIC"=use.DIC))
}

a<-func.gelman(ssm_grouper_tagvid.Lconly)
a

sink(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),
           "gelmanrubin_ssm_grouper.tagvid.Lconly.csv",sep=""))
print(a)
sink()


#make some plots
#make a plot with video estimates and population estimate on same plot
library(RColorBrewer)

#turn into long data set, year, posterior estimate
attach.jags(ssm_grouper_tagvid.Lconly)
dim(vid.prop)

pm.Year<-c(rep(seq(2005,2018,by=1),each=length(N.est[,1])))
pm.Estimate<-c(exp(N.est[,1]),exp(N.est[,2]),exp(N.est[,3]),exp(N.est[,4]),exp(N.est[,5]),
               exp(N.est[,6]),exp(N.est[,7]),exp(N.est[,8]),exp(N.est[,9]),exp(N.est[,10]),
               exp(N.est[,11]),exp(N.est[,12]),exp(N.est[,13]),exp(N.est[,14]))
post.mat<-cbind(pm.Year,pm.Estimate)
colnames(post.mat)<-c("Year","Estimate")
post.mat.df<-as.data.frame(post.mat)

pm.Year
library(ggplot2)
p10 <- ggplot(post.mat.df, aes(x = Year, y = Estimate, group=Year)) +
  #geom_boxplot(alpha=0.7) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="black")+
  geom_boxplot(width=0.1,outlier.shape = NA) + theme_minimal()+
  scale_y_continuous(name = "Population Estimate (# Fish)",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year") +
  #ggtitle("Boxplot of mean ozone by month") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")
p10

#now add video counts
head(vid.countsLC)
vid.countsLC.df<-as.data.frame(vid.countsLC)
#adjust for posterior estimate of proportion
quantile(vid.prop,c(.025,.5,.975)) #low, median, high 
lowprop<-quantile(vid.prop,c(.025,.5,.975))[[1]]
medprop<-quantile(vid.prop,c(.025,.5,.975))[[2]]
hiprop<-quantile(vid.prop,c(.025,.5,.975))[[3]]
vid.summ.LC$compnum<-vid.summ.LC$yr.avg/medprop
vid.summ.LC$compnum.L<-vid.summ.LC$yr.avg/lowprop
vid.summ.LC$compnum.H<-vid.summ.LC$yr.avg/hiprop
#vid.summ.LC2<-rbind(vid.summ.LC,c("2011",NA,NA,NA,NA,NA),c("2016",NA,NA,NA,NA,NA))

p10+geom_boxplot(data=vid.countsLC.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                 width=0.3)+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red")+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue")+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue")


#looking at video data pre 2008
post.mat.df.summ<-ddply(post.mat.df,.(Year),summarize,
                        est.med=median(Estimate),est.l=quantile(Estimate,.025), 
                        est.h=quantile(Estimate,.975))
g_smooth <- ggplot(post.mat.df.summ, aes(x = Year, y = est.med)) + geom_point()+geom_line()
taggraph<-g_smooth+ theme_minimal()+
  scale_y_continuous(name = "Population Estimate (# Fish)",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year",breaks=seq(2005,2018,2),limits=c(2004.5,2018.5))+
  theme_bw()+
  geom_ribbon(aes(ymin=post.mat.df.summ$est.l, ymax=post.mat.df.summ$est.h), 
              linetype=2, alpha=0.1)
taggraph
#bring in video pan counts
taggraph+geom_boxplot(data=vid.countsLC.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                      width=0.3)+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red")+geom_line()+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue")+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue")



#plot of posterior estimates from tagging model
#with boxplots of all video counts
#and projected pop size from video counts * posterior proportion of pop in video
g_smooth <- ggplot(post.mat.df.summ, aes(x = Year, y = est.med)) +
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=post.mat.df.summ$est.l, ymax=post.mat.df.summ$est.h), 
              linetype=2, alpha=.4)
taggraph2<-g_smooth+ theme_minimal()+
  scale_y_continuous(name = "Population Estimate (# Fish)",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year",breaks=seq(2006,2018,2),limits=c(2004.5,2018.5))+
  theme_bw()
taggraph2
#bring in video pan counts
taggraph2+geom_boxplot(data=vid.countsLC.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                       width=0.3)+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red",shape=4,size=1.2)+geom_line()+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue",shape=5,size=1.2)+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue",shape=5,size=1.2)


############################################################################
############################################################################
# Cayman Brac using vid.prop posteriors as priors
############################################################################
# State space model
###################################################################
###################################################################
#run state space model
library(R2jags)
sink("ssm.tags.jags.tagvid.CBonly")
cat("
    model { 
    # Priors and constraints for LC - just basic model
    N.est.CB[1] ~ dunif(log(1),log(5000))
    mean.mu.CB ~ dunif(-10, 10)          # Prior for mean rate of change
    sigma.proc.CB ~ dunif(0, 10)           # Prior for sd of state process
    sigma2.proc.CB <- pow(sigma.proc.CB, 2)
    tau.proc.CB <- pow(sigma.proc.CB, -2)
    
    #Prior for variance and SD of video proportion CB
    for(moo in uni.vid.year.CB){
    sigma.vid.CB[moo] ~ dunif(0, 100)           # Prior for sd of video prop
    sigma2.vid.CB[moo] <- pow(sigma.vid.CB[moo], 2)
    tau.vid.CB[moo] <- pow(sigma.vid.CB[moo], -2)
    } 
    
    #priors to video params estimated from CB
    vid.prop.CB<-vid.prop.prior
    
    ###########################################
    # Likelihood
    ###########################################
    ############# for CB ###############
    # State process for number of fish
    for (tt in 1:((yr.vid.endCB-yr.vid.startCB))){
    mu.CB[tt] ~ dnorm(mean.mu.CB, tau.proc.CB) 
    N.est.CB[tt+1] <- N.est.CB[tt]+mu.CB[tt] #is +mu becuase in log space
    }
    
    # Do mark-resight model here
    for (ii in 1:length(TAG_CNT.CB)){
    TAG_CNT.CB[ii] ~ dbin(probs.CB[ii], SURVEY_NUM.CB[ii]) #tag counts come from binomial distribution
    probs.CB[ii]<-TAG_NUM.CB[ii]/(exp(N.est.CB[Year.CB[ii]-(yr.vid.startCB-1)])*2) #proportion tagged, based on number tags out and sides of fish that year
    }
    
    ############################################
    ### Tie in Video
    #match video observations on CB
    for (nnn in 1:length(vid.number.CB)){
    vid.number.CB[nnn]~dnorm(vid.prop.CB*exp(N.est.CB[vid.year.CB[nnn]-(yr.vid.startCB-1)]),
    tau.vid.CB[vid.year.CB[nnn]-(yr.vid.startCB-1)])
    }
    
    }
    ",fill = TRUE)
sink()

###################################################################
# build data, parameters and MCMC settings for JAGS:
###################################################################
vid.prop.LC<-vid.prop
jags.data.tagvid.CBonly <- list(vid.prop.prior=median(vid.prop.LC),
                                #CB parameters
                                TAG_CNT.CB=TAG_CNT.CB,TAG_NUM.CB=TAG_NUM.CB,Year.CB=Year.CB,
                                SURVEY_NUM.CB=SURVEY_NUM.CB,
                                vid.year.CB=vid.CB.year,vid.number.CB=vid.CB.number,
                                uni.vid.year.CB=unique(vid.CB.year)-(yr.vid.startCB-1),
                                #counting params for LC and CB
                                yr.vid.startCB=yr.vid.startCB,
                                yr.vid.endCB=yr.vid.endCB)

# What should JAGS keep track of
jags.params.tagvid.CBonly = c("N.est.CB","mu.CB","mean.mu.CB","sigma2.proc.CB",
                              "sigma2.vid.CB")

#MCMC settings
mcmc.chainLength <- as.integer(2000000)  #Chainlength 100,000
mcmc.burn <- as.integer (800000)         #number samples to burn 40,000
mcmc.thin = 600                       #thinning interval 120
mcmc.chains = 4                        #number of MCMC chains 4

############################### run the model in JAGS########################################
ssm_grouper_tagvid.CBonly<- jags(jags.data.tagvid.CBonly, inits=NULL, 
                                 parameters.to.save= jags.params.tagvid.CBonly, 
                                 "ssm.tags.jags.tagvid.CBonly", n.chains = mcmc.chains, 
                                 n.thin = mcmc.thin, n.iter = mcmc.chainLength, 
                                 n.burnin = mcmc.burn, working.directory = getwd(),
                                 DIC=TRUE)
attach.jags(ssm_grouper_tagvid.CBonly)
ssm_grouper_tagvid.CBonly
DIC_model_surveyor<-ssm_grouper_tagvid.CBonly$BUGSoutput$DIC

save.image(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),"Model.tagvid.CBonly", "RData", sep = "."))


######################################################################
######################################################################
#Gelman-rubin statistics
#setwd(mywd)
func.gelman<-function(jags.file){
  mcmcjags.file<-as.mcmc(jags.file)
  n.var <- coda::nvar(mcmcjags.file)
  gelman <- matrix(NA, nrow=n.var, ncol=2)
  for (v in 1:n.var) {gelman[v,] <- coda::gelman.diag(mcmcjags.file[,v])$psrf }
  gelman.all <- gelman[which(!is.nan(gelman[,1])),] # Remove dummy variables (show up as NA) 
  gelman_short <- gelman[order(gelman[,1],decreasing=T),] 
  if(n.var>10) gelman_short <- gelman_short[1:10,] 
  gelman_fail <- c(length(which(gelman[,1]>1.01)), length(which(gelman[,1]>1.05)), length(which(gelman[,1]>1.1))) 
  #failing is >1.05, so middle number
  use.DIC=jags.file$BUGSoutput$DIC
  return(list("gelman_fail"=gelman_fail,"gelman_short"=gelman_short,"
              gelman.all"=gelman.all,"numbervar"=n.var,"DIC"=use.DIC))
}

a<-func.gelman(ssm_grouper_tagvid.CBonly)
a

sink(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),
           "gelmanrubin_ssm_grouper.tagvid.CBonly.csv",sep=""))
print(a)
sink()


#make some plots
#make a plot with video estimates and population estimate on same plot
library(RColorBrewer)

#turn into long data set, year, posterior estimate
attach.jags(ssm_grouper_tagvid.CBonly)

pm.Year<-c(rep(seq(2008,2018,by=1),each=8000))
pm.Estimate<-c(exp(N.est.CB[,1]),exp(N.est.CB[,2]),exp(N.est.CB[,3]),exp(N.est.CB[,4]),
               exp(N.est.CB[,5]),exp(N.est.CB[,6]),exp(N.est.CB[,7]),exp(N.est.CB[,8]),
               exp(N.est.CB[,9]),exp(N.est.CB[,10]),exp(N.est.CB[,11]))
post.mat<-cbind(pm.Year,pm.Estimate)
colnames(post.mat)<-c("Year","Estimate")
post.mat.df<-as.data.frame(post.mat)

library(ggplot2)
p10 <- ggplot(post.mat.df, aes(x = Year, y = Estimate, group=Year)) +
  #geom_boxplot(alpha=0.7) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="black")+
  geom_boxplot(width=0.1,outlier.shape = NA) + theme_minimal()+
  scale_y_continuous(name = "Population Estimate (# Fish)",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year") +
  #ggtitle("Boxplot of mean ozone by month") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")
p10

#now add video counts
head(vid.countsCB)
vid.countsCB.df<-as.data.frame(vid.countsCB)
#adjust for posterior estimate of proportion
vid.summ.CB$compnum<-vid.summ.CB$yr.avg/.6

p10+geom_boxplot(data=vid.countsCB.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                 width=0.3)+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red")


#looking at video data pre 2008
post.mat.df.summ<-ddply(post.mat.df,.(Year),summarize,
                        est.med=median(Estimate),est.l=quantile(Estimate,.025), 
                        est.h=quantile(Estimate,.975))
g_smooth <- ggplot(post.mat.df.summ, aes(x = Year, y = est.med)) + geom_point()+geom_line()
taggraph<-g_smooth+
  geom_ribbon(aes(ymin=post.mat.df.summ$est.l, ymax=post.mat.df.summ$est.h), 
              linetype=2, alpha=0.1)+ theme_minimal()+
  scale_y_continuous(name = "Population Estimate (# Fish)",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year",breaks=seq(2008,2018,2),limits=c(2007.5,2018.5))+
  theme_bw()
taggraph
#bring in video pan counts
taggraph+geom_boxplot(data=vid.countsCB.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                      width=0.3)+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red")+geom_line()+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue")+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue")



#plot of posterior estimates from tagging model
#with boxplots of all video counts
#and projected pop size from video counts * posterior proportion of pop in video
g_smooth <- ggplot(post.mat.df.summ, aes(x = Year, y = est.med)) +
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=post.mat.df.summ$est.l, ymax=post.mat.df.summ$est.h), 
              linetype=2, alpha=.4)
taggraph2<-g_smooth+ theme_minimal()+
  scale_y_continuous(name = "Population Estimate (# Fish)",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year",breaks=seq(2006,2018,2),limits=c(2004.5,2018.5))+
  theme_bw()
taggraph2
#bring in video pan counts
taggraph2+geom_boxplot(data=vid.countsCB.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                       width=0.3)+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red",shape=4,size=1.2)+geom_line()+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue",shape=5,size=1.2)+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue",shape=5,size=1.2)

graphname<-paste(format(Sys.time(), "%Y-%m-%d %I-%p"),
                 "Nassau_CB_tagcounts_videocounts_propconstfromLC", "tiff", sep = ".")
tiff(file=graphname,res=400,units="in", width=8, height=5,)
graph.ssm<-function(N.est.CB, nyears){
  fitted<-lower<-upper<-rep(NA,11) 
  N.est.CB<-exp(N.est.CB)
  for (i in 1:nyears){
    fitted[i]<-median(N.est.CB[,i])
    lower[i]<-quantile(N.est.CB[,i],0.025)
    upper[i]<-quantile(N.est.CB[,i],0.975)
  }
  m1<-0
  m2<-2000
  par(mar=c(4,4.5,1,1),cex=1.2)
  boxplot(Number~Year.Counted,dat=vid.countsCB.df,ylim=c(m1,m2),#xlim=c(0.5,nyears),
       ylab="Posterior Estimate (# Fish)",
       xlab="Year",las=1,col="black",at =c(1,6,10,11),
       type="l",lwd=2,frame=FALSE,axes=FALSE)
  axis(2,las=1)
  axis(1,at=seq(1,12,2), labels=seq(2008,2019,2))
  axis(1,at=0:nyears,labels=rep("",nyears+1),tcl=-0.25)
  polygon(x=c(.5:1.5,1.5:.5),y=c(lower[1],lower[1],upper[1],upper[1]),
          col=rgb(1, 0, 0,0.5),border="gray90")
  polygon(x=c(5.5:6.5,6.5:5.5),y=c(lower[6],lower[6],upper[6],upper[6]),
          col=rgb(1, 0, 0,0.5),border="gray90")
  polygon(x=c(9.5:10.5,10.5:9.5),y=c(lower[10],lower[10],upper[10],upper[10]),
          col=rgb(1, 0, 0,0.5),border="gray90")
  polygon(x=c(10.5:11.5,11.5:10.5),y=c(lower[11],lower[11],upper[11],upper[11]),
          col=rgb(1, 0, 0,0.5),border="gray90")
  points(seq(1,nyears,1),type="l",fitted,lwd=2)
  
}
graph.ssm(N.est.CB, nyears=11)
dev.off()


############################################################################
# Cayman Brac using vid.prop posteriors as priors
############################################################################
# State space model
###################################################################
###################################################################
#run state space model
library(R2jags)
sink("ssm.tags.jags.tagvid.CBonly.ownp")
cat("
    model { 
    # Priors and constraints for LC - just basic model
    N.est.CB[1] ~ dunif(log(1),log(5000))
    mean.mu.CB ~ dunif(-10, 10)          # Prior for mean rate of change
    sigma.proc.CB ~ dunif(0, 10)           # Prior for sd of state process
    sigma2.proc.CB <- pow(sigma.proc.CB, 2)
    tau.proc.CB <- pow(sigma.proc.CB, -2)
    
    #Prior for variance and SD of video proportion CB
    for(moo in uni.vid.year.CB){
    sigma.vid.CB[moo] ~ dunif(0, 100)           # Prior for sd of video prop
    sigma2.vid.CB[moo] <- pow(sigma.vid.CB[moo], 2)
    tau.vid.CB[moo] <- pow(sigma.vid.CB[moo], -2)
    } 
    
    #priors to video params estimated from CB
    vid.prop.CB~dunif(0,1)
    
    ###########################################
    # Likelihood
    ###########################################
    ############# for CB ###############
    # State process for number of fish
    for (tt in 1:((yr.vid.endCB-yr.vid.startCB))){
    mu.CB[tt] ~ dnorm(mean.mu.CB, tau.proc.CB) 
    N.est.CB[tt+1] <- N.est.CB[tt]+mu.CB[tt] #is +mu becuase in log space
    }
    
    # Do mark-resight model here
    for (ii in 1:length(TAG_CNT.CB)){
    TAG_CNT.CB[ii] ~ dbin(probs.CB[ii], SURVEY_NUM.CB[ii]) #tag counts come from binomial distribution
    probs.CB[ii]<-TAG_NUM.CB[ii]/(exp(N.est.CB[Year.CB[ii]-(yr.vid.startCB-1)])*2) #proportion tagged, based on number tags out and sides of fish that year
    }
    
    ############################################
    ### Tie in Video
    #match video observations on CB
    for (nnn in 1:length(vid.number.CB)){
    vid.number.CB[nnn]~dnorm(vid.prop.CB*exp(N.est.CB[vid.year.CB[nnn]-(yr.vid.startCB-1)]),
    tau.vid.CB[vid.year.CB[nnn]-(yr.vid.startCB-1)])
    }
    
    }
    ",fill = TRUE)
sink()

###################################################################
# build data, parameters and MCMC settings for JAGS:
###################################################################
jags.data.tagvid.CBonly.ownp <- list(#CB parameters
                                TAG_CNT.CB=TAG_CNT.CB,TAG_NUM.CB=TAG_NUM.CB,Year.CB=Year.CB,
                                SURVEY_NUM.CB=SURVEY_NUM.CB,
                                vid.year.CB=vid.CB.year,vid.number.CB=vid.CB.number,
                                uni.vid.year.CB=unique(vid.CB.year)-(yr.vid.startCB-1),
                                #counting params for LC and CB
                                yr.vid.startCB=yr.vid.startCB,
                                yr.vid.endCB=yr.vid.endCB)

# What should JAGS keep track of
jags.params.tagvid.CBonly.ownp = c("N.est.CB","mu.CB","mean.mu.CB","sigma2.proc.CB",
                              "sigma2.vid.CB","vid.prop.CB")

#MCMC settings
mcmc.chainLength <- as.integer(2000000)  #Chainlength 100,000
mcmc.burn <- as.integer (800000)         #number samples to burn 40,000
mcmc.thin = 600                       #thinning interval 120
mcmc.chains = 4                        #number of MCMC chains 4

############################### run the model in JAGS########################################
ssm_grouper_tagvid.CBonly.ownp<- jags(jags.data.tagvid.CBonly.ownp, inits=NULL, 
                                 parameters.to.save= jags.params.tagvid.CBonly.ownp, 
                                 "ssm.tags.jags.tagvid.CBonly.ownp", n.chains = mcmc.chains, 
                                 n.thin = mcmc.thin, n.iter = mcmc.chainLength, 
                                 n.burnin = mcmc.burn, working.directory = getwd(),
                                 DIC=TRUE)
attach.jags(ssm_grouper_tagvid.CBonly.ownp)
ssm_grouper_tagvid.CBonly.ownp

save.image(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),"Model.tagvid.CBonly.ownp",
                 "RData", sep = "."))


######################################################################
######################################################################
#Gelman-rubin statistics
#setwd(mywd)
func.gelman<-function(jags.file){
  mcmcjags.file<-as.mcmc(jags.file)
  n.var <- coda::nvar(mcmcjags.file)
  gelman <- matrix(NA, nrow=n.var, ncol=2)
  for (v in 1:n.var) {gelman[v,] <- coda::gelman.diag(mcmcjags.file[,v])$psrf }
  gelman.all <- gelman[which(!is.nan(gelman[,1])),] # Remove dummy variables (show up as NA) 
  gelman_short <- gelman[order(gelman[,1],decreasing=T),] 
  if(n.var>10) gelman_short <- gelman_short[1:10,] 
  gelman_fail <- c(length(which(gelman[,1]>1.01)), length(which(gelman[,1]>1.05)), length(which(gelman[,1]>1.1))) 
  #failing is >1.05, so middle number
  use.DIC=jags.file$BUGSoutput$DIC
  return(list("gelman_fail"=gelman_fail,"gelman_short"=gelman_short,"
              gelman.all"=gelman.all,"numbervar"=n.var,"DIC"=use.DIC))
}

a<-func.gelman(ssm_grouper_tagvid.CBonly.ownp)
a

sink(paste(format(Sys.time(), "%Y-%m-%d %I-%p"),
           "gelmanrubin_ssm_grouper.tagvid.CBonly.ownp.csv",sep=""))
print(a)
sink()

########################################################################################
########################################################################################
# FINAL FIGURES ##########
##########################
#make a figure with output from tagging model only and tag+video for LC
#tag+video
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2018_bothislands_withvideo")
load("2018-10-06 10-PM.Model.tagvid.CBonly.ownp.RData")
attach.jags(ssm_grouper_tagvid.Lconly)
dim(vid.prop)

pm.Year<-c(rep(seq(2005,2018,by=1),each=2000))
pm.Estimate<-c(exp(N.est[,1]),exp(N.est[,2]),exp(N.est[,3]),exp(N.est[,4]),exp(N.est[,5]),
               exp(N.est[,6]),exp(N.est[,7]),exp(N.est[,8]),exp(N.est[,9]),exp(N.est[,10]),
               exp(N.est[,11]),exp(N.est[,12]),exp(N.est[,13]),exp(N.est[,14]))
post.mat<-cbind(pm.Year,pm.Estimate)
colnames(post.mat)<-c("Year","Estimate")
post.mat.df<-as.data.frame(post.mat)

#now add video counts
head(vid.countsLC)
vid.countsLC.df<-as.data.frame(vid.countsLC)
#adjust for posterior estimate of proportion
quantile(vid.prop,c(.025,.5,.975)) #low, median, high 
lowprop<-quantile(vid.prop,c(.025,.5,.975))[[1]]
medprop<-quantile(vid.prop,c(.025,.5,.975))[[2]]
hiprop<-quantile(vid.prop,c(.025,.5,.975))[[3]]
vid.summ.LC$compnum<-vid.summ.LC$yr.avg/medprop
vid.summ.LC$compnum.L<-vid.summ.LC$yr.avg/lowprop
vid.summ.LC$compnum.H<-vid.summ.LC$yr.avg/hiprop

#looking at video data pre 2008
post.mat.df.summ<-ddply(post.mat.df,.(Year),summarize,
                        est.med=median(Estimate),est.l=quantile(Estimate,.025), 
                        est.h=quantile(Estimate,.975))
library(ggplot2)
g_smooth <- ggplot(post.mat.df.summ, aes(x = Year, y = est.med)) + geom_point()+geom_line()
taggraph<-g_smooth+ theme_minimal()+
  scale_y_continuous(name = " ",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = "Year",breaks=seq(2005,2018,2),limits=c(2004.5,2018.5))+
  geom_ribbon(aes(ymin=post.mat.df.summ$est.l, ymax=post.mat.df.summ$est.h), 
              linetype=2, alpha=0.1)+
  theme_bw()+
  theme(plot.margin=unit(c(-.1,.5,0.2,0), "cm"))
taggraph
#bring in video pan counts
figLCA<-taggraph+geom_boxplot(data=vid.countsLC.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                      width=0.3)+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red", shape=4)+geom_line()+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue",shape=18)+
  geom_point(data=vid.summ.LC, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue",shape=18)
#read in just tagging data 
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2018_estimation")
load("2018-06-07 11-AM.Basic.Model_binom.N.est_2018.RData")
attach.jags(ssm_grouper_Person)
LCtag.Year<-c(rep(seq(2008,2018,by=1),each=2000))
LCtag.Estimate<-c(exp(N.est[,1]),exp(N.est[,2]),exp(N.est[,3]),exp(N.est[,4]),exp(N.est[,5]),
               exp(N.est[,6]),exp(N.est[,7]),exp(N.est[,8]),exp(N.est[,9]),exp(N.est[,10]),
               exp(N.est[,11]))
LCtag.mat<-cbind(LCtag.Year,LCtag.Estimate)
colnames(LCtag.mat)<-c("Year","Estimate")
LCtag.mat.df<-as.data.frame(LCtag.mat)
#looking at video data pre 2008
LCtag.mat.df.summ<-ddply(LCtag.mat.df,.(Year),summarize,
                        est.med=median(Estimate),est.l=quantile(Estimate,.025), 
                        est.h=quantile(Estimate,.975))
library(ggplot2)
LCtag_smooth <- ggplot(LCtag.mat.df.summ, aes(x = Year, y = est.med)) + geom_point()+geom_line()
LCtaggraph<-LCtag_smooth+ theme_minimal()+
  scale_y_continuous(name = " ",
                     breaks = seq(0, 10000, 2000),
                     limits=c(0, 10000)) +
  scale_x_continuous(name = " ",breaks=seq(2005,2018,2),limits=c(2004.5,2018.5))+
  theme_bw()+
  geom_ribbon(aes(ymin=LCtag.mat.df.summ$est.l, ymax=LCtag.mat.df.summ$est.h), 
              linetype=2, alpha=0.1)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(.5,.5,-.1, 0), "cm"))
LCtaggraph

#to put 2 plots togethger
#figLCA - tag plus video
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2018_bothislands_withvideo")
grid.arrange(LCtaggraph , figLCA, nrow = 2, 
             left=textGrob("Population Estimate (# Fish)", rot = 90, vjust = 1))


#make final plots for Cayman Brac
attach.jags(ssm_grouper_tagvid.CBonly.ownp)
ssm_grouper_tagvid.CBonly.ownp
CB.Year<-c(rep(seq(2008,2018,by=1),each=8000))
CB.Estimate<-c(exp(N.est.CB[,1]),exp(N.est.CB[,2]),exp(N.est.CB[,3]),exp(N.est.CB[,4]),
               exp(N.est.CB[,5]),exp(N.est.CB[,6]),exp(N.est.CB[,7]),exp(N.est.CB[,8]),
               exp(N.est.CB[,9]),exp(N.est.CB[,10]),exp(N.est.CB[,11]))
CB.mat<-cbind(CB.Year,CB.Estimate)
colnames(CB.mat)<-c("Year","Estimate")
CB.mat.df<-as.data.frame(CB.mat)
#now add video counts
head(vid.countsCB)
vid.countsCB.df<-as.data.frame(vid.countsCB)
#adjust for posterior estimate of proportion
quantile(vid.prop.CB,c(.025,.5,.975)) #low, median, high 
lowprop<-quantile(vid.prop.CB,c(.025,.5,.975))[[1]]
medprop<-quantile(vid.prop.CB,c(.025,.5,.975))[[2]]
hiprop<-quantile(vid.prop.CB,c(.025,.5,.975))[[3]]
vid.summ.CB$compnum<-vid.summ.CB$yr.avg/medprop
vid.summ.CB$compnum.L<-vid.summ.CB$yr.avg/lowprop
vid.summ.CB$compnum.H<-vid.summ.CB$yr.avg/hiprop

#looking at video data pre 2008
CB.mat.df.summ<-ddply(CB.mat.df,.(Year),summarize,
                        est.med=median(Estimate),est.l=quantile(Estimate,.025), 
                        est.h=quantile(Estimate,.975))
library(ggplot2)
g_smooth <- ggplot(CB.mat.df.summ, aes(x = Year, y = est.med)) + geom_point()+geom_line()
CBgraph<-g_smooth+ theme_minimal()+
  scale_y_continuous(name = " ",
                     breaks = seq(0, 3000, 500),
                     limits=c(0, 3000)) +
  scale_x_continuous(name = "Year",breaks=seq(2008,2018,2),limits=c(2007.5,2018.5))+
  geom_rect(aes(xmin=2007.5, xmax=2008.5,ymin=CB.mat.df.summ$est.l[1], 
                ymax=CB.mat.df.summ$est.h[1]),linetype=2, alpha=0.05)+
  geom_rect(aes(xmin=2012.5, xmax=2013.5,ymin=CB.mat.df.summ$est.l[6], 
                ymax=CB.mat.df.summ$est.h[6]),linetype=2, alpha=0.05)+
  geom_rect(aes(xmin=2016.5, xmax=2017.5,ymin=CB.mat.df.summ$est.l[10], 
                ymax=CB.mat.df.summ$est.h[10]),linetype=2, alpha=0.05)+
  geom_rect(aes(xmin=2017.5, xmax=2018.5,ymin=CB.mat.df.summ$est.l[11], 
                ymax=CB.mat.df.summ$est.h[11]),linetype=2, alpha=0.05)+
  theme_bw()+
  theme(plot.margin=unit(c(-.1,.5,0.2,0), "cm"))
CBgraph
#bring in video pan counts
figCBA<-CBgraph+geom_boxplot(data=vid.countsCB.df,aes(x = Year.Counted+.25, y = Number, group=Year.Counted),
                              width=0.3)+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum, group=Year.Counted),
             col="red", shape=4)+geom_line()+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum.L, group=Year.Counted),
             col="blue",shape=18)+
  geom_point(data=vid.summ.CB, aes(x=Year.Counted+.25, y=compnum.H, group=Year.Counted),
             col="blue",shape=18)
figCBA

#just CB tagging data
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2008_BracWork")
load("2008Brac.RDATA")
result
CB2008est<-c(result$sims.array[,,1])
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2018_BracWork/Just1.2.code")
load("2018-02-15 04-PM.2018.tagcount_basic_binomial.RDATA")
for(i in 2018){NG_tagmod_byyear_basicbinomial(i,tagdat)}
CB2018est<-c(N.est)
CB.tag.mat<-cbind(c(rep(2008,1500),rep(2018,1500)),c(exp(CB2008est),exp(CB2018est)))
colnames(CB.tag.mat)<-c("Year","Estimate")
CB.tag.mat.df<-as.data.frame(CB.tag.mat)
CBtagonly <- ggplot(CB.tag.mat.df, aes(x = Year, y = Estimate, group=Year)) + 
  geom_boxplot()+geom_line()
CBtagonly

CB.tag.mat.summ<-ddply(CB.tag.mat.df,.(Year),summarize,med.CB=median(Estimate),
                       l.CB=quantile(Estimate,.025),h.CB=quantile(Estimate,.975))

CBtag.graph<-ggplot(CB.tag.mat.summ, aes(x = Year, y = med.CB, group=Year)) + 
  geom_point()+ theme_minimal()+
  scale_y_continuous(name = " ",
                     breaks = seq(0, 3000, 500),
                     limits=c(0, 3000)) +
  scale_x_continuous(name = "Year",breaks=seq(2008,2018,2),limits=c(2007.5,2018.5))+
  geom_rect(aes(xmin=2007.5, xmax=2008.5,ymin=CB.tag.mat.summ$l.CB[1], 
                ymax=CB.tag.mat.summ$h.CB[1]),linetype=2, alpha=0.05)+
  geom_rect(aes(xmin=2017.5, xmax=2018.5,ymin=CB.tag.mat.summ$l.CB[2], 
                ymax=CB.tag.mat.summ$h.CB[2]),linetype=2, alpha=0.05)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(.5,.5,-.1, 0), "cm"))
CBtag.graph
#to put 2 plots togethger
#figCBA - tag plus video
setwd("~/PROJECT_GrouperMoon/TagCountSurveySimulations/AbundanceEstimation/2018_bothislands_withvideo")
grid.arrange(CBtag.graph,figCBA, nrow = 2, 
             left=textGrob("Population Estimate (# Fish)", rot = 90, vjust = 1))
