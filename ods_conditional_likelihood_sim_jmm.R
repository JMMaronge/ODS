library(nlme)
set.seed(1104)

pop.m<-20000 # number of clusters
pop.n<- 5 # number within clusters
beta<-5 #slope for indicator 
sigma<- 1 #overall standard deviation in the linear model
tau_e<-0.8 #error correlation
reps=10000


beta.diff.est<-vector(length = reps)
beta.x_ibar.est<-vector(length = reps)
for(i in 1:reps){
  u<-rnorm(pop.m,mean = 0, sd=sqrt(sigma*tau_e)) #cluster samples
  u1<-rep(u,each=pop.n) # repeat each cluster sample n times
  estar<-rnorm(pop.m*pop.n,mean = 0, sd=sqrt(sigma*(1-tau_e))) # samples within each cluster
  err<-u1+estar #total error
  
  x<-rnorm(u1,1)
  x<-as.numeric(x>2.5)
  y<-beta*x+err
  dat<-data.frame(y=y,x=x,id=rep(c(1:pop.m),each=pop.n)) #make data
  agg.dat<-aggregate(y~id, dat, sum) # sum y by id
  case.samp<-sample(agg.dat$id[agg.dat$y>12],50) #sample cases
  control.samp<-sample(agg.dat$id[agg.dat$y<12],50)# sample controls
  samp<-c(case.samp, control.samp)
  samp.dat<-subset(dat,dat$id%in%samp) # get dataframe for sampled ids
  samp.agg.dat<-aggregate(x~id, samp.dat, mean) #calculate means for x
  x_ibar<-rep(samp.agg.dat$x,each=pop.n) # mach means dimensions with dat
  samp.dat$x_ibar<-x_ibar
  samp.dat$diff<-samp.dat$x-samp.dat$x_ibar # calculate x_ij-x_ibar
  
  fit<-lme(y~diff+x_ibar, data = samp.dat, random = ~1|id)
  beta.diff.est[i]<-fixed.effects(fit)[2]
  beta.x_ibar.est[i]<-fixed.effects(fit)[3]
}

hist(beta.diff.est)
hist(beta.x_ibar.est)