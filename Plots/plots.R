## Time-stamp: <2017-09-16 22:51:22 rathouz>

## This is all largely based on code from Jake M; PR just modified for his
## own use

library(ggplot2)
library(ggmosaic)
library(prodlim)
## setwd("~/Documents/Rathouz/Code/")
set.seed(474)

## Density plot
y11<-rnorm(5000,-2,2) ###generate data for D- population in density plot
y12<-rnorm(5000,4,3)
y<-c(y11,y12)
y21<-rnorm(5000,10,4) ###generate data for D pop. in density plot
y22<-rnorm(5000,14,3)
y2<-c(y21,y22)
df=data.frame(y1=y,y2=y2) #final data for pop density plot
rm(y11,y12,y21,y22,y,y2)

## Mosaic plot
datmat<-matrix(nrow=10000,ncol=2) #dumb way of creating data for mosaic plot uneven prop
datmat[(1:300),1]<-0
datmat[(1:300),2]<-"D"
datmat[(301:1000),1]<-1
datmat[(301:1000),2]<-"D"
datmat[(1001:8200),1]<-0
datmat[(1001:8200),2]<-"D-"
datmat[(8201:10000),1]<-1
datmat[(8201:10000),2]<-"D-"
df2<-data.frame(x=datmat[,1],y=datmat[,2]) #final data for mosaic plot
rm(datmat)

## Some graphic parameters
textsize <- 7.5
histbackgrounddens <- 0.15
hscaledens <- (2/3)
hscalecnt <- (1/6)

## Population side-by-side histograms
g <- ggplot(df, aes(x)) +
  geom_histogram(aes(x = y1, y = ..density..),
                 binwidth = diff(range(df$y1))/50, fill="blue") + 
  geom_histogram(aes(x = y2, y = -..density..), binwidth = diff(range(df$y2))/50, fill= "red") +
  geom_hline(yintercept=0,size=.25)+coord_flip()+theme_bw()+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank(),
        axis.title=element_text(size=20))+
  ylim(c(-.16,1)*hscaledens)+xlab("Exposure (X) Distribution")+
  annotate("text",x=25,y=-0.10*hscaledens,label="D (1%)",size=textsize)+
  annotate("text",x=25,y=.625*hscaledens,label="paste(bar(D),  \" (99%)\")",parse=TRUE,size=textsize)+
  annotate("rect",xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = 0,fill="red",alpha=histbackgrounddens)+
  annotate("rect",xmin = -Inf,xmax = Inf,ymin = 0,ymax = Inf,fill="blue",alpha=histbackgrounddens)
print(g) ###density plot
ggsave("density_plot_firstrun.pdf",device = "pdf")

## Population mosaic
g<- ggplot(data=df2)+geom_mosaic(aes(weight=1,x=product(x,y)),fill="white")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.text.y=element_blank(),
        axis.title=element_text(size=20),axis.text=element_text(size=20))+
  ylab("Binary (X) Exposure")+
  annotate("rect",xmin = 0,xmax = 0.1,ymin = .305,ymax = 1,fill="red",alpha=.5)+
  annotate("rect",xmin = 0,xmax = 0.1,ymin = 0,ymax = .295,fill="red",alpha=.5)+
  annotate("rect",xmin = .11,xmax = 1,ymin = .805,ymax = 1,fill="blue",alpha=.5)+
  annotate("rect",xmin = .11,xmax = 1,ymin = 0,ymax = .795,fill="blue",alpha=.5)+
  annotate("text",x=0.05,y=0.68,label="X=1\n(70%)",size=textsize)+
  annotate("text",x=0.55,y=0.90,label="X=1\n(20%)",size=textsize)+
  annotate("text",x=0.05,y=0.14,label="X=0\n(30%)",size=textsize)+
  annotate("text",x=0.55,y=0.42,label="X=0\n(80%)",size=textsize)
print(g)##mosaic plot with proportions
ggsave("mosaic_plot_firstrun.pdf",device = "pdf") 


y11<-rnorm(198,-2,2) # data for sample of size 400 for histogram uneven prop of D- pop
y12<-rnorm(198,4,3)
y<-c(y11,y12)
y21<-rnorm(2,10,4) # data for sample of size 400 for histogram uneven prop of D pop
y22<-rnorm(2,14,3)
y2<-c(y21,y22)
df=data.frame(y1=y,y2=y2) # final data for sample of size 400 for histogram uneven prop
rm(y11,y12,y21,y22,y,y2)

g = ggplot(df, aes(x)) +
  geom_histogram( aes(x = y1, y = ..count..), binwidth = diff(range(df$y1))/50, fill="blue") + 
  geom_histogram( aes(x = y2, y = -..count../100), binwidth = diff(range(df$y2))/50, fill= "red") +
  geom_hline(yintercept=0,size=.25)+coord_flip()+theme_bw()+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank(),
        axis.title=element_text(size=20))+
  xlab("Exposure (X) Distribution")+
  ylim(c(-20,150)*hscalecnt)+
  annotate("text",x=25,y=-13.5*hscalecnt,label="D (n=4)",size=textsize)+
  annotate("text",x=25,y=60*hscalecnt,label="paste(bar(D),  \" (n=396)\")",parse=TRUE,size=textsize)+
  annotate("rect",xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = 0,fill="red",alpha=histbackgrounddens)+  
  annotate("rect",xmin = -Inf,xmax = Inf,ymin = 0,ymax = Inf,fill="blue",alpha=histbackgrounddens)
print(g)   # histogram sample of size 400 for uneven prop
ggsave("histogram_plot_firstrun_n400.pdf",device = "pdf")

###generates numbers for labels on mosaic plot
pop.size<-400
rate.d<-0.01
num.d<-rbinom(1,pop.size,rate.d)
x.1.d<-rbinom(1,num.d,0.7)
x.0.d<-num.d-x.1.d
num.notd<-pop.size-num.d
x.1.notd<-rbinom(1,num.notd,0.2)
x.0.notd<-num.notd-x.1.notd
x.0.d
x.1.d
x.0.notd
x.1.notd


g<-ggplot(data=df2)+geom_mosaic(aes(weight=1,x=product(x,y)),fill="white")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.text.y=element_blank(),
        axis.title=element_text(size=20),axis.text=element_text(size=20))+
  ylab("Binary (X) Exposure")+
  annotate("rect",xmin = 0,xmax = 0.1,ymin = .305,ymax = 1,fill="red",alpha=.5)+
  annotate("rect",xmin = 0,xmax = 0.1,ymin = 0,ymax = .295,fill="red",alpha=.5)+
  annotate("rect",xmin = .11,xmax = 1,ymin = .805,ymax = 1,fill="blue",alpha=.5)+
  annotate("rect",xmin = .11,xmax = 1,ymin = 0,ymax = .795,fill="blue",alpha=.5)+
  annotate("text",x=0.05,y=0.68,label="X=1\n(n=1)",size=textsize)+
  annotate("text",x=0.55,y=0.90,label="X=1\n(n=72)",size=textsize)+
  annotate("text",x=0.05,y=0.14,label="X=0\n(n=0)",size=textsize)+
  annotate("text",x=0.55,y=0.42,label="X=0\n(n=327)",size=textsize)
print(g) # mosaic plot with sample size uneven prop
ggsave("mosaic_plot_firstrun_n400.pdf",device = "pdf") 

y11<-rnorm(100,-2,2)  # data for sample of size 400 for histogram even prop of D- pop
y12<-rnorm(100,4,3)
y<-c(y11,y12)
y21<-rnorm(100,10,4)  # data for sample of size 400 for histogram even prop of D pop
y22<-rnorm(100,14,3)
y2<-c(y21,y22)
df=data.frame(y1=y,y2=y2) # final data for sample size 400 for histogram even prop
rm(y11,y12,y21,y22,y,y2)

g = ggplot(df, aes(x)) +
  geom_histogram( aes(x = y1, y = ..count..), binwidth = diff(range(df$y1))/50, fill="blue") + 
  geom_histogram( aes(x = y2, y = -..count..), binwidth = diff(range(df$y2))/50, fill= "red") +
  geom_hline(yintercept=0,size=.25)+ coord_flip()+theme_bw()+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank(),
        axis.title=element_text(size=20))+
  xlab("Exposure (X) Distribution")+
  ylim(c(-150,150)*hscalecnt)+
  annotate("text",x=25,y=-75*hscalecnt,label="D (n=200)",size=textsize)+
  annotate("text",x=25,y=75*hscalecnt,label="paste(bar(D),  \" (n=200)\")",parse=TRUE,size=textsize)+
  annotate("rect",xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = 0,fill="red",alpha=histbackgrounddens)+  
  annotate("rect",xmin = -Inf,xmax = Inf,ymin = 0,ymax = Inf,fill="blue",alpha=histbackgrounddens)
print(g) # histogram sample size 400 for even prop
ggsave("histogram_plot_firstrun_n400_evenprop.pdf",device = "pdf")

datmat<-matrix(nrow=400,ncol=2) #dumb way of creating data for mosaic plot even prop
datmat[(1:60),1]<-0
datmat[(1:60),2]<-"D"
datmat[(61:200),1]<-1
datmat[(61:200),2]<-"D"
datmat[(201:360),1]<-0
datmat[(201:360),2]<-"D-"
datmat[(361:400),1]<-1
datmat[(361:400),2]<-"D-"
df2<-data.frame(x=datmat[,1],y=datmat[,2]) # final data for mosaic plot even prop
rm(datmat)

###generates numbers for labels of mosaic plots
pop.size<-400
rate.d<-0.50
num.d<-200
x.1.d<-rbinom(1,num.d,0.7)
x.0.d<-num.d-x.1.d
num.notd<-pop.size-num.d
x.1.notd<-rbinom(1,num.notd,0.2)
x.0.notd<-num.notd-x.1.notd
x.0.d
x.1.d
x.0.notd
x.1.notd


g<-ggplot(data=df2)+
  geom_mosaic(aes(weight=1,x=product(x,y)),fill="white")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.text.y=element_blank(),
        axis.title=element_text(size=20),axis.text=element_text(size=20))+
  ylab("Binary (X) Exposure")+
  annotate("rect",xmin = 0,xmax = 0.495,ymin = .305,ymax = 1,fill="red",alpha=.5)+
  annotate("rect",xmin = 0,xmax = 0.495,ymin = 0,ymax = .295,fill="red",alpha=.5)+
  annotate("rect",xmin = .505,xmax = 1,ymin = .805,ymax = 1,fill="blue",alpha=.5)+
  annotate("rect",xmin = .505,xmax = 1,ymin = 0,ymax = .795,fill="blue",alpha=.5)+
  
  annotate("text",x=0.25,y=0.68,label="X=1\n(n=137)",size=textsize)+
  annotate("text",x=0.75,y=0.90,label="X=1\n(n=39)",size=textsize)+
  annotate("text",x=0.25,y=0.14,label="X=0\n(n=63)",size=textsize)+
  annotate("text",x=0.75,y=0.42,label="X=0\n(n=161)",size=textsize)
print(g) # mosaic plot with sample size even prop
ggsave("mosaic_plot_firstrun_n400_evenprop.pdf",device = "pdf") 
