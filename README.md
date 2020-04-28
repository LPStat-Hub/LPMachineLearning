# LPMachineLearning <a href='https://github.com/LPML-hub/LPMachineLearning/'></a>
Main files for LPMachineLearning package. This is package provide 
Statistical modeling tools for converting a black-box ML algorithm
into an interpretable conditional distribution prediction machine, which
provides a wide range of facilities, including goodness-of-fit, various
types of exploratory graphical diagnostics, generalized feature selection,
predictive inference methods, and others.

More details can be found in [R package documentation](https://github.com/LPML-hub/LPMachineLearning/blob/master/LPMachineLearning-manual.pdf).


### Installation

Currently private repository, user can install it using the devtools:

```R
library(devtools)
install_github("LPML-hub/LPMachineLearning",auth_token = 'b59b80e7308e43df6ed800597149c63748a3b9eb')
```

### Usage Examples


Once installed, the package provide tools for following tasks:

#### Heterogeneity Component Analysis

This is an example given in section 3.1 of the reference paper, using the Online news popularity data set.
```{r, results='hide'}
library(h2o)
library(LPMachineLearning)
#loading data
data(onlineNews)
X<-onlineNews[,-60]
y<-onlineNews[,60]
#computing the mean estimates
h2o.init()
h2o.no_progress()
reg.dat<-as.h2o(cbind(y,X))
modelfit<- h2o.gbm(y=1, training_frame = reg.dat) #h2o.randomForest(y=1, training_frame = reg.dat)
yhat <- as.matrix(predict(modelfit,reg.dat))
```
```{r}
#heterogeneity analysis on residual series
HCA(X,y-yhat,m=c(4,6),method.ml='lm')
```
#### Pivot Uncertainty modeling
This example uses the Movie box-office revenue data(Voudouris et al., 2012). The goal of this study is to build a forecasting model for film revenue that predicts the distribution of $y$ given some $x$. In this case, we use $x=12$ for demonstration. 
```{r}
library(LPMachineLearning)
data(boxOffice)
attach(boxOffice)

X.test<-12
fc <-  c("cornflowerblue", "lightseagreen")  #"dodgerblue2","forestgreen", "chocolate1"
plot(x[x>9],y[x>9],cex=.8,col="gray70",ylab="After first week box office revenues",xlab="Opening day box office revenues",cex.lab=.9)
abline(v=X.test,col=fc,lty=2,lwd=2)

```



Usually the user will provide a pivot as an initial "guess" of the conditional distribution at $x=12$:

```{r}
pivot=function(x){dnorm(x,14.47,sd(y))}
y.axe=seq(8,21,length.out=1000)
plot(y.axe, pivot(y.axe),col="lightsalmon1",type="l",ylab="Pivot Density",xlab="",lwd=2)
```

This pivot will be used to compute a contrast density:
```{r,results='hide'}
X.test<-12
UPM.out<-UPM(x,y,X.test,m=c(2,6),method.ml='gbm',pivot=pivot)

#auxiliary function for plotting contrast density:
dhat.plot.norm<-function(LPcden.obj,df=15,mu.x0,sd.x0,col='steelblue'){
  y<-LPcden.obj$y
  for(plotid in 1:length(LPcden.obj$dhat)){
    
    dhat.obj<-LPcden.obj$dhat[[plotid]]
    u.axe<-seq(0.01,.99,length.out=1000)   ###this line changed
    y.axe<-qnorm(u.axe,mu.x0 ,sd.x0)    ###this line changed
    dvals<-dhat.obj(y.axe)
    delta0<-diff(range(dvals))
    par(mar=c(5,4.5,2,2))
    uaxe<-seq(0.01,.99,length.out=6)   ###this line changed
    zaxe<-qnorm(uaxe,mu.x0 ,sd.x0)    ###this line changed
    splineres<-smooth.spline(u.axe,dvals,df=df)
    
    y.acc<-floor(log10(max(splineres$y)))
    ymax<-round(max(splineres$y)/10^(y.acc))*10^(y.acc)
    
    plot(splineres$x,splineres$y,col=col,type='l',lwd=2,
         xlab='',ylab='',main='',cex.axis=.8, ylim=c(0,ymax),
         frame.plot=FALSE,xaxt='n')
    axis(side=1,at=uaxe,col='darkred',tcl=.3,line=1.1,labels=FALSE)
    axis(side=1,at=uaxe,col='gray0',tcl=-.3,line=1.25,labels=FALSE)
    text(uaxe, par("usr")[3] + 0.013*delta0, col='darkred',
         labels = seq(0,1,length.out=6), cex=.8, pos = 1, xpd = TRUE)
    text(uaxe, par("usr")[3] - 0.08*delta0, col='gray0',
         labels = round(zaxe,1), cex=.8, pos = 1, xpd = TRUE)
    mtext(expression(hat(d)),cex=1, side=2,line=2)
    mtext('u', side = 1, line = -.3, outer = FALSE, at = c(min(uaxe)-diff(range(uaxe))/20),col='darkred')
    mtext('y', side = 1, line = 1.2, outer = FALSE, at = c(min(uaxe)-diff(range(uaxe))/20),col='gray0')
  }
  
}
```
```{r}
dhat.plot.norm(UPM.out,df=12,mu.x0=14.47,sd.x0=sd(y),col=fc[1])   
abline(h=1,lty=2,col='bisque3')
```

Multiplying the pivot with this contrast density will net us the conditional density at $x=12$:
```{r}
y.axe=seq(9,20,length.out=1000)
par(mar=c(5,4.5,2,2))
plot(y.axe,UPM.out$cond.den[[1]](y.axe),type="l",col=fc[1],lwd=2,
     xlab="y",ylab="",main="")
points(c(12.5,16.1),c(-.001,-.001),pch=17,col="indianred1",cex=1.2)	
detach(boxOffice)	
```


#### Goddness of Fit

This tool provides graphical diagnostics and test statistic to check whether the models from UPM are congruent with the observed data. We use our stylized simulated example in the reference paper for demonstration:
```{r,results='hide'}
library(LPMachineLearning)
data(butterfly)
attach(butterfly)

n=length(y)
set.seed(129)
indx<- sample(1:n,floor(.15*n))
gof.knn<-UPM.gof(x,y,m=c(6,4),method="knn",k=15,indx=indx)
U.knn <- gof.knn$q.residuals
```
```{r}
par(mfrow=c(1,2))
#histogram:
hist(U.knn,10,prob=TRUE,col="grey95",main='Histogram',xlab="U")
abline(h=1,col="blue",lty=2)
#QQ plot:
plot(sort(U.knn),ecdf(U.knn)(sort(U.knn)),xlab="U",ylab="",type="l",main='QQ plot')
points(sort(U.knn),ecdf(U.knn)(sort(U.knn)), col="dimgrey")
abline(0,1,col="blue",lwd=1.5,lty=2)

```

#### d-sharpening
Here we show how we can provide sharpened samples compatible with the underlying stochastic data generating mechanism using our contrast density. Continue from the butterfly data set in the previous example, we want to obtain sharpened samples at $x=2$. First, we provide some sample points from the interval $x\in [0,4]$

```{r}
X.test=2
#true density at x=2:
dtrue<-function(x){
  dnorm(x,mean=X.test)*.5+dnorm(x,mean=-X.test)*.5
}
#pivot samples
y.c3<- y[x<4 & x>0]  #--pick a smart samples to start with
ygrid <- seq(-6,6,length=1000)
par(mar=c(5,4.5,2,2)) # control the margins before plotting
hist(y.c3,col="grey95",20,prob=TRUE,ylim=c(0,.2),xlab="",main="")
box()
lines(ygrid,dtrue(ygrid),col='dodgerblue2',lwd=2)
```

Using our `UPM()` function with argument `nsample=length(y.c3)` will generate the d-sharpened samples at $x=2$:
```{r,results='hide'}
set.seed(3302)
UPM.out<-UPM(x,y,X.test,ref.info=y.c3,m=c(4,4),method.ml='knn',k=15,nsample=length(y.c3))
y.c3.sharp <- UPM.out
```

```{r}
hist(y.c3.sharp,col="grey95",20,prob=TRUE,ylim=c(0,.21),xlab="",main="",xlim=c(-5.5,5.5))
box()
lines(ygrid,dtrue(ygrid),col='dodgerblue2',lwd=2)
detach(butterfly)
```

#### K-sample Problem
Our method also shares a connection with k-sample testing problem. Here we use the LDL cholesterol data set, and want to know whether different drug compounds change the LDL cholesterol levels. The hetergeneity component analysis gives us direct answer:
```{r,results='hide'}
data(cholesterol)
attach(cholesterol)
m=c(length(unique(x))-1,4)
comp_result<- HCA(x,y,m=m,method.ml='lm',alpha=NULL)
```
```{r}
par(mar = c(4.2, 4.5, 2, 2))  #bottom, left, top and right margins
dev.ratio <- length(x)*comp_result$dev.rate
bars<-matrix(dev.ratio,nrow=1)
colnames(bars)<-c("KW*","MOOD*","SKEW","KURT")
barplot(bars,col="bisque",xlab='Components of K-sample Analysis',ylab="",main="",ylim=c(0,12)) 
detach(cholesterol)
```

#### GSP


#### DIF

#### Quantile Regression


#### Prediction Interval


### References

Mukhopadhyay, S., and Wang, K. (2020)
<b>Breiman's 'Two Cultures' Revisited and Reconciled</b>. <i>Technical Report</i>.
