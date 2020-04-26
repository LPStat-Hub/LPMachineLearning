#paper-figs_upm
setwd("C:/Users/palan/Documents/deep/CodeCompile-LPUPM") 
install.packages("C:/Users/palan/Documents/deep/CodeCompile-LPUPM/currentbuild/LPMachineLearning_1.0.tar.gz", repos = NULL, type="source")

library(LPMachineLearning)



##---------------------------------------------##
####        Assisting Functions:             ####
##---------------------------------------------##
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


##---------------------------------------------##
####            Fig1:                        ####
##---------------------------------------------##

##---------------------------------------------##
####            Fig2:                        ####
##---------------------------------------------##
data(butterfly)
attach(butterfly)

X.test=2
UPM.out<-UPM(x,y,X.test,pivot=function(x){dnorm(x,mean(y),sd(y))},m=c(6,4),method.ml='knn',k=15)

####2(a)####
X.test=2
dtrue<-function(x){
  dnorm(x,mean=X.test)*.5+dnorm(x,mean=-X.test)*.5
}
y.axe=seq(-8,7.5,length.out=1000)
par(mar=c(4.2,4,2,2))
plot(y.axe,dtrue(y.axe),type="l",col="black",lwd=1.3,xlab="y",ylab="",main="",lty=2)
lines(y.axe,dnorm(y.axe,mean(y),sd(y)),col='dodgerblue2',lwd=1.8)
legend('topleft',legend=c('True',expression('Pivot: N(0, 2.5'^2*' )')),col=c('black','dodgerblue2'),lty=c(2,1),cex=.95)

####2(b)####
dhat.plot.norm(UPM.out,10,mu.x0=mean(y),sd.x0=sd(y))  
abline(h=1,lty=2,col='bisque3')

####2(c)####
##Note: HCA seem to have problem when running one dimension with glmnet and subset, check code later
m=c(4,6)
hca.out<-HCA(x,y,m=m,alpha=.05,method.ml='lm')
par(mar = c(4.2, 4.5, 2, 2))  #bottom, left, top and right margins 
dev.ratio <- hca.out$dev.rate
dev.ratio[hca.out$pval >.05] <- 0
bars<-matrix(dev.ratio,nrow=1)
colnames(bars)<-paste0('LP',1:m[2])
barplot(bars,col="bisque",xlab='components',ylab=expression(R[LP]^2*"  Heterogeneity Statistic"),main="")  #Heterogeneity Content

####2(d)####
UPM.out<-UPM(x,y,X.test,pivot=pivot,m=c(6,4),method.ml='knn',k=15,nsample=2000, credMass=.68)
y.axe=seq(-5.25,5.25,length.out=1000)
plot(y.axe,UPM.out$cond.den[[1]](y.axe),col='dodgerblue2',lwd=1.8,type="l",main="",xlab="",ylab="")

hdin<-UPM.out$hdi.laser$hdi[[1]]
segments(hdin[, 1], 0.005, hdin[, 2], 0.005, lwd=2, col='darkgoldenrod1')
hdin


####2(e)####
X.test<-seq(-4,4,length=100)
#The World Health Organisation uses 100u=(3, 15, 50, 85, 97) in its charts
UPM.out<-UPM(x,y,X.test,m=c(2,6),method.ml='lm',quantile.probs=c(.05,.3,.5,.7,.95))
Q.mat <- UPM.out$quantiles

par(mar=c(4.2,4,2,2))
plot(x,y,cex=.8,col="gray55",xlab="x",ylab="y",ylim=c(-6.5,5.55))
lines(smooth.spline(X.test,Q.mat[,1]), lty = 1, col = "dodgerblue2",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,2]), lty = 1, col = "forestgreen",lwd=1.85)
lines(X.test,rep(0,nrow(Q.mat)), lty = 1, col = "red",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,4]), lty = 1, col = "mediumorchid2",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,5]), lty = 1, col = "chocolate1",lwd=1.85)
legend("bottom",c(".95",".70",".50",".30",".05"),
       col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2"),
       lty=1,lwd=1.8, cex=0.65, inset=.02,horiz = TRUE)

####2(f)####

set.seed(129)
n<-length(y)
indx<- sample(1:n,floor(.15*n))
gof.knn <-UPM.gof(X=x,y=y,m=c(6,4),method="knn",k=15,indx=indx)

U <- gof.knn$q.residuals
hist(U,10,prob=TRUE)
abline(h=1,lty=2,col="blue")

par(mar=c(5,4,2,2))
plot(sort(U),ecdf(U)(sort(U)),xlab="U",ylab="",type="l")
points(sort(U),ecdf(U)(sort(U)), col="dimgrey")
abline(0,1,col="blue",lwd=1.5,lty=2)

detach(butterfly)

##---------------------------------------------##
####            Fig4:butterfly Ty            ####
##---------------------------------------------##
data(butterfly)
attach(butterfly)

Ty<-LP.basis(y, m=4, pivot = function(x){dnorm(x,0,sd(y))} , Fmid = FALSE)
u <- ecdf(x)(x)

knn.fit<-function(x,y,k){
  modelfit<-caret::knnreg(y~.,data=as.data.frame(x),k=k)
  return(predict(modelfit,newdata=data.frame(x)))   #
}

u0 <- .72
kk<-100

par(mar = c(4.5, 5, 4, 2))  #bottom, left, top and right margins 
plot(u,Ty[,1],xlab="u=F(x)",ylab=expression(paste(T[1],"(y)")),cex.lab=1.2,col="grey35",main=expression(paste("L",P[ paste("1|x")])),cex.main=1.65 ) 
lines(u,knn.fit(x,Ty[,1],k=kk),col="red",lwd=2)
segments(u0,-2,u0,0,lty=2,col="goldenrod1",lwd=2)
points(u0,-.05,col='blue',pch=19,cex=1.5)
mtext(expression(u[0]), side=1, line=.1, at=u0,col="red",cex=.8)

par(mar = c(4.5, 5, 4, 2))  #bottom, left, top and right margins 
plot(u,Ty[,2],xlab="u=F(x)",ylab=expression(paste(T[2],"(y)")),cex.lab=1.2,col="grey35",main=expression(paste("L",P[ paste("2|x")])),cex.main=1.65 ) 
lines(u,knn.fit(x,Ty[,2],k=kk),col="red",lwd=2)
segments(u0,-1.6,u0,-.2,lty=2,col="goldenrod1",lwd=2)
points(u0,-.225,col='blue',pch=19,cex=1.5)
mtext(expression(u[0]), side=1, line=.1, at=u0,col="red",cex=.8)

par(mar = c(4.5, 5, 4, 2))  #bottom, left, top and right margins 
plot(u,Ty[,3],xlab="u=F(x)",ylab=expression(paste(T[3],"(y)")),cex.lab=1.2,col="grey35",main=expression(paste("L",P[ paste("3|x")])),cex.main=1.65 ) 
lines(u,knn.fit(x,Ty[,3],k=kk),col="red",lwd=2)
segments(u0,-2.86,u0,-.01,lty=2,col="goldenrod1",lwd=2)
points(u0,0.03,col='blue',pch=19,cex=1.5)
mtext(expression(u[0]), side=1, line=.1, at=u0,col="red",cex=.8)

par(mar = c(4.5, 5, 4, 2))  #bottom, left, top and right margins 
plot(u,Ty[,4],xlab="u=F(x)",ylab=expression(paste(T[4],"(y)")),cex.lab=1.2,col="grey55",main=expression(paste("L",P[ paste("4|x")])),cex.main=1.65 ) 
lines(u,knn.fit(x,Ty[,4],k=kk),col="red",lwd=2)
segments(u0,-1.4,u0,-.42,lty=2,col="goldenrod1",lwd=2)
points(u0,-.45,col='blue',pch=19,cex=1.5)
mtext(expression(u[0]), side=1, line=.1, at=u0,col="red",cex=.8)

detach(butterfly)


##---------------------------------------------##
####            Fig5: bupa                   ####
##---------------------------------------------##
data(bupa)
attach(bupa)

####5(a):gbm fit and plot####
library(h2o)
h2o.init()
h2o.no_progress()
reg.dat<-as.h2o(cbind(y,x))
modelfit<- h2o.gbm(y=1, training_frame = reg.dat) #h2o.randomForest(y=1, training_frame = reg.dat)
ind<-which(x<=3.6&x>=3&y>4)
yhat <- as.matrix(predict(modelfit,reg.dat))

par(mar=c(5,4.5,2,2))
plot(x[-ind],y[-ind],xlab="log(GGT)",ylab="log(ALT)",col="grey15",cex.lab=1.2)
lines(x,yhat,col="blue2",lwd=2)
points(3.5,1.34,col="red",pch=17,cex=2)
points(x[ind],y[ind],col='grey15',pch=0,cex=.9)
mtext(expression(paste(x[0],"=3.5")), side=1, line=.55, at=3.5,col="red",cex=1.3)


####5(b):contrast density####

x0<-3.5
fit.reg<- smooth.spline(x,y)
mu.x0 <- predict(fit.reg,x0)$y
sd.x0 <- sd(y)
X.test=x0

UPM.out<-UPM(x,y,X.test,pivot=function(x){dnorm(x,mu.x0,sd.x0)},m=c(4,4),method.ml='gbm')
dhat.plot.norm(UPM.out,12,mu.x0,sd.x0)   ##this line changed as I corrected the dhat.plot.norm code
abline(h=1,lty=2,col='bisque3')


####5(c):conditonal density####

par(mar=c(5,4.5,2,2))
y.axe=seq(1.5,5.4,length.out=1000)
plot(y.axe,UPM.out$cond.den[[1]](y.axe),type="l",col="dodgerblue2",lwd=2,
     xlab="y",ylab="",ylim=c(0,1.5))
lines(y.axe,dnorm(y.axe,mu.x0,sd.x0),col="black",lwd=1.5,lty=2,xlab="y",ylab="")
legend('topright',legend=c(expression('Pivot N(3.35,.5'^2*')'),'Estimated f(y|x)'),col=c('black','dodgerblue2'),lty=c(2,1),cex=.88)  ## change Pivot N(3.35,.5^2)

detach(bupa)

##---------------------------------------------##
####        Fig6: bone and OnlineNews        ####
##---------------------------------------------##
data(bone)
attach(bone)
#######6(a)###### 
plot(x,y,xlab="Age",ylab="Relative spinal bone mineral density",col="grey25")
fit.reg<- smooth.spline(x,y)
yhat<-predict(fit.reg,x)$y
lines(x, yhat, col="red",lwd=2)
#######6(b)###### 
m=c(2,6)
comp_result<- HCA(x,y-yhat,m=m,method.ml='lm')
dev.ratio <- comp_result$dev.rate
dev.ratio[comp_result$pval >.05] <- 0
bars<-matrix(dev.ratio,nrow=1)
colnames(bars)<-paste0('LP',1:m[2])
par(mar = c(4.2, 4.5, 2, 2),mfrow=c(1,1))  #bottom, left, top and right margins 
barplot(bars,col="bisque",xlab='components',ylab=expression(R[LP]^2*"  Heterogeneity Statistic"),main="",ylim=c(0,.2))  #plotting prob: axis stops at .15

detach(bone)

#######6(c)###### 
data("onlineNews")
X<-onlineNews[,-60]
y<-onlineNews[,60]

h2o.init()
h2o.no_progress()
reg.dat<-as.h2o(cbind(y,X))
modelfit<- h2o.gbm(y=1, training_frame = reg.dat) #h2o.randomForest(y=1, training_frame = reg.dat)
yhat <- as.matrix(predict(modelfit,reg.dat))

rr <- y-yhat
s.res <- rr/sd(rr)  #standardized residuals
hist(s.res,50,col="grey95",prob=TRUE,main=" ",xlab="standardized residuals",xlim=c(-4.5,5))
lines(density(s.res),col="blue",lwd=2)

#######6(d)###### 
m=c(4,6)
comp_result<- HCA(X,y-yhat,m=m,method.ml='lm')
dev.ratio <- comp_result$dev.rate
dev.ratio[comp_result$pval >.05] <- 0
bars<-matrix(dev.ratio,nrow=1)
colnames(bars)<-paste0('LP',1:m[2])
par(mar = c(4.2, 4.5, 2, 2),mfrow=c(1,1))  #bottom, left, top and right margins 
barplot(bars,col="bisque",xlab='components',ylab=expression(R[LP]^2*"  Heterogeneity Statistic"),main="")  #plotting prob: axis stops at .15


##---------------------------------------------##
####             Fig7: film Data             ####
##---------------------------------------------##

data("boxOffice")
attach(boxOffice)

####7(a)#####
par(mar=c(5,4.5,2,2))
X.test<-c(12,14)
fc <-  c("cornflowerblue", "lightseagreen")  #"dodgerblue2","forestgreen", "chocolate1"
plot(x[x>9],y[x>9],cex=.8,col="gray70",ylab="After first week box office revenues",xlab="Opening day box office revenues",cex.lab=.9)
abline(v=X.test,col=fc,lty=2,lwd=2)


####7(b)#####
pivot=function(x){dnorm(x,14.47,sd(y))}
y.axe=seq(8,21,length.out=1000)
plot(y.axe, pivot(y.axe),col="lightsalmon1",type="l",ylab="Pivot Density",xlab="",lwd=2)

####7(c)#####
X.test<-12
UPM.out.gbm<-UPM(x,y,X.test,m=c(2,6),method.ml='gbm',pivot=pivot)
dhat.plot.norm(UPM.out.gbm,df=12,mu.x0=14.47,sd.x0=sd(y),col=fc[1])   ##this line changed as I corrected the dhat.plot.norm code
abline(h=1,lty=2,col='bisque3')


####7(d)#####
X.test<-14
UPM.out.gbm.2<-UPM(x,y,X.test,m=c(6,4),method.ml='gbm',pivot=pivot)
dhat.plot.norm(UPM.out.gbm.2,12,14.47,sd(y),col=fc[2])   ##this line changed as I corrected the dhat.plot.norm code
abline(h=1,lty=2,col='bisque3')

####7(e)#####
y.axe=seq(9,20,length.out=1000)
par(mar=c(5,4.5,2,2))
plot(y.axe,UPM.out.gbm$cond.den[[1]](y.axe),type="l",col=fc[1],lwd=2,
     xlab="y",ylab="",main="")
points(c(12.5,16.1),c(-.001,-.001),pch=17,col="indianred1",cex=1.2)		

####7(f)#####
plot(y.axe,UPM.out.gbm.2$cond.den[[1]](y.axe),type="l",col=fc[2],lwd=2,
     xlab="y",ylab="",main="")
points(14.45,-.001,pch=17,col="indianred1",cex=1.2)	

detach(boxOffice)


##---------------------------------------------##
####       Fig8: goodness-of-fit             ####
##---------------------------------------------##
data(butterfly)
attach(butterfly)
n=length(y)
set.seed(129)
indx<- sample(1:n,floor(.15*n))
gof.knn<-UPM.gof(x,y,m=c(6,4),method="knn",k=15,indx=indx)
gof.knn$pval
U.knn <- gof.knn$q.residuals
gof.rf<-UPM.gof(x,y,m=c(6,4),method="rf",indx=indx,ntrees = 20,max_depth = 10)
gof.rf$pval
U.rf <- gof.rf$q.residuals
gof.gbm<-UPM.gof(x,y,m=c(6,4),method="gbm",indx=indx)
gof.gbm$pval
U.gbm <- gof.gbm$q.residuals

###hist
par(mar=c(5,4,2,2),mfrow=c(1,3))

hist(U.knn,10,prob=TRUE,col="grey95",main='KNN',xlab="U")
abline(h=1,col="blue",lty=2)

hist(U.rf,10,prob=TRUE,col="grey95",main='RF',xlab="U")
abline(h=1,col="blue",lty=2)

hist(U.gbm,10,prob=TRUE,col="grey95",main='GBM',xlab="U")
abline(h=1,col="blue",lty=2)

##qq plots
par(mar=c(5,4,2,2),mfrow=c(1,3))

plot(sort(U.knn),ecdf(U.knn)(sort(U.knn)),xlab="U",ylab="",type="l",main='KNN')
points(sort(U.knn),ecdf(U.knn)(sort(U.knn)), col="dimgrey")
abline(0,1,col="blue",lwd=1.5,lty=2)

plot(sort(U.rf),ecdf(U.rf)(sort(U.rf)),xlab="U",ylab="",type="l",main='RF')
points(sort(U.rf),ecdf(U.rf)(sort(U.rf)), col="dimgrey")
abline(0,1,col="blue",lwd=1.5,lty=2)

plot(sort(U.gbm),ecdf(U.gbm)(sort(U.gbm)),xlab="U",ylab="",type="l",main='GBM')
points(sort(U.gbm),ecdf(U.gbm)(sort(U.gbm)), col="dimgrey")
abline(0,1,col="blue",lwd=1.5,lty=2)

detach(butterfly)


##---------------------------------------------##
####       Fig9:sample v sharpen             ####
##---------------------------------------------##
data(butterfly)
attach(butterfly)

####fig9(a):####
X.test=2
dtrue<-function(x){
  dnorm(x,mean=X.test)*.5+dnorm(x,mean=-X.test)*.5
}
y.c3<- y[x<4 & x>0]  #--pick a smart samples to start with
ygrid <- seq(-6,6,length=1000)
par(mar=c(5,4.5,2,2)) # control the margins before plotting
hist(y.c3,col="grey95",20,prob=TRUE,ylim=c(0,.2),xlab="",main="")
box()
lines(ygrid,dtrue(ygrid),col='dodgerblue2',lwd=2)


####fig9(b):####
set.seed(3302)
UPM.out<-UPM(x,y,X.test,ref.info=y.c3,m=c(4,4),method.ml='knn',k=15,nsample=length(y.c3))
y.c3.sharp <- UPM.out
par(mar=c(5,4.5,2,2))
hist(y.c3.sharp,col="grey95",20,prob=TRUE,ylim=c(0,.21),xlab="",main="",xlim=c(-5.5,5.5))
box()
lines(ygrid,dtrue(ygrid),col='dodgerblue2',lwd=2)

detach(butterfly)



##---------------------------------------------##
####       Fig10:LDLcholesterol              ####
##---------------------------------------------##
data("cholesterol")
attach(cholesterol)

####Fig 10(a)####
par(mar = c(4.2, 4.5, 2, 2))   #bottom, left, top and right margins
boxplot(y~x,names=c("I", "II", "III", "IV"),xlab='',ylab="LDL cholesterol levels")
mtext(side=1, text="Drug Compounds", line=2.5)

####Fig 10(b)####
m=c(length(unique(x))-1,4)
comp_result<- HCA(x,y,m=m,method.ml='lm',alpha=NULL)

par(mar = c(4.2, 4.5, 2, 2))  #bottom, left, top and right margins
dev.ratio <- length(x)*comp_result$dev.rate
bars<-matrix(dev.ratio,nrow=1)
colnames(bars)<-c("KW*","MOOD*","SKEW","KURT")
barplot(bars,col="bisque",xlab='Components of K-sample Analysis',ylab="",main="",ylim=c(0,12)) 

detach(cholesterol)

##---------------------------------------------##
####       Fig11:baseball cden               ####
##---------------------------------------------##

data(baseball)
attach(baseball)

#Fig 11 (a)
plot(x,y,xlab="Age",ylab="Weight" ,col="dimgrey")
#Fig 11 (b)
plot(density(y[x<26&x>24]),col="darksalmon",xlim=c(135,280),ylim=c(0,.021),main="",xlab="")
lines(density(y[x<30&x>29]),col="orange")
lines(density(y[x<36&x>34]),col="firebrick2")
lines(density(y[x>40]),col="darkred")
leg1<-expression('age: 25' %+-% 1)
leg2<-expression('age: 30' %+-% 1)
leg3<-expression('age: 35' %+-% 1)
legend("topright",c(leg1,leg2,leg3,"age > 40"),col=c("darksalmon", "orange", "firebrick2","darkred"),lty=1,lwd=1.5,cex=.9)

detach(baseball)

##---------------------------------------------##
####        Fig12:FeatureChart               ####
##---------------------------------------------##

library(reshape2)
library(ggplot2)
library(pheatmap)

data("onlineNews")
X<-onlineNews[,-60]
y<-onlineNews[,60]

coefmat<-matrix(0,3,ncol(X))

X<-as.matrix(X)
Tx<-eLP.poly(X,m=1)
Ty<-as.matrix(lpbasis(y,m=3))
set.seed(51)
for(i in 1:3){
  opt.lasso <- cv.glmnet(Tx, Ty[,i], family="gaussian", type.measure="mse",nfolds=20)
  fit.lasso <- glmnet(Tx, Ty[,i], family="gaussian", lambda=opt.lasso$lambda.1se)
  coef0<-fit.lasso$beta
  coefmat[i,]<-as.numeric(coef0)
}

ind1<-which(abs(coefmat[1,])>0)
ind2<-which(abs(coefmat[2,])>0)
ind3<-which(abs(coefmat[2,])>0)

indall<-union(ind1,union(ind2,ind3))

ind_sig<-order(colSums(coefmat^2),decreasing = TRUE)[1:10]
coefmat1<-t(abs(coefmat[,ind_sig]))
varnames<-colnames(X)[ind_sig]

rownames(coefmat1)<-varnames
colnames(coefmat1)<-c('location','scale','skewness')

draw_colnames_45 <- function (coln, gaps, ...) {
  coord <- pheatmap:::find_coordinates(length(coln), gaps)
  x     <- coord$coord - 0.5 * coord$size
  res   <- grid::textGrob(
    coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"),
    vjust = 1.25, hjust = 0.5, rot = 0, gp = grid::gpar(...)
  )
  return(res)
}
assignInNamespace(
  x = "draw_colnames",
  value = "draw_colnames_45",
  ns = asNamespace("pheatmap")
)
pheatmap(coefmat1,color=blues9,cluster_cols = FALSE,cluster_rows = FALSE)



##---------------------------------------------##
####        Fig13:FEV DIF                    ####
##---------------------------------------------##
library(ggplot2)
library(reshape2)
library(cowplot)
data("rosnerFEV")
attach(rosnerFEV)
m<-c(2,4)
method<-'gbm'
X.test<-10:19

DIF_out<-DIF(x,y,z,m=m,X.test=X.test,method=method)

##fours plots of kde pairs


plot_age=c(12:15)
plotlist<-list()
for(i in 1:length(plot_age)){
  age<-plot_age[i]
  den1<-density(y[x==age &z==0])
  den2<-density(y[x==age &z==1])
  x.axe<-seq(min(den1$x,den2$x),max(den1$x,den2$x),length.out=500)
  y.val1<-approx(den1$x,den1$y,x.axe,rule=2)$y
  y.val2<-approx(den2$x,den2$y,x.axe,rule=2)$y
  D0<-data.frame(x=x.axe,y1=y.val1,y2=y.val2)
  dfden<-melt(D0,id='x')
  plotlist[[i]]<-ggplot(data=dfden)+geom_line(aes(x=x,y=value,color=variable))+
    scale_colour_manual(values=c("y1"='steelblue','y2'='red'),labels=c("Non-Smoker", "Smoker"))+
    xlab('')+ylab('')+labs(title=paste0('Age=',age))+
    theme(text=element_text(size=13),
          plot.title = element_text(size=13,hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.line = element_line(),
          legend.position="bottom",
          legend.title=element_blank())
}

l1<-get_legend(plotlist[[1]])
denplots_main<-plot_grid(plotlist[[1]]+theme(legend.position="none"),
                         plotlist[[2]]+theme(legend.position="none"),
                         plotlist[[3]]+theme(legend.position="none"),
                         plotlist[[4]]+theme(legend.position="none"),ncol=2)
denplots<-plot_grid(denplots_main,l1,ncol=1,rel_heights = c(1,.13))

##barplot for DIF unpacking

CIFcomp<-matrix(0,4,4)
for(i in 1:4){
  CIFcomp[i,]<-DIF_out$comp.DIF[2+i,]
}
colnames(CIFcomp)<-c('location','scale','skewness','tail')

databar<-as.data.frame(CIFcomp)
databar$age=paste0('Age=',12:15)

bar_df<-melt(databar,id='age')
pbar<-ggplot(data=bar_df,aes(x=age, y=value, fill=variable))+
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  ylab('Distributional differences')+xlab('')+
  scale_fill_brewer(palette="Blues",direction=-1)+ #labs(fill = "Contribution:")+
  theme(text=element_text(size=13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=14),
        axis.line = element_line(),
        legend.position="bottom",
        legend.title=element_blank())

full_plot<-plot_grid(pbar,denplots,nrow=1,rel_widths=c(.4,.6))
full_plot

detach(rosnerFEV)


##---------------------------------------------##
####        Fig14:dutch quantile             ####
##---------------------------------------------##

data(dutch)
attach(dutch)
###goal is condtional quantile
X.test <- as.matrix(c(seq(min(x),3,length=30), seq(3,max(x),length=70)))
UPM.out<-UPM(x,y,X.test,m=c(6,6),method.ml='glmnet',quantile.probs = c(.03,.15,.5,.85,.97))
Q.mat <- UPM.out$quantiles
  
par(mar=c(4,4,2,2))
plot(x,y,cex=.4,col="gray75",xlab="Age",ylab="BMI",cex.lab=1.15)
lines(smooth.spline(X.test,Q.mat[,1]), lty = 1, col = "dodgerblue2",lwd=1.5)
lines(smooth.spline(X.test,Q.mat[,2]), lty = 1, col = "forestgreen",lwd=1.5)
lines(smooth.spline(X.test,Q.mat[,3]), lty = 1, col = "red",lwd=1.5)
lines(smooth.spline(X.test,Q.mat[,4]), lty = 1, col = "mediumorchid2",lwd=1.5)
lines(smooth.spline(X.test,Q.mat[,5],df=25), lty = 1, col = "chocolate1",lwd=1.5)
legend("topleft",c(".97",".85",".50",".15",".03"),col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2"),lty=1,lwd=1.5,cex=.9)

detach(dutch)

##---------------------------------------------##
####        Fig15:PredictionInt              ####
##---------------------------------------------##
####Fig15(a)####
data(butterfly)
attach(butterfly)
UPM.out<-UPM(x,y,X.test=2,pivot=function(x){dnorm(x,mean(y),sd(y))},m=c(6,4),
             method.ml='knn',k=15,nsample=length(y), credMass=.68,quantile.probs =c(.05,.95))

par(mar=c(4,4,2,2))
y.axe=seq(-5.25,5.25,length.out=1000)
plot(y.axe,UPM.out$cond.den[[1]](y.axe),col='dodgerblue2',lwd=1.8,type="l",main="",xlab="",ylab="",
     ylim=c(-.005,.22),xlim=c(-5,5.5))
points(UPM.out$cond.mean[1],.01,pch=16,cex=1.25,col="red")
points(c(-2,2),rep(.01,2),pch=16,cex=1.25,col="darkgreen")
legend("topright",c("cond mean", "cond mode"),pch=c(16,16),col=c("red","darkgreen"),cex=1)

##hdPI
hdin<-UPM.out$hdi.laser$hdi[[1]]
segments(hdin[, 1], 0.005, hdin[, 2], 0.005, lwd=1.5, col='darkgoldenrod1')
segments(hdin, rep(0.007,2), hdin, rep(0.003,2), lwd=1.5, col='darkgoldenrod1')
hdin

##QuantilePI
Q.mat <- UPM.out$quantiles
segments(Q.mat[1], 0, Q.mat[2], 0, lwd=1.5, col='azure4')
segments(Q.mat, rep(.002,2), Q.mat, rep(-.002,2), lwd=1.5, col='azure4')

##SE-based PI
sd0<-sd(UPM.out$samples)
segments(-sd0, -.006, sd0, -.006, lwd=1.5, col='chocolate3')
segments(c(-sd0,sd0), rep(-.008,2), c(-sd0,sd0), rep(-.004,2), lwd=1.5, col='chocolate3')
legend("topleft",c("hdPI","qPI","gPI"),col=c('darkgoldenrod1',"azure4",'chocolate3'),lty=1,lwd=1.5,cex=.9)

detach(butterfly)

####Fig15(b)####
data("autompg")
X<-autompg[,-8]
y<-autompg[,8]
ind <- 390  #390 is the dodge
X.test=X[ind,]

set.seed(1)
UPM.out<-UPM(X,y,X.test,m=c(4,6),method.ml='gbm',max_depth=10, credMass=.68,nsample=1000,quantile.probs=c(.16,.84))
k=1
y.axe=seq(29.5,36.5,length.out=1000)
ss<-smooth.spline(y.axe,UPM.out$cond.den[[k]](y.axe),df=15)
yhat<-ss$y
yhat[yhat<0]<-0
plot(ss$x,yhat,type='l',col='dodgerblue2',lwd=2,
     xlab='',ylab='',main="",ylim=c(-.05,max(yhat)),xlim=c(30.5,35.5))
points(UPM.out$cond.mean[k],.05,pch=16,cex=1.25,col="red")
points(y[ind[k]],.05,pch=16,cex=1.25,col="darkgreen")
legend("topright",c("cond mean", "actual mpg"),pch=c(16,16),col=c("red","darkgreen"),cex=1)

hdin<-UPM.out$hdi.laser$hdi[[1]]
segments(hdin[, 1], 0.02, hdin[, 2], 0.02, lwd=2, col='darkgoldenrod1')
segments(hdin, rep(0.005,2), hdin, rep(0.035,2), lwd=1.5, col='darkgoldenrod1')
#hdin

##qPI
Q.mat <- UPM.out$quantiles
segments(Q.mat[,1], -.02, Q.mat[,2], -.02, lwd=2, col='azure4')
segments(Q.mat, rep(-.035,2), Q.mat, rep(-0.005,2), lwd=2, col='azure4')

#gPI
s<-sd(UPM.out$samples)
mm<-mean(UPM.out$samples)

segments(mm-s, -0.06, mm+s, -0.06, lwd=2, col='chocolate3')
segments(c(mm-s,mm+s), rep(-.075,2), c(mm-s,mm+s), rep(-.045,2), lwd=2, col='chocolate3')
legend("topleft",c("hdPI","qPI","gPI"),col=c('darkgoldenrod1',"azure4",'chocolate3'),lty=1,lwd=1.5,cex=.9)



##---------------------------------------------##
####            Fig16:AutoMPG                ####
##---------------------------------------------##

data("autompg")
X<-autompg[,-8]
y<-autompg[,8]

x<-sort(X[,4])
m<- 4
TX <- LP.basis(x,m)
par(mfrow=c(2,2),mar=c(3,3,3,2))
ux<-ecdf(x)(x)
plot(ux,TX[,1],type="s",ylab="",xlab="",col="red",main = expression(T[1](x)) , lwd=2,cex.main=2,cex.axis=1.1  )
plot(ux,TX[,2],type="s",ylab="",xlab="",col="red",main = expression(T[2](x)) , lwd=2,cex.main=2,cex.axis=1.1  )
plot(ux,TX[,3],type="s",ylab="",xlab="",col="red",main = expression(T[3](x)) , lwd=2,cex.main=2,cex.axis=1.1  )
plot(ux,TX[,4],type="s",ylab="",xlab="",col="red",main = expression(T[4](x)) , lwd=2,cex.main=2,cex.axis=1.1  )

x<-sort(X[,5])
TX <- LP.basis(x,m)
ux<-ecdf(x)(x)
plot(ux,TX[,1],type="s",ylab="",xlab="",col="red",main = expression(T[1](x)) , lwd=2,cex.main=2,cex.axis=1.1  )
plot(ux,TX[,2],type="s",ylab="",xlab="",col="red",main = expression(T[2](x)) , lwd=2,cex.main=2,cex.axis=1.1  )
plot(ux,TX[,3],type="s",ylab="",xlab="",col="red",main = expression(T[3](x)) , lwd=2,cex.main=2,cex.axis=1.1  )
plot(ux,TX[,4],type="s",ylab="",xlab="",col="red",main = expression(T[4](x)) , lwd=2,cex.main=2,cex.axis=1.1  )


##---------------------------------------------##
####      Fig17&19:butterfly quantreg        ####
##---------------------------------------------##

data(butterfly)
attach(butterfly)
fc<-c("dodgerblue2", "forestgreen", "red", "mediumorchid2", "chocolate1")

####Fig 17####
library(quantreg)
par(mar=c(5,4.5,2,2))
plot(x,y,cex=.8,col="gray55",xlab="x",ylab="y",ylim=c(-6.5,5.55))
taus <- c(.05,.3,.5,.7,.95)
xx <- seq(min(x),max(x),length=100)
f <- coef(rq(y~x,tau=taus))
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
  lines(xx,yy[,i],col =fc[i],lwd=1.65)
}
legend("bottom",c(".95",".70",".50",".30",".05"),col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2")
       ,lty=1,lwd=1.8,horiz=TRUE, cex=0.75, inset=.02)


####Fig 19(a)####
library(grf)
q.forest<-quantile_forest(as.matrix(x),as.matrix(y),quantiles = c(.05,.3,.5,.7,.95))
yy = predict(q.forest, as.matrix(xx),quantiles = c(.05,.3,.5,.7,.95))
plot(x,y,cex=.8,col="gray55",xlab="x",ylab="y",ylim=c(-6.5,5.55))
for(i in 1:length(taus)){
  #lines(xx,yy[,i],col =fc[i],lwd=1.65)
  lines(smooth.spline(xx,yy[,i]),col =fc[i],lwd=1.65)
}
legend("bottom",c(".95",".70",".50",".30",".05"),col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2")
       ,lty=1,lwd=1.8,horiz=TRUE, cex=0.75, inset=.02)


####Fig 19(b)####
library(gbm)
reg.dat<-data.frame(y=y,x=x)
yy <- vector("list", length(taus))
set.seed(11)
for(i in seq_along(taus)){
  model<-gbm(y~x,data=reg.dat,distribution=list(name = "quantile", alpha = taus[i] ))
  best.iter <- gbm.perf(model, method = "OOB")
  yy[[i]] <-predict(model, newdata = data.frame(x=xx), n.trees = best.iter, type = "response")
}
plot(x,y,cex=.8,col="gray55",xlab="x",ylab="y",ylim=c(-6.5,5.55))
for(i in 1:length(taus)){
  lines(smooth.spline(xx,yy[[i]]),col =fc[i],lwd=1.65)
}
legend("bottom",c(".95",".70",".50",".30",".05"),col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2")
       ,lty=1,lwd=1.8,horiz=TRUE, cex=0.75, inset=.02)


####Fig 19(c)####
X.test<-seq(-4,4,length=100)
UPM.rf<-UPM(x,y,X.test,m=c(2,6),method.ml='rf',quantile.probs = c(.05,.3,.5,.7,.95), ntrees = 20,max_depth = 5)
UPM.gbm<-UPM(x,y,X.test,m=c(2,6),method.ml='gbm',quantile.probs = c(.05,.3,.5,.7,.95), ntrees = 20,max_depth = 5)

Q.mat <-UPM.rf$quantiles
plot(x,y,cex=.8,col="gray55",xlab="x",ylab="y",ylim=c(-6.5,5.55))
lines(smooth.spline(X.test,Q.mat[,1]), lty = 1, col = "dodgerblue2",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,2]), lty = 1, col = "forestgreen",lwd=1.85)
lines( smooth.spline(X.test,Q.mat[,3]), lty = 1, col = "red",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,4]), lty = 1, col = "mediumorchid2",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,5]), lty = 1, col = "chocolate1",lwd=1.85)
legend("bottom",c(".95",".70",".50",".30",".05"),col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2"),
       lty=1,lwd=1.8,horiz=TRUE, cex=0.65, inset=.02)



####Fig 19(d)####
Q.mat <-UPM.gbm$quantiles
plot(x,y,cex=.8,col="gray55",xlab="x",ylab="y",ylim=c(-6.5,5.55))
lines(smooth.spline(X.test,Q.mat[,1]), lty = 1, col = "dodgerblue2",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,2]), lty = 1, col = "forestgreen",lwd=1.85)
lines( smooth.spline(X.test,Q.mat[,3]), lty = 1, col = "red",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,4]), lty = 1, col = "mediumorchid2",lwd=1.85)
lines(smooth.spline(X.test,Q.mat[,5]), lty = 1, col = "chocolate1",lwd=1.85)
legend("bottom",c(".95",".70",".50",".30",".05"),col=c("chocolate1", "mediumorchid2", "red","forestgreen","dodgerblue2"),
       lty=1,lwd=1.8,horiz=TRUE, cex=0.65, inset=.02)



detach(butterfly)



##---------------------------------------------##
####            Fig18 Boston House           ####
##---------------------------------------------##

########################### Page 47: plot ################################
library(mlbench)
data( BostonHousing)
X<-BostonHousing[,1:13]
y<-BostonHousing[,14]
X<-data.matrix(X)

n<-length(y)

j=1
par(mar=c(4,4,2,2))
x<-X[,j]
plot(x,y,xlab="x=crime",ylab="y=price",main="",cex=.7,col='gray55',cex.lab=1.15)
splinexy <- smooth.spline(x,y)
lines(splinexy,col="red",lwd=2)

#exy <- sum(abs(y-predict(splinexy,x=x)$y))

u=rank(x)/n
plot(u,y,xlab="u=F(x)",ylab="y=price",main="",cex=.7,col='gray55',cex.lab=1.15)
splineuv <- smooth.spline(u,y)
lines(splineuv,col="red",lwd=2)

