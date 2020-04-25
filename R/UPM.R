UPM <-
function(X,y,X.test,pivot=NULL,m=c(4,6),method.ml='glmnet',LP_smooth='BIC',
              nsample=NULL, quantile.probs=NULL, credMass=.6, 
			  centering=TRUE,parallel=FALSE,...){ 
  extraparms<-list(...)
  z<-y
  X<-as.matrix(X)
  X.test<-matrix(X.test,ncol=ncol(X))
  out<-list()
  multivar=0
  if(ncol(X)>1){multivar=1}
  method.list<-c('glmnet','subset','knn','gbm','svm','rf')
  method.ml<-match.arg(method.ml, method.list)
  if(method.ml %in% c("gbm","rf")){
    h2o::h2o.init()
    h2o::h2o.no_progress()
  }else if(method.ml=="knn"){
    if(is.null(extraparms$k)){
      extraparms$k<-ceiling(sqrt(nrow(X)))
    }
  }
  
  
  Tx<-LP.basis(X,m[1])
  
  ##centering codes; Using LP, with selected method of smoothing
  
  if(max(is(pivot)=='function')){centering<-FALSE}
  if(centering==TRUE){
    centerproc<-z.lp.center(X,Tx,z,method.ml,X.test,m,extraparms)
    y<-centerproc$y
    z.mu.test<-centerproc$z.mu.test
    zmean<-centerproc$zmean
  }else{
    y<-z
    z.mu.test<-rep(0,nrow(X.test))
    zmean<-rep(0,length(z))
  }
  
  
  ##how to adjust pivot so it fits y?
  
  Ty<-lpbasis(y,m=m[2],pivot=pivot)
  colnames(Ty)<-paste('Ty',1:m[2],sep='')
  
  
  #####Getting Coefficients
  LP.coef<-matrix(0,nrow(X.test),m[2])
  colnames(LP.coef)<-paste('LP[',1:m[2],']',sep='')
  for(i in 1:nrow(X.test)){
    X.test0<-matrix(X.test[i,],nrow=1)
    LP.coef0<-sapply(1:m[2],LPregression,Tx,Ty,X,X.test0,m,method.ml,extraparms)
    if(!is.null(LP_smooth)&method.ml=='subset'){
      LP.coef[i,]<-LP.smooth(LP.coef0,n=length(y),method=LP_smooth)
    }else if(method.ml=='subset'){
      LP.coef[i,]<-LP.coef0[1,]
    }else{
      LP.coef[i,]<-LP.coef0
    }
  }
  out$LP.coef<-LP.coef
  out$cond.mean<-z.mu.test
  out$y.res<-y
  out$Ty<-Ty
  out$z<-z
  out$centering<-centering
  
  #####Getting conditional density
  
  dhat<-cond.denZ<-list()
  for(i in 1:nrow(X.test)){
    LPcoef <- LP.coef[i,]
    if(is.null(pivot)){
      #case1
      pooly<-density(y)$y
      y0<-density(y)$x
    }else if(max(is(pivot)=='function')){
      #case2
      y0<-seq(min(y),max(y),length.out=1000)
      g0 <- Vectorize(pivot)
      pooly<-g0(y0)
    }else if(max(is(pivot)=='numeric')){
      #case3
      pooly<-density(pivot)$y
      y0<-density(pivot)$x
    }
    Ty0<-Predict.LP.poly(y,Ty,y0)
    
    z.mu<-z.mu.test[i]
    dd<- 1+as.vector(LPcoef%*%t(Ty))
    dd[dd<0]<-0
    d.val <- approxfun(y,dd,method='linear',rule=2)
    condYX<-pooly*d.val(y0)
    condYX[condYX<0]<-0
    dhat[[i]]<- d.val
    cond.denZ[[i]]<-approxfun(y0+z.mu,condYX,method='linear',rule=2)
  }
  
  out$cond.den<-cond.denZ
  out$dhat<-dhat
  
  #####Getting samples
  if(!is.null(nsample)){
    if(parallel==TRUE){
      numCores<-round(detectCores()/2)
      cl<-makeCluster(numCores)
    }else{
      cl<-NULL
    }
    
    sample.list<-matrix(0,nsample,nrow(X.test))
    for(i in 1:nrow(X.test)){
      Lcoef<-as.matrix(LP.coef[i,])
      z.mu<-z.mu.test[i]
      sample.list[,i]<-g2l.sampler(nsample,LP.par=Lcoef,Y=y,clusters=cl)+z.mu
    }
    colnames(sample.list)<-paste0('Xtest',1:nrow(X.test))
    
    if(parallel==TRUE){
      stopCluster(cl)
    }
    
    out$samples<-sample.list
    ######HDIntervals
    hdi.list<-list(x=X.test,hdi=list())
    for(i in 1:nrow(X.test)){
      z.laser<-sample.list[,i]
      hdi.list$hdi[[i]]<-HDInterval::hdi(density(z.laser),credMass,allowSplit=TRUE)
    }
    out$hdi.laser<-hdi.list
  }
  
  if(!is.null(quantile.probs)){
	out$quantiles<-UPM.quantile(out, probs = quantile.probs)
  }
  return(out)
}
