
DIF<-function(X,y,z,m=c(2,4),X.test,method='gbm',...){
  extraparms<-list(...)
  X<-as.matrix(X)
  X.test<-matrix(X.test,ncol=ncol(X))
  
  out<-list(X.test=X.test)
  
  DIF.list<-rep(0,nrow(X.test))
  DIFcomp<-matrix(0,nrow(X.test),m[2])
  
  h2o.init()
  h2o.no_progress()
  reg.dat<-as.h2o(data.frame(y=y,X=X))
  regfun<-switch(method,'gbm'=h2o.gbm,'rf'=h2o.randomForest)
  arglist<-c(list(y=1, training_frame = reg.dat),extraparms)
  modelfit<- do.call(regfun,arglist)
  mu0 <- as.matrix(predict(modelfit,newdata=as.h2o(data.frame(X=X.test))))
  
  for(i in 1:nrow(X.test)){
    D.test<-rbind(c(X.test[i,],0),c(X.test[i,],1))
    pivot=function(x){dnorm(x,mu0[i],sd(y))}
    
    arglisti<-c(list(X=cbind(X,z),y=y,X.test=D.test,m=m,method.ml=method,pivot=pivot,nsample=length(y)),extraparms)
    LPcden.out<-do.call(UPM,arglisti)
    
    DIFcomp[i,]<-(LPcden.out$LP.coef[1,]-LPcden.out$LP.coef[2,])^2
    DIF.list[i]<-sum(DIFcomp[i,])
    
  }
  
  out$DIF<-DIF.list
  out$comp.DIF<-DIFcomp
  #ouput individual square difference (number of X.text by my), remove plots but retain the barplot codes in the examples.
  #output changes: only output DIF(vector); amd comp.DIF (matrix)
  return(out)
  
}