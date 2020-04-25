z.lp.center <-
function(X,Tx,z,method.ml,X.test,m,extraparms){
  ##centering codes; Using LP, with selected method of smoothing
  if(method.ml=='glmnet'){
    fit.mean<-glmnet::cv.glmnet(Tx,z,family="gaussian",intercept=TRUE)
    meanvals<-predict(fit.mean,newx=Tx, s="lambda.1se",type='response')
  }else if(method.ml=='subset'){
    centered.flag<-0
    Tx<-LP.basis(X,m[1])
    reg.dat=as.data.frame(cbind(z,Tx))
    if(ncol(Tx)>50){big.flag=TRUE}else{big.flag=FALSE}
    fit1 <- leaps::regsubsets(z~., data = reg.dat,intercept=TRUE,really.big=big.flag)
    id<-which.min(summary(fit1)$bic)
    coefi <- coef(fit1, id = id)
    if(length(coefi)<2){
      centered.flag<-1
      meanvals<-rep(mean(z),length(z))
      z.mu<-mean(z)
    }else{
      frmla<-paste0('z~',names(coefi)[2])
      if(length(coefi)>2){
        for(i in 2:length(coefi)){
          frmla<-paste0(frmla,'+',names(coefi)[i])
        }
      }
      lm.fit <- lm(as.formula(frmla),data.frame(Tx))
      meanvals=predict(lm.fit,data.frame(Tx))
    }
  }else if(method.ml =="svm"){
    reg.dat0<-as.data.frame(cbind( z,Tx))
    method.ml1<-match.arg(method.ml, c("svmRadial","gbm","rf"))
    if(is.null(extraparms$distribution)){extraparms$distribution="gaussian"}
    if(is.null(extraparms$verbose)){extraparms$verbose=FALSE}
    arglist<-c(list(form=as.formula('z~.'),data=reg.dat0,method=method.ml1),extraparms)
    modelfit<-do.call(train,arglist) 
    meanvals<-predict(modelfit,data.frame(Tx))
  }else if(method.ml=='knn'){
    if(is.null(extraparms$k)){
      extraparms$k=ceiling(sqrt(nrow(X)))
    }
    arglist<-c(list(formula=as.formula('z~.'),data=as.data.frame(Tx)),extraparms)
    modelfit<-do.call(caret::knnreg,arglist)
    meanvals<-predict(modelfit,data.frame(Tx))
  }else if(method.ml %in% c('gbm','rf')){
    reg.dat0<-as.h2o(cbind( z,Tx))
    fun0<-switch(method.ml,'gbm'=h2o.gbm,'rf'=h2o.randomForest)
    arglist<-c(list(y=1, training_frame = reg.dat0),extraparms)
    modelfit<- do.call(fun0,arglist)
    meanvals<-as.matrix(predict(modelfit,reg.dat0))
  }
  y<-z-meanvals
  z.mu.test<-rep(0,nrow(X.test))
  for(i in 1:nrow(X.test)){
    Tx.test<-eLP.poly.predict(X,Tx,X.test[i,],mx=m[1])
    if(method.ml=='glmnet'){
      z.mu<-predict(fit.mean,newx=as.matrix(Tx.test), s="lambda.1se",type='response')
    }else if(method.ml=='subset'){
      if(centered.flag==0){z.mu<-predict(lm.fit,Tx.test)}
    }else if(method.ml %in% c("svm","knn")){
      z.mu<-predict(modelfit,Tx.test)
    }else if(method.ml %in% c("gbm","rf")){
      z.mu<-as.matrix(predict(modelfit,h2o::as.h2o(Tx.test)))
    }
    z.mu.test[i]<-as.numeric(z.mu)
  }
  out<-list(y=y,zmean=meanvals,z.mu.test=z.mu.test)
  return(out)
}
