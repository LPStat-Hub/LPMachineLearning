
HCA<-function(X,y,m=c(4,6),alpha=0.05,method.ml='glmnet'){ 
  X<-as.matrix(X)
  Tx<-LP.basis(X,m[1])
  Ty<-LP.basis(y,m[2])
  colnames(Ty)<-paste('Ty',1:m[2],sep='')
  
  method.list<-c('glmnet','subset','lm')
  method.ml<-match.arg(method.ml, method.list)
  
  p.val <- dev.ratio<-f.stat <- rep(0,m[2])
  x.sig<-matrix(0,m[2],ncol(X))
  dict<-colnames(Tx)
  for(i in 1:m[2]){
    
    switch(method.ml,'glmnet'={
      opt.lasso <- glmnet::cv.glmnet(Tx, Ty[,i], family="gaussian", type.measure="mse",nfolds=20)
      fit.lasso <- glmnet::glmnet(Tx, Ty[,i], family="gaussian", lambda=opt.lasso$lambda.1se)
      ind<-which(abs(fit.lasso$beta)>0)
      reg.dat<-as.data.frame(cbind(Ty[,i],Tx[, ind]))
      names(reg.dat)<-c('Tyj',colnames(Tx)[ind])
      frmla<-'Tyj~.-1'
    },
    'subset'={
      reg.dat<-as.data.frame(cbind( Ty[,i],Tx))
      colnames(reg.dat)[1]<-'Tyj'
      if(ncol(Tx)>1){
        if(ncol(Tx)>50){big.flag=TRUE}else{big.flag=FALSE}
        fit1 <- leaps::regsubsets(Tyj~., data = reg.dat,intercept=FALSE,really.big=big.flag)
        
        id<-which.min(summary(fit1)$bic)
        coefi <- coef(fit1, id = id)
        
        frmla<-paste0('Tyj~',names(coefi)[1])
        if(length(coefi)>1){
          for(term_id in 2:length(coefi)){
            frmla<-paste0(frmla,'+',names(coefi)[term_id])
          }
        }
        frmla<-paste0(frmla,'-1')
        ind<-coefi
      }else{
        frmla<-'Tyj~.-1'
        colnames(reg.dat)[1]<-'Tyj'
        ind<-colnames(Tx)
      }
    },
    'lm'={
      reg.dat<-as.data.frame(cbind( Ty[,i],Tx))
      colnames(reg.dat)[1]<-'Tyj'
      frmla<-'Tyj~.-1'
      ind<-colnames(Tx)
    }
    
    )
    if(length(ind)==0){ 
      dev.ratio[i]<-0
      p.val[i] <- 1
    }
    else{ 
      slm <-summary(lm( as.formula(frmla),data=reg.dat))
      dev.ratio[i] <- slm$r.squared 
      fstat <- slm$fstatistic
      f.stat[i] <- fstat[1]
      p.val[i] <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
    }
  }
  
  #Heterogeneity Diagnostic Plot
  par(mfrow=c(1,2))
  dev.ratio.plot <- dev.ratio
  dev.ratio.plot[p.val >alpha] <- 0
  f.plot<-f.stat
  f.plot[p.val >alpha] <- 0
  
  bars1<-matrix(dev.ratio.plot,nrow=1)
  bars2<-matrix(f.plot,nrow=1)
  
  colnames(bars1)<-colnames(bars2)<-paste0('LP',1:m[2])
  barplot(bars1,col="bisque",xlab='components',ylab=expression(R[LP]^2*"  Heterogeneity Statistic"),main="") 
  barplot(bars2,col="lightblue",xlab='components',ylab=expression(F[LP]*"  Heterogeneity Statistic"),main="")  
  
  out<-list(f.stat=f.stat, dev.rate=dev.ratio, pval=p.val)
  
  return(out)
  
}