UPM.gof <-
function(X,y,m=c(4,6),method,indx,...){  #indx is the indices for the validation set
  X<-as.matrix(X)
  z<-y
  extraparms<-list(...)
  if(method=="knn"){
    if(is.null(extraparms$k)){
      extraparms$k<-ceiling(sqrt(nrow(X)))
    }
  }
  
  X<-as.matrix(X)
  
  upm.args<-c(list(X=X[-indx,],y=z[-indx],X.test=as.matrix(X[indx,]),m=m,method.ml=method),extraparms)
  UPM.out<-do.call(UPM,upm.args)
  y <- UPM.out$y
  y.test<- z[indx]-UPM.out$cond.mean
  dhat.list <- UPM.out$dhat
  U <- rep(NA, length(indx))
  for(i in 1:length(U)){
    U[i] <- Dfun_int(y.test[i],  dhat.list[[i]],y)
  }
  
  lpinfor <- sum(apply(LEG.fun(U,m=6),FUN="mean",2)^2) #LPINFOR
  pval <- 1- pchisq(length(indx)*lpinfor,df=6)
  
  par(mfrow=c(1,2))
  hist(U,20,prob=TRUE)
  abline(h=1,col="blue")
  
  plot(sort(U),ecdf(U)(sort(U)),xlab="U",ylab="")
  abline(0,1,col="blue",lwd=1.5)
  
  out<-list()
  out$q.residuals <- U
  out$qdiv <- lpinfor
  out$pval <- pval
  return(out)
}
