g2l.sampler <-function(n.simu=length(Y),LP.par,Y,pivot=NULL,clusters=NULL){
  lp_basis <- lpbasis(Y,length(LP.par),pivot=pivot)
  if(is.null(pivot)){
    f0<-ecdf(Y)
    pivot.sample<-Y
  }else if(max(is(pivot)=='numeric')){
    f0<-ecdf(pivot)
    pivot.sample<-pivot
  }else{
    dcustom<-pdqr::as_d(pivot)
    f0<-pdqr::as_p(dcustom)
    rf0<-pdqr::as_r(dcustom)
    pivot.sample<-as.numeric(rf0(n.simu))
  }
  d.val <- approxfun(f0(Y),1+lp_basis%*%LP.par, rule=2,method="constant",f=1 )
  d.max <- max(d.val(Y))
  sample_iter<-function(iter,dfun,M,Y){
    #for fixing result, remove afterwards.
    #set.seed(50*iter)
    for(i in 1:1000){
      u1 <- runif(1)
      if(dfun(u1) > runif(1)*M){
        out<-quantile(pivot.sample,u1);
        break;
      }else{
        out<-NA
      }
    }
    return(as.numeric(out))
  }

  iters<-as.matrix(1:n.simu)

  if(is.null(clusters)){
    out<-sapply(X=iters,FUN=sample_iter,
                dfun=d.val,M=d.max,Y=Y)
  }else{
    out<-parallel::parSapply(cl=clusters,X=iters,FUN=sample_iter,
                             dfun=d.val,M=d.max,Y=Y)
  }
  out<-as.numeric(out)
  return(out)
}
