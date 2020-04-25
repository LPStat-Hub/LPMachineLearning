GSP <-
function(X,y,comp,mx=NULL){
  X<-as.matrix(X)
  if(!is.null(mx)){
    Tx<-LP.basis(X,m=mx)
  }
  z<-y
  Ty<-as.matrix(lpbasis(z,m=max(comp)))[,comp]
  colnames(Ty)<-paste0('Ty',comp)
  
  
  if(is.null(mx)){
    coefarr<-array(NA,c(1,length(comp),ncol(X)),dimnames = list(c('X'),paste0('Ty',comp),colnames(X)))
    for(i in 1:length(comp)){
      opt.lasso <- cv.glmnet(X, Ty[,i], family="gaussian", type.measure="mse",nfolds=20)
      fit.lasso <- glmnet(X, Ty[,i], family="gaussian", lambda=opt.lasso$lambda.1se)
      coef0<-fit.lasso$beta
      coefarr[1,i,]<-as.numeric(coef0)
    }
  }else{
    coefarr<-array(NA,c(mx,length(comp),ncol(X)),dimnames = list(paste0('Tx',1:mx),paste0('Ty',comp),colnames(X)))
    for(i in 1:length(comp)){
      coefmat0<-matrix(NA,ncol(X),mx)
      opt.lasso <- cv.glmnet(Tx, Ty[,i], family="gaussian", type.measure="mse",nfolds=20)
      fit.lasso <- glmnet(Tx, Ty[,i], family="gaussian", lambda=opt.lasso$lambda.1se)
      coef0<-fit.lasso$beta
      for(j in 1:ncol(X)){
        for(k in 1:mx){
          x.name<-paste0('X',j,'T',k)
          ind0<-which(rownames(coef0)==x.name)
          if(length(ind0)==1){
            coefmat0[j,k]<-as.numeric(coef0[ind0])
          }
        }
      }
      coefarr[,i,]<-coefmat0
    } 
  }
  signif.mat<-as.matrix(apply(coefarr,c(2,3),function(x){sum(x^2,na.rm=TRUE)})>0)+0
  out<-list(coef=coefarr,signif.mat=signif.mat)
  return(out)
  
}
