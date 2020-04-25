LP.basis <-
function(X,m,pivot=NULL,Fmid=TRUE){
  X <- as.matrix(X)
  
  Xcols<-split(X, rep(1:ncol(X), each = nrow(X)))
  
  if(!is.null(pivot)|Fmid==FALSE){
    Tcols<-lapply(Xcols,lpbasis,m,pivot)
  }else{
    Tcols<-lapply(Xcols,eLP.univar,m)
  }
  colname_list<-c()
  for(i in 1:length(Tcols)){
    mi<-ncol(Tcols[[i]])
    colname_list<-c(colname_list,paste0('X',i,'T',1:mi))
  }
  T0<-do.call(cbind,Tcols)
  colnames(T0)<-colname_list
  return(T0)
}
