lpbasis <-
function(y,m,pivot=NULL){
  if(is.null(pivot)){
    #case1
    Ty<-LEG.fun(ecdf(y)(y),m=m)
  }else if(max(is(pivot)=='function')){
    #case2
    lb<-min(y)
    G<- function(y) {integrate(pivot, lower =lb , upper = y)$value}
    G <- Vectorize(G)
    u <- G(y)
    Ty <- as.matrix(LEG.fun(u, m = m))
  }else if(max(is(pivot)=='numeric')){
    #case3
    u.c3 <- ecdf(pivot)(y)
    Ty <- LEG.fun(u.c3,m=m)
  }
  return(Ty) 
}
