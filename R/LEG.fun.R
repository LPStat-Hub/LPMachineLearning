LEG.fun <-
function(u,m){
  m <- min( length(unique(u))-1, m   )
  sleg.f <- orthopolynom::slegendre.polynomials(m,normalized=TRUE)
  X <- orthopolynom::polynomial.values(sleg.f, u)
  S <- do.call(cbind,X)[,-1]
  return(S)
}
