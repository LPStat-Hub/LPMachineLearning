UPM.quantile <-
function(UPM.obj,probs=c(.1,.9)){  #UPM.obj is the UPM object: output of UPM function 
  z <- UPM.obj$z
  z.axe=seq(min(z),max(z),length.out=1000)
  k <- length(UPM.obj$cond.den)  # how many test samples are there
  Q.mat <- matrix(NA, k,length(probs))
  for(i in 1:k){
    f.hat <- UPM.obj$cond.den[[i]](z.axe)   
    ycs = (cumsum(f.hat) - (f.hat - f.hat[[1]])/2) * diff(z.axe[1:2])
    ycs = ycs/(ycs[[length(ycs)]])
    xin = z.axe
    maxi = length(ycs)
    qqs = sapply(as.list(probs), function(qu) {
      iii = sum(ycs <= qu)
      if (iii == maxi) 
        return(Inf)
      else if (iii == 0L) 
        return(-Inf)
      else {
        return(xin[[iii + 1]] + ((ycs[[iii + 1]] - qu)/(ycs[[iii + 
                                                               1]] - ycs[[iii]])) * (xin[[iii]] - xin[[iii + 
                                                                                                         1]]))
      }
    })
    Q.mat[i,] <- qqs
  }
  return(Q.mat)
}
