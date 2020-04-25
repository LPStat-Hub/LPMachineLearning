Dfun_int <-
function(y0,dhat,y,B=1000,tol=5e-3){   ##y0:target y value; dhat: conditional density function; y:list of centered observations.
  d.vals<-dhat(y)
  d.vals[d.vals<0]<-0
  u.Ymid <- (rank(y,ties.method = c("average")) - .5)/length(y)
  int_fun<- approxfun(as.numeric(u.Ymid),d.vals,
                      rule=2,method="linear",f=1)
  int_area <- as.numeric(integrate(int_fun,lower=0,upper=1,rel.tol=.0001,
                                   stop.on.error=FALSE)$value)
  integrate(int_fun,lower=0,upper=ecdf(y)(y0),subdivisions=B,rel.tol=tol,
            stop.on.error=FALSE)$value/int_area
}
