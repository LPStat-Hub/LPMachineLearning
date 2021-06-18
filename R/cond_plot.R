cond_plot<-function(UPMobj,x.target=NULL,yaxe=NULL,spline_smooth=TRUE,...){
  ##function for plotting conditional density of the UPM output
  ##X.test_id: which conditional density to plot, corresponding to the order of the UPM input "X.test"
  ##UPMobj: the UPM output
  if(is.null(yaxe)){
    yaxe=c(min(UPMobj$y),max(UPMobj$y))
  }
  y.axe<-seq(yaxe[1],yaxe[2],length.out=1000)
  X.test<-UPMobj$X.test
  X.test_id<-which(apply(X.test,1,function(x){sum((x-x.target)^2)==0}))

  if(is.null(x.target)){
    x.target=X.test[1,]
  }
  splineparms<-list(...)
  if(length(X.test_id)==0){
    stop("target X vector not found in UPM object.")
  }else{
    condfun<-UPMobj$cond.den[[X.test_id]]
    if(spline_smooth==FALSE){
      plot(y.axe,condfun(y.axe),type="l",col="cornflowerblue",lwd=2, xlab="y",ylab="",main="")
    }else{
      splineparms$x=y.axe
      splineparms$y=condfun(y.axe)
      scurve<-do.call(smooth.spline,splineparms)
      plot(scurve,main='',type="l",col="cornflowerblue",lwd=2, xlab="y",ylab="")
    }
  }
}
