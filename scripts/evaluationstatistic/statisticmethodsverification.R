reductionOfVariance=function(rmseF,rmseAF){
  if (rmseAF!=0){
  rv=1-(rmseF^2/rmseAF^2)
  return(rv)
  }
  else("error: /0")
}



pec=function(x){
  
  #x=getValueofPrediction(model_file, "wrf")
  
  a=sum(x$A)+sum(x$D)
  #print(a)
  b=sum(x$A)+sum(x$B)+sum(x$C)+sum(x$D)+0.00001
 # print(b)
  pec=a/b
  return(pec)
}

pod=function(x){
  a=sum(x$A)
  b=sum(x$A)+sum(x$C)+0.00001
  pod=a/b
  return(pod)
}

far=function(x){
  a=sum(x$B)
  b=sum(x$A)+sum(x$B)+0.00001
  far=a/b
  return(far)
  
  
}

ths=function(x){
  a=sum(x$A)
  b=sum(x$A)+sum(x$B)+sum(x$C)+0.00001
  ths=a/b
  return(ths)
}