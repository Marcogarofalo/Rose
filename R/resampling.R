comp_errorj<-function(jack){
  a<-mean(jack)
  N<-length(jack)
  err<- sqrt( ((N-1)/N) *sum((jack-a)^2))
  return(err)
}
