####This is a sample r file####

rm(list=ls())

#OLS function
ols<-function(n, mu, sd, a, b){
  #create variables
  x<-rnorm(n, mu, sd)
  e<-rnorm(n, 0, 1)
  y<-a+b*x+e
  X<-cbind(1, x)
  Y<-cbind(y)
  #regression
  b_hat<-solve(t(X)%*%X)%*%(t(X)%*%Y)
  #order
  z<-data.frame(b_hat)
  alpha<-z[1,]
  b_hat<-z[2,]
  #return values
  return(list("alpha"=alpha, "b_hat"=b_hat))
}

#test function
ols(1000, 3, 1, 7, 9)
