addtoList<-function(d){
  l<-list()
  for(i in 1:length(d)){
    l[i]<-d[i]
  }
  return(l)
}

findDistribution <- function(dataset, distribution){
  chooseParameters<-list()
  chooseDistributions<-list()
  k=0
  for (column in colnames(dataset)){
    k=k+1
    AIC<-list()
    param<-list()
    for (i in 1:length(distribution)){
      p <- as.vector(dataset[column])[,1]
      fit <- fitdist(p, distribution[i])
      AIC[i]<-fit$aic
      param[i]<-list(addtoList(fit$estimate))
    }
    index<-which.min(AIC)
    chooseDistributions[k]<-distribution[index]
    chooseParameters[k]<-param[index]
  }
  return(list(chooseDistributions,chooseParameters))
}

asCall <- function(fun, param){
  cc <-
    if (length(param) == 0)
      quote(FUN(x))
  else if(is.list(param)) {
    as.call(c(quote(FUN), c(quote(x), as.expression(param))))
  } else {
    as.call(c(quote(FUN), c(quote(x), substitute(param))))
  }
  cc[[1]] <- as.name(fun)
  cc
}

multiplicationVectorMatrix<-function(matrix,wagi){
  u<-as.matrix(matrix)
  x<-u
  for(i in 1:ncol(matrix)){
    x[,i]<-wagi[i]*u[,i]
  }
  return(x)
}

asCall <- function(fun, param){
  cc <-
    if (length(param) == 0)
      quote(FUN(x))
  else if(is.list(param)) {
    as.call(c(quote(FUN), c(quote(x), as.expression(param))))
  } else {
    as.call(c(quote(FUN), c(quote(x), substitute(param))))
  }
  cc[[1]] <- as.name(fun)
  cc
}

policzEX<-function(margins,paramMargins){
  wartosc_oczekiwana<-c()
  for(i in 1:length(margins)){
    #set.seed(42)
    qdf.expr <- asCall(paste0("r", margins[[i]]), paramMargins[[i]])
    x <- mean(eval(qdf.expr, list(x = 10000)))
    wartosc_oczekiwana[i]<-x
  }
  return(wartosc_oczekiwana)
}

Qmoduls <- function(margins,paramMargins,wwagi,means_distributions) {
  Qs<-c()
  dim <- length(margins)
  Q = rep(0,dim)
  for (i in 1:dim) {
    qdf.expr <- asCall(paste0("q", margins[[i]]), paramMargins[[i]])
    Qs <- c(Qs,eval(qdf.expr, list(x = 0.995)))
  }
  Qs_wagi<-(Qs-means_distributions)*wwagi
  return(Qs_wagi)
}

quantileDistributions <- function(data,margins,paramMargins,wwagi) {
  dim <- length(margins)
  x<- as.matrix(data)
  u<-x
  #x<-multiplicationVectorMatric(x,wagi)
  for (i in 1:dim) {
    qdf.expr <- asCall(paste0("q", margins[[i]]), paramMargins[[i]])
    x[,i] <- eval(qdf.expr, list(x = u[,i]))
  }
  ww<-multiplicationVectorMatrix(x,wwagi)
  Q<-quantile(rowSums(ww), 0.995) - mean(rowSums(ww))
  return(list(Q,x))
}

zwracaPodzial<-function(m){
  podzial<-c()
  podzial[1]<-0
  for( i in 1:m){
    podzial[i+1]<-i/m
  }
  return(podzial)
}


