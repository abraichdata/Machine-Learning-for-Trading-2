
#This code produces some techincal indicators used for stocks trading 

library(zoo);
library(tseries);

#Functions

#Moving Average

inidicator.MA <- function(p, w){
  
  n <- length(p)
  
  if(w>=n){
    break
  } else{
  
    a <- n-w+1
    b <- w-1
    M <- rep(0, times=a) 
  
  for(i in 1:a){
    M[i] <- mean(p[index(p)[i:b+i]])
  }
  
    M <- as.zoo(M)
    index(M) <- as.yearmon(index(p)[w:n])  
 
    return(M)
  }
}

#Rolling Standard Deviation

indicator.msd <- function(p, w){
  
  n <- length(p)
  
  if(w>=n){
    break
  } else{
    
    a <- n-w+1
    b <- w-1
    s <- rep(0, times=a) 
    
    for(i in 1:a){
      s[i] <- sd(p[index(p)[i:b+i]])
    }
    
    s <- as.zoo(s)
    index(s) <- as.yearmon(index(p)[w:n])  
   
    return(s)
  }
}


indicator.BB <- function(p, w){
  
  BB <- list()
  
  ma <- inidicator.MA(p,w)
  sd <- indicator.msd(p,w)
  
  BB[[1]] <- ma-2*sd
  BB[[2]] <- ma+2*sd
  
  return(BB)
}

#On Balance Volume

indicator.OBV <- function(p){
  
  obv <- diff(p)
  return(obv)
}





#Plotting functions

plot.inidicator.MA <- function(p, w, name){
  
  m <- inidicator.MA(p,w)
  n <- length(p)
  
  plot(p[w:n], type="l", col="black", ylab="Price", main=name)
  lines(m, type="l", col="blue")
  legend(x="topleft", legend=c(paste(c(name, " price"), collapse = ""), paste(c("Moving average with window ", w), collapse = "")), col=c("black", "blue"), lwd=1, cex=0.7)
  
}


plot.indicator.BB <- function(p, w, name){
  
  BB <- indicator.BB(p,w)
  n <- length(p)
  
  plot(p[w:n], type="l", col="black", ylab="Price", main=name)
  lines(BB[[1]], type="l", col="red")
  lines(BB[[2]], type="l", col="red")
  legend(x="topleft",legend=c(paste(c(name, " price"), collapse = ""), paste(c("Borlinger© bands with window ", w), collapse = "")) , col=c("black", "red"), lwd=1, cex=0.7)
  
}



plot.indicator.BBMA <- function(p, w, name){
  
  BB <- indicator.BB(p,w)
  m <- inidicator.MA(p,w)
  n <- length(p)
  
  plot(p[w:n], type="l", col="black", ylab="Price", main=name)
  lines(m, type="l", col="blue")
  lines(BB[[1]], type="l", col="red")
  lines(BB[[2]], type="l", col="red")
  legend(x="topleft",legend=c(paste(c(name, " price"), collapse = ""), paste(c("Moving average with window ", w), collapse = ""), paste(c("Borlinger© bands with window ", w), collapse = "")) , col=c("black", "blue", "red"), lwd=1, cex=0.7)
  
}



#Trials on historical data

SP_500_prices <- get.hist.quote(instrument="^gspc", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(SP_500_prices) <- as.yearmon(index(SP_500_prices))

MSFT_prices <- get.hist.quote(instrument="msft", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(MSFT_prices) <- as.yearmon(index(MSFT_prices))

NASDAQ_prices <- get.hist.quote(instrument="^ixic", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(NASDAQ_prices) <- as.yearmon(index(NASDAQ_prices))

APPLE_prices <- get.hist.quote(instrument="aapl", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(APPLE_prices) <- as.yearmon(index(APPLE_prices))


plot.inidicator.MA(SP_500_prices, 20, "SP500")
plot.inidicator.MA(MSFT_prices, 25, "MSFT")
plot.inidicator.MA(NASDAQ_prices, 10, "NASDAQ")
plot.inidicator.MA(APPLE_prices, 30, "APPLE")
plot.indicator.BB(APPLE_prices, 30, "APPLE")
plot.indicator.BBMA(NASDAQ_prices, 10, "NASDAQ")


