library(zoo);
library(tseries);

#functions

MAtrading <- function(p, w){
  
  n <- length(p)
  a <- n-w+1
  b <- w-1
  
  M <- rep(0, times=a) 
  s <- rep(0, times=a) 
  out <-list()
  
  
  for(i in 1:a){
    
    M[i] <- mean(p[index(p)[i:b+i]])
    s[i] <- sd(p[index(p)[i:b+i]])
  }
  
  M <- as.zoo(M)
  s <- as.zoo(s)
  
  index(M) <- as.yearmon(index(p)[w:n])  
  index(s) <- as.yearmon(index(p)[w:n])  
  
  out[[1]] <- M
  out[[2]] <- s
  
  return(out)
}


MAtrading.plot <- function(p, w, name){
  
  l <- MAtrading(p,w)
  n <- length(p)
  
  plot(p[w:n], type="l", col="black", ylab="price", main=name)
  lines(l[[1]], type="l", col="blue")
  lines(l[[1]]+2*l[[2]], type="l", col="red") 
  lines(l[[1]]-2*l[[2]], type="l", col="red")
  legend(x="topleft", legend=c(paste(c(name, " price"), collapse = ""), "Moving average", "Moving average+-2sd"), col=c("black", "blue", "red"), lwd=1, cex=0.7)
  
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


MAtrading.plot(SP_500_prices, 20, "SP500")
MAtrading.plot(MSFT_prices, 25, "MSFT")
MAtrading.plot(NASDAQ_prices, 10, "NASDAQ")
MAtrading.plot(APPLE_prices, 30, "APPLE")


