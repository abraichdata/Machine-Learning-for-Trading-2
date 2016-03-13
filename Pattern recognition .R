#THIS CODE IMPLEMENTS PATTERN RECOGNITION AND GIVES PREDICTION

library(quantmod);
library(TTR);


#Functions

get.pattern <- function(x, w, t0){
  
  if(length(x)<w || t0+w>length(x)){
    break
  } else{
    
    p <- c()
    p <- x[seq(from=t0, to=t0+w, by=1)]
    
    return(p)
  }
  
}

get.latest.pattern <- function(x, w){
  
  p <- c()
  p <- get.pattern(x, w, dim(x)[1]-w)
  
  return(p)
  
  }


chart.pattern <- function(p){
  
  chartSeries(p) 
  
}


plot.pattern <- function(p){
  
  plot(p) 
  
}


percent.change <- function(a,b){
  return((b-a)/a*100)
}


find.pattern <- function(x, w, t0){
  
 
    q <- c()
    p <- get.pattern(x,w,t0)
    
    for(i in 2:length(p)){
      
      q <- append(q,percent.change(as.numeric(p[i-1]),as.numeric(p[i])))
    }
    
    return(q)  

}



find.latest.pattern <- function(x, w){
  
  if(w>dim(x)[1]){
    break
  } else{
    
    c <- c()
    p <- get.latest.pattern(x, w)
    
    for(i in 2:length(p)){
      
      c <- append(c,percent.change(as.numeric(p[i-1]),as.numeric(p[i])))
    }
    
    return(c)  
  }
  
  
}



match.pattern <- function(x,w,alpha){
  
  times <- c()
  lp <- find.latest.pattern(x,w)
 
  b <- dim(x)[1]-w-3
  for(t in 1:b){
    
    p <- find.pattern(x, w, t)
    
    cost <- abs(p-lp)
    c <- mean(cost)
    
    if(c<alpha){
      print(t)
      print(cost)
      print(c)
      times <- append(times,t)
    }
    
  }
  
 
  for(s in times){
    print(as.numeric(x[seq(from=s, to=s+w, by=1)]))
    
    if(s==times[1]){
      plot(as.numeric(x[seq(from=times[1], to=times[1]+w, by=1)]), type="l", ylim=c(0,40))
    }else{
      lines(as.numeric(x[seq(from=s, to=s+w, by=1)]), type="l")
    }
    }
  
  
  plot(as.numeric(get.latest.pattern(x,w)), type="l", col="red", ylim=c(0,max(x)))
  
  return(times)
  
}

make.prediction <-function(x,w,alpha){
  
  times <- match.pattern(x,w,alpha)
  lp <- get.latest.pattern(x,w)
  pred <- c()
  
  for(s in times){
    
   pred <- append(pred, ((as.numeric(x[s+w+1])-as.numeric(x[s+w]))/(as.numeric(x[s+w]))*lp[w])+lp[w])
    
  }
  
  print(pred)
  print(mean(pred))
  plot(as.numeric(get.latest.pattern(x,w)), type="l", col="red", xlim=c(0,w+3), ylab=names(x))
  lines(rep(w+2, times=length(pred)), pred, col="blue", type="p" )
  lines(w+2, mean(pred), col="green", type="p" )
  
}

getSymbols("YHOO",src="google") # from google finance 
getSymbols("GOOG",src="yahoo") # from yahoo finance 

make.prediction(YHOO[,1],10,0.9)
make.prediction(YHOO[,4],10,1.2)
make.prediction(GOOG[,1],10,0.9)
make.prediction(GOOG[,4],10,0.9)