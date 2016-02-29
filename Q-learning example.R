#exmaple of Q-learning simulating a decision policy based on a setting with 5 rooms (=states) and actions (=door) to reach a certain room (=final goal)

library(igraph);

#Function: trainer for Q based on equation  Q(state, action) = R(state, action) + Gamma * Max[Q(next state, all actions)]

Q.learning.train <-function(s, a, Q, R, gamma, goal_st, epi){
  
  for(i in 1:epi){
    
    st <- sample(s, size=1, replace=TRUE) #set random initial set
    n_st <- st
    
    while(st!=goal_st){
      
      st <- n_st   #initialize current state as new state
      at <- sample(a, size=1, replace=TRUE) #set random action
      n_st <- at                           #state after action at
      
        if(is.na(R[st, at])==TRUE){
            Q[st,at] <- 0
                    } else {
            Q[st,at] <- R[st, at]+ gamma*max(Q[n_st, ])
                    }
        }
    }
  return(Q/max(Q))
}


#Function: policy maker

Q.learning.policy <-function(Q, in_st, goal_st){
  
  pi <-c()
  st <-in_st
 
  pi[1] <- in_st
 
  while(st!=goal_st){
    
    st <- match(max(Q[st,]), Q[st,])
    
    pi <- append(pi,st)
  }
  print(pi)
}

#Function: create a graph to plot using the igraph library of the configuration of states and actions according to the R matrix

Q.learning.graph <-function(R,s,a,Q){
  
  v <- c()
  for(i in 1:nrow(R)){
    
    for(j in 1:ncol(R)){
      if(is.na(R[i,j])==FALSE){
        v <- append(v, c(i,j))
      }
    }
  }
  
  g <- graph(v)
  V(g)$name  <- toupper(as.character(s))
  E(g)$weight <-Q.learning.graph.extweigths(Q)
  E(g)$name <-toupper(as.character(round(Q.learning.graph.extweigths(Q), digits=2)))
  return(g)
}

#Function: extract edge weigths from Q matrix

Q.learning.graph.extweigths <-function(Q){
  
  w <-c()
  for(i in 1:nrow(Q)){
    
    for(j in 1:ncol(Q)){
      if(Q[i,j]!=0){
        w <- append(w, Q[i,j])
      }
    }
  }
  
 return(w) 
}


#Function: plot a graph using the igraph library of the configuration of states and actions according to the R matrix

Q.learning.graph.plot <-function(R,s,a,Q){
  plot.igraph(Q.learning.graph(R,s,a,Q), main="Configuration States/Actions", layout = layout.fruchterman.reingold, vertex.size = 25, vertex.color="blue", vertex.frame.color= "white", vertex.label=V(Q.learning.graph(R,s,a,Q))$name, vertex.label.color = "white", vertex.label.family = "sans", edge.label=E(Q.learning.graph(R,s,a,Q))$name, edge.label.color = "red", edge.curved=TRUE, edge.width=3*Q.learning.graph.extweigths(Q), edge.color="black")
}

#Function: create sequence of plots indicating the application of the optimal policy

Q.learning.graph.optpoli <-function(R,s,a,Q, in_st, goal_st){
  
  g <- Q.learning.graph(R,s,a,Q)
  V(g)$color <- c("blue")
  E(g)$color <- c("black")
  pi <- Q.learning.policy(Q, in_st, goal_st)
  
  for(i in 1:length(pi)){
    
    if(i!=length(pi)){
    V(g)$color[pi[1:i]] <- c("red")
    E(g)[from(pi[i]) & to(pi[i+1])]$color <- c("red")
    plot.igraph(Q.learning.graph(R,s,a,Q), main="Configuration States/Actions", layout = layout.fruchterman.reingold, vertex.size = 25, vertex.color=V(g)$color, vertex.frame.color= "white", vertex.label=V(Q.learning.graph(R,s,a,Q))$name, vertex.label.color ="white" , vertex.label.family = "sans", edge.label=E(Q.learning.graph(R,s,a,Q))$name, edge.label.color = "red", edge.curved=TRUE, edge.width=3*Q.learning.graph.extweigths(Q), edge.color=E(g)$color)
    } else {
      V(g)$color[pi[1:i]] <- c("red")
      plot.igraph(Q.learning.graph(R,s,a,Q), main="Configuration States/Actions", layout = layout.fruchterman.reingold, vertex.size = 25, vertex.color=V(g)$color, vertex.frame.color= "white", vertex.label=V(Q.learning.graph(R,s,a,Q))$name, vertex.label.color ="white" , vertex.label.family = "sans", edge.label=E(Q.learning.graph(R,s,a,Q))$name, edge.label.color = "red", edge.curved=TRUE, edge.width=3*Q.learning.graph.extweigths(Q), edge.color=E(g)$color)
    }
    }
}


#initialize space of states, actions and goal state

s_data=seq(from=1, to=6, by=1)
a_data=seq(from=1, to=6, by=1) #action i represents moving to state i
goal_st_data=6

#initialize instant reward matrix R. Values of NA represent "not ammissible" choices of doors. The training will produce policies that do not go through these.
R_data=matrix(c(NA, NA, NA, NA, 0, NA, NA, NA, NA, 0, NA, 100, NA, NA, NA, 0, NA, NA, NA, 0, 0, NA, 0, NA, 0, NA, NA, 0, NA, 100, NA, 0, NA, NA, 0, 100), ncol=6, nrow=6, byrow=TRUE)

#initialize the matrix Q, learning parameter gamma and number of episodes for training
Q_data=matrix(rep(0, times=36), ncol=6, nrow=6)
gamma_data=0.8
epi_data=100



Q_data <- Q.learning.train(s_data, a_data, Q_data, R_data, gamma_data, goal_st_data, epi_data)
Q.learning.policy(Q_data, 1, goal_st_data)
Q.learning.graph.plot(R_data,s_data,a_data,Q_data)
Q.learning.graph.optpoli(R_data,s_data,a_data,Q_data, 3, 6)


