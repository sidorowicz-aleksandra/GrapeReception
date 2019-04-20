library(plyr)
library(sqldf)
library(Ecfun)

#---- 1. INITIALIZATION ----
T_horizon <- 32
load_treshold <- 75

# Trucks possibilities
Trucks <- c( rep(1,5), rep(2,5), rep(3,5), rep(4,5)) # Varieties
Trucks <- cbind(Trucks, rep(seq(5, 25, 5), 4))       # Load
Trucks <- cbind(Trucks, c(1:nrow(Trucks)))           # Id
Trucks <- as.data.frame(Trucks)
colnames(Trucks)<-c('Variety', 'Load', 'Id')

Trucks_B <- c( rep(1,10), rep(2,10), rep(3,10), rep(4,10)) # Varieties
Trucks_B <- cbind(Trucks_B, rep(seq(5, 50, 5), 4))         # Load
Trucks_B <- cbind(Trucks_B, c(1:nrow(Trucks_B)))           # Id
Trucks_B <- as.data.frame(Trucks_B)
colnames(Trucks_B)<-c('Variety', 'Load', 'Id')

#---- 2. FUNCTIONS ----

# Set diff (Returns set x truncated by one element y(element=truck type)) ----
new_setdiff<-function(x,y){   
  #if (nrow(x)==0) {return(x)
  if (nrow(y)==0) {return(x)
  }else{
    n<-nrow(x[x[,3] %in% y[,3],])  # number of identical trucktypes in x and y
    if (n>0){
      cut<-x[!(x[,3] %in% y[,3]),] # set  difference x\y
      if (n==1) {return(cut)
      }else {
        for (i in 1:(n-1))cut<-rbind(cut,y)
        colnames(cut)<-c('Variety','Load','Id')
        return(cut)}
    }else return(x)
  }
}

# Presses initiation ----
InitiatePresses <- function(){
  Presses<-cbind(rep(0,6),rep(0,6),rep(0,6),c(rep(25,4),rep(50,2)))
  Presses<-data.frame(Presses)
  colnames(Presses)<-c('Variety','Load','Start_time','Capacity')
  return(Presses)
}

# Time intervals converter ----
TimeConverter <- function(t){
  H <- (8 + floor(t/2)) %% 24
  M <- t %% 2 *30
  for (i in 1:length(t)){
  if (M[i]==0) M[i]='00'
  }
  return(paste(H,M,sep=':'))
}

# Aggregated Queue ----
QueueAggr <- function(Queue,t){
  Queue_aggr <- ddply(Queue[Queue$t<=t,],.(Variety), summarise, TotalLoad=sum(Load))
  return(Queue_aggr)
}

# All possible states of press at time t ----
X_S <- function(t){
  X_S<-c( 0, rep(1,8), rep(2,8), rep(3,8), rep(4,8) )    # Varieties
  X_S<-cbind(X_S, c( 0, 
                     rep( c( seq(5,20,5), rep(25,4) ),4 ) 
  ) )                                                    # Load
  X_S<-cbind(X_S, c( 0, 
                     rep( c(rep(0,4), t-c(0:3) ),4 )
  ) )                                                    # Start_time
  X_S<-as.data.frame(X_S)
  colnames(X_S)<-c('Variety', 'Load', 'Start_time')
  return(subset(X_S, Start_time>=0))}

X_B <- function(t) {
  X_B<-c( 0, rep(1,17), rep(2,17), rep(3,17), rep(4,17)) # Varieties
  X_B<-cbind(X_B, c( 0, 
                     rep( c( seq(5,45,5), rep(50,8) ),4 ) 
  ) )                                                    # Load
  X_B<-cbind(X_B, c( 0, 
                     rep( c(rep(0,9), t-c(0:7) ),4 )
  ) )                                                    # Start_time
  X_B<-as.data.frame(X_B)
  colnames(X_B)<-c('Variety', 'Load', 'Start_time')
  return(subset(X_B, Start_time>=0))}

# Gamma function  ----
Gamma_S <- function(x,t) {  
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety','Load','Start_time')
  
  if (x$Start_time!=0 & t-x$Start_time<4)  {                                 # Working?
    Res_1<-as.data.frame(c(0,0,nrow(Trucks)+1))
    if(dim(Res_1)[2]==1) Res_1<-data.frame(t(Res_1))
    colnames(Res_1)<-c('Variety','Load','Id')
    return( Res_1 )                                          
  }else { if (x$Load==0 | (x$Start_time!=0 & t-x$Start_time==4)) {           # Empty?
    return(rbind(Trucks, c(0,0,nrow(Trucks)+1)))                                
  } else return(rbind(subset(Trucks,Variety==x$Variety & Load<=(25-x$Load)), # Partially filled?
                      #c(x$Variety,0,nrow(Trucks)+1)))}
                      c(0,0,nrow(Trucks)+1)))}
}

Gamma_B <- function(x,t) { 
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety','Load','Start_time')
  
  if (x$Start_time!=0 & t-x$Start_time<8)  {                                 # Working?
    Res_1<-as.data.frame(c(0,0,nrow(Trucks_B)+1))
    if(dim(Res_1)[2]==1) Res_1<-data.frame(t(Res_1))
    colnames(Res_1)<-c('Variety','Load','Id')
    return( Res_1 )
  }else { if (x$Load==0 | (x$Start_time!=0 & t-x$Start_time==8)) {           # Empty?
    return( rbind(Trucks_B, c(0,0,nrow(Trucks_B)+1))) 
  } else return(rbind(subset(Trucks_B,Variety==x$Variety & Load<=(50-x$Load)), # Partially filled?
                      #c(x$Variety,0,nrow(Trucks_B)+1)))}
                      c(0,0,nrow(Trucks_B)+1)))}
}

Gamma_B_V <- function(x,t) { 
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety','Load','Start_time')
  
  if (x$Start_time!=0 & t-x$Start_time<8)  {                                 # Working?
    Res_1<-as.data.frame(c(0,0,nrow(Trucks)+1))
    if(dim(Res_1)[2]==1) Res_1<-data.frame(t(Res_1))
    colnames(Res_1)<-c('Variety','Load','Id')
    return( Res_1 )
  }else { if (x$Load==0 | (x$Start_time!=0 & t-x$Start_time==8)) {           # Empty?
    return( rbind(Trucks, c(0,0,nrow(Trucks)+1))) 
  } else return(rbind(subset(Trucks,Variety==x$Variety & Load<=(50-x$Load)), # Partially filled?
                      #c(x$Variety,0,nrow(Trucks)+1)))}
                      c(0,0,nrow(Trucks)+1)))}
}

Gamma_S_Q <- function(x,t,Queue) {  
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety','Load','Start_time')
  available_type <- 0
  
  if (x$Start_time!=0 & t-x$Start_time<4)  {                                 # Working?
    Res_1<-as.data.frame(c(0,0,nrow(Trucks)+1))
    if(dim(Res_1)[2]==1) Res_1<-data.frame(t(Res_1))
    colnames(Res_1)<-c('Variety','Load','Id')
    available_type <- Res_1
  }else { if (x$Load==0 | (x$Start_time!=0 & t-x$Start_time==4)) {           # Empty?
    available_type <- rbind(Trucks, c(0,0,nrow(Trucks)+1))  
  } else available_type <- rbind(subset(Trucks,Variety==x$Variety & Load<=(25-x$Load)), # Partially filled?
                      #c(x$Variety,0,nrow(Trucks)+1)))}
                      c(0,0,nrow(Trucks)+1))}
  QueueA <- QueueAggr(Queue,t-1)
  if(nrow(QueueA)>0){
  available_type <- sqldf("SELECT A.*
                          FROM available_type A
                          JOIN QueueA Q ON A.Variety=Q.Variety AND A.Load <= Q.TotalLoad")
  available_type <- rbind(available_type,c(0,0,nrow(Trucks)+1))
  colnames(available_type) <- c('Variety','Load','Id')}
  return(available_type)
}

Gamma_B_Q <- function(x,t,Queue) {  
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety','Load','Start_time')
  available_type <- 0
  
  if (x$Start_time!=0 & t-x$Start_time<8)  {                                 # Working?
    Res_1<-as.data.frame(c(0,0,nrow(Trucks_B)+1))
    if(dim(Res_1)[2]==1) Res_1<-data.frame(t(Res_1))
    colnames(Res_1)<-c('Variety','Load','Id')
    available_type <- Res_1 
  }else { if (x$Load==0 | (x$Start_time!=0 & t-x$Start_time==8)) {           # Empty?
    available_type<- rbind(Trucks_B, c(0,0,nrow(Trucks_B)+1))
  } else available_type <- rbind(subset(Trucks_B,Variety==x$Variety & Load<=(50-x$Load)), # Partially filled?
                      #c(x$Variety,0,nrow(Trucks_B)+1)))}
                      c(0,0,nrow(Trucks_B)+1))}
  
  QueueA <- QueueAggr(Queue,t-1)
  if(nrow(QueueA)>0){
  available_type <- sqldf("SELECT A.*
                          FROM available_type A
                          JOIN QueueA Q ON A.Variety=Q.Variety AND A.Load <= Q.TotalLoad")
  available_type <- rbind(available_type,c(0,0,nrow(Trucks)+1))
  colnames(available_type) <- c('Variety','Load','Id')}
  return(available_type)
}


# Transformation function ----
f_S <- function(t,x,z) { 
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety', 'Load', 'Start_time')
  
  z<-as.data.frame(z)
  if(dim(z)[2]==1) z<-as.data.frame(t(z))
  colnames(z)<-c('Variety', 'Load')
  
  n_variety<-z$Variety*ifelse(x$Start_time!=0 & (t-x$Start_time)==4 & z$Load==0,0,1)+
    x$Variety*ifelse(z$Load==0 & z$Variety==0 & ((x$Start_time==0 & x$Load>0) | (x$Start_time!=0 & (t-x$Start_time)<4)),1,0)
    
  n_load<- z$Load + 
    x$Load*ifelse(x$Start_time!=0 & (t-x$Start_time)==4,0,1)
  
  n_start<-t*
    ifelse(((x$Load+z$Load==25 & x$Start_time==0 )|(z$Load==25 & (x$Start_time!=0 & t-x$Start_time==4))),1,0)+
    x$Start_time*ifelse(x$Start_time!=0 & (t-x$Start_time)<4,1,0)
  
  X<-c(n_variety,n_load,n_start)
  
  X<-as.data.frame(t(X))
  if(dim(X)[2]==1) X<-data.frame(t(X))
  colnames(X)<-c('Variety', 'Load', 'Start_time')
  return(X)
}

f_B <- function(t,x,z) { 
  x<-as.data.frame(x)
  if(dim(x)[2]==1) x<-data.frame(t(x))
  colnames(x)<-c('Variety', 'Load', 'Start_time')
  
  z<-as.data.frame(z)
  if(dim(z)[2]==1) z<-as.data.frame(t(z))
  colnames(z)<-c('Variety', 'Load')
  
  n_variety<-z$Variety*ifelse(x$Start_time!=0 & (t-x$Start_time)==8 & z$Load==0,0,1)+
    x$Variety*ifelse(z$Load==0 & z$Variety==0 & ((x$Start_time==0 & x$Load>0)| (x$Start_time!=0 & (t-x$Start_time)<8)),1,0)
  
  n_load<-x$Load + z$Load -
    x$Load*ifelse(x$Start_time!=0 & (t-x$Start_time)==8,1,0)
  
  n_start<-t*
    ifelse(((x$Load+z$Load==50 & x$Start_time==0 )|(z$Load==50 & (t-x$Start_time==8))),1,0)+
    x$Start_time*ifelse(x$Start_time!=0 & (t-x$Start_time) %in% c(1,2,3,4,5,6,7),1,0)
  
  X<-c(n_variety,n_load,n_start)
  
  X<-as.data.frame(t(X))
  if(dim(X)[2]==1) X<-data.frame(t(X))
  colnames(X)<-c('Variety', 'Load', 'Start_time')
  return(X)
  
}

# Value function ----
Value_S <- function(x,t,Prob_TR){ 
  Available_trucks<-Gamma_S(x, t)
  Available_trucks<-as.data.frame(Available_trucks)
  if (dim(Available_trucks)[2]==1 )Available_trucks<-data.frame(t(Available_trucks))
  colnames(Available_trucks)<-c('Variety','Load','Id')
  
  x<-as.data.frame(x)
  if (dim(x)[2]==1) x<-as.data.frame(t(x))
  colnames(x)<-c('Variety', 'Load', 'Start_time')
  
  V_max<-0 
  
  for (i in 1:nrow(Available_trucks))
  { TRUCK<-Available_trucks[i,]
  V<-(Prob_TR[t,TRUCK$Id]*25*TRUCK$Variety*
        ifelse((x$Load+TRUCK$Load==25 & x$Start_time==0)|(TRUCK$Load==25 & t-x$Start_time==4),1,0))
  if (V_max<V) {V_max<-V 
  }else V_max<-V_max
  }
  return(V_max)
}

Value_B <- function(x,t,Prob_TR){ 
  Available_trucks<-Gamma_B_V(x, t)
  Available_trucks<-as.data.frame(Available_trucks)
  if (dim(Available_trucks)[2]==1 )Available_trucks<-data.frame(t(Available_trucks))
  colnames(Available_trucks)<-c('Variety','Load','Id')
  
  x<-as.data.frame(x)
  if (dim(x)[2]==1) x<-as.data.frame(t(x))
  colnames(x)<-c('Variety', 'Load', 'Start_time')
  
  V_max<-0 
  
  for (i in 1:nrow(Available_trucks))
  { TRUCK<-Available_trucks[i,]
  V<-(Prob_TR[t,TRUCK$Id]*50*TRUCK$Variety*
        ifelse((x$Load+TRUCK$Load==50 & x$Start_time==0)|(TRUCK$Load==50 & t-x$Start_time==8),1,0))
  if (V_max<V){ V_max<-V 
  }else {V_max<-V_max}
  }
  return(V_max)
}

Value_recursive_S <- function(t_start, T_horizon, Prob_TR) { # returns possible to generate profit until T_horizon, providing that in t_start we have a situatioon given as an argument
  X<-X_S(T_horizon+1)
  V<-cbind(X,rep(0,nrow(X)))
  colnames(V)[4]<-'Price'
  for (t in (T_horizon+1):(t_start+1))
  { 
    V_new<-data.frame()
    X<-X_S(t-1) # all possible states at  t-1
    for (j in 1:nrow(X))
    {
      x<-X[j,]
      Available_trucks<-Gamma_S(x, t)
      Available_trucks<-as.data.frame(Available_trucks)
      if (dim(Available_trucks)[2]==1 )Available_trucks<-data.frame(t(Available_trucks))
      colnames(Available_trucks)<-c('Variety','Load','Id')
      
      V_max<-0 
      for (k in 1:nrow(Available_trucks))
      {
        TRUCK<-Available_trucks[k,]
        x_new<-f_S(t,x,TRUCK) 
        if (x_new$Load==0){ V_NEXT<-subset(V,Variety==0,select=Price)}
        if (x_new$Load!=0 ){ V_NEXT<-subset(V,Variety==x_new$Variety & Load==x_new$Load & Start_time==x_new$Start_time,select=Price)}
        V_tmp<-Prob_TR[t-1,TRUCK$Id]*(25*TRUCK$Variety*
                                        ifelse(x_new$Load==25 & x_new$Start_time==t,1,0) + V_NEXT)
        if (V_max<V_tmp) V_max<-V_tmp
      }
      V_max<-as.data.frame(V_max)
      colnames(V_max)<-'Price'
      V_new<-rbind(V_new,c(x,V_max))
    }
    V<-V_new
    colnames(V)<-c('Variety','Load','Start_time','Price')
  }
  V[V$Start_time==t_start,4]<- V[V$Start_time==t_start,4]+subset(V,Start_time==t_start,select=Variety)*25 #dodanie do sytuacji, gdzie w czasie t_start uruchamiamy maszyne warto?ci zysku
  return(V)
}

Value_recursive_B<-function(t_start, T_horizon, Prob_TR) { 
  X<-X_B(T_horizon+1)
  V<-cbind(X,rep(0,nrow(X)))
  colnames(V)[4]<-'Price'
  for (t in (T_horizon+1):(t_start+1))
  { 
    V_new<-data.frame()
    X<-X_B(t-1) # all possible states at  t-1
    for (j in 1:nrow(X))
    {
      x<-X[j,]
      Available_trucks<-Gamma_B_V(x, t)
      Available_trucks<-as.data.frame(Available_trucks)
      if (dim(Available_trucks)[2]==1 )Available_trucks<-data.frame(t(Available_trucks))
      colnames(Available_trucks)<-c('Variety','Load','Id')
      V_max<-0 
      for (k in 1:nrow(Available_trucks))
      {
        TRUCK<-Available_trucks[k,]
        x_new<-f_B(t,x,TRUCK)
        if (x_new$Load==0){ V_NEXT<-subset(V,Variety==0,select=Price)}
        if (x_new$Load!=0 ){ V_NEXT<-subset(V,Variety==x_new$Variety & Load==x_new$Load & Start_time==x_new$Start_time,select=Price)}
        V_tmp<-Prob_TR[t-1,TRUCK$Id]*(50*TRUCK$Variety*
                                        ifelse(x_new$Load==50 & x_new$Start_time==t,1,0) + V_NEXT)
        if (V_max<V_tmp) V_max<-V_tmp
      }
      V_max<-as.data.frame(V_max)
      colnames(V_max)<-'Price'
      V_new<-rbind(V_new,c(x,V_max))
    }
    V<-V_new
    colnames(V)<-c('Variety','Load','Start_time','Price')
  }
  V[V$Start_time==t_start,4]<- V[V$Start_time==t_start,4]+subset(V,Start_time==t_start,select=Variety)*50
  return(V)
}

# Decisions and its profit ----
DECISIONS<-function(Presses,Queue,t,V_TS_all_n,V_TB_all_n){
  #expected final price for each state
  if(t<=T_horizon){
    V_TS<-read.xlsx(V_TS_all_n, sheetName = as.character(t))
    V_TB<-read.xlsx(V_TB_all_n, sheetName = as.character(t))
  }else{
    V_TS<-cbind(X_S(t),0)
    V_TB<-cbind(X_B(t),0)
    
    colnames(V_TS)<-c('Variety','Load','Start_time','Price')
    colnames(V_TB)<-c('Variety','Load','Start_time','Price')
    
    V_TS[V_TS$Start_time==t,4]<- subset(V_TS,Start_time==t,select=Variety)*25
    V_TB[V_TB$Start_time==t,4]<- subset(V_TB,Start_time==t,select=Variety)*50
  }
  
  if (!any(Queue$t<=t-1)){
    DECISION<-list(rep(0,6),rep(0,6))
  }else{
  
  TP1<-Gamma_S(Presses[1,c(1:3)],t)[,1:2]
  TP2<-Gamma_S(Presses[2,c(1:3)],t)[,1:2]
  TP3<-Gamma_S(Presses[3,c(1:3)],t)[,1:2]
  TP4<-Gamma_S(Presses[4,c(1:3)],t)[,1:2]
  TP5<-Gamma_B(Presses[5,c(1:3)],t)[,1:2]
  TP6<-Gamma_B(Presses[6,c(1:3)],t)[,1:2]
  
  #aggregated queue
  Queue_aggr <- ddply(Queue[Queue$t<=t-1,],.(Variety), summarise, TotalLoad=sum(Load))
  
  X1 <- apply(TP1,MARGIN=1,FUN=f_S,t=t,x=Presses[1,c(1:3)])
  X2 <- apply(TP2,MARGIN=1,FUN=f_S,t=t,x=Presses[2,c(1:3)])
  X3 <- apply(TP3,MARGIN=1,FUN=f_S,t=t,x=Presses[3,c(1:3)])
  X4 <- apply(TP4,MARGIN=1,FUN=f_S,t=t,x=Presses[4,c(1:3)])
  X5 <- apply(TP5,MARGIN=1,FUN=f_B,t=t,x=Presses[5,c(1:3)])
  X6 <- apply(TP6,MARGIN=1,FUN=f_B,t=t,x=Presses[6,c(1:3)])
  
  X1 <- do.call("rbind", X1)
  X2 <- do.call("rbind", X2)
  X3 <- do.call("rbind", X3)
  X4 <- do.call("rbind", X4)
  X5 <- do.call("rbind", X5)
  X6 <- do.call("rbind", X6)
  
  #X with price
  X1 <- join(X1,V_TS,by=c('Variety','Load','Start_time'),type="left")
  X2 <- join(X2,V_TS,by=c('Variety','Load','Start_time'),type="left")
  X3 <- join(X3,V_TS,by=c('Variety','Load','Start_time'),type="left")
  X4 <- join(X4,V_TS,by=c('Variety','Load','Start_time'),type="left")
  X5 <- join(X5,V_TB,by=c('Variety','Load','Start_time'),type="left")
  X6 <- join(X6,V_TB,by=c('Variety','Load','Start_time'),type="left")
  
  #Y with price
  Y1 <- cbind(TP1,Price=X1$Price)
  Y2 <- cbind(TP2,Price=X2$Price)
  Y3 <- cbind(TP3,Price=X3$Price)
  Y4 <- cbind(TP4,Price=X4$Price)
  Y5 <- cbind(TP5,Price=X5$Price)
  Y6 <- cbind(TP6,Price=X6$Price)
  
  #Y cutted to values which it can take accrding to aggregated queue
  Y1_pos <- subset(Y1,Y1$Variety==0) #this will be always satisfied, rbind other trucks
  Y2_pos <- subset(Y2,Y2$Variety==0)
  Y3_pos <- subset(Y3,Y3$Variety==0)
  Y4_pos <- subset(Y4,Y4$Variety==0)
  Y5_pos <- subset(Y5,Y5$Variety==0)
  Y6_pos <- subset(Y6,Y6$Variety==0)
  
  for (i in 1:nrow(Queue_aggr)){
    Y1_pos <- rbind(Y1_pos,subset(Y1,Y1$Variety==Queue_aggr$Variety[i] & Y1$Load<=Queue_aggr$TotalLoad[i]))
    Y2_pos <- rbind(Y2_pos,subset(Y2,Y2$Variety==Queue_aggr$Variety[i] & Y2$Load<=Queue_aggr$TotalLoad[i]))
    Y3_pos <- rbind(Y3_pos,subset(Y3,Y3$Variety==Queue_aggr$Variety[i] & Y3$Load<=Queue_aggr$TotalLoad[i]))
    Y4_pos <- rbind(Y4_pos,subset(Y4,Y4$Variety==Queue_aggr$Variety[i] & Y4$Load<=Queue_aggr$TotalLoad[i]))
    Y5_pos <- rbind(Y5_pos,subset(Y5,Y5$Variety==Queue_aggr$Variety[i] & Y5$Load<=Queue_aggr$TotalLoad[i]))
    Y6_pos <- rbind(Y6_pos,subset(Y6,Y6$Variety==Queue_aggr$Variety[i] & Y6$Load<=Queue_aggr$TotalLoad[i]))
  }
  
  #map of Y id
  Y_pos_map <- unique(rbind(Y1_pos[,1:2],
                           Y2_pos[,1:2],
                           Y3_pos[,1:2],
                           Y4_pos[,1:2],
                           Y5_pos[,1:2],
                           Y6_pos[,1:2]))
  rownames(Y_pos_map) <- 1:nrow(Y_pos_map)
  
  #Change Y_pos to v.l id lists
  Y1_id <- as.numeric(apply(Y1_pos[,1:2],MARGIN=1, FUN = function(x) which(Y_pos_map[,1]==x[1] & Y_pos_map[,2]==x[2])[1]))
  Y2_id <- as.numeric(apply(Y2_pos[,1:2],MARGIN=1, FUN = function(x) which(Y_pos_map[,1]==x[1] & Y_pos_map[,2]==x[2])[1]))
  Y3_id <- as.numeric(apply(Y3_pos[,1:2],MARGIN=1, FUN = function(x) which(Y_pos_map[,1]==x[1] & Y_pos_map[,2]==x[2])[1]))
  Y4_id <- as.numeric(apply(Y4_pos[,1:2],MARGIN=1, FUN = function(x) which(Y_pos_map[,1]==x[1] & Y_pos_map[,2]==x[2])[1]))
  Y5_id <- as.numeric(apply(Y5_pos[,1:2],MARGIN=1, FUN = function(x) which(Y_pos_map[,1]==x[1] & Y_pos_map[,2]==x[2])[1]))
  Y6_id <- as.numeric(apply(Y6_pos[,1:2],MARGIN=1, FUN = function(x) which(Y_pos_map[,1]==x[1] & Y_pos_map[,2]==x[2])[1]))
  
  #6D cartesian products of Y_pos
  Y_pos = data.frame(t(c(1,1,1,1,1,1)))
  colnames(Y_pos) = c('Y1','Y2','Y3','Y4','Y5','Y6')
  it <- expand.grid(Y1_id,Y2_id,Y3_id,Y4_id,Y5_id,Y6_id)
  
  # dataframe with load and with vectors
  x_map_v = data.frame(matrix(0,nrow(it),6))
  x_map_l = data.frame(matrix(0,nrow(it),6))
  for (i in 1:nrow(Y_pos_map)){
    x_map_v[it==i] <- Y_pos_map$Variety[i]
    x_map_l[it==i] <- Y_pos_map$Load[i]
  }
  
  # filtering dataframes and applying conditions for queue adherance
  rows_Y_pos <- 1:nrow(it)
  for (i in 1:nrow(Queue_aggr)){
    load_sums <- rowSums(x_map_l*(x_map_v==Queue_aggr$Variety[i]))
    rows_Y_pos <- intersect(rows_Y_pos,which(load_sums<=Queue_aggr$TotalLoad[i]))
  }
  
  total_load_sums <-rowSums(x_map_l)
  rows_Y_pos <- intersect(rows_Y_pos,which (total_load_sums<=load_treshold))
  
  y_v_pos <- x_map_v[rows_Y_pos,]
  rownames(y_v_pos) <- 1:nrow(y_v_pos)
  y_l_pos <- x_map_l[rows_Y_pos,]
  rownames(y_l_pos) <- 1:nrow(y_l_pos)
  
  y1 <- as.data.frame(cbind(Variety=y_v_pos[,1],Load=y_l_pos[,1]))
  y2 <- as.data.frame(cbind(Variety=y_v_pos[,2],Load=y_l_pos[,2]))
  y3 <- as.data.frame(cbind(Variety=y_v_pos[,3],Load=y_l_pos[,3]))
  y4 <- as.data.frame(cbind(Variety=y_v_pos[,4],Load=y_l_pos[,4]))
  y5 <- as.data.frame(cbind(Variety=y_v_pos[,5],Load=y_l_pos[,5]))
  y6 <- as.data.frame(cbind(Variety=y_v_pos[,6],Load=y_l_pos[,6]))

  y1_p <- join(y1,Y1,by=c('Variety','Load'),type="left")
  y2_p <- join(y2,Y2,by=c('Variety','Load'),type="left")
  y3_p <- join(y3,Y3,by=c('Variety','Load'),type="left")
  y4_p <- join(y4,Y4,by=c('Variety','Load'),type="left")
  y5_p <- join(y5,Y5,by=c('Variety','Load'),type="left")
  y6_p <- join(y6,Y6,by=c('Variety','Load'),type="left")
  
  y_Price <- rowSums(cbind(y1_p$Price,
                   y2_p$Price,
                   y3_p$Price,
                   y4_p$Price,
                   y5_p$Price,
                   y6_p$Price))
  
  # loss preparation
  decision_summed <- list()
  for(v in 1:4){
    y_ll_pos <- y_l_pos
    y_ll_pos[y_v_pos!=v] <- 0
    decision_summed = cbind(decision_summed,rowSums(y_ll_pos))
  }
  decision_summed <- as.data.frame(decision_summed)
  colnames(decision_summed) <- 1:4
  row.names(decision_summed) <- 1:nrow(decision_summed)
  decision_summed$`1` <- as.numeric(decision_summed$`1`)
  decision_summed$`2` <- as.numeric(decision_summed$`2`)
  decision_summed$`3` <- as.numeric(decision_summed$`3`)
  decision_summed$`4` <- as.numeric(decision_summed$`4`)
  
  # decion Id
  decision_summed_unique <- unique(decision_summed)
  decision_summed_unique$Id <- 1:nrow(decision_summed_unique)
  rownames(decision_summed_unique) <- 1:nrow(decision_summed_unique)
  
  # write decisions as Decision_5
  for (Id_d in decision_summed_unique$Id){
    # write queue as Queue_5
    Queue_t <- Queue[Queue$t<=t-1,]
    Queue_t <- Queue_t[order(Queue_t$Variety,Queue_t$t),]
    Queue_t_5 <- Queue_t[rep(row.names(Queue_t),Queue_t$Load/5),]
    Queue_t_5$Load <- 5
    Queue_t_5$Id <- 1:nrow(Queue_t_5)
    rownames(Queue_t_5) <- 1:nrow(Queue_t_5)

    Decision_5 <- decision_summed_unique[Id_d,] # take decision
    Decision_5 <- as.data.frame(t(rbind(Variety = c(1:4), Load = Decision_5[1,1:4]))) # transposition
    Decision_5 <- Decision_5[rep(row.names(Decision_5),Decision_5$Load/5),] # 5t transformation 
    if (nrow(Decision_5)>0) {
      Decision_5$Load <- 5

      for (id.i in 1:nrow(Decision_5)){
        dec_i <- Decision_5[id.i,]
        matching_subs <- subset(Queue_t_5,Queue_t_5$Variety==dec_i$Variety & Queue_t_5$Load==dec_i$Load)
        Queue_t_5 <- Queue_t_5[Queue_t_5$Id != min(matching_subs$Id),]
      }
    }
    
    # Loss
    Queue_degr <- Queue_t_5[t+1-Queue_t_5$t==5,]
    Queue_wasted <- Queue_t_5[t+1-Queue_t_5$t==9,]
    
    C_degr <- sum((Queue_degr$Variety-1)*Queue_degr$Load)
    C_wasted <- sum(Queue_wasted$Variety*Queue_wasted$Load)
    
    decision_summed_unique$Loss[Id_d] <- C_degr+C_wasted
  }
  
  decision_summed <- join(decision_summed, decision_summed_unique,by=c('1','2','3','4'))
  y_income <- y_Price - decision_summed$Loss
  
  max_y <- which(y_income==max(y_income))
  DECISION <- list(y_v_pos[max_y,],y_l_pos[max_y,])
  return(DECISION)
  }
}

# Decision application ----
ApplyDecisions <- function(Presses, Queue, i, d_v, d_l,V,C_degr,C_wasted){
  if (any(d_l>0)){
    Queue_t <- subset(Queue,Queue$t<=i-1)
    v_uniq <- unique(as.numeric(d_v))
    v_uniq <- subset(v_uniq,v_uniq>0)
    
    presses_to_optimize <- which(d_v==0)
    for (k in 1:length(v_uniq)){ # for each variety
      d_l_v <- d_l*(d_v==v_uniq[k])
      Queue_v <- subset(Queue_t,Queue_t$Variety == v_uniq[k])
      Queue_v <- Queue_v[order(Queue_v$t),]
      rownames(Queue_v) <- 1:nrow(Queue_v)
      
      d_l_v_new <-d_l_v*0
      again = 1
      while (again==1){
        if (any(d_l_v_new!=0)) d_l_v=d_l_v_new
        again = 0
        for (kl in which(d_l_v>0)){
          if(d_l[kl]==Queue_v$Load[1]){ #exact match
            d_l_v_new[kl] <- 0
            d_l[kl] <- 0
            # update press
            if (kl<5){ Presses[kl,-4] <- f_S(i,Presses[kl,-4],Queue_v[1,1:2])
            }else Presses[kl,-4] <- f_B(i,Presses[kl,-4],Queue_v[1,1:2])
            # update queue
            Queue_t <- Queue_t[-which(Queue_t$Id==Queue_v$Id[1]),] 
            Queue <- Queue[-which(Queue$t==Queue_v$t[1] & Queue$Id==Queue_v$Id[1]),]
            Queue_v <- Queue_v[-1,]
            if (nrow(Queue_v)>0) rownames(Queue_v)<- 1:nrow(Queue_v)
          }else if(d_l[kl]<Queue_v$Load[1]){ #too much
            # update press
            if (kl<5){ Presses[kl,-4] <- f_S(i,Presses[kl,-4],as.data.frame(c(Queue_v[1,1],d_l[kl])))
            }else Presses[kl,-4] <- f_B(i,Presses[kl,-4],as.data.frame(c(Queue_v[1,1],d_l[kl])))
            # update queue
            Queue_t$Load[which(Queue_t$Id==Queue_v$Id[1])] <-as.numeric(Queue_v[1,2])-as.numeric(d_l[kl])
            Queue$Load[which(Queue$t==Queue_v$t[1] & Queue$Id==Queue_v$Id[1])] <- as.numeric(Queue_v[1,2])-as.numeric(d_l[kl])
            Queue_v[1,2] <- as.numeric(Queue_v[1,2])-as.numeric(d_l[kl])
            d_l_v_new[kl] <- 0
            d_l[kl] <- 0
          }else{ #not enough
            again = 1
            d_l_v_new[kl] <- d_l[kl] - Queue_v[1,2]
            d_l[kl] <- d_l[kl] - Queue_v[1,2]
            # update press
            if (kl<5){ Presses[kl,-4] <- f_S(i,Presses[kl,-4],Queue_v[1,1:2])
            }else Presses[kl,-4] <- f_B(i,Presses[kl,-4],Queue_v[1,1:2])
            # update queue
            Queue_t <- Queue_t[-which(Queue_t$Id==Queue_v$Id[1]),] 
            Queue <- Queue[-which(Queue$t==Queue_v$t[1] & Queue$Id==Queue_v$Id[1]),]
            Queue_v <- Queue_v[-1,]
            if (nrow(Queue_v)>0)rownames(Queue_v)<- 1:nrow(Queue_v)
          }
        }
      }
    }
    
    # update main queue indexes
    if (nrow(subset(Queue,Queue$t<=i))>0){
      Queue[Queue$t<=i,3]<-c(1:nrow(subset(Queue,t<=i)))
      rownames(Queue) <- 1:nrow(Queue)}
    
    #if there are not updatet presses:
    if (length(presses_to_optimize)>0){
      for (pr in presses_to_optimize){
        if (pr<5){ Presses[pr,-4] <- f_S(i,Presses[pr,-4],as.data.frame(t(c(0,0))))
        }else Presses[pr,-4] <- f_B(i,Presses[pr,-4],as.data.frame(t(c(0,0))))
      }
    }
    
    # increase value function
    V1<-25*Presses$Variety[1]*ifelse(Presses$Load[1]==25 & Presses$Start_time[1]==i,1,0)
    V2<-25*Presses$Variety[2]*ifelse(Presses$Load[2]==25 & Presses$Start_time[2]==i,1,0)
    V3<-25*Presses$Variety[3]*ifelse(Presses$Load[3]==25 & Presses$Start_time[3]==i,1,0)
    V4<-25*Presses$Variety[4]*ifelse(Presses$Load[4]==25 & Presses$Start_time[4]==i,1,0)
    V5<-50*Presses$Variety[5]*ifelse(Presses$Load[5]==50 & Presses$Start_time[5]==i,1,0)
    V6<-50*Presses$Variety[6]*ifelse(Presses$Load[6]==50 & Presses$Start_time[6]==i,1,0)
    
    V<-V+V1+V2+V3+V4+V5+V6
    print(Presses)
  }else{
    # Queue update
    if (nrow(subset(Queue,Queue$t<=i))>0){
      Queue[Queue$t<=i,3]<-c(1:nrow(subset(Queue,t<=i)))
      rownames(Queue) <- 1:nrow(Queue)}
    
    # Presses update
    Presses[1,-4] <- f_S(i,Presses[1,-4],as.data.frame(t(c(0,0))))
    Presses[2,-4] <- f_S(i,Presses[2,-4],as.data.frame(t(c(0,0))))
    Presses[3,-4] <- f_S(i,Presses[3,-4],as.data.frame(t(c(0,0))))
    Presses[4,-4] <- f_S(i,Presses[4,-4],as.data.frame(t(c(0,0))))
    Presses[5,-4] <- f_B(i,Presses[5,-4],as.data.frame(t(c(0,0))))
    Presses[6,-4] <- f_B(i,Presses[6,-4],as.data.frame(t(c(0,0))))
    print(Presses)
  }
  
  #Trucks waiting times check
  if (any(i+1-Queue$t==5)){ # +1 since in the next simulation it will be fresh
    Queue_degr <- subset(Queue,i+1-Queue$t==5)
    C_degr <- C_degr + sum((Queue_degr$Variety-1)*Queue_degr$Load)
    Queue[i+1-Queue$t==5,1] <- 1            # change variety to 1 if truck waits  2 hours
  }
  
  if (any(i+1-Queue$t==9)){ # +1 since in the next simulation it will be fresh
    Queue_wasted <- subset(Queue,i+1-Queue$t==9)
    C_wasted <- C_wasted + sum(Queue_wasted$Variety*Queue_wasted$Load)
    Queue <- subset(Queue,i+1-Queue$t!=9)   # delete the truck if it waits 4 hours
  }
  
  return(list(Queue,Presses,V,C_degr,C_wasted))
}