source("functions_model_opti.R")
library(xlsx)

T_horizon <-32
T_expand <- 2

#---- RUN MODEL FUNCTION ----
run_model<- function(Queue,Prob_TR, V_TS_all_n, V_TB_all_n){
  
  # Initiation of presses ----
  Presses <- InitiatePresses()
  
  # Initialize profit and costs ----
  V <- 0
  C_degr <- 0
  C_wasted <- 0
  max_V <- sum(Queue$Variety*Queue$Load)
  
  ##################################################################################
  
  # Algorithm
  START<-Sys.time()
  for(i in 2:(T_horizon+T_expand))
  {
    #Trucks waiting times check
    if (any(i-Queue$t==5)){
      Queue_degr <- subset(Queue,i-Queue$t==5)
      C_degr <- C_degr + sum((Queue_degr$Variety-1)*Queue_degr$Load)
      Queue[i-Queue$t==5,1] <- 1            # change variety to 1 if truck waits  2 hours
    }

    if (any(i-Queue$t==9)){
      Queue_wasted <- subset(Queue,i-Queue$t==9)
      C_wasted <- C_wasted + sum(Queue_wasted$Variety*Queue_wasted$Load)
      Queue <- subset(Queue,i-Queue$t!=9)   # delete the truck if it waits 4 hours
    }
    
    DEC<-DECISIONS(Presses,Queue,i,V_TS_all_n,V_TB_all_n)
    DEC_v <- DEC[1][[1]]
    DEC_l <- DEC[2][[1]]
    #print('*')
    if (any(DEC_l>0)){
    Queue_t <- subset(Queue,Queue$t<=i-1) # up to time t
    
    #choose randomly maximizing strategy
    d <- sample(1:nrow(DEC_v),1)
    d_v <- DEC_v[d,]
    d_l <- DEC_l[d,]

    v_uniq <- unique(as.numeric(d_v))
    v_uniq <- subset(v_uniq,v_uniq>0)
    
    presses_to_optimize <- which(d_v==0)
    for (k in 1:length(v_uniq)){ # for each variety
      d_l_v <- d_l*(d_v==v_uniq[k])
      Queue_v <- subset(Queue_t,Queue_t$Variety == v_uniq[k])
      Queue_v <- Queue_v[order(Queue_v$t),]
      if (nrow(Queue_v)>0) rownames(Queue_v) <- 1:nrow(Queue_v)
      
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
    }
  }
  
  Presses_not_filled <- subset(Presses,Presses$Start_time==0)

  result <- rbind(c('Evalutaion time:',Sys.time()-START),
        c('Maximal profit:',max_V),
        c('Obtained:',V+sum(Presses_not_filled$Variety*Presses_not_filled$Load)),
        c('Degradation loss:',C_degr),
        c('Waste loss:',C_wasted),
        c('Remain loss:',ifelse(nrow(Queue)>0,sum(Queue$Variety*Queue$Load),0)))
  result <- as.data.frame(result)
  colnames(result) <- c('Measure','Result')
  
  print(result)
  return(result)
  
}

#---- SIMULATION ----
file_names <- c('R_R_')

for (f in file_names){
  print(f)
    file_name = f
    Queue <- read.csv(paste0('data/Poisson/',file_name,'Queue.csv'))
    Prob_TR <- read.csv(paste0('data/Poisson/',file_name,'Prob_TR.csv'))
    V_TS_all_n <- paste0('data/Poisson/',file_name,'V_TS_all.xlsx')
    V_TB_all_n <- paste0('data/Poisson/',file_name,'V_TB_all.xlsx')
    
    txt <- run_model(Queue,Prob_TR, V_TS_all_n, V_TB_all_n)

    write.xlsx(txt,paste0('Results_10_',f,'.xlsx'),row.names =FALSE)

}


