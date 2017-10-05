
#1 v 4
#2 v 3
#1 v 3
#2 v 4
#1 v 2
#3 v 4
library(tidyverse)
load("WomenNationalsMOVs.Rdata")
n<-10000
poolA<-c(1,8,12,13)
poolB<-c(2,7,11,14)
poolC<-c(3,6,10,15)
poolD<-c(4,5,9,16)

Get_Pool_results<- function(MOV, pool, pool_name, n){
  
  pool_results <- tibble(sim=1:n,
                         score_1_4 = rbinom(n, 29-abs(MOV[pool[4],pool[1]]), 0.5)+max(MOV[pool[4],pool[1]],0),
                         score_1_3 = rbinom(n, 29-abs(MOV[pool[3],pool[1]]), 0.5)+max(MOV[pool[3],pool[1]],0),
                         score_1_2 = rbinom(n, 29-abs(MOV[pool[2],pool[1]]), 0.5)+max(MOV[pool[2],pool[1]],0),
                         score_2_4 = rbinom(n, 29-abs(MOV[pool[4],pool[2]]), 0.5)+max(MOV[pool[4],pool[2]],0),
                         score_2_3 = rbinom(n, 29-abs(MOV[pool[3],pool[2]]), 0.5)+max(MOV[pool[3],pool[2]],0),
                         score_3_4 = rbinom(n, 29-abs(MOV[pool[4],pool[3]]), 0.5)+max(MOV[pool[4],pool[3]],0) ) #Give MOV as advantage at beginning of game to 15.
  
  
  pool_results <- gather(pool_results, key=teams, value=score, contains("score") ) %>%
    separate(teams, c("DROP", "team1", "team2")) %>% select(-DROP) 
  
  pool_results <- 
    pool_results %>% 
    mutate(mov=if_else(score >=15, 15-(29-score), -(15-score)) ) #Find MOV
  
  #Determine placement in pools
  
  team_results <- pool_results %>%
    gather(key=seed, value=team, contains("team")) %>%
    mutate(team=rownames(MOV)[pool[as.integer(team)]],
           seed=if_else(seed=="team1", "Hi", "Lo"),
           mov=if_else(seed=="Lo", -mov, mov)) %>% 
    group_by(sim, team) %>% summarise(wins=sum(mov>0), 
                                      pt_diff=sum(mov)) %>%
    arrange(sim,desc(wins),desc(pt_diff)) %>%
    mutate(pool_place=row_number(), pool_name=pool_name) 
  
  return(team_results)
}


results<- bind_rows(Get_Pool_results(MOV, poolA, "A", n),
                    Get_Pool_results(MOV, poolB, "B", n),
                    Get_Pool_results(MOV, poolC, "C", n),
                    Get_Pool_results(MOV, poolD, "D", n))

results <- results %>% mutate(round="pool")


results %>% 
  ungroup() %>%
  group_by(team, pool_place, pool_name) %>%
  summarise(count=n()) %>%
  ggplot(aes(fill= parse_factor(team, rownames(MOV)), x= pool_place, y=count/n))+geom_col(  ) + facet_wrap(~pool_name)+
  labs(x="Place", y="Proportion times", fill="Team")

results <- results %>% mutate(round="pool")


pq_results <- tibble(sim=rep(NA_integer_,4*n),team1=NA_integer_,team2=NA_integer_, score=NA_integer_, mov=NA_integer_, game=NA_character_)
q_results <- tibble(sim=rep(NA_integer_,4*n),team1=NA_integer_,team2=NA_integer_, score=NA_integer_, mov=NA_integer_, game=NA_character_)
s_results <- tibble(sim=rep(NA_integer_,2*n),team1=NA_integer_,team2=NA_integer_, score=NA_integer_, mov=NA_integer_, game=NA_character_)
f_results <- tibble(sim=rep(NA_integer_,n),team1=NA_integer_,team2=NA_integer_, score=NA_integer_, mov=NA_integer_, game=NA_character_)

Get_PQ_result<-function(MOV, results, simulation, pool_name1, pool_place1, pool_name2, pool_place2 ){
  team1=which(filter(results, sim==simulation & pool_name==pool_name1 & pool_place==pool_place1) %>% .$team == rownames(MOV))
  team2=which(filter(results, sim==simulation & pool_name==pool_name2 & pool_place==pool_place2) %>% .$team == rownames(MOV))
  #cat(team1, "," ,team2)
  score=rbinom(1, 29-abs(MOV[team2,team1]), 0.5)+max(MOV[team2,team1],0)
  mov=if_else(score >=15, 15-(29-score), -(15-score))
  return(c(simulation,team1, team2, score, mov))
}

Get_Game_result<-function(MOV, team1, team2){
  team1=as.numeric(team1)
  team2=as.numeric(team2)
  score=rbinom(1, 29-abs(MOV[team2,team1]), 0.5)+max(MOV[team2,team1],0)
  mov=if_else(score >=15, 15-(29-score), -(15-score))
  return(c(team1, team2, score, mov))
}

for(i in 1:n){
  #Prequarters
  #B2 v C3
  loop_results <- results %>% filter(sim==i)
  pq_results[i,]<-c(Get_PQ_result(MOV, loop_results, i, "B", 2, "C", 3),"PQ 1")
  #Winner v A1
  if(as.numeric(pq_results[i,]$mov) > 0 ){
    q_results[i,] <- c(Get_PQ_result(MOV, loop_results, i, "A", 1, "B", 2),"Q 1")
  } else {
    q_results[i,] <- c(Get_PQ_result(MOV, loop_results, i, "A", 1, "C", 3),"Q 1")
  }
  #B3 v C2

  pq_results[i+n,]<-c(Get_PQ_result(MOV, loop_results, i, "B", 3, "C", 2),"PQ 2")

  #Winner v D1
  if(as.numeric(pq_results[i+n,]$mov) > 0 ){
    q_results[i+n,] <- c(Get_PQ_result(MOV, loop_results, i, "D", 1, "B", 3),"Q 2")
  } else {
    q_results[i+n,] <- c(Get_PQ_result(MOV, loop_results, i, "D", 1, "C", 2),"Q 2")
  }
  
  
  #D2 v A3
  
  pq_results[i+2*n,]<-c(Get_PQ_result(MOV, loop_results, i, "D", 2, "A", 3), "PQ 3")
  
  #Winner v C1
  if(as.numeric(pq_results[i+2*n,]$mov) > 0 ){
    q_results[i+2*n,] <- c(Get_PQ_result(MOV, loop_results, i, "C", 1, "D", 2),"Q 3")
  } else {
    q_results[i+2*n,] <- c(Get_PQ_result(MOV, loop_results, i, "C", 1, "A", 3),"Q 3")
  }
  #D3 v A2
  
  #Winner v B1
  pq_results[i+3*n,]<-c(Get_PQ_result(MOV, loop_results, i, "D", 3, "C", 2), "PQ 4")
  if(as.numeric(pq_results[i+3*n,]$mov) > 0 ){
    q_results[i+3*n,] <- c(Get_PQ_result(MOV, loop_results, i, "B", 1, "D", 3),"Q 4")
  } else {
    q_results[i+3*n,] <- c(Get_PQ_result(MOV, loop_results, i, "B", 1, "A", 2),"Q 4")
  }
  
  Q1winner <- ifelse(as.numeric(q_results[i,]$mov>0), q_results[i,]$team1, q_results[i,]$team2)
  Q2winner <- ifelse(as.numeric(q_results[i+n,]$mov>0), q_results[i+n,]$team1, q_results[i+n,]$team2)

  s_results[i,] <- c(i,Get_Game_result(MOV, Q1winner, Q2winner), "S 1")
  
  Q3winner <- ifelse(as.numeric(q_results[i+2*n,]$mov>0), q_results[i+2*n,]$team1, q_results[i+2*n,]$team2)
  Q4winner <- ifelse(as.numeric(q_results[i+3*n,]$mov>0), q_results[i+3*n,]$team1, q_results[i+3*n,]$team2)
  
  s_results[i+n,] <- c(i,Get_Game_result(MOV, Q3winner, Q4winner), "S 2")
  
  S1winner <- ifelse(as.numeric(s_results[i,]$mov>0), s_results[i,]$team1, s_results[i,]$team2)
  S2winner <- ifelse(as.numeric(s_results[i+n,]$mov>0), s_results[i+n,]$team1, s_results[i+n,]$team2)
  
  f_results[i,] <- c(i,Get_Game_result(MOV, S1winner, S2winner), "F 1")

   cat(i, "\n")
}

braket_results <- bind_rows(pq_results,q_results,s_results,f_results)
  
braket_results <- braket_results %>% 
  mutate( team1=parse_factor(rownames(MOV)[as.integer(team1)], levels=rownames(MOV)),
          team2=parse_factor(rownames(MOV)[as.integer(team2)], levels=rownames(MOV))) %>%
  separate(game, c("round", "game"))
  
braket_results <- braket_results %>% gather(key=seed, value=team, contains("team")) %>%
  mutate(sim=as.numeric(sim), score=as.numeric(score), mov=as.numeric(mov),
    seed=if_else(seed=="team1", "Hi", "Lo"),
         mov=if_else(seed=="Lo", -mov, mov))

braket_results %>% 
  group_by(round, team) %>%
  summarize(times=n()/n) 

braket_results %>% 
  group_by(round, team, mov>0) %>%
  summarize(times=n()/n) %>% rename(win=`mov > 0`) %>% 
  filter(round=="F", win)

