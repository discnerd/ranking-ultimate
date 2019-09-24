load("WomenNationalsMOVs.Rdata")

load("WomenSim.Rdata")
n<-nrow(results)/16
n

women_pools <- results %>% group_by(team) %>% summarise(E=mean(wins))
women_E <- bracket_summary %>% mutate(bonus=if_else(as.numeric(round)>1,as.numeric(round)-1, 0),
weight_win = bonus * times) %>%
group_by(team) %>% summarize(E=sum(weight_win)) %>% left_join(women_pools, by=c("team")) %>%
mutate(E=E.x+E.y) %>% arrange(E) %>% 
  mutate(team=parse_factor(team, row.names(MOV)), seed=as.numeric(team), division="Women")



load("MenNationalsMOVs.Rdata")
load("MensSim.Rdata")
n<-nrow(results)/16
n

women_pools <- results %>% group_by(team) %>% summarise(E=mean(wins))
men_E <- bracket_summary %>% mutate(bonus=if_else(as.numeric(round)>1,as.numeric(round)-1, 0),
                                      weight_win = bonus * times) %>%
  group_by(team) %>% summarize(E=sum(weight_win)) %>% left_join(women_pools, by=c("team")) %>%
  mutate(E=E.x+E.y) %>% arrange(E) %>%
  mutate(team=parse_factor(team, row.names(MOV)), seed=as.numeric(team), division="Men")


load("MixedNationalsMOVs.Rdata")
load("MixedSim.Rdata")
n<-nrow(results)/16
n

women_pools <- results %>% group_by(team) %>% summarise(E=mean(wins))
mixed_E <- bracket_summary %>% mutate(bonus=if_else(as.numeric(round)>1,as.numeric(round)-1, 0),
                                    weight_win = bonus * times) %>%
  group_by(team) %>% summarize(E=sum(weight_win)) %>% left_join(women_pools, by=c("team")) %>%
  mutate(E=E.x+E.y) %>% arrange(E) %>% 
  mutate(team=parse_factor(team, row.names(MOV)), seed=as.numeric(team), division="Mixed")


E <- bind_rows(mixed_E,men_E, women_E)


my_team=c("Brute Squad", "Revolver", "Nemesis", "Wild Card")

E %>% filter(team %in% my_team) %>% select(-contains(".")) %>% 
  summarize_if(is.numeric, funs(sum))
