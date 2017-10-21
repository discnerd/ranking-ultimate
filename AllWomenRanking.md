# USAU Womens Rankings
`r format(Sys.time(), '%d %B, %Y')`  




```r
library(RSelenium)
rD <- rsDriver()
```

```
## checking Selenium Server versions:
```

```
## BEGIN: PREDOWNLOAD
```

```
## BEGIN: DOWNLOAD
```

```
## BEGIN: POSTDOWNLOAD
```

```
## checking chromedriver versions:
```

```
## BEGIN: PREDOWNLOAD
```

```
## BEGIN: DOWNLOAD
```

```
## BEGIN: POSTDOWNLOAD
```

```
## checking geckodriver versions:
```

```
## BEGIN: PREDOWNLOAD
```

```
## BEGIN: DOWNLOAD
```

```
## BEGIN: POSTDOWNLOAD
```

```
## checking phantomjs versions:
```

```
## BEGIN: PREDOWNLOAD
```

```
## BEGIN: DOWNLOAD
```

```
## BEGIN: POSTDOWNLOAD
```

```
## [1] "Connecting to remote server"
## $applicationCacheEnabled
## [1] FALSE
## 
## $rotatable
## [1] FALSE
## 
## $mobileEmulationEnabled
## [1] FALSE
## 
## $networkConnectionEnabled
## [1] FALSE
## 
## $chrome
## $chrome$chromedriverVersion
## [1] "2.33.506120 (e3e53437346286c0bc2d2dc9aa4915ba81d9023f)"
## 
## $chrome$userDataDir
## [1] "C:\\Users\\mr437799\\AppData\\Local\\Temp\\scoped_dir7484_7308"
## 
## 
## $takesHeapSnapshot
## [1] TRUE
## 
## $pageLoadStrategy
## [1] "normal"
## 
## $databaseEnabled
## [1] FALSE
## 
## $handlesAlerts
## [1] TRUE
## 
## $hasTouchScreen
## [1] FALSE
## 
## $version
## [1] "61.0.3163.100"
## 
## $platform
## [1] "Windows NT"
## 
## $browserConnectionEnabled
## [1] FALSE
## 
## $nativeEvents
## [1] TRUE
## 
## $acceptSslCerts
## [1] TRUE
## 
## $locationContextEnabled
## [1] TRUE
## 
## $webStorageEnabled
## [1] TRUE
## 
## $browserName
## [1] "chrome"
## 
## $takesScreenshot
## [1] TRUE
## 
## $javascriptEnabled
## [1] TRUE
## 
## $cssSelectorsEnabled
## [1] TRUE
## 
## $setWindowRect
## [1] TRUE
## 
## $unexpectedAlertBehaviour
## [1] ""
## 
## $id
## [1] "0c498a33a832187f673dc7ce49485486"
```

```r
remDr <- rD[["client"]]
```

```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Warning: package 'tidyr' was built under R version 3.4.2
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(rvest)
```

```
## Loading required package: xml2
```

```
## 
## Attaching package: 'rvest'
```

```
## The following object is masked from 'package:purrr':
## 
##     pluck
```

```
## The following object is masked from 'package:readr':
## 
##     guess_encoding
```

```r
library(XML)
```

```
## 
## Attaching package: 'XML'
```

```
## The following object is masked from 'package:rvest':
## 
##     xml
```

```r
remDr$navigate("http://play.usaultimate.org/teams/events/rankings/")
# elem <- remDr$findElement(using = 'xpath', "//*/option[@value = 'Club']")
# elem$clickElement()

elem <- remDr$findElement(using = 'xpath', "//*/option[@value = '2']")
elem$clickElement()

elem <- remDr$findElement(using = 'id', "CT_Main_0_btnSubmit")
elem$clickElement()
```


```r
elem <- remDr$findElement(using="id", value="CT_Main_0_gvList")
data<-elem$getElementAttribute("outerHTML")[[1]]  %>% read_html() %>%
  html_table(fill=TRUE)


teams <- data[[1]][-(nrow(data[[1]])-1),-8:-9] %>% filter(!is.na(Losses))

teams$Links <- elem$getElementAttribute("outerHTML")[[1]] %>% htmlTreeParse(useInternalNodes=TRUE) %>% xpathApply("//a", xmlGetAttr, 'href') %>% unlist %>% .[1:nrow(teams)]

teams <- teams %>% filter( Wins !=0 | Losses !=0 )
```


```r
library(stringr)
num_pages <- data[[2]][2] %>% str_split(.," of ") %>% .[[1]] %>% .[2] %>% as.numeric()
for( n in 2:num_pages){
  
  
  if(n<10){
    link <- paste0("CT_Main_0$gvList$ctl23$ctl00$ctl0",n)
  } else{
    link <- paste0("CT_Main_0$gvList$ctl23$ctl00$ctl",n)
  }
  elem <- remDr$findElement(using="partial link text", value="Next ")
  elem$clickElement()

  elem <- remDr$findElement(using="id", value="CT_Main_0_gvList")
  data<-elem$getElementAttribute("outerHTML")[[1]]  %>% read_html() %>%
    html_table(fill=TRUE)

  new_teams <- data[[1]][-(nrow(data[[1]])-1),-8:-9] %>% filter(!is.na(Losses))

  new_teams$Links <- elem$getElementAttribute("outerHTML")[[1]] %>%
    htmlTreeParse(useInternalNodes=TRUE) %>% 
    xpathApply("//a", xmlGetAttr, 'href') %>% unlist %>% .[1:nrow(new_teams)]

  teams <- bind_rows(teams, new_teams)
  
}

rm(data)
rm(elem)
teams <- teams %>% filter( Wins !=0 | Losses !=0) %>% 
  filter( !grepl("[Tt]esting",Team)) %>% 
  #filter(`Competition Level` != "College") %>%  
  filter(`Competition Level` == "Club") %>%  
  arrange(Team) 

teams <- teams %>% mutate(Wins = 0, Losses = 0)
```

```r
# 
# remDr$navigate("http://play.usaultimate.org/teams/events/team_rankings/?RankSet=College-Women")
# 
# elem <- remDr$findElement(using="link text", value="View All")
# elem$clickElement()
# 
# elem <- remDr$findElement(using="id", value="CT_Main_0_gvList")
# data<-elem$getElementAttribute("outerHTML")[[1]]  %>% read_html() %>%
#   html_table(fill=TRUE)
# 
# teamsInfo <- data[[1]][-(nrow(data[[1]])-1),-11:-12] %>% filter(!is.na(Losses))
# 
# teamsInfo$Links <- elem$getElementAttribute("outerHTML")[[1]] %>% htmlTreeParse(useInternalNodes=TRUE) %>% xpathApply("//a", xmlGetAttr, 'href') %>% unlist %>% .[1:nrow(teamsInfo)]
# 
# teams <- left_join(teams,teamsInfo,by=c("Links", "Competition Level", "Gender Division")) %>% select(-Wins.y, -Losses.y, -Wins.x, -Losses.x) %>% rename(Team=Team.x, School=Team.y) %>% mutate(Rank=as.numeric(Rank), `Power Rating`=as.numeric(`Power Rating`))
# 
# teams <- teams %>% arrange(Team) %>% mutate(Wins=0, Losses=0)
# 
# rm(teamsInfo)
```


```r
scores <-data.frame(Team1=character(), Score1=numeric(), 
                    Team2=character(), Score2=numeric(), Date=character())

for( i in 1:length(teams$Team)){
  Sys.sleep(runif(1))
  url <- paste0("http://play.usaultimate.org", teams$Links[i])
  remDr$navigate(url)
  #pull table of games
  elem <- remDr$findElement(using = "id", value="CT_Right_0_gvEventScheduleScores")
  data <- elem$getElementAttribute("outerHTML")[[1]] %>% 
    read_html() %>% html_table(fill=TRUE)
  data <- data[[1]]
  names(data) <- c("Date", "Scores", "Opponent")
  data <- filter(data, Date != Scores)
  data <- data %>% separate(Scores, c("For", "Against"))
  data <- data %>% filter(!is.na(Against) & !is.na(For)) %>% filter(Opponent != "N/A") %>%
    mutate(For=ifelse(For=="W", 1, 
                      ifelse(For=="F", -1, 
                             ifelse(For=="L",0,
                                    as.numeric(For)))),
           Against=ifelse(Against=="W", 1, 
                          ifelse(Against=="F", -1, 
                                 ifelse(Against=="L",0,
                                        as.numeric(Against))))
    )
  data$Links <- elem$getElementAttribute("outerHTML")[[1]] %>%
    htmlTreeParse(useInternalNodes=TRUE) %>% 
    xpathApply("//a", xmlGetAttr, 'href') %>% 
    unlist %>% grep("Eventteam",., value=TRUE) %>% .[1:length(data$Against)]
  
  for(j in 1:length(data$Links)){
    oppID = grep(data$Links[j], teams$Links, fixed=TRUE)
    if( length(oppID) >0){
      if( oppID > i){
        scores <- bind_rows(scores, 
                            tibble(Team1=teams$Team[i], 
                                   Score1=data$For[j], 
                                   Team2=teams$Team[oppID], 
                                   Score2=data$Against[j], 
                                   Date=data$Date[j])
        )
        if(!is.na(data$For[j]) && !is.na(data$Against[j])){
          if(data$For[j]>data$Against[j]){
            teams$Wins[i]=teams$Wins[i]+1;
            teams$Losses[oppID]=teams$Losses[oppID]+1;
          } else{
            teams$Losses[i]=teams$Losses[i]+1;
            teams$Wins[oppID]=teams$Wins[oppID]+1;
          }
        }
      }
    }
    #if opposing team is later in list (match on URL) add the game to the dataframe of games
    
    
  }
}
```

```
## Warning: Too few values at 1 locations: 30
```

```
## Warning in bind_rows_(x, .id): binding factor and character vector,
## coercing into character vector
```

```
## Warning in bind_rows_(x, .id): binding character and factor vector,
## coercing into character vector
```

```
## Warning in bind_rows_(x, .id): binding factor and character vector,
## coercing into character vector
```

```
## Warning in bind_rows_(x, .id): binding character and factor vector,
## coercing into character vector
```

```
## Warning in bind_rows_(x, .id): binding factor and character vector,
## coercing into character vector
```

```
## Warning in bind_rows_(x, .id): binding character and factor vector,
## coercing into character vector
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 14
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 30
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 2 locations: 13, 21
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 29
```

```
## Warning: Too few values at 1 locations: 37
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 13
```

```
## Warning: Too few values at 2 locations: 20, 32
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 38
```

```
## Warning: Too few values at 1 locations: 40
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 33
```

```
## Warning: Too few values at 1 locations: 39
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 30
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 2 locations: 27, 40
```

```
## Warning: Too few values at 1 locations: 32
```

```
## Warning: Too few values at 1 locations: 37
```

```
## Warning: Too few values at 1 locations: 33
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 6
```

```
## Warning: Too few values at 1 locations: 25
```

```
## Warning: Too few values at 1 locations: 44
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 6
```

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```r
scores <- scores %>% filter(!is.na(Score1) & !is.na(Score2))
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).


```r
remDr$close()
rD[["server"]]$stop()
```

```
## [1] TRUE
```

```r
gc(rD)
```

```
##           used (Mb) gc trigger (Mb) max used (Mb)
## Ncells  921461 49.3    1442291 77.1  1442291 77.1
## Vcells 1294876  9.9    2552219 19.5  1990512 15.2
```

```r
save(scores, teams, file=paste0("AllWomenScores",format(Sys.time(),"%Y %m %d"),".Rdata"))
```


```r
#MOV of 6 is insurmountable
P=matrix(c(1   ,-1/2,0   ,0,0,0,0,0,0,0,0,
           -1/2,1   ,-1/2,0   ,0,0,0,0,0,0,0,
           0   ,-1/2,1   ,-1/2,0   ,0,0,0,0,0,0,
           0   ,0   ,-1/2,1   ,-1/2,0,0,0,0,0,0,
           0,0   ,0   ,-1/2,1   ,-1/2,0,0,0,0,0,
           0,0,0   ,0   ,-1/2,1   ,-1/2,0,0,0,0,
           0,0,0,0   ,0   ,-1/2,1   ,-1/2,0,0,0,
           0,0,0,0,0   ,0   ,-1/2,1   ,-1/2,0,0,
           0,0,0,0,0,0   ,0   ,-1/2,1   ,-1/2,0,
           0,0,0,0,0,0,0   ,0   ,-1/2,1   ,-1/2,
           0,0,0,0,0,0,0,0   ,0   ,-1/2,1   )
         ,nrow=11)
v=c(0,0,0,0,0,0,0,0,0,0,1/2)
shares<-solve(P,v)
```

```r
library(Matrix)
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following object is masked from 'package:tidyr':
## 
##     expand
```

```r
A=sparseMatrix(seq(1,length(teams$Team)),seq(1,length(teams$Team)),x=0)
b=rep(1,length(teams$Team))

for(i in 1:length(scores$Team1)){
  if(abs(scores[i,]$Score1-scores[i,]$Score2)>=6){
    if(scores[i,]$Score1>scores[i,]$Score2){
      Share1<-1
      Share2<-0
    } else{
      Share2<-1
      Share1<-0
    }
  } else{
    Share1<-shares[scores[i,]$Score1-scores[i,]$Score2+6]
    Share2<-1-Share1
  }
  team1=match(scores[i,]$Team1,teams$Team)
  team2=match(scores[i,]$Team2,teams$Team)
  if( is.na(team1) | is.na(team2)){ next }
  A[team1,team2]=
    A[team1,team2]+Share2
  A[team2,team1]=
    A[team2,team1]+Share1
  A[team1,team1]=
    A[team1,team1]+Share1
  A[team2,team2]=
    A[team2,team2]+Share2
}
image(A)
```

![](AllWomenRanking_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
which(rowSums(A)==0)
```

```
## [1] 11 63
```



```r
A_unnormed <- A
for(i in 1:length(teams$Team)){
  if(sum(A[i,])!=0){ 
    A[i,]=A[i,]/sum(A[i,])
  }
}
```

```r
b=t(rep(1,length(teams$Team)))
for( i in 1:10000){
  b<-b%*%A
}
```


```r
library(tidyverse)
teams$Index = 1:length(teams$Team)
rankedteams<-bind_cols(teams, tibble(Rating=as.numeric(b)))
rankedteams <-arrange(rankedteams, desc(Rating))
```


```r
write.csv(rankedteams, paste("USAU All Women RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = TRUE)

rankedteams %>% filter(`Competition Level`== "Club") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Club Women RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

rankedteams %>% filter(`Competition Level`== "Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Masters Women RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

rankedteams %>% filter(`Competition Level`== "Grand Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Grand Masters Women RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)
```


```r
rankedteams %>% filter(`Competition Level`== "Club") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team                                Wins   Losses       Rating  City                   State            
-----  ---------------------------------  -----  -------  -----------  ---------------------  -----------------
    1  Brute Squad                           26        3   21.3572799  Boston                 MA               
    2  Seattle Riot                          31        5   18.7578228  Seattle                WA               
    3  Molly Brown                           26        4   18.6059734  Denver/Boulder         CO               
    4  Fury                                  22        6   13.6221101  San Francisco          CA               
    5  Scandal                               28        9    5.0544437  Washington             DC               
    6  Club Deportivo Revolution              5        2    4.8014202  Medellin               Colombia         
    7  Traffic                               12       11    3.1521223  Vancouver              BC               
    8  Ozone                                 21        9    2.5731368  Atlanta                GA               
    9  Underground                           26       17    1.9196124  Seattle                WA               
   10  Heist                                 19       17    1.8462928  Madison                WI               
   11  MUD                                    2        5    1.8184252  Edogawa-ku             Tokyo            
   12  Nightlock                             18       21    1.7181299  Bay Area               CA               
   13  Showdown                              15       17    1.4888610  Texas City             TX               
   14  Schwa                                 15       16    1.1835679  Portland               OR               
   15  6ixers                                22        7    0.9674362  Toronto                Ontario          
   16  Pop                                   26       12    0.8267492  Minneapolis            MN               
   17  Rival                                 18       11    0.8027620  Columbus/Ann Arbor     OH               
   18  Nemesis                               24       13    0.4941353  Chicago                IL               
   19  Siege                                 23        9    0.4438097  Boston                 MA               
   20  LOL                                   13        6    0.4300520  Oakland                CA               
   21  BENT                                  14       15    0.3015030  New York               NY               
   22  Phoenix                               20        6    0.2783979  Durham                 NC               
   23  Wildfire                              11       14    0.2589298  San Diego              CA               
   24  Iceni                                  0        7    0.2399778  London                 SW4 8QW          
   25  Sneaky House Hippos                    8       10    0.2053505  Vancouver              British Columbia 
   26  Huck the Patriarchy                    5        2    0.1125308  Seattle                WA               
   27  Hot Metal                             20       12    0.0899781  Pittsburgh             PA               
   28  uno                                    2        5    0.0799926  Toyota                 Aichi            
   29  Grit                                  18       12    0.0550438  Washington             DC               
   30  Iris                                  10        3    0.0482461  Québec                 Quebec           
   31  Elevate                               18        8    0.0481546  Salt Lake City         UT               
   32  Viva                                   9       14    0.0462588  Los Angeles            CA               
   33  Wicked                                16        8    0.0431554  Kansas City            KS               
   34  Fusion                                12        9    0.0431222  Winnipeg               Manitoba         
   35  FAB                                   18        8    0.0429517  Bay Area               CA               
   36  Green                                 20        4    0.0228607  Philadelphia           PA               
   37  Venus                                  3        4    0.0221747  Montreal               Quebec           
   38  Notorious C.L.E.                      13       14    0.0205551  Cleveland              OH               
   39  Stella                                 4        3    0.0146043  Ottawa                 Ontario          
   40  Deadly Viper Assassination Squad       8       17    0.0135098  Oakland                CA               
   41  Virginia Rebellion                    13       15    0.0131422  Richmond               VA               
   42  Portland Ivy                          13       11    0.0127508  Portland               OR               
   43  Vice                                  15       15    0.0118288  Cambridge              MA               
   44  Salty                                  3        4    0.0101483  Halifax                Nova scotia      
   45  PPF                                    4        3    0.0074228  Kitchener-Waterloo     Ontario          
   46  Outbreak                              17        9    0.0065906  Atlanta                GA               
   47  Phoenix Uprising                       9       13    0.0062659  Phoenix                AZ               
   48  Dish                                  21       10    0.0059116  Chicago                IL               
   49  Pine Baroness                         15       18    0.0058556  Princeton              NJ               
   50  Steel                                 21       14    0.0052400  Birmingham             AL               
   51  Tempo                                  1        6    0.0049330  Bay Area               CA               
   52  Colorado Small Batch                  11       14    0.0046296  Denver                 CO               
   53  Brooklyn Book Club                    18       10    0.0040549  Brooklyn               NY               
   54  Zephyr                                 5        8    0.0039606  Vancouver              British Columbia 
   55  Tabby Rosa                            20        9    0.0039179  Gainesville            FL               
   56  Maeve                                 11       12    0.0038716  Dallas                 TX               
   57  Indy Rogue                            22       12    0.0038497  Indianapolis           IN               
   58  Jackwagon                              9       12    0.0036964  Boulder/Denver         CO               
   59  Backhanded                             8       20    0.0030123  Baltimore/Washington   DC               
   60  Seattle Soul                           7       10    0.0027937  Seattle                WA               
   61  Fiasco                                16       12    0.0023224  Miami                  FL               
   62  Venom                                  4       13    0.0021969  Tucson                 AZ               
   63  Rice Crispies                          4        2    0.0020524  St. Paul               MN               
   64  Helix                                 17       15    0.0019519  Chicago                IL               
   65  Trainwreck                             3       10    0.0017337  Fort Collins           CO               
   66  Savage                                 6        6    0.0015918  Halifax                Nova Scotia      
   67  Seattle Beat                           4        8    0.0015246  Seattle                WA               
   68  Sureshot                              19       12    0.0013638  Cincinnati             OH               
   69  Queen Cake                            11       12    0.0013265  New Orleans            LA               
   70  Inferno                                6       16    0.0008519  Houston                TX               
   71  Further                                2        5    0.0007961  Eugene                 OR               
   72  Jinx                                   9        7    0.0007458  Portland               ME               
   73  Storm                                  3        4    0.0007006  Montreal               Quebec           
   74  Suffrage                               9       17    0.0006585  Washington             DC               
   75  Boomslang                              9       14    0.0006407  Albany                 NY               
   76  Independence                           5       13    0.0005159  Philadelphia           PA               
   77  MystiKuE                              12       14    0.0004448  Milwaukee              WI               
   78  Broad City                            10       16    0.0004376  Philadelphia           PA               
   79  Encore                                 6       14    0.0004126  Nashville              TN               
   80  Wendigo                                1       11    0.0004091  Vancouver              British Columbia 
   81  Monsoon Ultimate                       4       11    0.0003977  Flagstaff              AZ               
   82  Sparks                                 8       15    0.0003834  Cincinnati             OH               
   83  Snap                                   9       16    0.0002027  Minneapolis            MN               
   84  Crackle                               11       15    0.0001747  Minneapolis            MN               
   85  Rogue                                  3       10    0.0001202  Chapel Hill            NC               
   86  Hoax                                   6       19    0.0001179  Charleston             SC               
   87  Mystik                                 3        3    0.0000959  Montréal               Québec           
   88  Lockdown                               3       25    0.0000943  Madison                WI               
   89  Salt City Spirit                       6       10    0.0000852  Syracuse               NY               
   90  EXO                                    2        4    0.0000625  Sherbrooke             Québec           
   91  Baywatch                               2       13    0.0000527  New Britain            CT               
   92  Fresh Grannies                         1        4    0.0000523  Seattle                WA               
   93  Honey Pot                              5       20    0.0000447  Chattanooga            TN               
   94  WOLP                                   1        4    0.0000442  Northampton            MA               
   95  Autonomous                             2       15    0.0000216  Ann Arbor/Detroit      MI               
   96  Roc Paper Scissors                     2        7    0.0000212  Rochester              NY               
   97  Frenzy                                 3       21    0.0000117  Chicago                IL               
   98  DINO                                   1       18    0.0000080  New York               NY               
   99  Orbit                                  0       14    0.0000049  Atlanta                GA               
  100  HOPE                                   0        6    0.0000044  Providence             RI               
  101  PLOW                                   0        1    0.0000044  Northampton            MA               
  102  Filthy Gorgeous                        1        6    0.0000003  Saint Louis            MO               
  103  BMN                                    0        5    0.0000000  Boston                 MA               
  103  Clemson (Tiger Alumni)                 0        0    0.0000000  Clemson                SC               
  103  PLU Alum                               0        0    0.0000000  Tacoma                 WA               
  103  Temptress                              0       10    0.0000000  Dallas                 TX               

```r
rankedteams %>% filter(`Competition Level`== "Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team    Wins   Losses   Rating  City   State 
-----  -----  -----  -------  -------  -----  ------

```r
rankedteams %>% filter(`Competition Level`== "Grand Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team    Wins   Losses   Rating  City   State 
-----  -----  -----  -------  -------  -----  ------



```r
library(tidyverse)
library(network)
```

```
## network: Classes for Relational Data
## Version 1.13.0 created on 2015-08-31.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##                     Mark S. Handcock, University of California -- Los Angeles
##                     David R. Hunter, Penn State University
##                     Martina Morris, University of Washington
##                     Skye Bender-deMoll, University of Washington
##  For citation information, type citation("network").
##  Type help("network-package") to get started.
```

```r
library(sna)
```

```
## Loading required package: statnet.common
```

```
## 
## Attaching package: 'statnet.common'
```

```
## The following object is masked from 'package:base':
## 
##     order
```

```
## sna: Tools for Social Network Analysis
## Version 2.4 created on 2016-07-23.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##  For citation information, type citation("sna").
##  Type help(package="sna") to get started.
```

```r
library(ggnetwork)
library(forcats)
library(readr)
division="Women"
level="Club"

clubTeams <- teams %>% filter(`Competition Level` == level)
clubScores <- scores %>% filter(Team1 %in% clubTeams$Team & Team2 %in% clubTeams$Team)
Ranking <- rankedteams %>% filter(`Competition Level`== level) %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State)
n<-network.initialize(nrow(clubTeams), directed = TRUE, multiple = TRUE)
network.vertex.names(n)<-clubTeams$Team
n %v% "rank" <- left_join(clubTeams, Ranking, by = "Team" ) %>%
  .$Rank
n %v% "rating" <- left_join(clubTeams, Ranking, by = "Team" ) %>%
  .$Rating
add.edges(n, 
          parse_factor(clubScores$Team1,clubTeams$Team), 
          parse_factor(clubScores$Team2,clubTeams$Team), 
          names.eval = rep("PtDiff", nrow(clubScores)), 
          vals.eval = clubScores %>% mutate(PtDiff= Score1 - Score2) %>%
            .$PtDiff )
```

## Game Network


```r
net<-ggnetwork(n %s% which( n %v% "rank" < 26), layout="fruchtermanreingold")
```

```
## Warning in fortify.network(x, ...): duplicated edges detected
```

```r
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(alpha=0.1)+
  geom_nodes( aes(color=rating), alpha=0.5 ) +theme_blank()+
  scale_color_gradient(low="purple", high="gold")+
  geom_nodelabel_repel(aes(label=vertex.names))
```

```
## Warning: Ignoring unknown parameters: segment.color
```

![](AllWomenRanking_files/figure-html/plotNetwork-1.png)<!-- -->
