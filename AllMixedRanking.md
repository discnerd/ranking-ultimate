# USAU Mixed Rankings
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
## [1] "C:\\Users\\mr437799\\AppData\\Local\\Temp\\scoped_dir2428_21816"
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
## [1] "3db964f8e90e029eef9c26ff63c842bb"
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

elem <- remDr$findElement(using = 'xpath', "//*/option[@value = '1']")
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
  filter(`Competition Level` != "College") %>%  arrange(Team) 

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
  #Sys.sleep(runif(1))
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
## Warning: Too few values at 1 locations: 15
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
## Warning: Too few values at 1 locations: 5
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
## Warning: Too few values at 4 locations: 1, 2, 3, 4
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
## Warning: Too few values at 1 locations: 16
```

```
## Warning: Too few values at 1 locations: 25
```

```
## Warning: Too few values at 1 locations: 8
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
## Warning: Too few values at 29 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
## 12, 13, 14, 15, 16, 17, 18, 19, 20, ...
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

## Warning: Too few values at 1 locations: 6
```

```
## Warning: Too few values at 1 locations: 36
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
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 1 locations: 15
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
## Warning: Too few values at 1 locations: 15
```

```
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 1 locations: 8
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
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 4 locations: 29, 30, 31, 32
```

```
## Warning: Too few values at 4 locations: 22, 23, 24, 25
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
## Warning: Too few values at 1 locations: 18
```

```
## Warning: Too few values at 1 locations: 7
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

## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 26 locations: 8, 9, 10, 11, 12, 13, 14, 15, 16,
## 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, ...
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
## Warning: Too few values at 2 locations: 1, 4
```

```
## Warning: Too few values at 2 locations: 1, 2
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
## Warning: Too few values at 2 locations: 1, 4
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

```
## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9
```

```
## Warning: Too many values at 1 locations: 29
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
## Warning: Too few values at 3 locations: 13, 14, 15
```

```
## Warning: Too few values at 1 locations: 26
```

```
## Warning: Too few values at 2 locations: 6, 33
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

```
## Warning: Too few values at 1 locations: 8
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
## Warning: Too few values at 4 locations: 4, 5, 6, 7
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
## Warning: Too few values at 1 locations: 23
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
## Warning: Too few values at 1 locations: 19
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
## Warning: Too few values at 1 locations: 10
```

```
## Warning: Too few values at 1 locations: 19
```

```
## Warning: Too few values at 2 locations: 6, 7
```

```
## Warning: Too few values at 29 locations: 8, 9, 10, 11, 12, 13, 14, 15, 16,
## 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, ...
```

```
## Warning: Too few values at 1 locations: 7
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
## Warning: Too few values at 1 locations: 18
```

```
## Warning: Too few values at 1 locations: 4
```

```
## Warning: Too few values at 1 locations: 13
```

```
## Warning: Too few values at 1 locations: 14
```

```
## Warning: Too few values at 1 locations: 28
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
## Warning: Too few values at 1 locations: 18
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
## Warning: Too many values at 1 locations: 15
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
## Warning: Too few values at 1 locations: 1
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
## Warning: Too few values at 2 locations: 5, 31
```

```
## Warning: Too few values at 1 locations: 19
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
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 1 locations: 4
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
## Warning: Too few values at 1 locations: 15
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
## Warning: Too few values at 1 locations: 4
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
## Warning: Too few values at 1 locations: 5
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
## Warning: Too few values at 1 locations: 7
```

```
## Warning: Too few values at 2 locations: 21, 22
```

```
## Warning: Too few values at 3 locations: 7, 8, 9
```

```
## Warning: Too few values at 3 locations: 14, 15, 16
```

```
## Warning: Too few values at 3 locations: 7, 8, 9
```

```
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 1 locations: 31
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
## Warning: Too few values at 1 locations: 4
```

```
## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9
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
## Warning: Too few values at 1 locations: 4
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
## Warning: Too few values at 2 locations: 32, 33
```

```
## Warning: Too few values at 4 locations: 22, 27, 28, 29
```

```
## Warning: Too few values at 1 locations: 8
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
## Warning: Too many values at 1 locations: 15
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
## Warning: Too few values at 1 locations: 11
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
## Warning: Too few values at 4 locations: 11, 12, 13, 14
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
## Warning: Too few values at 3 locations: 15, 16, 17
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
## Warning: Too few values at 1 locations: 6
```

```
## Warning: Too few values at 1 locations: 8
```

```
## Warning: Too few values at 3 locations: 26, 27, 28
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
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 2 locations: 29, 30
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
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 1 locations: 7
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
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 3 locations: 19, 20, 21
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

```
## Warning: Too few values at 1 locations: 25
```

```
## Warning: Too few values at 1 locations: 6
```

```
## Warning: Too few values at 1 locations: 5
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
## Warning: Too few values at 4 locations: 8, 33, 36, 37
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
## Warning: Too few values at 1 locations: 2
```

```
## Warning: Too few values at 1 locations: 6

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

```
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 2 locations: 1, 2
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
## Warning: Too few values at 3 locations: 1, 2, 3

## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning: Too few values at 1 locations: 15
```

```
## Warning: Too few values at 1 locations: 8
```

```
## Warning: Too few values at 3 locations: 1, 2, 3
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

```
## Warning: Too few values at 1 locations: 29
```

```
## Warning: Too many values at 1 locations: 23
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
## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9
```

```
## Warning: Too few values at 1 locations: 39
```

```
## Warning: Too few values at 1 locations: 7
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
## Warning: Too few values at 1 locations: 5
```

```
## Warning: Too few values at 3 locations: 1, 2, 3
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

```
## Warning: Too few values at 4 locations: 1, 2, 3, 4
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
## Warning: Too few values at 1 locations: 1
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
## Warning: Too few values at 2 locations: 6, 33
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
## Warning: Too few values at 1 locations: 5
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
## Warning: Too few values at 1 locations: 7
```

```
## Warning: Too few values at 1 locations: 15
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
## Warning: Too few values at 26 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
## 12, 13, 14, 15, 16, 17, 18, 19, 20, ...
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
## Warning: Too few values at 2 locations: 1, 2
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
## Warning: Too few values at 3 locations: 7, 8, 9
```

```
## Warning: Too few values at 1 locations: 28
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
## Warning: Too few values at 3 locations: 1, 2, 3
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
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too many values at 1 locations: 25
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
## Warning: Too few values at 1 locations: 7
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
## Warning: Too few values at 1 locations: 3
```

```
## Warning: Too few values at 1 locations: 5
```

```
## Warning: Too few values at 1 locations: 7

## Warning: Too few values at 1 locations: 7
```

```
## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9

## Warning: Too few values at 3 locations: 7, 8, 9
```

```
## Warning: Too few values at 1 locations: 8
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
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too many values at 1 locations: 34
```

```
## Warning: Too few values at 1 locations: 7
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
## Ncells  923334 49.4    1442291 77.1  1442291 77.1
## Vcells 1321688 10.1    2552219 19.5  2359075 18.0
```

```r
save(scores, teams, file=paste0("AllMixedScores",format(Sys.time(),"%Y %m %d"),".Rdata"))
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

![](AllMixedRanking_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
which(rowSums(A)==0)
```

```
##  [1]  28  31  78 127 131 140 149 200 212 215 221 226 237 242 259 317 363
## [18] 364 372 412 413 418 427
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
write.csv(rankedteams, paste("USAU All Mixed RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = TRUE)

rankedteams %>% filter(`Competition Level`== "Club") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Club Mixed RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

rankedteams %>% filter(`Competition Level`== "Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Masters Mixed RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)
```



```r
rankedteams %>% filter(`Competition Level`== "Club") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team                                                     Wins   Losses       Rating  City                        State            
-----  ------------------------------------------------------  -----  -------  -----------  --------------------------  -----------------
    1  Seattle Mixtape                                            24        3   25.1094660  Seattle                     WA               
    2  Slow White                                                 32        6   24.2930796  Boston                      MA               
    3  Wild Card                                                  40        5   22.7442414  Boston                      MA               
    4  AMP                                                        21        3   18.8768648  Philadelphia                PA               
    5  BFG                                                        27        8   16.5438864  Seattle                     WA               
    6  Drag'n Thrust                                              23        9   12.0917500  Minneapolis                 MN               
    7  Blackbird                                                  25       11    8.5636540  San Francisco               CA               
    8  shame.                                                     23        9    7.6745777  Fort Collins                CO               
    9  Toro                                                       31        7    7.0611985  Durham                      NC               
   10  Steamboat                                                  23       14    7.0577032  Cincinnati                  OH               
   11  Mischief                                                   19       13    6.9901064  San Francisco Bay Area      CA               
   12  Public Enemy                                               17       11    6.6137600  Dallas/Fort Worth           TX               
   13  Metro North                                                19       14    6.0679501  Mianus                      CT               
   14  NOISE                                                      17       15    5.9287422  Madison                     WI               
   15  Love Tractor                                               17       14    4.8618807  Denver                      CO               
   16  XIST                                                       25        7    4.8204781  New York                    NY               
   17  The Chad Larson Experience                                 17       14    4.3337587  Ames                        IA               
   18  Alloy                                                      11        8    3.2917314  Pittsburgh                  PA               
   19  Ambiguous Grey                                             16        8    3.2773409  Washington                  DC               
   20  Polar Bears                                                19        8    3.2497009  Berkeley                    CA               
   21  7 Figures                                                  22        5    2.7244038  Los Angeles                 CA               
   22  UPA                                                        18        7    2.7114219  Chicago                     IL               
   23  G-Unit                                                     15       10    2.6819072  Gainesville                 FL               
   24  Birdfruit                                                  22        6    2.6487015  Seattle                     WA               
   25  BoyShe                                                     25       11    2.6376630  Boise                       ID               
   26  No Touching!                                               12       14    2.5173167  Twin Cities                 MN               
   27  UNION                                                      13       11    2.4605298  Toronto                     Ontario          
   28  Distelfink                                                 23        6    2.1589879  Lancaster                   PA               
   29  Bang!                                                      17       10    1.9563775  West Chester                PA               
   30  Dub Club                                                   19       11    1.8582940  West Chester                PA               
   31  Columbus Cocktails                                         29        6    1.8246844  Columbus                    OH               
   32  Bozos                                                      17       10    1.7683051  Bozeman                     MT               
   33  Snake Country Bromance                                      5        1    1.7660226  SOMERVILLE                  MA               
   34  Charlotte Storm                                            23        7    1.7137245  Charlotte                   NC               
   35  BW Ultimate                                                21       14    1.6438867  Sunnyvale                   CA               
   36  Bucket                                                     20        3    1.5975626  Atlanta                     GA               
   37  Cosa Nostra                                                15       13    1.5405121  Austin                      TX               
   38  Minnesota Star Power                                       27        7    1.3951730  Minneapolis/ St. Paul       MN               
   39  The Feminists                                              19       15    1.2294529  New York                    NY               
   40  Battleship                                                 13        8    1.1991711  St-Jean-sur-Richelieu       Quebec           
   41  Sparkle Ponies                                             15       12    1.1858048  Rockville                   MD               
   42  Freakshow                                                   2        0    1.1666667  Newark                      DE               
   43  Swing State                                                25       14    1.1651355  Orlando                     FL               
   44  Classy                                                     18       10    1.1539898  San Francisco               CA               
   45  Happy Valley                                               13       11    1.0438256  Amherst                     MA               
   46  Garbage Plates                                             20       10    1.0121062  Rochester                   NY               
   47  Happy Hour                                                 15        2    0.9811173  McMinnville                 OR               
   48  SHINSHU LOOSE-Upper                                         2        5    0.9779316  Matsumoto                   Nagano           
   49  Jughandle                                                  19       10    0.9741203  Princeton                   NJ               
   50  Bird                                                       21       14    0.8956734  Minneapolis                 MN               
   51  Donuts                                                     17       11    0.8846426  Bay Area                    CA               
   52  Pegasus                                                    12       12    0.8459795  Seattle                     WA               
   53  American Barbecue                                          14        7    0.8389544  San Francisco               CA               
   54  Tequila Mockingbird                                        25       10    0.8346225  Chicago-apolis              IN               
   55  Geekshow                                                    0        2    0.8333333  Newark                      DE               
   56  Sellout                                                    17        5    0.7893382  Austin/Houston/Dallas       TX               
   57  Heartless                                                  21       11    0.7718835  Burlington                  VT               
   58  JLP                                                        20        8    0.7359223  Atlanta                     GA               
   59  Thoroughbred                                               14       13    0.7320128  Saint Louis                 MO               
   60  League of Shadows                                          21       17    0.7264795  Boston                      MA               
   61  Grand Army                                                 16       17    0.7220660  Brooklyn                    NY               
   62  Darkwing                                                   18       13    0.7016194  Mansfield                   MA               
   63  Alchemy                                                    23       10    0.7006834  San Francisco               CA               
   64  Mojo Jojo                                                  26       12    0.6609941  Minneapolis                 MN               
   65  Titan NE                                                   17       15    0.6314532  Bar Harbor                  ME               
   66  The Administrators                                         18        4    0.6179848  Willamette Valley           OR               
   67  PleasureTown                                               13       18    0.5967768  Boston                      MA               
   68  Alternative Stacks                                         19       17    0.5763964  Fairfield                   CT               
   69  Chalice                                                    13       12    0.5756777  St. Louis                   MO               
   70  Cahoots                                                    23       10    0.5602778  Asheville                   NC               
   71  Team Mexico                                                 1        6    0.5550299  Ciudad de Mexico            Mex              
   72  Townies                                                    19       11    0.5545694  Ithaca                      NY               
   73  HAOS                                                       18       12    0.5455705  Watertown                   MA               
   74  Anchor                                                      6        2    0.5435209  Halifax                     Nova Scotia      
   75  Mesteño                                                    16       12    0.5328391  Denver                      CO               
   76  Powermove                                                  13       13    0.5208632  New York                    NY               
   77  Ant Madness                                                24       13    0.5136847  Arlington                   VA               
   78  Murmur                                                     25       11    0.4947309  Athens                      GA               
   79  tHUMP                                                      21        9    0.4938753  Houston                     TX               
   80  Clue                                                       12        9    0.4563227  Philadelphia                PA               
   81  Method                                                     24        9    0.4457020  Huntsville                  AL               
   82  Risky Business                                             20       12    0.4282250  Dallas                      TX               
   83  Sunken Circus                                              17       14    0.4171890  Portland                    ME               
   84  Northern Comfort                                           22       16    0.4077650  Milwaukee                   WI               
   85  Malice in Wonderland                                       19        9    0.4009371  Raleigh                     NC               
   86  Family Style                                               14       10    0.3985215  Los Angeles                 CA               
   87  Platypi                                                    11       10    0.3923968  Chico                       CA               
   88  Rainbow                                                    20       13    0.3923915  Somerville                  MA               
   89  Moontower                                                  14       16    0.3917242  Austin                      TX               
   90  Cutthroat                                                  17       14    0.3681366  Reno                        NV               
   91  Buffalo Lake Effect                                        19       10    0.3623018  Buffalo                     NY               
   92  Shakedown                                                  18       11    0.3546793  Chicago                     IL               
   93  Long Beach Legacy                                          13        9    0.3504126  Long Beach                  CA               
   94  Mixed Nuts                                                 11       20    0.3469026  Boston                      MA               
   95  Hybrid                                                     23        8    0.3386826  Ann Arbor                   MI               
   96  Rubix                                                      12        8    0.3261827  Phoenix                     AZ               
   97  Argo                                                       11       17    0.3228982  San Francisco               CA               
   98  Sweet Action                                               14       19    0.3221599  Denver                      CO               
   99  Toast                                                      17       11    0.3096972  Grand Rapids                MI               
  100  Providence Mob                                             13       17    0.2934770  Providence                  RI               
  101  Firefly                                                    16       18    0.2932453  San Francisco               CA               
  102  8 Bit Heroes                                               18       10    0.2884952  Frederick                   MD               
  103  Prion                                                      18       11    0.2884416  Champaign-Urbana            IL               
  104  RUT                                                        12       13    0.2745317  Burlington                  VT               
  105  Albany Airbenders                                          11       19    0.2730766  Albany                      NY               
  106  Homegrown Ultimate                                          5        2    0.2571695  Nelson                      British Columbia 
  107  Fireball                                                    8        5    0.2493113  Rockville                   MD               
  108  #Birdz                                                     13        8    0.2482165  Long Island                 NY               
  109  Crash                                                       5       18    0.2479986  Kitchener-Waterloo-Guelph   Ontario          
  110  Goose Lee                                                  18       10    0.2317330  Cincinnati                  OH               
  111  Crucible                                                   17       12    0.2272019  Pittsburgh                  PA               
  112  American Hyperbole                                         15       18    0.2211034  Baltimore                   MD               
  113  Unlimited Swipes                                           13       15    0.2182120  New York                    NY               
  114  Enough Monkeys                                              7       10    0.2160095  Hartford                    VT               
  115  Fear and Loathing                                          11       13    0.2041648  Las Vegas                   NV               
  116  VIP Club                                                    6        7    0.2018056  Quebec                      QC               
  117  Lovebomb                                                    4        4    0.1786468  Ben Avon                    PA               
  118  AC Bandits                                                 17        9    0.1777300  Oakland                     CA               
  119  Buckwild                                                    5        9    0.1674892  Sacramento                  CA               
  120  Dixon Melons                                                6        4    0.1660555  Missoula                    MT               
  121  Mutiny                                                     18       14    0.1621158  St. Petersburg              FL               
  122  Sir Walter Rowdy                                           13        7    0.1567180  Raleigh                     NC               
  123  Thanksgiving                                                4        1    0.1566760  Bozeman                     MT               
  124  panIC                                                      11       14    0.1531576  Iowa City                   IA               
  125  Scarecrow                                                  12       21    0.1507985  Manchester                  NH               
  126  The Bandits                                                11       10    0.1496693  Montclair                   NJ               
  127  Sabotage                                                    9       12    0.1484761  Philadelphia                PA               
  128  LoveShack                                                  17       14    0.1465952  Atlanta                     GA               
  129  Blitzkrieg                                                  8       14    0.1428951  Kansas City                 MO               
  130  Asylum                                                     14       15    0.1425567  Athens                      OH               
  131  Tex Mix                                                    12       17    0.1417426  Dallas                      TX               
  132  Mad Udderburn                                              17       19    0.1414665  Madison                     WI               
  133  TAU                                                        10        4    0.1408306  Winston Salem               NC               
  134  Freetail                                                   12       12    0.1376515  San Antonio                 TX               
  135  Mental Toss Flycoons                                        6       10    0.1364436  Missoula                    MT               
  136  Instant Karma                                              11        9    0.1358078  Tucson                      AZ               
  137  Friday Night Couch                                         12       13    0.1353516  Seattle                     WA               
  138  Tyrannis                                                   14       16    0.1322681  Charlottesville             VA               
  139  Garbage                                                     4        8    0.1304907  Seattle                     WA               
  140  Fable                                                       6       11    0.1288244  Vancouver                   BC               
  141  Lawn Patrol                                                 6       10    0.1287865  San Jose                    CA               
  142  Impact                                                     11        9    0.1222465  Wichita                     KS               
  143  Trash Pandas                                               12       15    0.1174789  Nashville                   TN               
  144  Legion                                                     14       13    0.1135740  Lynchburg                   VA               
  145  DTH                                                         9        9    0.1131641  Cambridge                   MA               
  146  Misfits                                                     5        6    0.1119362  Spokane                     WA               
  147  The Hucking Dead                                           16       19    0.1104141  Jacksonville                FL               
  148  LORD                                                       15       18    0.1099081  Fairfax                     VA               
  149  Chef Curry with the Pot                                     7        7    0.1067886  Arlington                   VA               
  150  Dead Reckoning                                              7       10    0.1036213  Portland                    ME               
  151  Superstition                                               11       18    0.1010155  Phoenix                     AZ               
  152  Guns for Hire                                               3        3    0.0973449  Missoula                    MT               
  153  Coalition Ultimate                                         12       10    0.0955158  Minneapolis                 MN               
  154  Blowing Heat 2.0                                            7        7    0.0931072  New Castle                  DE               
  155  Los  Heros                                                 16       21    0.0922719  Bloomington                 IN               
  156  Crush                                                       9       10    0.0916500  Knoxville                   TN               
  157  Free Ride                                                  10       12    0.0875381  Columbia                    MO               
  158  Makeshift Returns                                           5        1    0.0872992  Portland                    OR               
  159  rubber duck ultimate.                                      10        6    0.0872652  Fayetteville                AR               
  160  Boomtown                                                   10        8    0.0852791  Tulsa                       OK               
  161  Petey's Pirates                                            18       14    0.0851433  Columbus                    OH               
  162  Rat City                                                    5       16    0.0750337  Baltimore                   MD               
  163  Varsity                                                     7       10    0.0710572  New York                    NY               
  164  North Coast Disc Co.                                       13       16    0.0702006  Cleveland                   OH               
  165  Mimosas                                                     7       17    0.0686235  Berkeley                    CA               
  166  Nautilus                                                    6        7    0.0678736  Farmington                  CT               
  167  Possum                                                      9        6    0.0661960  Greenville                  SC               
  168  Moonshine                                                  10       19    0.0643435  Lexington                   KY               
  169  Springs Mixed Ulty Team (SMUT)                             10       10    0.0641067  Colorado Springs            CO               
  170  Memphis Hustle & Flow                                       9        7    0.0611643  Memphis                     TN               
  171  ELevate                                                    13       21    0.0591573  Chicago                     IL               
  172  Raft Ultimate                                               2        4    0.0569453  Ottawa                      Ontario          
  173  Wingdings                                                   7       11    0.0556565  Des Moines                  IA               
  174  Robot                                                       6        6    0.0556062  Santa Barbara               CA               
  175  All Jeeps, All Night                                        2        8    0.0553549  Boulder                     CO               
  176  Rocket LawnChair                                           13       12    0.0531709  Ann Arbor                   MI               
  177  Jabba                                                      15       19    0.0517644  Chicago                     IL               
  178  Midnight Whiskey                                            8        4    0.0514200  Eugene                      OR               
  179  Drunk in Space                                              2        9    0.0509471  Boston                      MA               
  180  ¡Fiesta!                                                    6        6    0.0504941  San Diego                   CA               
  181  Animals                                                     2        5    0.0502787  Sherbrooke                  Québec           
  182  Snail                                                       2        2    0.0499727  Geneseo                     NY               
  183  Liquid Hustle                                              10       20    0.0487650  Indianapolis                IN               
  184  Spirit Fowl                                                11       17    0.0472670  Saint Paul                  MN               
  185  MxD                                                         9       13    0.0460252  College Park                MD               
  186  Mixed Results                                               9       16    0.0452881  Knoxville                   TN               
  187  NEKUD                                                       4        7    0.0436326  Burke                       VT               
  188  Wildstyle                                                  10       11    0.0417673  Austin                      TX               
  189  Big                                                         2        4    0.0406214  Anchorage                   AK               
  190  Spawn                                                       2        3    0.0405803  Fredericton                 NB               
  191  Merry Band of Muffin Makers from the Land of Megastar       3        3    0.0395362  Denver                      CO               
  192  Stormborn                                                   7       19    0.0390485  Washington                  DC               
  193  Mastodon                                                    8       12    0.0382554  Madison                     WI               
  194  The Strangers                                               8       14    0.0379842  Westminster                 CO               
  195  Local 613                                                   4        3    0.0357360  Kingston                    ON               
  196  Absolute Zero                                               8       12    0.0333214  Santa Clara                 CA               
  197  Stackcats                                                   9       20    0.0328673  Chicago                     IL               
  198  Bitmap                                                      5       17    0.0327338  Philadelphia                PA               
  199  OMBU                                                        4        5    0.0305635  Portland                    OR               
  200  Baywatch                                                    7       15    0.0299531  Pensacola                   FL               
  201  Dumpster Fire                                               4       10    0.0272176  North Charleston            SC               
  202  Dream Team Frisbee                                          3        4    0.0270689  Columbia                    SC               
  203  Quest                                                       2        6    0.0266846  Québec                      Québec           
  204  Boomtown Pandas                                             6       22    0.0251007  Madison                     WI               
  205  Swing                                                       4       12    0.0250094  San Diego                   CA               
  206  Heavy Flow                                                  6       19    0.0248700  Columbia                    MD               
  207  District Cocktails                                          6       14    0.0241320  Washington                  DC               
  208  Mississippi Blues                                           7       10    0.0224585  Jackson                     MS               
  209  BRUH                                                        4       10    0.0223373  Clemson                     SC               
  210  Doppler Effect                                             10       19    0.0221291  Detroit                     MI               
  211  DR                                                          3       15    0.0217994  Berkeley                    CA               
  212  Igneous Ultimate                                            3       14    0.0215711  Bend                        OR               
  213  Fifth Element                                               6       12    0.0207619  Louisville                  KY               
  214  Feral Cows                                                  5       13    0.0203808  Palo Alto                   CA               
  215  Bulleit Train                                               7       17    0.0196085  Seattle                     WA               
  216  OutKast                                                     6       12    0.0195341  Atlanta                     GA               
  217  Swipe >                                                     4        3    0.0188329  Fossil Cove                 OK               
  218  Pink Pear                                                   4        8    0.0187301  NYC                         NY               
  219  Breakers Mark                                               3        4    0.0180605  Portland                    OR               
  220  Balloon                                                     4       14    0.0168237  College Station             TX               
  221  Serial Crusher Theory                                       5        3    0.0159153  Twin Cities                 MN               
  222  Carolina Reign                                              4       16    0.0157743  Winston-Salem               NC               
  223  $Griz Alum$-Upper                                           2        4    0.0157352  Missoula                    MT               
  224  Magic City Mayhem                                           4        7    0.0152157  Birmingham                  AL               
  225  Mixed on the Rock                                           3        8    0.0149782  Little Rock                 AR               
  226  Mixed Pi                                                    4        4    0.0144066  Lexington                   KY               
  227  Flicks and Chill                                            3        3    0.0140454  Austin                      TX               
  228  Glitter Bomb                                                6       14    0.0138229  San Francisco               CA               
  229  SOFA Spartans                                               1        4    0.0134811  Oneonta                     NY               
  230  Bold City                                                   2       10    0.0125725  Jacksonville                FL               
  231  Beezer Ultimate                                             1        4    0.0120094  Durham                      NH               
  232  Swag State                                                  4       12    0.0119501  Orlando                     FL               
  233  The Tossers                                                 4        2    0.0112942  Austin                      TX               
  234  Turmoil                                                     2        8    0.0109833  Memphis                     TN               
  235  Oh My!                                                     10       12    0.0104898  Portland                    OR               
  236  Turnstyle                                                   3       16    0.0103118  New York                    NY               
  237  Get A Baby                                                  2       10    0.0101343  Plymouth                    NH               
  238  Crop That Top (Davidson)                                    0        7    0.0099566  Davidson                    NC               
  239  Hairy Otter                                                 2        5    0.0098021  Nashville                   TN               
  240  FENOD & Friends                                             1        2    0.0097826  Cambridge                   MA               
  241  EDM                                                         1       11    0.0097449  Denver                      CO               
  242  Skyhawks                                                    6       16    0.0086589  South Bend                  IN               
  243  I-79                                                        6       17    0.0076551  Pittsburgh/Erie             PA               
  244  Mousetrap                                                   6       15    0.0074945  Madison                     WI               
  245  Carroll College (The Fighting Saints)                       0        6    0.0074353  Helena                      MT               
  246  Tlacuaches                                                  4        9    0.0071439  McAllen                     TX               
  247  7 Express                                                   2        4    0.0070013  New York                    NY               
  248  FIRE ULTIMATE CLUB                                          0        7    0.0068378  BOGOTA                      CUNDINAMARCA     
  249  Twonies                                                     1        4    0.0067069  Ithaca                      NY               
  250  ThunderCats                                                 2        5    0.0061458  Charlotte                   NC               
  251  Friend Zone                                                 2       12    0.0060607  Gainesville                 GA               
  252  #Beez                                                       4       15    0.0058443  Long Island                 NY               
  253  Los Penguinos                                               3        9    0.0054739  DC/Baltimore                DC               
  254  Thunderpants the Magic Dragon                               4       12    0.0049420  Dayton                      OH               
  255  Flying Circus Disc Club                                     4       12    0.0047334  Portland                    OR               
  256  CUFAX                                                       1        7    0.0041197  Cranford                    NJ               
  257  Slam                                                        3        5    0.0034848  Greenville                  SC               
  258  Taco Truck PNC                                              1        5    0.0029824  Turlock                     CA               
  259  Rogue                                                       0       11    0.0026615  Tucson                      AZ               
  260  Mud Turtles                                                 1       13    0.0026372  Houston                     TX               
  261  Family Julez                                                2        6    0.0023914  Newark                      DE               
  262  Hot Stix                                                    0       18    0.0018987  San Francisco               CA               
  263  Pocket City Approach                                        0        6    0.0018304  Evansville                  IN               
  264  Pandatime                                                   0       11    0.0016333  Towson                      MD               
  265  The Greatest Team in America                                2       11    0.0013185  Baltimore                   MD               
  266  Identity Crisis                                             0        6    0.0011134  Boone                       NC               
  267  CimaSalta-Upper                                             1        3    0.0010294  Jacksonville                FL               
  268  HORUS ULTIMATE TEAM                                         1        5    0.0010051  MEXICO CITY                 MEXICO CITY      
  269  Norfolkin' Around                                           0        6    0.0009828  Norfolk                     VA               
  270  ALTimate Brews                                              3       18    0.0008832  Madison                     WI               
  271  Flat Earth Club                                             1        5    0.0008681  Washington                  DC               
  272  Taco Cat                                                    1       12    0.0006480  Chicago                     IL               
  273  Natural Twenties                                            0        5    0.0006431  Portland                    OR               
  274  defunct                                                     0        5    0.0004556  Middlebury                  CT               
  275  Mishigami                                                   1        8    0.0001486  East Lansing                MI               
  276  Sun Bats                                                    0        5    0.0000761  Austin                      TX               
  277  Petey's Parrots                                             2       19    0.0000269  Columbus                    OH               
  278  WOOcestershire Sauce                                        0        4    0.0000005  Wooster                     OH               
  279  Bro Kittens                                                 0        4    0.0000000  Athens                      OH               
  279  Choco Ghost House                                           0        4    0.0000000  Portland                    OR               
  279  Delta Breeze                                                0        7    0.0000000  Sacramento                  CA               
  279  Missoula Yoursoula                                          0        3    0.0000000  Missoula                    MT               
  279  MUTT                                                        0        6    0.0000000  Saint Paul                  MN               
  279  N.A.B.P.S. (Not Awesome But Pretty Solid)                   0        6    0.0000000  Madison                     WI               

```r
rankedteams %>% filter(`Competition Level`== "Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team                         Wins   Losses      Rating  City            State            
-----  --------------------------  -----  -------  ----------  --------------  -----------------
    1  UPAARP                          6        2   4.5536727  Chicago         IL               
    2  512                             8        1   4.3983298  Austin          TX               
    3  Members Only                    7        3   2.4445589  SoCal           CA               
    4  Molasses Disaster               8        2   2.1110319  Boston          MA               
    5  Hey Babe                        7        3   1.9598312  Minneapolis     MN               
    6  San Francisco Bridge Club       6        4   1.8526471  San Francisco   CA               
    7  Trickle Down                    5        5   0.7917594  Portland        OR               
    8  Old Parts and Pandas            3        7   0.5288036  Seattle         WA               
    9  Prime                           5        4   0.2431945  Greenville      SC               
   10  Charge!                         2        4   0.1062934  Washington      DC               
   11  Old Fashioned                   3        6   0.0581835  Madison         WI               
   12  Third Party                     4        6   0.0240328  Washington      DC               
   13  Trainwreck                      1        8   0.0204583  Durham          NC               
   14  Old DErty People                1        2   0.0100938  Wilmington      DE               
   15  Millennial Falcons              0        2   0.0034025  Saint Louis     MO               
   16  Drunken Masters                 0        2   0.0000000  Somerville      MA               
   16  Fossil                          0        2   0.0000000  Tampa           FL               
   16  Mastadon                        0        2   0.0000000  Vancouver       British Columbia 
   16  Oso Busco                       0        1   0.0000000  Denver          CO               
   16  Sacramentos-Freshmakers         0        2   0.0000000  Sacramento      CA               


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
division="Mixed"
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

![](AllMixedRanking_files/figure-html/plotNetwork-1.png)<!-- -->

