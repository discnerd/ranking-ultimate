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
## [1] "2.32.498550 (9dec58e66c31bcc53a9ce3c7226f0c1c5810906a)"
## 
## $chrome$userDataDir
## [1] "C:\\Users\\mr437799\\AppData\\Local\\Temp\\scoped_dir5652_22288"
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
## [1] "d25f81e0aec6ef15cf4e40d098121911"
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
## Warning: Too few values at 26 locations: 8, 9, 10, 11, 12, 13, 14, 15, 16,
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
## Ncells  922666 49.3    1442291 77.1  1442291 77.1
## Vcells 1319922 10.1    2552219 19.5  2368599 18.1
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
##  [1]  27  30  77  94 121 125 134 143 192 204 207 212 216 227 246 273 305
## [18] 351 352 360 398 399 404 413
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
-----  ------------------------------------------------------  -----  -------  -----------  --------------------------  -------------
    1  Seattle Mixtape                                            20        3   24.1453675  Seattle                     WA           
    2  Slow White                                                 28        6   20.5613318  Boston                      MA           
    3  Wild Card                                                  36        3   19.6965736  Boston                      MA           
    4  AMP                                                        17        3   16.9915272  Philadelphia                PA           
    5  BFG                                                        23        7   15.5815861  Seattle                     WA           
    6  Drag'n Thrust                                              19        7   12.2676749  Minneapolis                 MN           
    7  Blackbird                                                  21        9    8.4320059  San Francisco               CA           
    8  Steamboat                                                  21       10    8.1535017  Cincinnati                  OH           
    9  Toro                                                       29        3    7.8382301  Durham                      NC           
   10  Mischief                                                   17        9    7.3213920  San Francisco Bay Area      CA           
   11  shame.                                                     21        6    6.6768692  Fort Collins                CO           
   12  NOISE                                                      16       10    6.5548514  Madison                     WI           
   13  Metro North                                                17       10    5.1389086  Mianus                      CT           
   14  XIST                                                       25        7    5.0386599  New York                    NY           
   15  The Chad Larson Experience                                 17       14    4.6897778  Ames                        IA           
   16  Public Enemy                                               14        8    3.9994732  Dallas/Fort Worth           TX           
   17  Love Tractor                                               17        8    3.8787476  Denver                      CO           
   18  Polar Bears                                                19        8    3.7438320  Berkeley                    CA           
   19  Ambiguous Grey                                             16        8    3.5343066  Washington                  DC           
   20  Alloy                                                      11        8    3.4488973  Pittsburgh                  PA           
   21  7 Figures                                                  22        5    3.1454884  Los Angeles                 CA           
   22  UPA                                                        18        7    3.0070413  Chicago                     IL           
   23  Birdfruit                                                  22        6    2.9468743  Seattle                     WA           
   24  BoyShe                                                     25       11    2.9357033  Boise                       ID           
   25  G-Unit                                                     15       10    2.6865944  Gainesville                 FL           
   26  UNION                                                      13       11    2.4866350  Toronto                     Ontario      
   27  Distelfink                                                 23        6    2.3168043  Lancaster                   PA           
   28  Columbus Cocktails                                         29        6    2.1312309  Columbus                    OH           
   29  Bang!                                                      17       10    2.1010878  West Chester                PA           
   30  Dub Club                                                   19       11    2.0652717  West Chester                PA           
   31  Charlotte Storm                                            23        7    1.9596874  Charlotte                   NC           
   32  Bozos                                                      17       10    1.9410001  Bozeman                     MT           
   33  BW Ultimate                                                21       14    1.8277634  Sunnyvale                   CA           
   34  No Touching!                                               10       10    1.7772450  Twin Cities                 MN           
   35  Bucket                                                     20        3    1.7668041  Atlanta                     GA           
   36  Snake Country Bromance                                      5        1    1.7205938  SOMERVILLE                  MA           
   37  Cosa Nostra                                                15       13    1.6482625  Austin                      TX           
   38  Minnesota Star Power                                       27        7    1.6047894  Minneapolis/ St. Paul       MN           
   39  Classy                                                     18       10    1.2988391  San Francisco               CA           
   40  Swing State                                                25       14    1.2980630  Orlando                     FL           
   41  Sparkle Ponies                                             15       12    1.2655970  Rockville                   MD           
   42  The Feminists                                              19       15    1.2636310  New York                    NY           
   43  Battleship                                                 13        8    1.2102987  St-Jean-sur-Richelieu       Quebec       
   44  Freakshow                                                   2        0    1.1666667  Newark                      DE           
   45  SHINSHU LOOSE-Upper                                         2        5    1.0989462  Matsumoto                   Nagano       
   46  Happy Hour                                                 15        2    1.0956647  McMinnville                 OR           
   47  Happy Valley                                               13       11    1.0743063  Amherst                     MA           
   48  Jughandle                                                  19       10    1.0425132  Princeton                   NJ           
   49  Garbage Plates                                             20       10    1.0399776  Rochester                   NY           
   50  Bird                                                       21       14    1.0089294  Minneapolis                 MN           
   51  Donuts                                                     17       11    1.0085961  Bay Area                    CA           
   52  American Barbecue                                          14        7    0.9618315  San Francisco               CA           
   53  Tequila Mockingbird                                        25       10    0.9384828  Chicago-apolis              IN           
   54  Pegasus                                                    12       12    0.9373448  Seattle                     WA           
   55  Thoroughbred                                               14       13    0.8602068  Saint Louis                 MO           
   56  Geekshow                                                    0        2    0.8333333  Newark                      DE           
   57  Mojo Jojo                                                  26       12    0.8210879  Minneapolis                 MN           
   58  JLP                                                        20        8    0.8149317  Atlanta                     GA           
   59  Sellout                                                    17        5    0.8147290  Austin/Houston/Dallas       TX           
   60  Heartless                                                  21       11    0.7926532  Burlington                  VT           
   61  Alchemy                                                    23       10    0.7862308  San Francisco               CA           
   62  Grand Army                                                 16       17    0.7550860  Brooklyn                    NY           
   63  League of Shadows                                          21       17    0.7516593  Boston                      MA           
   64  Darkwing                                                   18       13    0.7208932  Mansfield                   MA           
   65  The Administrators                                         18        4    0.6916112  Willamette Valley           OR           
   66  Chalice                                                    13       12    0.6563186  St. Louis                   MO           
   67  Titan NE                                                   17       15    0.6475447  Bar Harbor                  ME           
   68  Cahoots                                                    23       10    0.6371848  Asheville                   NC           
   69  PleasureTown                                               13       18    0.6165361  Boston                      MA           
   70  Alternative Stacks                                         19       17    0.5852551  Fairfield                   CT           
   71  Team Mexico                                                 1        6    0.5795279  Ciudad de Mexico            Mex          
   72  Townies                                                    19       11    0.5720665  Ithaca                      NY           
   73  Mesteño                                                    16       12    0.5708977  Denver                      CO           
   74  HAOS                                                       18       12    0.5606635  Watertown                   MA           
   75  Murmur                                                     25       11    0.5541005  Athens                      GA           
   76  Anchor                                                      6        2    0.5528777  Halifax                     Nova Scotia  
   77  Ant Madness                                                24       13    0.5460651  Arlington                   VA           
   78  Powermove                                                  13       13    0.5434309  New York                    NY           
   79  tHUMP                                                      21        9    0.5426852  Houston                     TX           
   80  Method                                                     24        9    0.5002733  Huntsville                  AL           
   81  Clue                                                       12        9    0.4959261  Philadelphia                PA           
   82  Risky Business                                             20       12    0.4674004  Dallas                      TX           
   83  Malice in Wonderland                                       19        9    0.4572979  Raleigh                     NC           
   84  Northern Comfort                                           22       16    0.4493329  Milwaukee                   WI           
   85  Family Style                                               14       10    0.4468988  Los Angeles                 CA           
   86  Platypi                                                    11       10    0.4413065  Chico                       CA           
   87  Sunken Circus                                              17       14    0.4282296  Portland                    ME           
   88  Moontower                                                  14       16    0.4204086  Austin                      TX           
   89  Cutthroat                                                  17       14    0.4157306  Reno                        NV           
   90  Shakedown                                                  18       11    0.4048457  Chicago                     IL           
   91  Rainbow                                                    20       13    0.4014024  Somerville                  MA           
   92  Long Beach Legacy                                          13        9    0.3945061  Long Beach                  CA           
   93  Hybrid                                                     23        8    0.3848647  Ann Arbor                   MI           
   94  Buffalo Lake Effect                                        19       10    0.3764011  Buffalo                     NY           
   95  Argo                                                       11       17    0.3613030  San Francisco               CA           
   96  Mixed Nuts                                                 11       20    0.3517029  Boston                      MA           
   97  Toast                                                      17       11    0.3500524  Grand Rapids                MI           
   98  Rubix                                                      12        8    0.3481683  Phoenix                     AZ           
   99  Firefly                                                    16       18    0.3286927  San Francisco               CA           
  100  Prion                                                      18       11    0.3251840  Champaign-Urbana            IL           
  101  Sweet Action                                               14       19    0.3241209  Denver                      CO           
  102  8 Bit Heroes                                               18       10    0.3136365  Frederick                   MD           
  103  Providence Mob                                             13       17    0.3008588  Providence                  RI           
  104  RUT                                                        12       13    0.2814834  Burlington                  VT           
  105  Albany Airbenders                                          11       19    0.2812435  Albany                      NY           
  106  Fireball                                                    8        5    0.2674428  Rockville                   MD           
  107  Crash                                                       5       18    0.2665569  Kitchener-Waterloo-Guelph   Ontario      
  108  Goose Lee                                                  18       10    0.2638794  Cincinnati                  OH           
  109  #Birdz                                                     13        8    0.2589746  Long Island                 NY           
  110  Crucible                                                   17       12    0.2439388  Pittsburgh                  PA           
  111  American Hyperbole                                         15       18    0.2422446  Baltimore                   MD           
  112  Fear and Loathing                                          11       13    0.2301861  Las Vegas                   NV           
  113  Unlimited Swipes                                           13       15    0.2246030  New York                    NY           
  114  Enough Monkeys                                              7       10    0.2219313  Hartford                    VT           
  115  VIP Club                                                    6        7    0.2072127  Quebec                      QC           
  116  AC Bandits                                                 17        9    0.1979442  Oakland                     CA           
  117  Lovebomb                                                    4        4    0.1952404  Ben Avon                    PA           
  118  Buckwild                                                    5        9    0.1862859  Sacramento                  CA           
  119  Mutiny                                                     18       14    0.1816381  St. Petersburg              FL           
  120  Sir Walter Rowdy                                           13        7    0.1754767  Raleigh                     NC           
  121  panIC                                                      11       14    0.1749556  Iowa City                   IA           
  122  Asylum                                                     14       15    0.1695819  Athens                      OH           
  123  LoveShack                                                  17       14    0.1639883  Atlanta                     GA           
  124  Mad Udderburn                                              17       19    0.1612910  Madison                     WI           
  125  Sabotage                                                    9       12    0.1593572  Philadelphia                PA           
  126  Blitzkrieg                                                  8       14    0.1563798  Kansas City                 MO           
  127  TAU                                                        10        4    0.1561787  Winston Salem               NC           
  128  The Bandits                                                11       10    0.1555433  Montclair                   NJ           
  129  Tex Mix                                                    12       17    0.1550263  Dallas                      TX           
  130  Scarecrow                                                  12       21    0.1547863  Manchester                  NH           
  131  Friday Night Couch                                         12       13    0.1533277  Seattle                     WA           
  132  Freetail                                                   12       12    0.1487916  San Antonio                 TX           
  133  Instant Karma                                              11        9    0.1480872  Tucson                      AZ           
  134  Tyrannis                                                   14       16    0.1452974  Charlottesville             VA           
  135  Lawn Patrol                                                 6       10    0.1447201  San Jose                    CA           
  136  Garbage                                                     4        8    0.1446335  Seattle                     WA           
  137  Fable                                                       6       11    0.1434268  Vancouver                   BC           
  138  Impact                                                     11        9    0.1398465  Wichita                     KS           
  139  Mental Toss Flycoons                                        6        9    0.1385229  Missoula                    MT           
  140  Trash Pandas                                               12       15    0.1315302  Nashville                   TN           
  141  Misfits                                                     5        6    0.1245434  Spokane                     WA           
  142  Legion                                                     14       13    0.1234847  Lynchburg                   VA           
  143  The Hucking Dead                                           16       19    0.1212001  Jacksonville                FL           
  144  LORD                                                       15       18    0.1194246  Fairfax                     VA           
  145  DTH                                                         9        9    0.1163486  Cambridge                   MA           
  146  Chef Curry with the Pot                                     7        7    0.1149058  Arlington                   VA           
  147  Superstition                                               11       18    0.1143897  Phoenix                     AZ           
  148  Coalition Ultimate                                         12       10    0.1118501  Minneapolis                 MN           
  149  Dead Reckoning                                              7       10    0.1063882  Portland                    ME           
  150  Los  Heros                                                 16       21    0.1048378  Bloomington                 IN           
  151  Crush                                                       9       10    0.1038888  Knoxville                   TN           
  152  Blowing Heat 2.0                                            7        7    0.1007138  New Castle                  DE           
  153  Free Ride                                                  10       12    0.0988782  Columbia                    MO           
  154  Makeshift Returns                                           5        1    0.0976591  Portland                    OR           
  155  rubber duck ultimate.                                      10        6    0.0975814  Fayetteville                AR           
  156  Dixon Melons                                                2        3    0.0957150  Missoula                    MT           
  157  Petey's Pirates                                            18       14    0.0957002  Columbus                    OH           
  158  Boomtown                                                   10        8    0.0940591  Tulsa                       OK           
  159  Possum                                                      9        6    0.0796472  Greenville                  SC           
  160  Rat City                                                    5       16    0.0785567  Baltimore                   MD           
  161  North Coast Disc Co.                                       13       16    0.0773203  Cleveland                   OH           
  162  Mimosas                                                     7       17    0.0766867  Berkeley                    CA           
  163  Varsity                                                     7       10    0.0738006  New York                    NY           
  164  Moonshine                                                  10       19    0.0726465  Lexington                   KY           
  165  Nautilus                                                    6        7    0.0697351  Farmington                  CT           
  166  Memphis Hustle & Flow                                       9        7    0.0681190  Memphis                     TN           
  167  ELevate                                                    13       21    0.0676626  Chicago                     IL           
  168  Springs Mixed Ulty Team (SMUT)                             10       10    0.0673028  Colorado Springs            CO           
  169  Wingdings                                                   7       11    0.0634570  Des Moines                  IA           
  170  Robot                                                       6        6    0.0624725  Santa Barbara               CA           
  171  Rocket LawnChair                                           13       12    0.0609407  Ann Arbor                   MI           
  172  All Jeeps, All Night                                        2        8    0.0589349  Boulder                     CO           
  173  Jabba                                                      15       19    0.0586500  Chicago                     IL           
  174  Raft Ultimate                                               2        4    0.0582031  Ottawa                      Ontario      
  175  Midnight Whiskey                                            8        4    0.0572904  Eugene                      OR           
  176  ¡Fiesta!                                                    6        6    0.0561804  San Diego                   CA           
  177  Liquid Hustle                                              10       20    0.0552193  Indianapolis                IN           
  178  Spirit Fowl                                                11       17    0.0535571  Saint Paul                  MN           
  179  Drunk in Space                                              2        9    0.0523078  Boston                      MA           
  180  Snail                                                       2        2    0.0519174  Geneseo                     NY           
  181  Animals                                                     2        5    0.0509315  Sherbrooke                  Québec       
  182  Mixed Results                                               9       16    0.0506279  Knoxville                   TN           
  183  MxD                                                         9       13    0.0479602  College Park                MD           
  184  Big                                                         2        4    0.0454561  Anchorage                   AK           
  185  Wildstyle                                                  10       11    0.0451571  Austin                      TX           
  186  NEKUD                                                       4        7    0.0445802  Burke                       VT           
  187  Mastodon                                                    8       12    0.0431966  Madison                     WI           
  188  Stormborn                                                   7       19    0.0421914  Washington                  DC           
  189  Spawn                                                       2        3    0.0416980  Fredericton                 NB           
  190  Merry Band of Muffin Makers from the Land of Megastar       3        3    0.0406133  Denver                      CO           
  191  The Strangers                                               8       14    0.0395784  Westminster                 CO           
  192  Stackcats                                                   9       20    0.0376133  Chicago                     IL           
  193  Local 613                                                   4        3    0.0373528  Kingston                    ON           
  194  Absolute Zero                                               8       12    0.0373198  Santa Clara                 CA           
  195  Bitmap                                                      5       17    0.0342685  Philadelphia                PA           
  196  OMBU                                                        4        5    0.0341275  Portland                    OR           
  197  Baywatch                                                    7       15    0.0334112  Pensacola                   FL           
  198  Dumpster Fire                                               4       10    0.0305134  North Charleston            SC           
  199  Dream Team Frisbee                                          3        4    0.0302859  Columbia                    SC           
  200  Boomtown Pandas                                             6       22    0.0291843  Madison                     WI           
  201  Swing                                                       4       12    0.0277809  San Diego                   CA           
  202  Quest                                                       2        6    0.0274100  Québec                      Québec       
  203  Heavy Flow                                                  6       19    0.0258147  Columbia                    MD           
  204  District Cocktails                                          6       14    0.0257822  Washington                  DC           
  205  Doppler Effect                                             10       19    0.0252982  Detroit                     MI           
  206  Mississippi Blues                                           7       10    0.0251221  Jackson                     MS           
  207  BRUH                                                        4       10    0.0244190  Clemson                     SC           
  208  Igneous Ultimate                                            3       14    0.0240880  Bend                        OR           
  209  DR                                                          3       15    0.0238242  Berkeley                    CA           
  210  Fifth Element                                               6       12    0.0231576  Louisville                  KY           
  211  Feral Cows                                                  5       13    0.0226923  Palo Alto                   CA           
  212  Bulleit Train                                               7       17    0.0220118  Seattle                     WA           
  213  OutKast                                                     6       12    0.0218480  Atlanta                     GA           
  214  Swipe >                                                     4        3    0.0208143  Fossil Cove                 OK           
  215  Breakers Mark                                               3        4    0.0201450  Portland                    OR           
  216  Pink Pear                                                   4        8    0.0192884  NYC                         NY           
  217  Balloon                                                     4       14    0.0184349  College Station             TX           
  218  Serial Crusher Theory                                       5        3    0.0183570  Twin Cities                 MN           
  219  Carolina Reign                                              4       16    0.0176678  Winston-Salem               NC           
  220  Magic City Mayhem                                           4        7    0.0170650  Birmingham                  AL           
  221  Mixed on the Rock                                           3        8    0.0166620  Little Rock                 AR           
  222  Mixed Pi                                                    4        4    0.0157854  Lexington                   KY           
  223  Glitter Bomb                                                6       14    0.0155694  San Francisco               CA           
  224  Flicks and Chill                                            3        3    0.0152725  Austin                      TX           
  225  Bold City                                                   2       10    0.0139610  Jacksonville                FL           
  226  SOFA Spartans                                               1        4    0.0138843  Oneonta                     NY           
  227  Swag State                                                  4       12    0.0133374  Orlando                     FL           
  228  The Tossers                                                 4        2    0.0123506  Austin                      TX           
  229  Beezer Ultimate                                             1        4    0.0123416  Durham                      NH           
  230  Turmoil                                                     2        8    0.0123035  Memphis                     TN           
  231  Oh My!                                                     10       12    0.0117247  Portland                    OR           
  232  Crop That Top (Davidson)                                    0        7    0.0112645  Davidson                    NC           
  233  Hairy Otter                                                 2        5    0.0109645  Nashville                   TN           
  234  Turnstyle                                                   3       16    0.0105522  New York                    NY           
  235  Get A Baby                                                  2       10    0.0104049  Plymouth                    NH           
  236  EDM                                                         1       11    0.0103819  Denver                      CO           
  237  FENOD & Friends                                             1        2    0.0100286  Cambridge                   MA           
  238  Skyhawks                                                    6       16    0.0097572  South Bend                  IN           
  239  Mousetrap                                                   6       15    0.0085656  Madison                     WI           
  240  I-79                                                        6       17    0.0081390  Pittsburgh/Erie             PA           
  241  FIRE ULTIMATE CLUB                                          0        7    0.0080729  BOGOTA                      CUNDINAMARCA 
  242  Tlacuaches                                                  4        9    0.0077876  McAllen                     TX           
  243  7 Express                                                   2        4    0.0072120  New York                    NY           
  244  Twonies                                                     1        4    0.0069075  Ithaca                      NY           
  245  ThunderCats                                                 2        5    0.0068674  Charlotte                   NC           
  246  Friend Zone                                                 2       12    0.0067762  Gainesville                 GA           
  247  #Beez                                                       4       15    0.0060639  Long Island                 NY           
  248  Los Penguinos                                               3        9    0.0058672  DC/Baltimore                DC           
  249  Thunderpants the Magic Dragon                               4       12    0.0055081  Dayton                      OH           
  250  Flying Circus Disc Club                                     4       12    0.0052846  Portland                    OR           
  251  CUFAX                                                       1        7    0.0044369  Cranford                    NJ           
  252  Slam                                                        3        5    0.0038447  Greenville                  SC           
  253  Taco Truck PNC                                              1        5    0.0033466  Turlock                     CA           
  254  Rogue                                                       0       11    0.0029820  Tucson                      AZ           
  255  Mud Turtles                                                 1       13    0.0028938  Houston                     TX           
  256  Family Julez                                                2        6    0.0025730  Newark                      DE           
  257  Hot Stix                                                    0       18    0.0021146  San Francisco               CA           
  258  Pocket City Approach                                        0        6    0.0020487  Evansville                  IN           
  259  Pandatime                                                   0       11    0.0016963  Towson                      MD           
  260  The Greatest Team in America                                2       11    0.0013774  Baltimore                   MD           
  261  Identity Crisis                                             0        6    0.0012222  Boone                       NC           
  262  CimaSalta-Upper                                             1        3    0.0011511  Jacksonville                FL           
  263  HORUS ULTIMATE TEAM                                         1        5    0.0010867  MEXICO CITY                 MEXICO CITY  
  264  Norfolkin' Around                                           0        6    0.0010665  Norfolk                     VA           
  265  ALTimate Brews                                              3       18    0.0009574  Madison                     WI           
  266  Flat Earth Club                                             1        5    0.0009047  Washington                  DC           
  267  Taco Cat                                                    1       12    0.0007244  Chicago                     IL           
  268  Natural Twenties                                            0        5    0.0007181  Portland                    OR           
  269  defunct                                                     0        5    0.0004731  Middlebury                  CT           
  270  Mishigami                                                   1        8    0.0001674  East Lansing                MI           
  271  Sun Bats                                                    0        5    0.0000823  Austin                      TX           
  272  Petey's Parrots                                             2       19    0.0000293  Columbus                    OH           
  273  WOOcestershire Sauce                                        0        4    0.0000005  Wooster                     OH           
  274  Bro Kittens                                                 0        4    0.0000000  Athens                      OH           
  274  Choco Ghost House                                           0        4    0.0000000  Portland                    OR           
  274  Delta Breeze                                                0        7    0.0000000  Sacramento                  CA           
  274  Missoula Yoursoula                                          0        3    0.0000000  Missoula                    MT           
  274  MUTT                                                        0        6    0.0000000  Saint Paul                  MN           
  274  N.A.B.P.S. (Not Awesome But Pretty Solid)                   0        6    0.0000000  Madison                     WI           

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

