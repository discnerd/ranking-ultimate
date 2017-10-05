# USAU Mens Rankings
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
## [1] "C:\\Users\\mr437799\\AppData\\Local\\Temp\\scoped_dir10276_10802"
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
## [1] "3ebb1cc5f32ff6cb6b6504c3db4f1552"
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

elem <- remDr$findElement(using = 'xpath', "//*/option[@value = '17']")
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
  filter(`Competition Level` != "College") %>%  
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
scores <-data.frame(Team1=character(), Score1=numeric(), Link1=character(),
                    Team2=character(), Score2=numeric(), Link2=character(),
                    Date=character())

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
                                   Link1=teams$Links[i],
                                   Team2=teams$Team[oppID], 
                                   Score2=data$Against[j],
                                   Link2=teams$Links[oppID],
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
## Warning: Too few values at 1 locations: 2
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
## Warning: Too many values at 1 locations: 20
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
## Warning: Too few values at 1 locations: 10
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
## Warning: Too many values at 1 locations: 28
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
## Warning: Too few values at 3 locations: 25, 26, 27
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
## Warning: Too few values at 2 locations: 15, 27
```

```
## Warning: Too few values at 2 locations: 3, 17
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
## Warning: Too few values at 2 locations: 3, 17
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
## Warning: Too few values at 3 locations: 31, 32, 33
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
## Warning: Too few values at 3 locations: 20, 21, 22
```

```
## Warning: Too few values at 1 locations: 12
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
## Warning: Too few values at 3 locations: 18, 19, 20
```

```
## Warning: Too few values at 1 locations: 12
```

```
## Warning: Too few values at 2 locations: 5, 6
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
## Warning in ifelse(For == "L", 0, as.numeric(For)): NAs introduced by
## coercion
```

```
## Warning in ifelse(Against == "L", 0, as.numeric(Against)): NAs introduced
## by coercion
```

```
## Warning: Too few values at 3 locations: 22, 23, 24
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
## Warning: Too few values at 3 locations: 27, 28, 29
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
## Warning: Too few values at 1 locations: 3
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
## Warning: Too few values at 3 locations: 27, 28, 29
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
## Warning: Too few values at 1 locations: 3
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
## Warning: Too few values at 3 locations: 31, 32, 33
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
## Warning: Too few values at 3 locations: 25, 26, 27
```

```
## Warning: Too few values at 1 locations: 11
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
## Warning: Too few values at 1 locations: 12
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
## Warning: Too few values at 5 locations: 18, 19, 20, 21, 22
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
## Warning: Too few values at 3 locations: 27, 28, 29
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
## Warning: Too few values at 3 locations: 27, 28, 29
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
## Warning: Too few values at 2 locations: 3, 17
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
## Warning: Too few values at 3 locations: 25, 26, 27
```

```
## Warning: Too few values at 3 locations: 27, 28, 29
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
## Warning: Too many values at 1 locations: 7
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
## Warning: Too few values at 1 locations: 3
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
## Warning: Too few values at 1 locations: 23
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
## Warning: Too few values at 3 locations: 25, 26, 27
```

```
## Warning: Too few values at 3 locations: 28, 29, 30
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
## Warning: Too many values at 1 locations: 22
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
## Warning: Too few values at 1 locations: 3
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
## Warning: Too few values at 3 locations: 4, 6, 7
```

```
## Warning: Too few values at 3 locations: 28, 29, 30
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
## Warning: Too few values at 3 locations: 3, 5, 7
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
## Warning: Too few values at 1 locations: 11
```

```
## Warning: Too few values at 3 locations: 4, 6, 7
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
## Warning: Too few values at 1 locations: 2
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
## Ncells  922291 49.3    1442291 77.1  1442291 77.1
## Vcells 1317231 10.1    2552219 19.5  2354728 18.0
```

```r
save(scores, teams, file=paste0("AllMenScores",format(Sys.time(),"%Y %m %d"),".Rdata"))
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
  team1=match(scores[i,]$Link1,teams$Links)
  team2=match(scores[i,]$Link2,teams$Links)
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

![](AllMenRanking_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
which(rowSums(A)==0)
```

```
## [1]   2  36  75 190 230
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
write.csv(rankedteams, paste("USAU All Men RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = TRUE)

rankedteams %>% filter(`Competition Level`== "Club") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Club Men RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

rankedteams %>% filter(`Competition Level`== "Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Masters Men RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)

rankedteams %>% filter(`Competition Level`== "Grand Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% write.csv( paste("USAU Grand Masters Men RW", format(Sys.time(),"%Y %m %d"),".csv",sep=""), row.names = FALSE)
```


```r
rankedteams %>% filter(`Competition Level`== "Club") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team                                         Wins   Losses       Rating  City                           State          
-----  ------------------------------------------  -----  -------  -----------  -----------------------------  ---------------
    1  Revolver                                       21        3   32.5001114  San Francisco                  CA             
    2  Sockeye                                        21        6   27.7986280  Seattle                        WA             
    3  Ring of Fire                                   23        3   17.3482709  Raleigh                        NC             
    4  Ironside                                       22        8   15.6538821  Boston                         MA             
    5  Truck Stop                                     20        5   13.6672304  Washington                     DC             
    6  HIGH FIVE                                      19        7   12.1391104  Ohio                           MI             
    7  Dig                                            24        5   11.6575207  Boston                         MA             
    8  Chicago Machine                                14       10   10.4733857  Chicago                        IL             
    9  Florida United                                 19        2    8.9722873  Florida                        FL             
   10  Johnny Bravo                                   16        8    8.9635756  Denver                         CO             
   11  GOAT                                           19        7    8.2863673  Toronto                        Ontario        
   12  PoNY                                           16       10    8.1699917  New York                       NY             
   13  Sub Zero                                       16       11    6.7667896  Minneapolis                    MN             
   14  Patrol                                         16       10    6.7359972  Philadelphia                   PA             
   15  SoCal Condors                                  17        7    6.1594807  Calico                         CA             
   16  Doublewide                                     13        6    5.4235299  Austin                         TX             
   17  Guerrilla                                      18       15    5.2840253  Oakland                        CA             
   18  Medicine Men                                   27       13    5.2724700  Baltimore                      MD             
   19  Rhino                                          17       11    5.0634711  Portland                       OR             
   20  Chain Lightning                                19       12    4.9257789  Atlanta                        GA             
   21  Pittsburgh Temper                              16       10    4.0229091  Pittsburgh                     PA             
   22  Inception                                      20        9    3.7830266  Denver                         CO             
   23  Freaks                                         28        6    3.4289872  Huntsville                     AL             
   24  Furious George                                 12       13    3.2160511  Vancouver                      BC             
   25  Madison Club                                   13       13    3.0792695  Madison                        WI             
   26  Dark Star                                      22        5    3.0512834  Eugene                         OR             
   27  Voodoo                                         20        5    2.9233911  Seattle                        WA             
   28  Richmond Floodwall                             24        9    2.4708714  Richmond                       VA             
   29  PowderHogs                                     13       10    2.4331966  Salt Lake City                 UT             
   30  Mad Men                                        28        8    2.3589582  Madison                        WI             
   31  CLE Smokestack                                 21       12    2.1483959  Cleveland                      OH             
   32  Cash Crop                                      17        7    2.0496549  Triangle                       NC             
   33  Garden State Ultimate                          28       10    1.9715133  Princeton                      NJ             
   34  Brickyard                                      28        6    1.9201726  Indianapolis                   IN             
   35  Sprawl                                         21        5    1.7750481  Phoenix                        AZ             
   36  Ghost Train                                    14       17    1.6770757  Seattle                        WA             
   37  Turbine                                        20       13    1.5055074  Durham                         NC             
   38  Prairie Fire                                   11       16    1.4872553  Kansas City                    KS             
   39  Illusion                                       27        8    1.3004327  Iowa City                      IA             
   40  Ironmen                                        22       13    1.2366194  Birmingham                     AL             
   41  Black Market                                   29       11    1.1711941  Lombard                        IL             
   42  Lost Boys                                      19       12    1.1435108  South Carolina                 SC             
   43  CUS Bologna                                     1        6    0.9695531  Bologna                        Emilia Romagna 
   44  Streetgang                                     17       10    0.9681854  San Diego                      CA             
   45  Mockingbird                                    12        3    0.9221224  Montréal                       QC             
   46  Dreadnought                                    13        9    0.9166412  Fayetteville                   AR             
   47  Singlewide                                     18        3    0.9101122  Austin                         TX             
   48  Colt                                           24        7    0.9085653  Southington                    CT             
   49  Republic                                       12       14    0.8817295  Los Angeles                    CA             
   50  Clapham Ultimate                                2        5    0.8727534  London                         UK             
   51  Sawtooth                                       15       11    0.8110047  Boise                          ID             
   52  Pesterbug                                      19       13    0.7630828  Pawtucket                      RI             
   53  CITYWIDE Special                               17       11    0.7352412  Philadelphia                   PA             
   54  MKE                                            26       10    0.7241401  Milwaukee                      WI             
   55  Big Wrench                                     14       20    0.7166319  Somerville                     MA             
   56  John Doe                                       13       12    0.7152420  Washington                     DC             
   57  General Strike                                 13        8    0.6993739  Winnipeg                       Manitoba       
   58  Beachfront Property                            21       21    0.6753565  Chicago                        IL             
   59  Sundowners                                     15       10    0.6702538  Santa Barbara                  CA             
   60  Rip City Ultimate                              13       19    0.6469904  Portland                       OR             
   61  Blueprint                                      24       14    0.6461421  NYC                            NY             
   62  Bullet                                         15       20    0.6172599  Atlanta                        GA             
   63  Dallas United: Desperados                      13       11    0.6094882  Dallas                         TX             
   64  Green River Swordfish                          13       12    0.6083505  Davis                          CA             
   65  The Killjoys                                   10        9    0.5891889  Orem                           UT             
   66  Haymaker                                       20       15    0.5790789  Chicago                        IL             
   67  Choice City Hops                               13       10    0.5695225  Fort Collins                   CO             
   68  KC SmokeStack                                  11       19    0.5243464  Kansas City                    KS             
   69  Kentucky Flying Circus                         26       14    0.4924075  Louisville                     KY             
   70  Blade                                           5       15    0.4774340  Nashville                      TN             
   71  Oakgrove Boys                                  15       11    0.4752112  ARLINGTON                      VA             
   72  Plex                                           14       12    0.4694148  Dallas                         TX             
   73  Rougaroux                                      14       12    0.4595340  Baton Rouge/New Orleans        LA             
   74  Ham                                            13       10    0.4486044  Bellingham                     WA             
   75  Enigma                                         18       21    0.4189590  Dayton                         OH             
   76  Rush Hour                                      17       16    0.4136170  Atlanta                        GA             
   77  Imperial                                       14       15    0.4112630  Saint Paul                     MN             
   78  Inside Rakete                                   0        7    0.3907673  Darmstadt                      Hesse          
   79  Scythe                                         14       16    0.3817708  Omaha                          NE             
   80  CaSTLe                                         13       17    0.3775646  St. Louis                      MO             
   81  Boston Baked Beans                              8        7    0.3590923  Boston                         MA             
   82  Red Circus                                      5       13    0.3555883  Halifax                        Nova Scotia    
   83  Dallas United: Outlaws                         11       16    0.3541549  Dallas                         TX             
   84  Climax                                         11       18    0.3509772  Minneapolis                    MN             
   85  Deathsquad                                     18       15    0.3481681  Needham                        MA             
   86  Shade                                          17       13    0.3294026  New York                       NY             
   87  Four                                           15       12    0.3286554  Ann Arbor                      MI             
   88  Schweingeist                                   19       15    0.3204573  Cincinnati                     OH             
   89  Club M - Manic                                 13        8    0.3192736  Montreal                       quebec         
   90  Cerberus                                       15       16    0.3128547  Athens                         GA             
   91  Bruises                                        15        9    0.3009508  Boston                         MA             
   92  Chattanooga Gentlemen's Club                   12       18    0.2837940  Chattanooga                    TN             
   93  Brickhouse                                      7        7    0.2769786  Durham                         NC             
   94  Phoenix                                         7        6    0.2691449  Ottawa, ON                     Ontario        
   95  Riverside Ultimate                             14        5    0.2679917  Austin                         TX             
   96  JAWN                                           15       17    0.2564740  Philadelphia                   PA             
   97  Omen                                           21       12    0.2513610  Orlando                        FL             
   98  South Shore Line                               16       12    0.2463014  Chicago                        IL             
   99  T.R.W.F.B.                                      4        1    0.2450845  Seattle                        WA             
  100  The Roofing Co.                                13        9    0.2396562  Indianapolis                   IN             
  101  Right Coast                                     3        5    0.2254780  Wilmington                     NC             
  102  DTH                                            10       13    0.2194091  Houston                        TX             
  103  Syndicate                                       6       10    0.2165532  Colorado Springs               CO             
  104  BaNC                                           14       14    0.2143188  Charlotte                      NC             
  105  Papa Bear                                      10       14    0.2141452  Houston                        TX             
  106  Red Tide                                       13       12    0.2100751  Portland                       ME             
  107  Premium                                        11       10    0.2091095  Beaumont                       TX             
  108  Westchester Magma Bears                        16       17    0.2056295  Yorktown Heights               NY             
  109  Wisconsin Hops                                 10       12    0.2040907  Stevens Point                  WI             
  110  ISO Atmo                                        7       21    0.2026953  Boulder                        CO             
  111  Supercell                                      10       15    0.2021162  Oklahoma City                  OK             
  112  Breakers                                        9       15    0.1988996  Providence                     RI             
  113  Memphis Belle                                  10       11    0.1866209  Memphis                        TN             
  114  Black Market X                                  6        0    0.1854801  Lombard                        IL             
  115  Thundersnow                                     6       14    0.1832838  Flagstaff                      AZ             
  116  Holy City Heathens                              8       12    0.1653381  Charleston                     SC             
  117  Rampage                                         5       10    0.1536461  Huntsville                     AL             
  118  Mango Tree                                     13       13    0.1438666  Ann Arbor                      MI             
  119  War Machine                                    11       21    0.1387025  Birmingham                     AL             
  120  Blackfish                                       5        6    0.1381388  Vancouver                      BC             
  121  Chimney                                        14       18    0.1292775  Cleveland                      OH             
  122  DNA                                             4        2    0.1286171  Surrey                         BC             
  123  Union                                           6       18    0.1227011  Seattle                        WA             
  124  Refinery                                        4        4    0.1224081  Vancouver                      BC             
  125  Black Lung                                      9       18    0.1147519  Lexington                      KY             
  126  SURE                                            8       10    0.1107174  Seattle                        WA             
  127  Rawhide                                         7       13    0.1044233  Tulsa                          OK             
  128  Spring Creek Ascension                         10        7    0.1016300  Tomball                        TX             
  129  DeMo                                            9       11    0.0968752  Des Moines                     IA             
  130  SA Spares                                       5        9    0.0958884  San Antonio                    TX             
  131  Flicky Stardust and the Cutters From Mars       2        5    0.0865715  Eugene                         OR             
  132  Grand Trunk                                     2        4    0.0857327  Toronto                        ON             
  133  Dredge                                         11       11    0.0813248  Rhinebeck                      NY             
  134  H.O.G. Ultimate                                 6       20    0.0779672  Macon                          GA             
  135  Slag Dump                                      12       14    0.0776854  Pittsburgh                     PA             
  136  houSE                                           8       14    0.0769078  Kenosha                        WI             
  137  The Bucket Brigade                              4        9    0.0751622  Hot Springs                    AR             
  138  Adelphos                                       15       12    0.0739066  Philadelphia                   PA             
  139  Dallas United: Neato Banditos                   5       14    0.0716163  Dallas                         TX             
  140  Satellite                                      11        9    0.0703073  Normal                         IL             
  141  Shockoe Slip Flatball Club                     10        8    0.0701265                                                
  142  Xavier's School for Gifted Youngsters          11        8    0.0701020  Syracuse                       NY             
  143  Watchdogs                                      12       13    0.0680861  Bethesda                       MD             
  144  Fénix Ultimate                                  4        8    0.0636618  Mexico City                    Mexico City    
  145  Low Five                                        8       17    0.0628398  Columbus                       OH             
  146  Dragon Army                                     2        3    0.0627884  Seattle                        WA             
  147  UpRoar                                         18        7    0.0579230  Tampa Bay                      FL             
  148  Somerville BAG                                  8       12    0.0576557  Somerville                     MA             
  149  Midnight Meat Train                             5       13    0.0557146  Grand Rapids                   MI             
  150  Connecticut Connvicts                          11       16    0.0537138  Manchester                     CT             
  151  Riverside Messengers-B                          4       11    0.0523825  Austin                         TX             
  152  Auxiliary                                       6       13    0.0522393  Quad Cities                    IA             
  153  Shades                                          5        3    0.0511217  Northampton                    MA             
  154  Deaf Fruit                                      6       10    0.0490975  Lubbock                        TX             
  155  Fat and Mediocre                                4        8    0.0484783  Brooklyn                       NY             
  156  Cloverleaf                                      1       10    0.0455721  Tacoma                         WA             
  157  Flash Flood                                     3        3    0.0444462  Flower Mound                   TX             
  158  Madd Dogg                                       1        8    0.0408971  Provo                          UT             
  159  THE BODY                                        8        6    0.0407112  Minneapolis                    MN             
  160  AK Pipeline                                     6       19    0.0389879  Akron                          OH             
  161  Coastal Empire                                  4        3    0.0345050  Savannah                       GA             
  162  AndyGator                                       1        7    0.0292828  Baton Rouge                    LA             
  163  Interesting Tummy Birds                         1        3    0.0285776  Fort Collins                   CO             
  164  Sons                                            2        5    0.0258300  Palo Alto                      CA             
  165  Cream City Crooks                               4       22    0.0227523  Milwaukee                      WI             
  166  Swangos                                         1        3    0.0225629  Austin                         TX             
  167  Sherbrooke Gentlemen's Club                     4        3    0.0215839  Sherbrooke                     QC             
  168  spokane flatline                                1        9    0.0199250  spokane                        WA             
  169  Baemaker                                        9       11    0.0192630  Chicago                        IL             
  170  Pittsburgh Puppies                              4        4    0.0192198  Pittsburgh                     PA             
  171  Helots                                          8       16    0.0192166  Sparta                         NJ             
  172  Munch Box                                       6        7    0.0186395  Fairfax                        VA             
  173  Dirty D                                         7       15    0.0163986  Greater Detroit                MI             
  174  Black Market Y                                  1        4    0.0161581  Lombard                        IL             
  175  DingWop                                         7        7    0.0148925  Duluth                         MN             
  176  Club M - Rage                                   2        5    0.0143616  Montreal                       Quebec         
  177  Black Market II                                 5       18    0.0142377  Lombard                        IL             
  178  Transatlantic Danger Zone                       0        4    0.0131631  CIncinnati                     OH             
  179  Garden Party                                   10       17    0.0130990  New Brunswick                  NJ             
  180  Shrike                                          4        1    0.0109416  Ottawa                         ON             
  181  Shrimp Boat                                     5        9    0.0104996  TALLAHASSEE                    FL             
  182  Solar Eclipse                                   4       10    0.0094336  Lexington                      KY             
  183  Journeymen                                      3        3    0.0079571  Sunnyvale                      CA             
  184  El Niño                                        11        6    0.0076266  Oakland Park                   FL             
  185  Bear Proof                                      5       14    0.0070862  Harrisburg                     PA             
  186  Black Knights                                   3        4    0.0069073  West Point                     NY             
  187  Green Bay Quackers                              3        4    0.0066725  Appleton-Oshkosh-Green Bay     WI             
  188  Barefoot                                        1        4    0.0064751  Tupelo                         MS             
  189  Red Tide Black                                  1        5    0.0064737  Portland                       ME             
  190  Mutiny                                          4       11    0.0064087  Burlington                     VT             
  191  Black Penguins Club                             6        9    0.0063479  Bourbonnais                    IL             
  192  Gargantuan Spider Dragons-Senior                1        4    0.0062558  Lewiston                       ME             
  193  Bomb Squad                                      1        9    0.0054205  Bel Air                        MD             
  194  Gridlock                                        2       10    0.0041981  Los Angeles                    CA             
  195  Gulls                                           1        5    0.0041617  Berkeley                       CA             
  196  Defy the Meta                                   0        3    0.0041175  Amherst                        MA             
  197  Winc City Fog of War                            5       14    0.0039059  Winchester                     VA             
  198  Spirit Quest                                    1        5    0.0036789  Salt Lake City - Salt Lake     UT             
  199  Hippie Mafia                                    6       13    0.0036612  Milwaukee                      WI             
  200  Big Sky                                         2        4    0.0030173  Salt Lake City                 UT             
  201  Barden Btate Bultimate                          2        5    0.0027477  Maplewood                      NJ             
  202  The Silent Flatballers                          2        3    0.0027058  New York                       NY             
  203  Brawl                                           1       12    0.0026327  Tempe                          AZ             
  204  Ultimate Impact                                 2        4    0.0026302  San Francisco                  CA             
  205  Club M - Magma                                  4       14    0.0025962  Montreal                       Québec         
  206  Hammerhead                                      5       16    0.0025901  Jacksonville                   FL             
  207  East Rock Candy Mountain                        3       14    0.0025608  New Haven                      CT             
  208  Mississippi Valley Tundra Swans                 3        4    0.0022333  Winona                         MN             
  209  First Order                                     3        9    0.0021349  Roanoke                        VA             
  210  AOL (All Outta Love)                            1        6    0.0020187  Philadephia                    PA             
  211  Snip Snip                                       1        6    0.0018311  Northfield                     MN             
  212  CerB WerB-B                                     2        5    0.0017389  Athens                         GA             
  213  Vicious Cycle                                   5        9    0.0010756  Gainesville                    FL             
  214  Jelly Straus                                    2        6    0.0009907  Williamsburg                   VA             
  215  Space Coast Ultimate                            3       13    0.0009193  Melbourne                      FL             
  216  Path of Totality                                1        6    0.0008997  Bethlehem                      PA             
  217  Tyranny                                         2       14    0.0008672  Tampa                          FL             
  218  The Boys                                        0        8    0.0008121  Cincinnati                     OH             
  219  Honey Bashfords                                 0        6    0.0006468  Chapel Hill                    NC             
  220  Vortex                                          1        6    0.0004522  Chicago                        IL             
  221  Nasty Girls                                     0        7    0.0003197  Champaign                      IL             
  222  Rogue Grimace                                   0        6    0.0002539  Evanston                       IL             
  223  Club M - Inferno                                0        6    0.0001115  Montreal                       Québec         
  224  Los Beeros                                      2        5    0.0001033  Bloomington                    IN             
  225  "Parkland" PLU Alumni                           0        0    0.0000000  Parkland                       WA             
  225  Allen's Army                                    0        6    0.0000000  Orono                          ME             
  225  Black Penguins Alumni                           0        0    0.0000000  Bourbonnais                    IL             
  225  Buffalo Ultimate                                2        4    0.0000000  Buffalo                        NY             
  225  Club Test Team                                  0        0    0.0000000  Boulder                        CO             
  225  EXIT 54                                         0        4    0.0000000  Smithtown                      NY             
  225  HB Boys                                         0        7    0.0000000  Arlington                      VA             
  225  Marxmen                                         0        5    0.0000000  Clifton                        NJ             
  225  MaverNads                                       2        4    0.0000000  Kitchener-Waterloo-North Bay   Ontario        
  225  Miami Alumni                                    0        0    0.0000000  Oxford                         OH             
  225  Reaper Alum                                     0        0    0.0000000  Charleston                     IL             
  225  SPACE INVADERS                                  2        5    0.0000000  Fredericksburg                 VA             
  225  Stuyvesant Sticky Fingers                       0        5    0.0000000  New York City                  NY             
  225  Syracuse Doom                                   0        6    0.0000000  Syracuse                       NY             
  225  Team Rocket                                     2        4    0.0000000  Katonah                        NY             

```r
rankedteams %>% filter(`Competition Level`== "Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team                      Wins   Losses      Rating  City                          State    
-----  -----------------------  -----  -------  ----------  ----------------------------  ---------
    1  Johnny Encore               14        2   1.3561541  Denver                        CO       
    2  Rest Stop                    6        2   0.3984928  Baltimore                     MD       
    3  Voltron 2020                 6        2   0.3923582  Seattle                       WA       
    4  Boneyard                     6        1   0.3795538  Raleigh-Durham                NC       
    5  Black Cans & Highlands       5        4   0.2382284  Washington                    DC       
    6  All Bashed Out               6        3   0.1743212  Albany "Cap City Bang Bang"   NY       
    7  Surly                       11        2   0.1664359  MINNEAPOLIS                   MN       
    8  Crawl                        6        3   0.0399884  Phoenix                       AZ       
    9  Horse                        1        1   0.0138872  New York                      NY       
   10  Gravy Train Wreck            1        6   0.0062171  Seattle                       WA       
   11  Pacemaker                    9        3   0.0058650  Chicago                       IL       
   12  Get Off My Lawn              0        2   0.0036545  Burlington                    VT       
   13  Beyondors                    1        1   0.0024882  Santa Barbara                 CA       
   14  Surly Cynic                  9        6   0.0009793  Minneapolis                   MN       
   15  Naptown                      6        6   0.0005474  Indianapolis                  IN       
   16  s.u.r.l.y. BURNSIDE          0        2   0.0004984  Gresham                       OR       
   17  Geronimo                     7        6   0.0003919  Dallas/Fort Worth             TX       
   18  Young OG's                   0        2   0.0003555  San Diego                     CA       
   19  (burnside) TIPP CITY         2        5   0.0003167  Portland                      OR       
   20  Royal Stag                   5        8   0.0001511  Madison                       WI       
   21  Winnipeg                     4        4   0.0000834  Winnipeg                      Manitoba 
   22  Outlaw                       4        9   0.0000635  Fayetteville                  AR       
   23  Polar Vortex                 2        9   0.0000389  Chicago                       IL       
   24  Xtra Surly                   0        6   0.0000320  Minneapolis                   MN       
   25  FoG                          0        5   0.0000016  Cincinnati                    OH       
   26  Clippers                     0        2   0.0000000  Baltimore                     MD       
   26  curmudgeon                   2        4   0.0000000  Houston                       TX       
   26  HOV Violators                1        1   0.0000000  Arlington                     VA       
   26  Mr Miyagi                    0        5   0.0000000  Arlington                     TX       
   26  Woolly Mammoth               0        1   0.0000000  Gainesville                   FL       

```r
rankedteams %>% filter(`Competition Level`== "Grand Masters") %>% mutate(Rank=dense_rank(desc(Rating))) %>% select(Rank, Team, Wins, Losses, Rating, City, State) %>% knitr::kable()
```



 Rank  Team                                Wins   Losses      Rating  City              State            
-----  ---------------------------------  -----  -------  ----------  ----------------  -----------------
    1  Surly                                  6        0   0.3237563  Minneapolis       MN               
    2  Johnny Walker                         15        2   0.3213045  Denver/Boulder    CO               
    3  No Country                            15        2   0.1324505  Brattleboro       VT               
    4  Shadows                                9        2   0.1059148  Oakland           CA               
    5  Endless Sunset                        10        8   0.0861944  San Diego         CA               
    6  Reckon                                 9        3   0.0500343  GA/TN/etc         GA               
    7  ShutDown                               5        2   0.0290209  Washington        DC               
    8  Kalakala                               9        7   0.0251788  Seattle           WA               
    9  Hootenanny                            11        9   0.0236209  Raleigh           NC               
   10  Old Man Winter                         7        4   0.0184462  chicago           IL               
   11  Bighorn                                7        5   0.0148624  Boulder           CO               
   12  projectile dysfunction team HELM       6        5   0.0125467  columbus          OH               
   13  Critical Mass                          3        8   0.0048791  Northampton       MA               
   14  GrandMaster Flash                      8       13   0.0035688  Orlando           FL               
   15  Sick Hammers                           3        7   0.0025808  Austin            TX               
   16  DinoSlam                               1        1   0.0022493  Portland          OR               
   17  Moscow State                           2        3   0.0020636  New York          NY               
   18  Age Against the Machine                2        3   0.0019017  Cincinnati        OH               
   19  Borderline                             4        4   0.0017636  Maritimes         Nova Scotia      
   20  Rust                                   2        4   0.0009872  Birmingham        AL               
   21  Goatswitcher                           0        5   0.0009208  Montgomery        AL               
   22  Old Growth (OG)                        2       10   0.0004846  Silicon Valley    CA               
   23  Brooklyn                               0        4   0.0001977  Brooklyn, USA     NY               
   24  Grandmasters of the Universe           1        5   0.0000149  Winnipeg          Manitoba         
   25  Big D N R                              0        4   0.0000000  Dallas            TX               
   25  Cal Grand Masters                      0        5   0.0000000  San Diego         CA               
   25  Legion                                 0        2   0.0000000  Vancouver         British Columbia 
   25  Rust Belt                              0        4   0.0000000  Greater Detroit   MI               


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
division="Men"
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

![](AllMenRanking_files/figure-html/plotNetwork-1.png)<!-- -->
