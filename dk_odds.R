library(rvest)
library(stringr)

dk.url = "https://sportsbook.draftkings.com/leagues/football/88670561?category=game-lines&subcategory=game"
dk.read = read_html(dk.url)
xp = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div[1]/div/div[2]/div/div[2]'

season.lines = (dk.read %>% 
  html_elements(xpath = xp) %>% 
  html_table())[[1]]

season.lines$spread.odds = str_extract(season.lines$SPREAD,"[+-][0-9]{3}")
season.lines$total.odds = str_extract(season.lines$TOTAL,"[+-][0-9]{3}")

season.lines$SPREAD = gsub("[+-][0-9]{3}", "", season.lines$SPREAD)
season.lines$TOTAL = gsub("[+-][0-9]{3}", "", season.lines$TOTAL)

season.lines = season.lines[which(!is.na(season.lines$spread.odds)),]

season.lines$total.side = substring(season.lines$TOTAL, 1, 1)
season.lines$TOTAL = str_extract(season.lines$TOTAL,"[\\.0-9]+")
