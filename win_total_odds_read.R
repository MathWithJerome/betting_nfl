library(rvest)
library(dplyr)

win.totals.l = list()
for (y in 1989:2022) {
  url = read_html(paste0("https://www.sportsoddshistory.com/nfl-win/?y=",y,"&sa=nfl&t=win&o=t"))
  tab = url %>% html_node(xpath = '//*[@id="content"]/div/table') %>% html_table()
  tab$season = y
  win.totals.l[[as.character(y)]] = tab
}

win.total = do.call(rbind,win.totals.l) %>% 
  mutate(`Over Odds` = as.numeric(gsub("--","-",`Over Odds`)),
         `Under Odds` = as.numeric(`Under Odds`))
         #over.imp = implied.prob(`Over Odds`),
         #under.imp = implied.prob(`Under Odds`))


coy2000 = coy %>% filter(Year > 1999) %>% left_join(win.total, by = c("Tm" = "Team", "Year" = "season"))
coy2000[,c()]
coy2000 = coy2000 %>% mutate(ActualWins - `Win Total`)


win.total = win.total %>% left_join(team_map, by = c("Team" = "V1")) %>% rename("team" = "V2") %>% #select(-Team) %>% 
  select(team, everything())
win.total = win.total[complete.cases(win.total),]
avg_pt_diff_next_wintotal = merge(avg_pt_diff_next, win.total, by = c("team","season"))
#avg_pt_diff_wintotal = merge(avg_pt_diff_next, win.total, by = c("team","season"))
avg_pt_diff_wintotaly = merge(avg_pt_diff[season==2021,], fd.wins[,c("team","wins","over.imp")], by = "team", suffixes = c("",".total"))
#avg_pt_diff_next_wintotal = merge(avg_pt_diff_next, win.total, by = c("team","season"))

merge(avg_pt_diff, avg_pt_diff[,c("team","season","wins")], by.x = c("team","next_season"), by.y = c("team","season"), suffixes = c("","_next"))
