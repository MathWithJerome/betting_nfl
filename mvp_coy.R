library(nflreadr)
library(nflfastR)
library(rvest)
library(dplyr)
library(ggplot2)

#scheds = load_schedules(T)

pfr.team = c("crd","atl","rav","car","buf","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","rai",
             "sdg","ram","mia","min","nwe","nor","nyg","nyj","phi","pit","sfo","sea","tam","oti","was")
team.records.l = list()
for (team in pfr.team) {
  url = paste0("https://www.pro-football-reference.com/teams/",team,"/index.htm")
  tabs = read_html(url) %>% html_element("table") %>% html_table()
  print(team)
  print(dim(tabs))
  names(tabs) = unlist(tabs[1,])
  tabs = tabs[-1,]
  tabs = tabs[which(!tabs$Year %in% c("","Year")),]
  team.records.l[[team]] = tabs#rbind(team.records,tabs)
}
team.records = do.call(rbind,team.records.l)
team.records[,c(1,4:6,9:11,17:length(team.records))] = apply(team.records[,c(1,4:6,9:11,17:length(team.records))],2,as.numeric)
names(team.records)[17:20] = c("OffPts","OffYds","DefPts","DefYds")
team.records = team.records %>%
  filter(Year <= 2021) %>% 
  mutate(Tm = gsub("\\*","",Tm))
  
team_names_pfr = read_csv("team_names_pfr.csv")
nfl_coy = read_csv("nfl_coy.csv", skip = 1)
nfl_mvp = read_csv("nfl_mvp.csv")

#team.records = team.records %>% left_join(team_names_pfr %>% select(Tm,Tm2) %>% distinct())
team.records = team.records %>% left_join(nfl_mvp, by = c("Tm" = "Team", "Year" = "Season"))
anti_join(nfl_mvp,team.records, by = c("Team" = "Tm", "Season" = "Year"))

team.records %>% filter(!is.na(Player)) %>% View()

mvp.votes = opoy.votes = dpoy.votes = oroy.votes = droy.votes = coy.votes = data.frame()
for (y in 1970:2021) {
  url = paste0("https://www.pro-football-reference.com/awards/awards_",y,".htm")
  caps = read_html(url) %>% html_elements("caption") %>% html_text()
  tabs = read_html(url) %>% html_elements("table") %>% html_table()
  names(tabs) = caps
  t.mvp = tabs[[which.min(grep("mvp",caps, ignore.case = T))]]
  names(t.mvp) = ifelse(names(t.mvp)=="", unlist(t.mvp[1,]), paste0(names(t.mvp),unlist(t.mvp[1,])))
  t.mvp = t.mvp[-1,]
  t.mvp$Year = y
  mvp.votes = bind_rows(mvp.votes,t.mvp)
  t.coy = tabs[[min(grep("coach",caps, ignore.case = T))]]
  names(t.coy) = ifelse(names(t.coy)=="", unlist(t.coy[1,]), paste0(names(t.coy),unlist(t.coy[1,])))
  t.coy = t.coy[-1,]
  t.coy$Year = y
  coy.votes = bind_rows(coy.votes,t.coy)
}

mvp.votes$Share = gsub("\\%","",mvp.votes$Share)
mvp.votes[,c(1,5:length(mvp.votes))] = apply(mvp.votes[,c(1,5:length(mvp.votes))], 2, as.numeric)
mvp.votes = mvp.votes %>% group_by(Player,Pos) %>% 
  mutate(prev.votes = cumsum(Votes)-Votes)

mvp = mvp.votes %>% 
  left_join(team.records %>% select(Year, Tm, `Div. Finish`, W, OffPts, DefPts, SoS, `T/G`,`Pts±`,`Yds±`), 
            by = c("Tm" = "Tm", "Year" = "Year")) %>% 
  #filter(Rk <= 2) %>% 
  arrange(-Year)

mvp1 = mvp %>% filter(Year > 2010, Rk==1)
mvp2 = mvp %>% filter(Year > 2010, Rk==2)
mvp.1v2 = mvp1 %>% left_join(mvp2, by = "Year", suffic = c(".1",".2"))

players.w.votes = mvp.odds$player[which(mvp.odds$player %in% mvp.votes$Player)]
mvp.votes %>% #filter(Player %in% players.w.votes) %>% 
  group_by(Player) %>% 
  summarise(total.votes = sum(Votes)) %>% 
  arrange(-total.votes)

vote.win.diff = mvp.votes %>% group_by(Player,Pos) %>% 
  summarise(first.win = min(Year[which(Rk==1)]), 
            first.vote = min(Year[which(Votes>0)]), 
            Wins = sum(Rk==1),
            Rk = Rk) %>% 
  filter(Rk==1) %>%
  distinct() %>% 
  mutate(diff = first.win - first.vote)

coy.votes$Share = gsub("\\%","",coy.votes$Share)
coy.votes[,c(1,4:12)] = apply(coy.votes[,c(1,4:12)], 2, as.numeric)
coy.votes = coy.votes %>% group_by(Coach) %>% 
  mutate(prev.votes = cumsum(Votes)-Votes)

coy = coy.votes %>% left_join(team.records %>% select(Year, Tm, `Div. Finish`, OffPts, DefPts, SoS, `T/G`,`Pts±`,`Yds±`), 
                              by = c("Tm" = "Tm", "Year" = "Year")) %>% 
  filter(Rk <= 2) %>% 
  arrange(-Year)

coaches.w.votes = coy.odds$coach[which(coy.odds$coach %in% coy.votes$Coach)]
coaches.wo.votes = coy.odds$coach[which(!coy.odds$coach %in% coy.votes$Coach)]
ggplot(data = mvp.votes %>% filter(Pos %in% c("QB","RB","WR")), aes(x = Year, y = Share, color = Pos)) + geom_line() + geom_point()
