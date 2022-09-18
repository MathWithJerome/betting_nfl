normalize = function(x) {
  norm = (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
  return(norm)
}

cos.sim <- function(A, B) {
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   

library(stringr)
library(rvest)
library(dplyr)
url.l = "https://www.pro-football-reference.com/play-index/record_finder.cgi?request=1&year_min=1956&year_max=2020&losses=1" 
url.t = "https://www.pro-football-reference.com/play-index/record_finder.cgi?request=1&year_min=1956&year_max=2020&ties=1" 
url.w = "https://www.pro-football-reference.com/play-index/record_finder.cgi?request=1&year_min=1956&year_max=2020&wins=1" 
#"
records = bind_rows(read_html(url.w) %>% html_node("table") %>% html_table(),
                    read_html(url.l) %>% html_node("table") %>% html_table(),
                    read_html(url.t) %>% html_node("table") %>% html_table())
records$last.season = records$Year - 1
records = merge(records, records, by.x = c("Tm","last.season"), by.y = c("Tm","Year"), all.x = T, suffixes = c("",".last"))
records$last.season.last = NULL
# records = records %>% 
#   mutate(W.p = W / (W + T + L),
#          W.change = W - W.last,
#          W.p.last = W.last / (W.last + T.last + L.last),
#          W.p.change = W.p - W.p.last,
#          playoff.id = factor(ifelse(`Playoff Result` %in% c("",NA), "No","Yes")),
#          playoff.last.id = factor(ifelse(`Playoff Result.last` %in% c("",NA), "No","Yes"))) 

names.tmp = word(records$Tm, 1, 2)
split.id = sapply(strsplit(records$Tm, " "), length)
names.tmp = data.frame(city = ifelse(split.id > 2, word(records$Tm, 1, 2), word(records$Tm, 1)), 
                       team.name = ifelse(split.id > 2, word(records$Tm, 3), word(records$Tm, 2))) 
records = cbind(records, names.tmp)

#mvp <- read_csv("nfl_mvp.txt")
#mvp = bind_rows(read_html("https://en.wikipedia.org/wiki/Associated_Press_NFL_Most_Valuable_Player_Award") %>% html_nodes("table") %>% .[2:3] %>% html_table(fill = T))
#mvp = mvp[which(!grepl("None awarded", mvp$Player)),]
library(readr)
library(ggplot2)
mvp <- read_csv("nfl_mvp.csv")
mvp$Player = gsub("[^A-Z \\. a-z]+", "", mvp$Player)
names.tmp = word(mvp$Team, 1, 2)
split.id = sapply(strsplit(mvp$Team, " "), length)
names.tmp = data.frame(city = ifelse(split.id > 2, word(mvp$Team, 1, 2), word(mvp$Team, 1)), 
                       team.name = ifelse(split.id > 2, word(mvp$Team, 3), word(mvp$Team, 2))) 
mvp = cbind(mvp,names.tmp)
mvp$city = NULL

mvp = merge(mvp, records, by.x = c("Season", "team.name"), by.y = c("Year","team.name"), all.y = T, suffixes = c("",".last"))
# mvp = merge(mvp[,1:5], records, by.x = c("Season", "Team"), by.y = c("Year","Tm"), all.x = T)
# mvp = merge(mvp, records, by.x = c("last.season", "Team"), by.y = c("Year","Tm"), all.x = T, suffixes = c("",".last"))
mvp.r.o = mvp %>%
  mutate(W.p = 100*(W + .5*T) / (W + T + L),
         W.change = W - W.last,
         W.p.last = 100*(W.last + T.last) / (W.last + T.last + L.last),
         W.p.change = W.p - W.p.last,
         playoff.id = factor(ifelse(`Playoff Result`=="", "No","Yes")),
         playoff.last.id = factor(ifelse(`Playoff Result.last`=="", "No","Yes")),
         qb.id = factor(ifelse(Position=="Quarterback", "QB","non-QB")),
         mvp.id = factor(ifelse(is.na(Player), "No", "Yes"))) %>%
  group_by(Season) %>%
  mutate(W.rank = rank(-W.p, ties.method = "min"),
         W.rank.01 = 100*W.rank/32,
         W.rank.last = rank(-W.p.last, ties.method = "min"),
         #W.rank.last.01 = W.rank.last/32,
         W.p.change.rank = rank(-W.p.change, ties.method = "min"), 
         W.rank.change = W.rank.last - W.rank, 
         W.last.bin = cut(W.last, breaks = c(0,9,10,11,13,16)), 
         W.rank.last.bin = cut(W.rank.last, breaks = c(0,1,2,7,9,32)), 
         W.bin = cut(W, breaks = c(0,11,13,16))) %>% #W.rank.bin = cut(W.rank.last, breaks = c(0,1,2,7,9,32))
  filter(Season >= 1978 & !is.na(W.last))

mvp.r = mvp.r.o[which(mvp.r.o$W >= 9),]
#[which(mvp.r$mvp.id=="Yes"),]
ggplot(data = mvp.r, aes(x = W.rank.last, fill = mvp.id)) + geom_histogram(bins = 20) + facet_wrap(mvp.id~., scales = "free_y")
library(Ckmeans.1d.dp)
mvp.r$km.W.last = "0"
mvp.r$km.W.last[which(mvp.r$W.last >= 9)] = Ckmeans.1d.dp(mvp.r$W.last[which(mvp.r$W.last >= 9)], k = c(3))$cluster #, "W.p.last"
ggplot(data = mvp.r, aes(x = W.last, fill = km.W.last)) + geom_histogram(bins = 16) #+ facet_wrap(mvp.id~., scales = "free_y")
ggplot(data = mvp.r, aes(x = W.last, y = mvp.id)) + geom_jitter()

mvp.r.o$km.W.last = "0"
mvp.r.o$km.W.last[which(mvp.r.o$W.last >= 9)] = Ckmeans.1d.dp(mvp.r.o$W.last[which(mvp.r.o$W.last >= 9)], k = c(3))$cluster #, "W.p.last"


library(rpart); library(rpart.plot); library(caret)
W.last.rpart = list()
for (i in 1:1000) {
  tmp.rpart = rpart(data = mvp.r[c(which(mvp.r$mvp.id=="Yes"), sample(x = which(mvp.r$mvp.id=="No"), size = .12*nrow(mvp.r))),], 
                    mvp.id ~ W.last, control = rpart.control(xval = 10, cp = .01, minsplit = 5, minbucket = 5)) 
  W.last.rpart[[i]] = as.character(predict(tmp.rpart, data.frame(W.last = 1:16), type = "class"))
}
last.rpart = do.call(rbind, W.last.rpart)
data.frame(W.last = 1:16, Yes = apply(last.rpart, 2, function(x) {mean(x == "Yes")}))

#rpart.plot(W.last.rpart)

mvp.lm = lm(data = mvp.r[which(mvp.r$mvp.id=="Yes"),], W ~ (W.last + W.p.change.rank + playoff.last.id)*km.W.last - 1)
mvp.lm.step = step(mvp.lm, direction = "backward")
summary(mvp.lm)

summary(mvp.lm.step)
library(randomForest)
mvp.rf = randomForest(data = mvp.r[which(mvp.r$mvp.id=="Yes"),],  W.p ~ W.p.last + W.p.change.rank * playoff.last.id)
mvp.rf$importance

mean.mvp.W = mean(mvp.r$W.p[which(mvp.r$mvp.id=="Yes")])
x.mvp = mvp.r.o[which(mvp.r.o$Season==2020),]
x.mvp$W.p.last = x.mvp$W.p
x.mvp$W.last.rank.01 = x.mvp$W.rank.01
x.mvp$playoff.last.id = x.mvp$playoff.id
x.mvp$playoff.id = x.mvp$W.rank = x.mvp$W = NULL
x.mvp$mvp.id = "Yes"
x.mvp$W.last = 16*x.mvp$W.p.last/100
x.mvp$pred.mvp.lm = predict(mvp.lm, x.mvp)
x.mvp$pred.mvp.rf = predict(mvp.rf, x.mvp)
x.mvp$W.needed.lm = x.mvp$pred.mvp.lm - 16*mean.mvp.W/100
x.mvp$W.needed.rf = 16*(x.mvp$pred.mvp.rf - mean.mvp.W)/100
View(x.mvp[,c("team.name","W.last","pred.mvp.lm", "W.needed.lm","pred.mvp.rf", "W.needed.rf")])

library(ggrepel)
ggplot(data = x.mvp, aes(x = W.last, y = W.needed.rf)) + geom_point() + 
  geom_text_repel(aes(label = team.name, size = 3.5))

ggplot(data = mvp.r, aes(color = mvp.id, y = W.change, x = W.rank.last)) + geom_jitter(aes(size = mvp.id, alpha = mvp.id))  +  geom_smooth(method = "lm", se = F) + 
  scale_alpha_discrete(range = c(.3, 1)) + scale_size_discrete(range = c(1.5, 3))
  #scale_x_continuous(breaks = 1:10/10, minor_breaks = NULL) #+ scale_y_continuous(breaks = 1:10/10, minor_breaks = NULL)

ggplot(data = mvp.r, aes(x = W.p.last, y = W.p, color = mvp.id)) + geom_jitter(aes(size = mvp.id, alpha = mvp.id)) +  geom_smooth(method = "lm", se = F) +
  scale_alpha_discrete(range = c(.3, 1)) + scale_size_discrete(range = c(1.5, 3))#geom_abline() + 
  #lims(x = c(.25,1), y = c(.25,1))

ggplot(data = mvp.r[which(mvp.r$mvp.id=="Yes"),], aes(x = -W.rank.change, y = -W.rank, color = qb.id)) + geom_jitter() #+ 
  #lims(x = c(-32,-1), y = c(-32, -1))

ggplot(data = mvp.r, aes(x = W.p.change, y = mvp.id)) + geom_point()

t.test(data = mvp.r[which(mvp.r$mvp.id=="Yes"),], W.p ~ playoff.last.id, conf.level = .90)
t.test(data = mvp.r[which(mvp.r$mvp.id=="Yes"),], W.rank ~ playoff.last.id, conf.level = .90)
t.test(data = mvp.r[which(mvp.r$mvp.id=="Yes"),], W.p.change ~ playoff.last.id, conf.level = .90)

t.test(data = mvp.r, W.p ~ mvp.id, conf.level = .95)  
t.test(data = mvp.r, W.rank.change ~ mvp.id, conf.level = .95)
t.test(data = mvp.r, W.change ~ mvp.id, conf.level = .95)
t.test(data = mvp.r, W.p.change ~ mvp.id, conf.level = .95)
t.test(data = mvp.r[which(mvp.r$W.p >= .6875),], W.p.change ~ mvp.id, conf.level = .95)
cor(mvp.r[which(mvp.r$mvp.id=="Yes" & mvp.r$qb.id=="QB"),c("W.rank.last", "W.rank")])
x = mvp.r$W.rank.last[which(mvp.r$mvp.id=="Yes" & mvp.r$qb.id=="non-QB")]
y = mvp.r$W.rank[which(mvp.r$mvp.id=="Yes" & mvp.r$qb.id=="non-QB")]
cor.test(x, y)

cor.test(mvp.r$W.rank.last, mvp.r$W.rank)
table(mvp.r$W.rank, mvp.r$mvp.id)
table(mvp.r$W.rank.last, mvp.r$mvp.id)
table(mvp.r$W.last[which(mvp.r$mvp.id=="Yes")], mvp.r$W[which(mvp.r$mvp.id=="Yes")])
table(mvp.r$W.change[which(mvp.r$mvp.id=="Yes")], mvp.r$W.rank[which(mvp.r$mvp.id=="Yes")])
t1 = table(-mvp.r$W.rank.last[which(mvp.r$W.rank <= 10 & mvp.r$W.rank.last <= 25 & mvp.r$mvp.id=="Yes")], 
           -mvp.r$W.rank[which(mvp.r$W.rank <= 10 & mvp.r$W.rank.last <= 25 & mvp.r$mvp.id=="Yes")])
t2 = table(-mvp.r$W.rank.last[which(mvp.r$W.rank <= 10 & mvp.r$W.rank.last <= 25)], 
           -mvp.r$W.rank[which(mvp.r$W.rank <= 10 & mvp.r$W.rank.last <= 25)])
library(tidyr)
library(dplyr)
mvp.rank.tab = mvp.r %>% group_by(W.rank, W.rank.last, mvp.id) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = mvp.id, values_from = n, values_fill = 0) %>%
  mutate(total = Yes + No, 
         p.given.rank = Yes/total, 
         p.given.mvp = Yes/sum(mvp.r$mvp.id=="Yes"))

ggplot(data = mvp.rank.tab, aes(x = W.rank.last, y = W.rank, alpha = p.given.rank)) + geom_point(size = 3) + 
  scale_x_continuous(breaks = seq(0, 32, 2), limits = c(1, 32)) + 
  scale_y_continuous(breaks = seq(0, 32, 2), limits = c(1, 10))

ggplot(data = mvp.r, aes(x = W.rank.last, y = W.rank, color = mvp.id)) + geom_jitter(aes(size = mvp.id, alpha = mvp.id))  +  geom_smooth(method = "lm", se = F) + 
  scale_alpha_discrete(range = c(.3, 1)) + scale_size_discrete(range = c(1.5, 3)) #+ scale_x_continuous(labels = 30:1)

t(table(mvp.r[which(mvp.r$`Playoff Result.last`=="Won SB"), c("mvp.id", "Playoff Result")]))

library(rpart)
library(rpart.plot)

rank.pred = expand.grid(W.rank = 1:10, W.rank.last = 1:24, playoff.last.id = levels(mvp.r$playoff.last.id))
rank.pred = merge(rank.pred, mvp.rank.tab, all.x = T)
rank.pred[which(is.na(rank.pred), arr.ind = T)] = 0

mvp.rpart = rpart(data = mvp.r, mvp.id ~ W.rank + W.rank.last + W.last.bin + playoff.last.id, 
                  control = rpart.control(cp = .005, minsplit = 5, minbucket = 5))
rpart.plot(mvp.rpart)
rank.pred$rpart.pred = predict(mvp.rpart, rank.pred)[,"Yes"]

mvp.logit = glm(data = mvp.r, mvp.id ~ W.rank + W.change * W.last.bin + playoff.last.id, family = binomial(link = "logit"))
summary(mvp.logit)
rank.pred$logit.pred = predict(mvp.logit, rank.pred, type = "response")

mvp.rf = train(data = mvp.r, mvp.id ~  W.last + W.last.bin + playoff.last.id, method = "rf")
varImp(mvp.rf)
rank.pred$rf.pred = predict(mvp.rf, rank.pred, type = "prob")[,"Yes"]

mvp.nb = train(data = mvp.r, mvp.id ~ W.rank + W.last + playoff.last.id, method = "nb")
rank.pred$nb.pred  = predict(mvp.nb, rank.pred, type = "prob")[,"Yes"]

mvp.ada = train(data = mvp.r, mvp.id ~ W.rank + W.rank.last + playoff.last.id, method = "adaboost")
rank.pred$ada.pred  = predict(mvp.ada, rank.pred, type = "prob")[,"Yes"]

mvp.knn = train(data = mvp.r, mvp.id ~ W.rank + W.rank.last + playoff.last.id, method = "knn", tuneGrid = expand.grid(k = 12:36))
rank.pred$knn.pred  = predict(mvp.knn, rank.pred, type = "prob")[,"Yes"]

ggplot(data = rank.pred, aes(x = W.rank.last, y = W.rank, alpha = logit.pred)) + geom_point(size = 3) + 
  facet_grid(~playoff.last.id) + 
  scale_x_continuous(breaks = seq(0, 24, 2), limits = c(1, 24)) + 
  scale_y_continuous(breaks = seq(0, 24, 2), limits = c(1, 10))

ggplot(data = rank.pred, aes(x = W.rank, y = rf.pred, color = W.rank.last, fill = W.rank.last)) + 
  geom_point() + geom_line()

mvp.r.o$Tm[which(mvp.r.o$Season==2019 & grepl("Raiders", mvp.r.o$Tm))] = "Las Vegas Raiders"
mvp.r.o$Tm[which(mvp.r.o$Season==2019 & grepl("Washington", mvp.r.o$Tm))] = "Washington Football Team"
team.rank <- read_csv("nF_teamProj.csv")#read.csv("C:/Users/Jerome/Desktop/pfr_proj/team.rank.csv")
team.rank = merge(team.rank, teams_ab, by.x = "Team", by.y = "TmAb")
team.rank = merge(team.rank, mvp.r.o[which(mvp.r.o$Season==2019),c("W.rank","W.last.bin","Tm","playoff.id")], by = "Tm", suffixes = c("",".last"), all.x = T)
team.rank = merge(team.rank, mvp.rank.tab, all.x = T)
names(team.rank)[22] = "playoff.last.id"
team.rank$ada.pred  = predict(mvp.ada, team.rank, type = "prob")[,"Yes"]
team.rank$nb.pred  = predict(mvp.nb, team.rank, type = "prob")[,"Yes"]
team.rank$rf.pred = predict(mvp.rf, team.rank, type = "prob")[,"Yes"]
#team.rank$logit.pred = predict(mvp.logit, team.rank, type = "response")
team.rank$knn.pred = predict(mvp.knn, team.rank, type = "prob")[,"Yes"]
team.rank[,grep("\\.pred", names(team.rank))] = apply(team.rank[,grep("\\.pred", names(team.rank))], 2, function(x) {round(x, 4)})
team.rank$mean.pred.rank = apply(apply(team.rank[,grep("\\.pred", names(team.rank))], 2, function(x) {rank(-x)}), 1, mean)

#### season similarity ----
off.season = read_html("https://www.pro-football-reference.com/years/NFL/#team_stats_per_team::season") %>% 
  html_nodes("table") %>% .[2] %>%  html_table(fill = T)
off.season = off.season[[1]]
names(off.season) = ifelse(off.season[1,] %in% c("Yds", "TD", " ", "", "Att"), paste0(names(off.season),off.season[1,]), off.season[1,])
names(off.season) = trimws(names(off.season))
off.season = off.season[which(off.season$Rk!="Rk" & off.season$Year %in% 1941:2019),]
off.season = merge(games.per.year, off.season, by = "Year")
off.season = as.data.frame(apply(off.season, 2, as.numeric))

off.season = off.season %>%
  mutate(`Y/G` = PassingYds/games,
         `Cmp%` = 100*Cmp/PassingAtt,
         `TD%` = 100*PassingTD/PassingAtt,
         `Int%` = 100*Int/PassingAtt)
         #`1D` = `1stD.1`)

off.season.l = list()
for (y in 1950:2019) {
 tmp = read_html(paste0("https://www.pro-football-reference.com/years/",y,"/passing.htm")) %>% 
          html_node("table") %>%  html_table(fill = T)
 if (y %in% favre$Year) {
   tmp = tmp[!grepl("Favre", tmp$Player),]
  }
 tmp = tmp[1:25,c("Player","Y/G","Cmp%","TD%","Int%","Y/A")]
 tmp$Year = y
 off.season.l[[as.character(y)]] = tmp
 print(y)
}

off.season = do.call(rbind,off.season.l)
off.season[,-1] = as.data.frame(apply(off.season[,-1], 2, as.numeric))

season.id = which(off.season$Year >= 1949)
off.season.s = as.data.frame(cbind(off.season[season.id,c("Player","Year")], apply(off.season[season.id,c("Y/G","Cmp%","TD%","Int%","Y/A")], 2, normalize)))


favre = read_html("https://www.pro-football-reference.com/players/F/FavrBr00.htm") %>% 
  html_node("table") %>%  html_table(fill = T)
#favre$pro.bowl = grepl("\\*", favre$Year)
#favre$all.pro = grepl("\\+", favre$Year)
favre$Year = gsub("[^0-9]+", "", favre$Year)
favre = favre[2:20,]

favre.s = as.data.frame(matrix(nrow = nrow(favre), ncol = length(off.season.s)-1, NA))
names(favre.s) = names(off.season.s)[-1]
favre.s$Year = favre$Year
for (j in names(off.season.s)[-c(1,2)]) {
  #x.mean = mean(off.season[,j], na.rm = T)
  #x.sd = sd(off.season[,j], na.rm = T)
  #favre.s[,j] = (favre[,j] - x.mean)/x.sd
  x.min = min(off.season[,j], na.rm = T)
  x.max = max(off.season[,j], na.rm = T)
  favre.s[,j] = (favre[,j] - x.min)/(x.max - x.min)
}

era.comp = as.data.frame(matrix(nrow = nrow(favre.s), ncol = 3, NA))
names(era.comp) = c("year", "like.player", "like.year")#,"unlike.player", "unlike.year","total.sim")
era.comp$year = favre.s$Year
sims = data.frame(matrix(nrow = nrow(off.season.s), ncol = nrow(favre.s), NA))
names(sims) = paste0("season",favre.s$Year)
sim.agg = list()
for (i in 1:nrow(favre.s)) {
  f = favre.s[i,2:length(favre.s)] #off.season.s[i,2:(length(off.season.s)-2)]#
  sim = apply(off.season.s[,-c(1,2)], 1, function(x) {as.numeric(dist(rbind(x,f)))})#{cos.sim(x, f)})
  era.comp[i,c("like.player", "like.year")] = off.season[which.min(sim),c("Player","Year")]
  #era.comp[i,c("unlike.player", "unlike.year")] = off.season[which.max(sim),c("Player","Year")]
  #era.comp$total.sim[i] = mean(sim)
  sims[,i] = sim
  #q = as.data.frame(cbind(off.season.s$Year, sim))
  #sim.agg[[i]] = q %>% group_by(V1) %>% summarise(mean(sim), year = i + 1991)
  print(i)
}
sims$Season = off.season.s$Year
sims.m = melt(sims, id.vars = "Season")

games.per.year = data.frame(Year = mvp$Season, games = mvp$W + mvp$T + mvp$L)
games.per.year = rbind(games.per.year, data.frame(Year = 1935:(min(mvp$Season)-1), games = 12))
games.per.year$games[which(games.per.year$Year %in% c(1937:1942, 1946))] = 11
games.per.year$games[which(games.per.year$Year %in% c(1943:1945))] = 10
games.per.year = unique(games.per.year)

library(foreach)
library(doParallel)
cl = makeCluster(6)
registerDoParallel(cl)

sims = foreach (i = 1:nrow(favre.s), .combine = cbind) %dopar% {
  f = favre.s[i,2:length(favre.s)] #off.season.s[i,2:(length(off.season.s)-2)]#
  #apply(off.season.s[,-c(1,2)], 1, function(x) {cos.sim(x, f)})
  apply(off.season.s[,-c(1,2)], 1, function(x) {as.numeric(dist(rbind(x,f)))})
  #era.comp[i,c("like.player", "like.year")] = off.season[which.min(sim),c("Player","Year")]
  #era.comp[i,c("unlike.player", "unlike.year")] = off.season[which.max(sim),c("Player","Year")]
  #era.comp$total.sim[i] = mean(sim)
  #sims[,i] = sim
  #q = as.data.frame(cbind(off.season.s$Year, sim))
  #sim.agg[[i]] = q %>% group_by(V1) %>% summarise(mean(sim), year = i + 1991)
}
stopImplicitCluster()
sims = cbind(off.season.s[,c("Player","Year")],sims)
names(sims) = c("Player","Year",paste0("Season_",favre.s$Year))
sims.m = melt(sims[,-1], id.vars = "Year")

X = sims.m %>%
  group_by(Year, variable) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarise(min.dist = min(value), 
            closest.season = Year[which(value==min.dist)])

ggplot(data = X, aes(x = value, fill = factor(Season))) + geom_histogram() + facet_grid(variable~.)

X.c = dcast(X, Year ~ variable)

#### adding divisions ----
nfl_divisions <- read_csv("nfl_divisions.csv")
nfl_divisions$team = trimws(nfl_divisions$team)
nfl_divisions$team = gsub("[^[:print:]]", "", nfl_divisions$team)
nfl_divisions$uid = 1:nrow(nfl_divisions)

nfl_divisions$uid = NA
mvp$uid = 1:nrow(mvp)
for (i in 1:nrow(nfl_divisions)) {
  # if (nfl_divisions$to[i] >= 1978) {
  nfl_divisions$uid[i] = which(mvp$Tm==nfl_divisions$team[i] & mvp$Season >= nfl_divisions$from[i] & mvp$Season <= nfl_divisions$to[i])
  #}
}

  
  