library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(modelr)

pbp = data.frame()
for (y in 2011:2021) {
  x = readRDS(paste0("C:\\Users\\jerom\\Desktop\\Projects\\play_data\\play_by_play_",y,".rds"))
  pbp = rbind(x,pbp)
  print(y)
}

setDT(pbp)
setDT(pbp.update)

#### whole league ----
avg_pt_diff.w = data.table(team = character(), season = integer(), week = integer(),
                         w.diff = numeric(), final.diff = numeric(), #w.wp = numeric(),
                         w.pythag = numeric(), final.pythag = numeric(), game_id = character(),
                         win = numeric())
#e = .01
for (y in 2011:2021) {
  print(y)
  for (t in unique(pbp.update$home_team)) {
    tmp = pbp[(away_team==t|home_team==t) & !is.na(drive) & season_type=="REG",# & game_id=="2020_01_CLE_BAL",
                c("game_id","home_team","away_team","total_home_score","total_away_score","drive","drive_time_of_possession","result","season","week")]
    tmp = tmp[,.(home_score = min(total_home_score), away_score = min(total_away_score), result = max(result)), #wp = vegas_home_wp[1],
              by = .(game_id, week, season, home_team, away_team, drive, drive_time_of_possession)]
    tmp[,`:=` (drive_duration = period_to_seconds(ms(drive_time_of_possession)),
               pt.diff = ifelse(home_team==t, home_score - away_score, away_score - home_score),
               pythag = ifelse(home_team==t, home_score^2/(away_score^2 + home_score^2), away_score^2/(away_score^2 + home_score^2)))]
    tmp[is.na(pythag),pythag := 0.5]
    tmp[,final.diff := ifelse(away_team==t, -result, result)]
    tmp$result = NULL
    #tmp.f = unique(tmp[,c("game_id","final.diff")])
    tmp.f = tmp[,.(team = t, 
                   final.diff = max(final.diff), 
                   tm_score = max(ifelse(home_team==t, home_score, away_score)), 
                   opp_score = max(ifelse(home_team==t, away_score, home_score)),
                   avg.diff = sum(pt.diff*drive_duration)/sum(drive_duration),
                   avg.pythag = sum(pythag*drive_duration)/sum(drive_duration)), by = .(game_id,week,season)]
    tmp.f[,`:=` (win = ifelse(final.diff > 0, 1, 0), 
                 final.pythag = tm_score^2/(tm_score^2 + opp_score^2))]
    #tmp.f/,   
    avg_pt_diff.w = rbind(avg_pt_diff.w, tmp.f)
  }
}

avg_pt_diff.w[,final.diff.g := final.diff/16]

ggplot(data = avg_pt_diff[season==2020,], aes(x = scale(final.pythag), y = scale(w.pythag), label = team)) + geom_text(size = 3, color = "dark green") +
  geom_smooth(se = F, method = "lm") +
  geom_text(data = avg_pt_diff[season==2020,], aes(x = scale(final.diff), y = scale(w.diff), label = team), size = 3, color = "dark orange")
  
ggplot(data = avg_pt_diff, aes(x = w.diff, y = final.diff)) + geom_point() +
  geom_smooth(se = F, method = "lm")

pd.fit = lm(data = avg_pt_diff, final.diff ~ w.diff); summary(pd.fit)
pd.g.fit = lm(data = avg_pt_diff, final.diff.g ~ w.diff); summary(pd.g.fit)
py.fit = lm(data = avg_pt_diff, final.pythag ~ w.pythag); summary(py.fit)
pd.win = lm(data = avg_pt_diff, wins ~ final.diff); summary(pd.win)# + scale(w.diff)))
pdw.win = lm(data = avg_pt_diff, wins ~ w.diff); summary(pdw.win)
py.win = lm(data = avg_pt_diff, wins ~ final.pythag); summary(py.win)
pyw.win = lm(data = avg_pt_diff, wins ~ w.pythag); summary(pyw.win)

same.rpart = rpart(data = avg_pt_diff, wins ~ final.diff + w.diff + final.pythag + w.pythag, 
                 control = rpart.control(cp = .005, minsplit = 0.05*nrow(avg_pt_diff), minbucket = 0.05*nrow(avg_pt_diff)))
 
rpart.plot(same.rpart)

same.season = avg_pt_diff %>%
  filter(season == 2020) %>% 
  select(c(1,3:7)) %>%
  add_predictions(pd.fit, var = "pd") %>% 
  add_predictions(pd.win, var = "final.diff.wins") %>%
  mutate(pd.diff = final.diff - pd) %>% 
  rename("final.diff1" = "final.diff") %>% 
  rename("final.diff" = "pd") %>% 
  add_predictions(pdw.win, var = "w.diff.wins") %>% 
  mutate(wins.diff = w.diff.wins - final.diff.wins) %>% 
  add_predictions(same.rpart)
  
avg_pt_diff[,next_season := season + 1]

avg_pt_diff_next = merge(avg_pt_diff, avg_pt_diff[,c("team","season","wins")], by.x = c("team","next_season"), by.y = c("team","season"), suffixes = c("","_next"))
avg_pt_diff_next[,wins.diff := wins_next - wins]

pd.n.win = lm(data = avg_pt_diff_next, wins_next ~ final.diff); summary(pd.n.win)# + scale(w.diff)))
pdw.n.win = lm(data = avg_pt_diff_next, wins_next ~ w.diff); summary(pdw.n.win)
py.n.win = lm(data = avg_pt_diff_next, wins_next ~ final.pythag); summary(py.n.win)
pyw.n.win = lm(data = avg_pt_diff_next, wins_next ~ w.pythag); summary(pyw.n.win)

pd.lm = lm(data = avg_pt_diff_next, wins_next ~ scale(final.diff) + scale(w.diff)); summary(pd.lm)
pd.rpart = rpart(data = avg_pt_diff_next, wins_next ~ final.diff + w.diff, #+ final.pythag + w.pythag, 
                 control = rpart.control(cp = 0.01, xval = 20, minsplit = .05*nrow(avg_pt_diff_next), 
                                         minbucket = .05*nrow(avg_pt_diff_next)))
rpart.plot(pd.rpart)

pd.rpart.full = rpart(data = avg_pt_diff_next, wins_next ~ final.diff + w.diff + final.pythag + w.pythag, 
                 control = rpart.control(cp = 0.001, xval = 20, minsplit = .04*nrow(avg_pt_diff_next), 
                                         minbucket = .04*nrow(avg_pt_diff_next)))
rpart.plot(pd.rpart.full)

avg_pt_diff_next[,`:=` (w.diff.s = scale(w.diff), final.diff.s = scale(final.diff))]
ggplot(data = melt(avg_pt_diff_next, id.vars = "wins_next", measure = c("w.diff.s","final.diff.s")), 
       aes(y = wins_next, x = value, color = variable)) + geom_point() + geom_smooth()

avg_pt_diff_next$part = as.factor(round(predict(pd.rpart, avg_pt_diff_next)))#as.factor(round(predict(pd.rpart, avg_pt_diff_next),1))
avg_pt_diff_next[,diff.gap := scale(final.diff) - scale(w.diff)]
avg_pt_diff_next[,pythag.gap := final.pythag - w.pythag]
library(Ckmeans.1d.dp)
avg_pt_diff_next[,final.diff.bin := cut(final.diff, breaks = c(-Inf,-120,-60, -20, 20, 60, 120,Inf))]
ggplot(data = avg_pt_diff_next, aes(x = diff.gap, y = wins_next, color = final.diff.bin)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = avg_pt_diff_next, aes(x = pythag.gap, y = wins_next, color = part)) + geom_point() + geom_smooth()

pd.lm.p = lm(data = avg_pt_diff_next, wins_next ~ (diff.gap) + part); summary(pd.lm.p)

avg_pt_diff_next$lm = predict(pd.lm, avg_pt_diff_next)
avg_pt_diff_next$lm.p = predict(pd.lm.p, avg_pt_diff_next)
avg_pt_diff_next$rpart = predict(pd.rpart, avg_pt_diff_next)
avg_pt_diff_next$rpart.full = predict(pd.rpart.full, avg_pt_diff_next)

#RMSE(avg_pt_diff_next$part, avg_pt_diff_next$wins_next)
r1 = RMSE(avg_pt_diff_next$lm, avg_pt_diff_next$wins_next); r1
r2 = RMSE(avg_pt_diff_next$lm.p, avg_pt_diff_next$wins_next); r2
r3 = RMSE(avg_pt_diff_next$rpart, avg_pt_diff_next$wins_next); r3
r4 = RMSE(avg_pt_diff_next$rpart.full, avg_pt_diff_next$wins_next); r4

#MAE(avg_pt_diff_next$part, avg_pt_diff_next$wins_next)
m1 = MAE(avg_pt_diff_next$lm, avg_pt_diff_next$wins_next); m1
m2 = MAE(avg_pt_diff_next$lm.p, avg_pt_diff_next$wins_next); m2
m3 = MAE(avg_pt_diff_next$rpart, avg_pt_diff_next$wins_next); m3
m4 = MAE(avg_pt_diff_next$rpart.full, avg_pt_diff_next$wins_next); m4

tmp = avg_pt_diff
tmp$final.diff=NULL
names(tmp)[10] = "final.diff"

win.pred = avg_pt_diff %>%
  filter(season == 2020) %>% 
  select(c(1,3:7)) %>%
  #add_predictions(pd.n.fit, var = "pd") %>% 
  add_predictions(pd.n.win, var = "final.diff.wins") %>%
  #mutate(pd.diff = final.diff - pd) %>% 
  #rename("final.diff1" = "final.diff") %>% 
  #rename("final.diff" = "pd") %>% 
  add_predictions(pdw.n.win, var = "w.diff.wins") %>% 
  mutate(final.diff.wins = (17/16)*final.diff.wins, 
         w.diff.wins = (17/16)*w.diff.wins,
         wins.diff = w.diff.wins - final.diff.wins) %>% 
  add_predictions(pd.rpart, var = "part")
#
#### split season ----
avg_pt_diff_split = data.table(team = character(), season = integer(), 
                         w.diff.1 = numeric(), final.diff.1 = numeric(), 
                         w.diff.2 = numeric(), final.diff.2 = numeric(),
                         wins = numeric())

for (y in 2011:2020) {
  print(y)
  for (t in unique(pbp20$home_team)) {
    tmp = pbp[season==y & (away_team==t|home_team==t) & !is.na(drive) & season_type=="REG",# & game_id=="2020_01_CLE_BAL",
              c("game_id","home_team","away_team","total_home_score","total_away_score","drive","drive_time_of_possession","result","vegas_home_wp","week")]
    #c("game_id","home_team","away_team","total_home_score","total_away_score","drive","drive_time_of_possession")] #,"vegas_wp","vegas_home_wp" 
    tmp[,season_half := ifelse(week <= 9, "first", "second")]
    tmp = tmp[,.(home_score = min(total_home_score), away_score = min(total_away_score), wp = vegas_home_wp[1], result = max(result)), 
              by = .(game_id, home_team, away_team, drive, drive_time_of_possession, season_half)]
    #tmp = tmp[wp > e & wp < 1-e,]
    tmp[,drive_duration := period_to_seconds(ms(drive_time_of_possession))]
    tmp[,pt.diff := ifelse(home_team==t, home_score - away_score, away_score - home_score)]
    tmp[,final.diff := ifelse(away_team==t, -result, result)]
    tmp$result = NULL
    tmp.f = unique(tmp[,c("game_id","final.diff","season_half")])
    tmp.f[,win := ifelse(final.diff > 0, 1, 0)]
    tmp.f$win[which(tmp.f$final.diff==0)] = 1/2
    x = tmp[,.(avg.diff = sum(pt.diff*drive_duration)/sum(drive_duration)), by = season_half]
    avg_pt_diff_split = rbind(avg_pt_diff_split, data.table(team = t, season = y, 
                                                w.diff.1 = x$avg.diff[1], final.diff.1 = sum(tmp.f$final.diff[which(tmp.f$season_half=="first")]),
                                                w.diff.2 = x$avg.diff[2], final.diff.2 = sum(tmp.f$final.diff[which(tmp.f$season_half=="second")]), 
                                                wins = sum(tmp.f$win)))
  }
}

pd.fit.1 = lm(data = avg_pt_diff, final.diff.1 ~ w.diff.1); summary(pd.fit.1)
pd.fit.2 = lm(data = avg_pt_diff, final.diff.2 ~ w.diff.2); summary(pd.fit.2)
data.table(team = avg_pt_diff_split$team[which(avg_pt_diff_split$season==2020)], 
           old.pd.1 = avg_pt_diff_split$final.diff.1[which(avg_pt_diff_split$season==2020)], 
           new.pd = pd.fit.1$fitted.values[which(avg_pt_diff_split$season==2020)], 
           old.pd.2 = avg_pt_diff_split$final.diff.2[which(avg_pt_diff_split$season==2020)], 
           new.pd = pd.fit.2$fitted.values[which(avg_pt_diff_split$season==2020)])[order(team),]

avg_pt_diff_split[,next_season := season + 1]
avg_pt_diff_split_next = merge(avg_pt_diff_split, avg_pt_diff_split[,c("team","season","wins")], by.x = c("team","next_season"), by.y = c("team","season"), suffixes = c("","_next"))
pd.lm.s = lm(data = avg_pt_diff_split_next, wins_next ~ scale(final.diff.1) + scale(w.diff.2))
summary(pd.lm.s)

pd.rpart.s = rpart(data = avg_pt_diff_split_next, wins_next ~ final.diff.1 + final.diff.2 + w.diff.1 + w.diff.2,
                 control = rpart.control(cp = 0.01, xval = 20, minsplit = .05*nrow(avg_pt_diff_split_next), 
                                         minbucket = .05*nrow(avg_pt_diff_split_next)))
rpart.plot(pd.rpart.s)
X = expand.grid(final.diff.1 = seq(-100, 100, 5), w.diff.1 = seq(-10, 10, .5),final.diff.2 = seq(-100, 100, 5), w.diff.2 = seq(-10, 10, .5))
X$pred = predict(pd.rpart.s, X)
ggplot(data = X, aes(x = final.diff.2, y = w.diff.2, color = pred)) + geom_point()

#### fanduel sportsbook ----
fd.wins = read.csv("fd_wins.csv")

fd.wins = fd.wins %>% 
  mutate(over.imp = implied.prob(over),
         under.imp = implied.prob(under),
         over.imp.c = 1 - over.imp,
         under.imp.c = 1 - under.imp,
         sum.imp = over.imp + under.imp, 
         over.diff = over.imp - under.imp.c, 
         under.diff = under.imp - over.imp.c)

eliminator_raw <- read_csv("../eliminator/eliminator_raw.csv", col_names = FALSE)
eliminator.p = as.data.frame(apply(eliminator_raw[,2:19], 2, function(x) {as.numeric(gsub("\\D","",x))/1000}))
eliminator.opp = as.data.frame(apply(eliminator_raw[,2:19], 2, function(x) {gsub("[^A-Z]","",x)}))
eliminator.opp[which(eliminator.opp=="WSH",arr.ind = T)] = "WAS"
eliminator.opp[which(eliminator.opp=="JAC",arr.ind = T)] = "JAX"
exp.wins = data.frame(team = eliminator_raw$X1, wins = rowSums(eliminator.p, na.rm = T))
exp.wins = merge(exp.wins, team_map, by.x = "team", by.y = "V1")
names(exp.wins)[c(1,3,4)] = c("team.long","team","Division")

exp.wins = merge(nF_exp_wins, team_map, by.x = "team.long", by.y = "V1")
names(exp.wins)[c(4,5)] = c("team","Division")

wins.comp = merge(fd.wins, exp.wins, by = "team", suffixes = c("",".nf"))
wins.comp = wins.comp %>% 
  mutate(delta = wins - wins.nf, 
         delta.c = ceiling(wins) - wins.nf)
ggplot(data = wins.comp, aes(x = ceiling(wins), y = wins.nf, label = team)) + geom_text() + geom_abline()
ggplot(data = wins.comp, aes(x = wins, y = wins.nf, label = team)) + geom_text() + geom_abline()

ggplot(data = wins.comp) + 
  geom_point(aes(x = over.imp - under.imp.c, y = under.imp - over.imp.c)) + geom_abline()

sched.str = data.frame()
i = 32
for (i in 1:32) {
  tmp = data.frame(t(eliminator.opp[i,]))
  names(tmp) = "team"
  tmp$week = 1:18
  tmp = merge(tmp, exp.wins)
  sched.str = rbind(sched.str, data.frame(team = eliminator_raw$X1[i], opp = sum(tmp$wins), n = nrow(tmp)))
}

#### change in wins between season ----
avg_pt_diff_next[,final.diff.g := final.diff/16]
avg_pt_diff_next[,wins.bin := cut(wins, breaks = c(-1,3,6,9,12,16))]
wd.lm = lm(data = avg_pt_diff_next, wins.diff ~ wins.bin*(final.diff.g + w.diff) -1); summary(wd.lm)

sd(avg_pt_diff$final.diff.g)
sd(avg_pt_diff$w.diff)
pd.freq = data.frame()#team = NA, avg.win.diff = NA, n = NA, p = NA)
for (t in teams) {
  t.pd = avg_pt_diff[team==t & season==2020,c("team","final.diff.g","w.diff","wins")]
  t.pd.n = avg_pt_diff_next[#final.diff.g <= t.pd$final.diff.g+4 & final.diff.g >= t.pd$final.diff.g-4 & 
                            w.diff <= t.pd$w.diff+3 & w.diff >= t.pd$w.diff-3 & 
                            wins <= t.pd$wins+2 & wins >= t.pd$wins-2,]
  # print(paste(t, round(mean(t.pd.n$wins.diff),3), nrow(t.pd.n), 
  #             round(sum(t.pd.n$wins_next>=fd.wins$wins[which(fd.wins$team==t)])/nrow(t.pd.n),4)))
  pd.freq = rbind(pd.freq, data.frame(team = t, 
                                      avg.win.diff = round(mean(t.pd.n$wins.diff),3), 
                                      n = nrow(t.pd.n),
                                      line = fd.wins$wins[which(fd.wins$team==t)],
                                      p = round(sum(t.pd.n$wins_next>=fd.wins$wins[which(fd.wins$team==t)])/nrow(t.pd.n),4)))
  
}
View(pd.freq)

library(FNN)
k.in = avg_pt_diff_next_wintotal[,.(team, wins, final.diff.g, w.diff, `Win Total`, over.imp)]
m.wins = mean(k.in$wins/16); sd.wins = sd(k.in$wins/16)
m.final.diff.g = mean(k.in$final.diff.g); sd.final.diff.g = sd(k.in$final.diff.g)
m.w.diff = mean(k.in$w.diff); sd.w.diff = sd(k.in$w.diff)
m.win.total = mean(k.in$`Win Total`/16); sd.win.total = sd(k.in$`Win Total`/16)
m.over.imp = mean(k.in$over.imp); sd.over.imp = sd(k.in$over.imp)
k.in[,`:=` (wins.s = (wins/16 - m.wins)/sd.wins, 
            final.diff.g.s = (final.diff.g - m.final.diff.g)/sd.final.diff.g, 
            w.diff.s = (w.diff - m.w.diff)/sd.w.diff, 
            win.total.s = (`Win Total`/16 - m.win.total)/sd.win.total,
            over.imp.s = (over.imp - m.over.imp)/sd.over.imp)]
k.query = avg_pt_diff_wintotal20[,.(team, wins, final.diff.g, w.diff, wins.total, over.imp)]
k.query[,`:=` (wins.s = (wins/16 - m.wins)/sd.wins, 
               final.diff.g.s = (final.diff.g - m.final.diff.g)/sd.final.diff.g, 
               w.diff.s = (w.diff - m.w.diff)/sd.w.diff, 
               win.total.s = (wins.total/17 - m.win.total)/sd.win.total,
               over.imp.s = (over.imp - m.over.imp)/sd.over.imp)]
k = 40
neighbors = get.knnx(data = k.in[,.(
                            wins.s, 
                            final.diff.g.s,
                            win.total.s,
                            over.imp.s,
                            w.diff.s)],
             query = k.query[,.(
                                wins.s, 
                                final.diff.g.s,
                                win.total.s,
                                over.imp.s,
                                w.diff.s)], 
             algorithm = "cover_tree", k = k)
#t = "HOU"
for (t in teams) {
  if (t==teams[1]) {
    k.wins = data.frame()
  }
  t.k = avg_pt_diff_next_wintotal[neighbors$nn.index[which(k.query$team==t),],.(wins, final.diff.g, w.diff,wins_next,Result)]
  k.wins = rbind(k.wins, 
                 data.frame(team = t, wins.last = avg_pt_diff$wins[which(avg_pt_diff$team==t & avg_pt_diff$season==2020)],
                            fd.wins = fd.wins$wins[which(fd.wins$team==t)],
                            avg.winp.next = mean(t.k$wins_next)/16, 
                            over.p = sum(t.k$wins_next + mean(t.k$wins_next)/16 >= fd.wins$wins[which(fd.wins$team==t)])/k,
                            over.p.class = sum(t.k$Result %in% c("Over","Push"))/k,
                            avg.k.dist = mean(neighbors$nn.dist[which(k.query$team==t),])
                            )
  )
}
View(k.wins)

fd.k.wins = merge(fd.wins, k.wins, by = "team")
fd.k.wins = merge(fd.k.wins, team_map[1:32,], by.x = "team", by.y = "V2")
fd.k.wins = fd.k.wins %>% 
  mutate(net.over.p = if_else(over.p >= .5, over.p - over.imp, (1-over.p) - under.imp),
         net.over.class = if_else(over.p.class >= .5, over.p.class - over.imp, (1-over.p.class) - under.imp)) %>% 
  rename("team.long" = "V1", "Division" = "V3")
  
ggplot(data = fd.k.wins, aes(x = over.p, y = over.p.class, label = team)) + geom_text() + geom_abline()

library(caret)

over.in = avg_pt_diff_next_wintotal[,over.id := ifelse(Result=="Over", "T", "F")]
trainers.id = createDataPartition(over.in$Result, list = F, p = .8)
trainers = over.in[trainers.id,.(w.diff,final.diff.g,w.pythag,final.pythag,wins,`Win Total`,over.imp)]
testers = over.in[-trainers.id,.(w.diff,final.diff.g,w.pythag,final.pythag,wins,`Win Total`,over.imp)]
y.train = over.in$over.id[trainers.id]
y.test = over.in$over.id[-trainers.id]

ou = train(data = trainers, x = trainers, y = y.train, method = "rf")
ou$results
confusionMatrix(predict(ou, testers), as.factor(y.test))
