library(readr)

coy2022 <- read_csv("coy2022.csv", col_names = FALSE)

coy.odds = data.frame()
for (j in 1:length(coy2022)) {
  tmp = coy2022[names(coy2022)[j]]
  date = unlist(tmp[nrow(tmp),])
  tmp = tmp[-nrow(tmp),]
  names = unlist(tmp[seq(from = 1, to = nrow(tmp), by = 2),])
  odds = unlist(tmp[seq(from = 2, to = nrow(tmp), by = 2),])
  tmp.df = data.frame(coach = names, odds = odds)
  tmp.df$date = gsub(",.+","",date)
  #row.names(tmp.df) = NULL
  coy.odds = rbind(coy.odds, tmp.df)
}
row.names(coy) = NULL

mvp2022 <- read_csv("mvp2022.csv", col_names = FALSE)

mvp.odds = data.frame()
for (j in 1:length(mvp2022)) {
  tmp = mvp2022[names(mvp2022)[j]]
  date = unlist(tmp[nrow(tmp),])
  tmp = tmp[-nrow(tmp),]
  names = unlist(tmp[seq(from = 1, to = nrow(tmp), by = 2),])
  odds = unlist(tmp[seq(from = 2, to = nrow(tmp), by = 2),])
  tmp.df = data.frame(player = names, odds = odds)
  tmp.df$date = gsub(",.+","",date)
  #row.names(tmp.df) = NULL
  mvp.odds = rbind(mvp.odds, tmp.df)
}
row.names(mvp) = NULL

sb2022 <- read_csv("SBodds.csv", col_names = FALSE)

sb.odds = data.frame(Team = sb2022$X1[seq(from = 1, to = nrow(sb2022), by = 2)], 
                     Odds = sb2022$X1[seq(from = 2, to = nrow(sb2022), by = 2)])

sb.div = merge(division_odds, sb.odds, by = "Team")
sb.div$Odds.y = as.numeric(sb.div$Odds.y)

sb.odds = data.frame()
for (j in 1:length(sb.odds)) {
  tmp = mvp2022[names(sb.odds)[j]]
  date = unlist(tmp[nrow(tmp),])
  tmp = tmp[-nrow(tmp),]
  names = unlist(tmp[seq(from = 1, to = nrow(tmp), by = 2),])
  odds = unlist(tmp[seq(from = 2, to = nrow(tmp), by = 2),])
  tmp.df = data.frame(player = names, odds = odds)
  tmp.df$date = gsub(",.+","",date)
  #row.names(tmp.df) = NULL
  mvp.odds = rbind(mvp.odds, tmp.df)
}
row.names(mvp) = NULL
