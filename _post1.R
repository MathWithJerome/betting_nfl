#soup.logit = 
ou.small.logit = glm(data = soup.tot[which(soup.tot$favorite.close!="push"),], close.result ~ home.change + total.change, 
                  family = binomial(link = "logit"))
summary(l.small.logit)
u.small.logit = glm(data = soup.tot[which(soup.tot$favorite.close!="push"),], close.result ~ home.change + total.change, 
                     family = binomial(link = "logit"))
summary(ou.small.logit)

curve = expand.grid(
  total.change = seq(min(soup.tot$total.change, na.rm = TRUE), max(soup.tot$total.change, na.rm = TRUE), by = (max(soup.tot$total.change, na.rm = TRUE)-min(soup.tot$total.change, na.rm = TRUE))/100),
  spread.change = seq(min(soup.tot$spread.change, na.rm = TRUE), max(soup.tot$spread.change, na.rm = TRUE), by = (max(soup.tot$spread.change, na.rm = TRUE)-min(soup.tot$spread.change, na.rm = TRUE))/100)
)
curve$close.result = factor(ifelse(predict(small.logit, curve, type = "response") < .5,"over","under"), levels = c("over","under"))
hull.o = curve[chull(curve[which(curve$close.result=="over"),1:2]),]
hull.u = curve[chull(curve[which(curve$close.result=="under"),1:2]),]
hull = rbind(hull.o, hull.u)

hull <- curve[,c("total.change","spread.change","close.result")] %>%
  group_by(close.result) %>%
  slice(chull(total.change, spread.change))
ggplot(data = soup.tot) + geom_jitter(aes(x = total.change, y = spread.change, color = close.result)) + 
  #geom_path(data = curve, aes(x = total.change, y = spread.change))
  geom_polygon(data = hull, aes(x = total.change, y = spread.change, color = close.result), alpha = 0.2)

exp(small.logit$coefficients)
library(oddsratio)
or_glm(data = soup.tot[which(soup.tot$favorite.close!="push"),], model = small.logit, incr = list(total.change = .5, spread.change = .5))

step.logit = step(soup.logit, direction = "both")
small.pred = factor(ifelse(predict(small.logit, testers[which(testers$favorite.open!="push"),], type = "response") < .5,"over","under"), levels = c("over","under"))
confusionMatrix(small.pred, testers$close.result[which(testers$favorite.open!="push")], positive = "over")
testers$correct = ifelse(testers$close.result==pred$nn.class, "yes", "no")
ggplot(data = testers) + geom_histogram(aes(x = total.change, fill = close.result)) + facet_grid(close.result~correct)
soup.knn = knn3(data = trainers, close.result ~ total.change + home.change, k = 3)
confusionMatrix(factor(ifelse(predict(small.logit, testers, type = "response") < .5,"over","under"), levels = c("over","under")), 
                testers$close.result, positive = "over")
predict(soup.knn, testers, type = "class")
confusionMatrix(predict(soup.knn, testers, type = "raw"), 
                testers$close.result, positive = "over")
library(Ckmeans.1d.dp)
total.change.km = Ckmeans.1d.dp(soup$total.change, k = c(1,5))
qplot(soup$total.change)
importance(soup.rf1)
summary(step.logit)
cor(soup.tot$total.change[which(soup.tot$close.result=="over")], soup.tot$open.total[which(soup.tot$close.result=="over")])
cor(soup.tot$total.change[which(soup.tot$close.result=="under")], soup.tot$open.total[which(soup.tot$close.result=="under")])

p = seq(0.1, .95, by = 0.01)
qplot(x = p, y = 100*p/(1-p))

r = seq(.5, 4, .05)
qplot(x = r, y = r/(r+1))
cbind(r, r/(r+1))



