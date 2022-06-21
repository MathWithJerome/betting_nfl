library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(pROC)
library(doParallel)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)


#### trainers and testers ----

trainers.id = createDataPartition(soup$home.win, p = .7, list = F)
trainers = soup[trainers.id,] #%>% select(-total.result))
testers = soup[-trainers.id,] #%>% select(-total.result))

#### over/under forumla ----
form.h = formula(total.result ~ total.change.p + home.open + home.change + week + 
                   mean.pts.n.home + mean.against.n.home + mean.epa.n.home + score.lag.home + opp.score.lag.home + epa.lag.home +
                   mean.pts.n.away + mean.against.n.away + mean.epa.n.away + score.lag.away + opp.score.lag.away + epa.lag.away
)
# form.w = formula(total.result ~ open.total + total.change + home.open + home.change + mean.pts.n + mean.against.n + mean.epa.n + week)
# form.wb = formula(total.result ~ open.total + total.change + home.open + home.change + (mean.pts.n + mean.against.n + mean.epa.n) * week.bin)



#### simple logistic reg ----
cl = makePSOCKcluster(8); registerDoParallel(cl); t = Sys.time()
caret.glm = train(form.h, data = soup, method = "glmStepAIC", 
                  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
Sys.time() - t; stopCluster(cl)
glm.pred = data.frame(p.hat = predict(caret.glm, testers, type = "prob"), 
                      result = testers$total.result)
glm.thresh = glm.opt = data.frame()
# for (n in 1:200) {
#   trainers.id = createDataPartition(testers$total.result, p = .7, list = F)
#   t = testers[trainers.id,] #%>% select(-total.result))
#   #tes = testers[-trainers.id,] #%>% select(-total.result))
for (i in 50:70) {
  ass = as.factor(ifelse(glm.pred$p.hat.over > i/100 , "under", "over"))
  M = confusionMatrix(ass, testers$total.result, mode = "everything")
  d = M$table[2,1]+M$table[2,2]
  b = M$table[2,1]
  a = round(b/d,4)
  glm.thresh = rbind(glm.thresh, data.frame(i = i/100, p.win = a, p.arrive = b/sum(M$table), p = (2*a-1)*b/sum(M$table)))
}

for (i in 30:50) {
  ass = as.factor(ifelse(glm.pred$p.hat.over > i/100 , "under", "over"))
  M = confusionMatrix(ass, testers$total.result, mode = "everything")
  d = M$table[1,1]+M$table[1,2]
  b = M$table[1,1]
  a = round(b/d,4)
  glm.thresh = rbind(glm.thresh, data.frame(i = i/100, p.win = a, p.arrive = b/sum(M$table), p = (2*a-1)*b/sum(M$table)))
}
#   glm.opt = rbind(glm.opt, glm.thresh[which.max(glm.thresh$p),])
# }

#### caret xgboost ----
library(doParallel)

# non.trainers.id = createDataPartition(soup$total.result, p = .4, list = F)
# testers.id = sample(non.trainers.id, size = .5*length(non.trainers.id))
# trainers = soup[-non.trainers.id,] #%>% select(-total.result))
# testers = soup[testers.id,] #%>% select(-total.result))
# validators = soup[setdiff(non.trainers.id, testers.id),]

trainers.id = createDataPartition(soup$total.result, p = .7, list = F)
trainers = soup[trainers.id,]
testers = soup[-trainers.id,]

library(randomForest)
costMatrix = matrix(c(1,1,1,1), nrow = 2)
soup.rf = randomForest(data = soup, form.h, importance = TRUE, ntree = 2000, 
                         parms = list(loss = costMatrix), 
                         test = testers, strata = "week.bin",  
                         mtry = 4, classwt = c(1,1))
testers$rf = predict(soup.rf.h, testers)
confusionMatrix(testers$rf, testers$total.result)
testers$rf.p = as.data.frame(predict(soup.rf.h, testers, type = "prob"))$over
p = .65
confusionMatrix(testers$rf[which(testers$rf.p > p|testers$rf.p < 1-p)], 
                testers$total.result[which(testers$rf.p > p|testers$rf.p < 1-p)])
soup.rf.h$importance

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, search = "random")
# train a xgbTree model using caret::train

cl = makePSOCKcluster(8)
registerDoParallel(cl)
t = Sys.time()
soup.xg = train(form.h, data = soup, method = "xgbTree", trControl = fitControl)
Sys.time() - t
stopCluster(cl)

testers$mod = predict(model, testers)
testers$mod.p = predict(model, testers, type = "prob")$over
p = .6
confusionMatrix(testers$mod[which(testers$mod.p > p|testers$mod.p < 1-p)], 
                testers$total.result[which(testers$mod.p > p|testers$mod.p < 1-p)])

model.match = data.frame(rf.c = predict(soup.rf.h, testers),
                         xg.c = predict(model, testers),
                         result = testers$total.result,
                         xg = predict(model, testers, type = "prob")$over,
                         rf = as.data.frame(predict(soup.rf.h, testers, type = "prob"))$over, 
                         glm = glm.pred$p.hat.over)

model.match = model.match %>% mutate(match = rf.c==xg.c,
                       good.match = ifelse(match,rf.c==result,F))
                         #rp = predict(soup.rpart, testers)[,2])
ggplot(data = model.match, aes(x = rf, y = xg, color = good.match)) + 
  geom_point() + 
  facet_grid(match~.)

library(rpart)
library(rpart.plot)
soup.rpart = rpart(result ~ xg + rf + glm, data = model.match, control = rpart.control(cp = .02))#, minbucket = .00001*nrow(data)))
rpart.plot(soup.rpart, extra = 102)
x = data.frame(match = predict(soup.rf.h, validators)==predict(model, validators),
               result = validators$total.result,
               xg = predict(model, validators, type = "prob")$over,
               rf = as.data.frame(predict(soup.rf.h, validators, type = "prob"))$over)
confusionMatrix(predict(soup.rpart, x, type = "class"), x$result)

library(PRROC)

plot(pr.curve(scores.class0 = rowSums(model.match[,3:5]), curve = T) )

#### xgboost ----

soup.w = soup %>% 
  mutate(one = 1) %>% #mean.pts.home, mean.pts.away
  pivot_wider(names_from = week.bin, values_from = one, values_fill = 0, names_prefix = "week.") %>% 
  select(total.result, open.total, total.change, home.open, home.change, `week.[1,4)`, `week.[4,8)`,`week.[8,12)`,`week.[12,Inf)`) %>% 
  mutate(total.result.01 = ifelse(total.result=="over",1,0))

trainers.id = createDataPartition(soup.w$total.result, p = .7, list = F)
trainers.x = as.matrix(soup.w[trainers.id,] %>% select(-total.result,-total.result.01))
trainers.y = as.matrix(soup.w[trainers.id,] %>% select(total.result.01))
testers.x = as.matrix(soup.w[-trainers.id,] %>% select(-total.result.01,-total.result))
testers.y = as.matrix(soup.w[-trainers.id,] %>% select(total.result.01))

library(xgboost)

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE,
  search = "random"
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = c(0.1,0.5),
                       gamma=c(0,1,10),
                       min_child_weight = 1,
                       subsample = 1
)

# set.seed(0) 
# xgb_model = train(
#   trainers.x, trainers.y,  
#   trControl = xgb_trcontrol,
#   tuneGrid = xgbGrid,
#   method = "xgbTree",
#   metric = "Accuracy"
# )

xg = xgboost(data = soup.w, label = total.result.01, nrounds = 200,# booster = "dart", 
             trControl = xgb_trcontrol,
             tuneGrid = xgbGrid,
             params = list(objective = "binary:logistic", nthread = 8), eval_metric = "error")
confusionMatrix(factor(ifelse(predict(xg, testers.x)>.5,1,0)), factor(testers.y))

xg.rp = rpart(data = data.frame(p = predict(xg, testers.x), y = factor(testers.y)),
              y~p, control = rpart.control(cp = .02))
rpart.plot(xg.rp, extra = 108)


#### predict winners ----
form.w = formula(home.win ~ total.change.p + home.open + home.change + week + 
                   mean.pts.n.home + mean.against.n.home + mean.epa.n.home + score.lag.home + opp.score.lag.home + epa.lag.home +
                   mean.pts.n.away + mean.against.n.away + mean.epa.n.away + score.lag.away + opp.score.lag.away + epa.lag.away
)
x.glm = glm(form.w, data = soup, family = binomial())
predict(x.glm, testers, "response")
qplot(x = pred.glm, y = predict(x.glm, testers, "response"), color = factor(ifelse(predict(x.glm, testers, "response") >.5,"TRUE","FALSE"))==testers$home.win)
table(predict(caret.glm, testers), factor(ifelse(predict(x.glm, testers, "response") >.5,"TRUE","FALSE")))
confusionMatrix(factor(ifelse(predict(x.glm, testers, "response") >.6,"TRUE","FALSE")), testers$home.win)
df = data.frame(x = predict(x.glm, testers, "response"), y = testers$home.win, pred = factor(ifelse(predict(x.glm, testers, "response") >.6,"TRUE","FALSE"))) %>% 
  mutate(match = y==pred, score = x*match)
sum(df$score)
## glm
cl = makePSOCKcluster(8); registerDoParallel(cl); t = Sys.time()
caret.glm = train(form.w, data = soup, method = "glmStepAIC", 
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                                           preProcOptions = list(c("center","scale"))))
Sys.time() - t; stopCluster(cl)
caret.glm$finalModel
confusionMatrix(predict(caret.glm, testers), testers$home.win)
pred.glm = unlist(predict(caret.glm, testers, type = "prob")["TRUE"])
df = data.frame(x = pred.glm, y = testers$home.win, pred = predict(caret.glm, testers)) %>% 
  mutate(match = y==pred, score = x*match)
sum(df$score)
ggplot(df,aes(x = x)) + geom_histogram(aes(fill = y), bins = 25) + 
  facet_grid(y~.)

## rf
costMatrix = matrix(c(1,1,1,1), nrow = 2)
rf.w = randomForest(data = trainers, form.w, importance = TRUE, ntree = 2000, 
                       parms = list(loss = costMatrix), 
                       test = testers,  
                       mtry = 4, classwt = c(1,1))
rf.w$importance
testers$rf = predict(rf.w, testers)
confusionMatrix(testers$rf, testers$home.win)
pred.rf = unlist(predict(rf.w, testers, type = "prob")[,2])
df = data.frame(x = pred.rf, y = testers$home.win, pred = predict(caret.glm, testers)) %>% 
  mutate(match = y==pred, score = x*match)
sum(df$score)
ggplot(df,aes(x = x)) + geom_histogram(aes(fill = y), bins = 50) + 
  facet_grid(y~.)

## xg
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, search = "random")
# train a xgbTree model using caret::train

cl = makePSOCKcluster(8); registerDoParallel(cl); t = Sys.time()
xg.w = train(form.w, data = trainers, method = "xgbTree", trControl = fitControl)
Sys.time() - t; stopCluster(cl)

confusionMatrix(predict(soup.xg, testers), testers$home.win)
pred.xg = unlist(predict(xg.w, testers, type = "prob")[,2])
df = data.frame(x = pred.xg, y = testers$home.win, pred = predict(caret.glm, testers)) %>% 
  mutate(match = y==pred, score = x*match, bin = cut(x, seq(0,1,.05)))
sum(df$score)
df.agg = df %>% group_by(bin) %>% summarise(tf.ratio = sum(as.character(y)=="TRUE")/max(.5,sum(as.character(y)=="FALSE")))
ggplot(df,aes(x = x)) + geom_histogram(aes(fill = y), bins = 50) + 
  facet_grid(y~.)
ggplot(df.agg, aes(x = bin, y = tf.ratio)) + geom_point()

mods = data.frame(glm = pred.glm, rf = pred.rf, xg = pred.xg, 
                  home.win = testers$home.win)
library(rpart)
library(rpart.plot)
mods.rpart = rpart(data = mods, home.win~., control = rpart.control(cp = .01))
rpart.plot(mods.rpart)
