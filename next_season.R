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

#### prior years ----
#soup = soup[which(soup$year > 2007 & abs(soup$total.change) <= 10 & abs(soup$spread.change) <= 10),]

trainers = soup[which(soup$season %in% 2012:2020),]
testers = soup[which(soup$season %in% 2020),]

trainers.id = createDataPartition(soup$season, p = .6, list = F)
trainers = soup[trainers.id,]
testers = soup[-trainers.id,]

#trainers$total.change.p.downHalf = trainers$total.change.p.upHalf = trainers$total.change.p

form.h = formula(total.result ~ (open.total + total.change + home.open + home.change + mean.pts.home + mean.pts.away) * week.bin)
form.s = formula(total.result ~ total.change.p)
costMatrix = matrix(c(0,3,1,0), nrow = 2)

#soup.nb = naiveBayes(data = trainers, total.result ~ open.total + total.change + spread.open + spread.change + week + favorite.open + fav.change)
soup.rf.h = randomForest(data = trainers, form.h, importance = TRUE, ntree = 2000, parms = list(loss = costMatrix))
soup.rf.s = randomForest(data = trainers, form.s, importance = TRUE, ntree = 2000)
soup.logit.h = glm(data = trainers, form.h, family = binomial(link = "logit"), weights = ifelse(trainers$total.result=="over", 1,1))
soup.nn.h = nnet(data = trainers, form.h, size = 30, maxit = 2000, parms = list(loss = costMatrix))
soup.svm.h = svm(data = trainers, form.h, cross = 10, probability = TRUE, degree = 4, type = "nu-classification", parms = list(loss = costMatrix))

importance(soup.rf.h)

pred.h = testers %>% transmute(
  week.bin = testers$week.bin,
  actual = testers$total.result,
  rf = predict(soup.rf.h, testers, type = "prob")[,"over"],
  rf.class = predict(soup.rf.h, testers, type = "response"),
  logit = predict(soup.logit.h, testers, type = "response"),
  logit.class = factor(ifelse(logit < .50, "over","under"), levels = c("over","under")),
  #rf.s = predict(soup.rf.s, testers, type = "prob")[,"over"],
  #rf.s.class = predict(soup.rf.s, testers, type = "response"),
  nn = 1-predict(soup.nn.h, testers, type = "raw")[,1], #[,"over"],
  nn.class = factor(predict(soup.nn.h, testers, type = "class"), levels = c("over","under")),
  svm = attr(predict(soup.svm.h, testers, probability = TRUE),"probabilities")[,"over"],
  svm.class = predict(soup.svm.h, testers)
)

confusionMatrix(pred.h$rf.class[which(pred.h$rf > .6 | pred.h$rf < .4 )], pred.h$actual[which(pred.h$rf > .6 | pred.h$rf < .4)], positive = "over")
confusionMatrix(pred.h$nn.class[which(pred.h$nn > .95 | pred.h$nn < .05 )], pred.h$actual[which(pred.h$nn > .95 | pred.h$nn < .05)], positive = "over")
confusionMatrix(pred.h$svm.class[which(pred.h$svm > .6 | pred.h$svm < .4 )], pred.h$actual[which(pred.h$svm > .6 | pred.h$svm < .4)], positive = "over")
confusionMatrix(pred.h$logit.class[which(pred.h$logit > .6 | pred.h$logit < .4 )], pred.h$actual[which(pred.h$logit > .6 | pred.h$logit < .4 )], positive = "over")

pred.h$rf.logit = (pred.h$rf + 1 - pred.h$logit)/2
pred.h$rf.logit.class = factor(ifelse(pred.h$rf.logit > .5, "over", "under"))
confusionMatrix(pred.h$rf.logit.class[which(pred.h$rf.logit > .6 | pred.h$rf.logit < .4 )], pred.h$actual[which(pred.h$rf.logit > .6 | pred.h$rf.logit < .4 )], positive = "over")

roc(pred.h$actual, pred.h$svm, plot = TRUE, main = "SVM")
roc(pred.h$actual, pred.h$rf, plot = TRUE, main = "RF")
roc(pred.h$actual, pred.h$nn, plot = TRUE, main = "NN")
roc(pred.h$actual, pred.h$logit, plot = TRUE, main = "LOGIT")

pred.m.h = melt(pred.h[,c("actual","rf","nn","svm","week.bin")], id.vars = c("actual","week.bin"))
ggplot(pred.m.h) + geom_point(aes(x = value, y = variable, color = actual)) + facet_grid(week.bin ~.)
#ggplot(pred.m.h) + geom_point(aes(x = value, y = actual, color = variable))

trainers.sub = trainers#[which(trainers$week.bin=="[8,12)"),]
cl = makePSOCKcluster(8)
registerDoParallel(cl)
fitCntrl = trainControl(method = "repeatedcv", number = 10, repeats = 10)
soup.gbm = train(data = trainers.sub, form.h, method = "gbm", trControl = fitCntrl, 
                 preProcess = c("center", "scale", "pca"), weights = ifelse(trainers$total.result=="over", 1,1))
soup.knn = train(data = trainers.sub, form.h, method = "knn", trControl = fitCntrl, preProcess = c("center", "scale"),
                 weights = ifelse(trainers$total.result=="over", 1,1))
soup.glm = train(data = trainers.sub, form.h, method = "glm", trControl = fitCntrl, preProcess = c("center", "scale"),
                 weights = ifelse(trainers$total.result=="over", 1,1))
soup.rfc = train(data = trainers.sub, form.h, method = "rf", trControl = fitCntrl, preProcess = c("center", "scale"),
                 weights = ifelse(trainers$total.result=="over", 1,1))
soup.nb = train(data = trainers.sub, form.h, method = "nb", trControl = fitCntrl, preProcess = c("center", "scale"),
                 weights = ifelse(trainers$total.result=="over", 1,1))
t1 = Sys.time()
soup.dnn = train(data = trainers, form.h, method = "dnn", trControl = fitCntrl, preProcess = c("center", "scale"),
                weights = ifelse(trainers$total.result=="over", 1,1))
t2 = Sys.time()
t2-t1
stopCluster(cl)

pred = testers %>% mutate(
  actual = testers$total.result,
  gbm = predict(soup.gbm, testers, type = "prob")[,"over"],
  gbm.class = predict(soup.gbm, testers, type = "raw"),
  glm = predict(soup.glm, testers, type = "prob")[,"over"],
  glm.class = predict(soup.glm, testers, type = "raw"),
  knn = predict(soup.knn, testers, type = "prob")[,"over"],
  knn.class = predict(soup.knn, testers, type = "raw"),
  #svm = attr(predict(soup.svm, testers, probability = TRUE),"probabilities")[,"over"],
  #svm.class = predict(soup.svm, testers),
  #nb = predict(soup.nb, testers, type = "prob")[,"over"],
  #nb.class = predict(soup.nb, testers, type = "raw"),
  rfc = predict(soup.rf.h, testers, type = "prob")[,"over"],
  rfc.class = predict(soup.rf.h, testers, type = "response"),
  rf = predict(soup.rfc, testers, type = "prob")[,"over"],
  rf.class = predict(soup.rfc, testers, type = "raw"),
  #rf1 = predict(soup.rf1, testers, type = "prob")[,"over"],
  #rf1.class = predict(soup.rf1, testers, type = "response"),
  #rf2 = predict(soup.rf2, testers, type = "prob")[,"over"],
  #rf2.class = predict(soup.rf2, testers, type = "response"),
  #nn = 1-(predict(soup.nn, testers, type = "raw")[,1]), #[,"over"],
  #nn.class = factor(predict(soup.nn, testers, type = "class"), levels = c("over","under")),
  dnn = predict(soup.dnn, testers, type = "prob")[,"over"],
  dnn.class = predict(soup.dnn, testers, type = "raw"),
  #logit = predict(soup.logit, testers, type = "response"),
  #logit.class = factor(ifelse(logit > .50, "over","under"), levels = c("over","under"))
)

pred.m = melt(pred[,c("actual","rf","rfc","nn","dnn","gbm","knn","svm","nb","logit")], id.vars = "actual")
ggplot(pred.m) + geom_jitter(aes(x = value, y = variable, color = actual), alpha = .6)

confusionMatrix(pred$logit.class, pred$actual)
#roc(testers$total.result, pred$logit, plot = TRUE, main="LOGIT")
confusionMatrix(pred$gbm.class, pred$actual, positive = "over")
#roc(testers$total.result, pred$gbm, plot = TRUE, main = "GBM")
confusionMatrix(pred$glm.class, pred$actual, positive = "over")
#roc(pred$actual, pred$glm, plot = TRUE, main = "GLM")
confusionMatrix(pred$knn.class[which(pred$knn < .5)], pred$actual[which(pred$knn < .5)], positive = "over")
#roc(pred$actual, pred$knn, plot = TRUE, main = "KNN")
confusionMatrix(pred$svm.class, pred$actual, positive = "over")
#roc(pred$actual, pred$svm, plot = TRUE, main = "SVM")
confusionMatrix(pred$nb.class, pred$actual, positive = "over")
#roc(pred$actual, pred$nb, plot = TRUE, main = "NB")
#View(cbind(as.character(pred$actual), round(as.numeric(pred$rf),3))) [which(pred$rf < .25 | pred$rf > .75)]
confusionMatrix(pred$rf.class, pred$actual, positive = "over")
#roc(pred$actual, pred$rf, plot = TRUE, main = "RF")
confusionMatrix(pred$rfc.class, pred$actual, positive = "over")
#roc(pred$actual, pred$rf1, plot = TRUE, main = "RF1")
confusionMatrix(pred$rf2.class, pred$actual, positive = "over")
#roc(pred$actual, pred$rf2, plot = TRUE, main = "RF2")
confusionMatrix(pred$nn.class, pred$actual)
confusionMatrix(pred$nn.class[which(pred$nn < .2)], pred$actual[which(pred$nn < .2)])
#roc(pred$actual, pred$nn, plot = TRUE, main="NN")
library(Ckmeans.1d.dp)
pred$dnn.clust = Ckmeans.1d.dp(pred$dnn, c(2,4))$cluster
  #factor(as.numeric(as.factor(kmeans(pred$dnn, 3)$cluster)))
confusionMatrix(pred$dnn.class, pred$actual)
dnn.min = list()
for (i in 2:length(unique(pred$dnn.clust))) {
  dnn.min[i] = min(pred$dnn[which(pred$dnn.clust==as.character(i))])
  #print(confusionMatrix(pred$dnn.class[which(pred$dnn.clust == i)], pred$actual[which(pred$dnn.clust == i)]))
}

#### ensemble ----
roc(pred$actual, pred$dnn, plot = TRUE, main="DNN")
ggplot(data = pred) + geom_jitter(aes(x = dnn, y = actual, color = factor(dnn.clust)))

soup.ens = glm(data = pred, actual ~ (svm + gbm + knn + nb + glm + knn + rf + nn + open.total + total.change + home.open + home.change + week) * week.bin,
               family = binomial(link = "logit"))
soup.ens = step(soup.ens, direction = "forward")
summary(soup.ens)
# confusionMatrix(factor(ifelse(predict(soup.ens, validaters, type = "response") < .5, "over","under"), levels = c("over","under")), 
#                 validaters$total.result)
costMatrix = matrix(c(0,1,1,0), nrow = 2)
#soup.rpart.ens = rpart(data = pred, actual ~ (svm + gbm + knn + nb + glm + knn + rfc + dnn + open.total + total.change + home.open + home.change + week.bin),
#                       control = rpart.control(minsplit = 15, minbucket = .1*nrow(pred),  cp = 0.025), parms = list(loss = costMatrix))

soup.rpart.ens = list()
for (i in 2:length(unique(pred$dnn.clust))) {
  soup.rpart.ens[[i]] = rpart(data = pred[which(pred$dnn.clust == i),], actual ~ (svm + gbm + knn + nb + glm + nn + rf + week.bin),
                       control = rpart.control(minsplit = 15, minbucket = .1*nrow(pred),  cp = 0.025), parms = list(loss = costMatrix))
  rpart.plot(soup.rpart.ens[[i]], extra = 102)
}
rpart.plot(soup.rpart.ens[[1]], extra = 102)
rpart.plot(soup.rpart.ens[[2]], extra = 102)
rpart.plot(soup.rpart.ens[[3]], extra = 102) 
rpart.plot(soup.rpart.ens[[4]], extra = 102)
#pred$rpart = predict(soup.rpart.ens, pred, type = "class")
#confusionMatrix(pred$rpart, pred$actual)

val = validaters %>% 
  mutate(
    actual = validaters$total.result,
    gbm = predict(soup.gbm, validaters, type = "prob")[,"over"],
    gbm.class = predict(soup.gbm, validaters, type = "raw"),
    glm = predict(soup.glm, validaters, type = "prob")[,"over"],
    glm.class = predict(soup.glm, validaters, type = "raw"),
    knn = predict(soup.knn, validaters, type = "prob")[,"over"],
    knn.class = predict(soup.knn, validaters, type = "raw"),
    svm = attr(predict(soup.svm, validaters, probability = TRUE),"probabilities")[,"over"],
    svm.class = predict(soup.svm, validaters),
    nb = predict(soup.nb, validaters, type = "prob")[,"over"],
    nb.class = predict(soup.nb, validaters, type = "raw"),
    rf = predict(soup.rf.h, validaters, type = "prob")[,"over"],
    rf.class = predict(soup.rf.h, validaters, type = "response"),
    rfc = predict(soup.rf, validaters, type = "prob")[,"over"],
    rfc.class = predict(soup.rf, validaters, type = "raw"),
    rf1 = predict(soup.rf1, validaters, type = "prob")[,"over"],
    rf1.class = predict(soup.rf1, validaters, type = "response"),
    rf2 = predict(soup.rf2, validaters, type = "prob")[,"over"],
    rf2.class = predict(soup.rf2, validaters, type = "response"),
    dnn = predict(soup.dnn, validaters, type = "prob")[,"over"],
    dnn.class = predict(soup.dnn, validaters, type = "raw"),
    nn = 1-predict(soup.nn, validaters, type = "raw")[,1], #[,"over"],
    nn.class = factor(predict(soup.nn, validaters, type = "class")),
    logit = predict(soup.logit, validaters, type = "response")
  )

val$dnn.clust = 1
val$ens.pred = NA
for (i in 2:length(unique(pred$dnn.clust))) {
  val$dnn.clust[which(val$dnn >= dnn.min[i])] = i
  dnn.min[i] = min(pred$dnn[which(pred$dnn.clust==as.character(i))])
  val$ens.pred[which(val$dnn.clust==i)] = as.character(predict(soup.rpart.ens[[i]], val[which(val$dnn.clust==i),], type = "class"))
}
val$dnn.clust = factor(val$dnn.clust)


val$ens.pred = as.factor(val$ens.pred)
table(val$ens.pred)
ggplot(data = val) + geom_jitter(aes(x = dnn, y = actual, color = dnn.clust))
confusionMatrix(as.factor(val$ens.pred[which(val$week.bin=="[1,4)")]), val$actual[which(val$week.bin=="[1,4)")])
confusionMatrix(val$ens.pred[which(val$dnn.clust==1)], val$actual[which(val$dnn.clust==1)])
confusionMatrix(val$ens.pred[which(val$dnn.clust==2)], val$actual[which(val$dnn.clust==2)])
confusionMatrix(val$ens.pred[which(val$dnn.clust==3)], val$actual[which(val$dnn.clust==3)])
table(val$ens.pred, val$actual)
confusionMatrix(factor(val$nn.class[which(val$nn > .99 & val$svm < .53)], levels = c("over","under")), val$total.result[which(val$nn > .99 & val$svm < .53)])

predict(soup.rpart.ens, val, type = "class")

confusionMatrix(predict(soup.rpart.ens, val, type = "class"), val$total.result)

x =  #1:nrow(val)
  which(val$nb <= .041)
confusionMatrix(predict(soup.rpart.ens, val[x,], type = "class"), val$actual[x])
View(validaters)


#### sweet spot accuraccy ----
## rework so be week and year to translate to actual weekly betting
m.o = length(testers.1$logit.class)
acc = array()
for (y in 1:2) {
  for (w in 1:3) {
    tmp = testers.1[which(testers.1$year==(2016+y) & testers.1$week==w),]
    tmp = tmp[order(tmp$logit.prob, decreasing = TRUE),]
    for (l in 1:(nrow(tmp)-1)) {
      for (r in (l+1):nrow(tmp)) {
        cm = confusionMatrix(tmp$logit.class[l:r], tmp$total.result[l:r])
        acc = rbind(acc, data.frame(l = l, r = r, acc = cm$overall["Accuracy"]))
      }
    }
    print(w)
  }
} 

acc = acc[-1,]
acc.agg = aggregate(data = acc, acc ~ l + r, FUN = function(x) {prod(x)^(1/length(x))})
acc.agg$ev = (acc.agg$r-acc.agg$l+1)*acc.agg$acc  # E[X] = np
acc.agg$var = (acc.agg$r-acc.agg$l+1)*acc.agg$acc*(1-acc.agg$acc)
acc.agg$cov = sqrt(acc.agg$var)/acc.agg$ev

ggplot(data = acc.agg, aes(x = r, y = l)) + geom_raster(aes(fill = cov), interpolate = TRUE)
ggplot(data = acc.agg) + geom_line(aes(x = l, y = cov)) + facet_grid(r~.)

ggplot(testers[which(testers$week.bin=="[1,4)"),]) + geom_point(aes(x = logit.prob, y = total.result))
logit.roc = roc(testers$total.result[which(testers$week.bin=="[1,4)")], testers$logit.prob[which(testers$week.bin=="[1,4)")], plot = TRUE, main="RF")
roc(testers$total.result[which(testers$week.bin=="[1,4)")], testers$rf.prob[which(testers$week.bin=="[1,4)")], plot = TRUE, main="LOGIT")
costMatrix = matrix(c(0,1,1,0), nrow = 2)

