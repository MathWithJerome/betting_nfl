#### Over-Under ----
ou <- read.csv("C:/Users/Jerome/Desktop/bettingLineNFL/week1to17_OU.csv", header=FALSE, stringsAsFactors=FALSE)

ou$week = 17
for (w in 1:16) {
  w1 = min(grep(paste("NFL Week",w,"Over Under Totals"), ou$V1))
  w2 = min(grep(paste("NFL Week",w+1,"Over Under Totals"), ou$V1)) - 1
  ou$week[w1:w2] = w
}
ou$V2 = as.Date(ou$Date, "%Y-%m-%d")
ou.names = ou[2,]
ou = ou[which(!is.na(ou$V2)),]
ou$year = ifelse(months(ou$V2)=="January", as.integer(format(ou$V2, "%Y"))+1, as.integer(format(ou$V2, "%Y")))
ou = tidyr::separate(ou, V1, rep(" ",4))
names(ou) = c("home","home.score","away","away.score","date","open.total","open.result","close.total",
              "close.result","total","week","year")

ou[c(2,4,6,8,10)] = apply(ou[,c(2,4,6,8,10)], 2, FUN = as.numeric)
ou$total.change = ou$close.total - ou$open.total
ou$week.bin = cut(ou$week, c(1,4,8,12,Inf), right = FALSE)
ou = ou[which(ou$close.result!="push" & abs(ou$total.change) <= 14),]
ou$close.result = as.factor(ou$close.result)
table(ou$close.result)

#### Spread ----
#https://thefootballlines.com/nfl/week-1/point-spreads

sp <- read.csv("C:/Users/Jerome/Desktop/bettingLineNFL/spreads.csv", header=FALSE, stringsAsFactors=FALSE)

sp$week = 17
for (w in 1:16) {
  w1 = min(grep(paste("NFL Week",w), sp$V1))
  w2 = min(grep(paste("NFL Week",w+1), sp$V1)) - 1
  sp$week[w1:w2] = w
}
sp$V2 = as.Date(sp$V2, "%Y-%m-%d")
sp.names = sp[2,]
sp = sp[which(!is.na(sp$V2)),]
sp$year = ifelse(months(sp$V2)=="January", as.integer(format(sp$V2, "%Y"))+1, as.integer(format(sp$V2, "%Y")))
sp = tidyr::separate(sp, V1, rep(" ",4))
names(sp) = c("home","home.score","away","away.score","date","road.open","road.close","open.total", "home.open",
              "home.close","close.total","week","year")

sp[c(2,4,6:13)] = apply(sp[,c(2,4,6:13)], 2, FUN = as.numeric)
sp$total.change = sp$close.total - sp$open.total
sp$week.bin = cut(sp$week, c(1,4,8,12,Inf), right = FALSE)
# sp = sp[which(sp$close.result!="push" & abs(sp$total.change) <= 7),]
# sp$close.result = as.factor(sp$close.result)
# table(sp$close.result)
soup = merge(ou, sp, all.x = TRUE)
soup$home.change = soup$home.close - soup$home.open
soup$favorite.open = soup$favorite.close = "push"
soup$favorite.open[which(soup$home.open < 0)] = "home"
soup$favorite.open[which(soup$home.open > 0)] = "away"
soup$favorite.close[which(soup$home.close < 0)] = "home"
soup$favorite.close[which(soup$home.close > 0)] = "away"
soup$spread.open = abs(soup$home.open)
soup$spread.close = abs(soup$home.close)
soup$spread.change = soup$spread.open - soup$spread.close
soup$fav.change = ifelse(soup$favorite.open==soup$favorite.close, "no", "yes")
#soup$cover.result = "push"
#soup$cover.result[which(soup$home.score - soup$away.score > soup$home.close)] = "cover"

#### 538 elo ----
nfl_elo <- read_csv("nfl_elo.csv")
#### model fitting ----
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(pROC)
library(doParallel)

soup.tot = soup[which(soup$year > 2010 & soup$close.result != "push" & abs(soup$total.change) <= 10 & abs(soup$spread.change) <= 10),]
soup.tot$close.result = factor(as.character(soup.tot$close.result),levels = c("over", "under"))
z = createDataPartition(soup.tot$favorite.open, p = .63, list = FALSE)
trainers = soup.tot[z,]
testers = soup.tot[-z,]

form = formula(close.result ~ (open.total + total.change + home.open + home.change) * week.bin)
soup.nb = naiveBayes(data = trainers, close.result ~ open.total + total.change + spread.open + spread.change + week + favorite.open + fav.change)
soup.rf = randomForest(data = trainers, form, importance = TRUE, ntree = 1000)
soup.logit = glm(data = trainers, form, family = binomial(link = "logit"), 
                 weights = ifelse(trainers$close.result=="over", 1.1,1))
soup.nn = nnet(data = trainers, form, size = 20, maxit = 5000)
soup.svm = svm(data = trainers, form, cross = 10, probability = TRUE, kernel = "linear")#, degree = 4, type = "nu-classification")

cl = makePSOCKcluster(8)
registerDoParallel(cl)
fitCntrl = trainControl(method = "repeatedcv", number = 10, repeats = 10)
soup.gbm = train(data = trainers, form, method = "gbm", metric = "kappa", trControl = fitCntrl, 
                 preProcess = c("center", "scale"), weights = ifelse(trainers$close.result=="over", 1.1,1))
soup.knn = train(data = trainers, form, method = "knn", trControl = fitCntrl, preProcess = c("center", "scale"),
                 weights = ifelse(trainers$close.result=="over", 1.1,1))
soup.glm = train(data = trainers, form, method = "glm", trControl = fitCntrl, preProcess = c("center", "scale"),
                 weights = ifelse(trainers$close.result=="over", 1.1,1))
soup.rf = train(data = trainers, form, method = "rf", trControl = fitCntrl, preProcess = c("center", "scale"),
                weights = ifelse(trainers$close.result=="over", 1,1))
soup.nn = train(data = trainers, form, method = "nnet", trControl = fitCntrl, preProcess = c("center", "scale"),
                weights = ifelse(trainers$close.result=="over", 1,1))
stopCluster(cl)
#soup.x = expand.grid(seq(20, 60, 2))
pred = data.frame(
  actual = testers$close.result,
  gbm = predict(soup.gbm, testers, type = "prob")[,"over"],
  gbm.class = predict(soup.gbm, testers, type = "raw"),
  glm = predict(soup.glm, testers, type = "prob")[,"over"],
  glm.class = predict(soup.glm, testers, type = "raw"),
  knn = predict(soup.knn, testers, type = "prob")[,"over"],
  knn.class = predict(soup.knn, testers, type = "raw"),
  svm = attr(predict(soup.svm, testers, probability = TRUE),"probabilities")[,"over"],
  svm.class = predict(soup.svm, testers),
  nb = predict(soup.nb, testers, type = "raw")[,"over"],
  nb.class = predict(soup.nb, testers, type = "class"),
  rf = predict(soup.rf, testers, type = "prob")[,"over"],
  rf.class = predict(soup.rf, testers, type = "raw"),
  rf1 = predict(soup.rf1, testers, type = "prob")[,"over"],
  rf1.class = predict(soup.rf1, testers, type = "response"),
  nn = predict(soup.nn, testers, type = "prob")[,"over"],
  nn.class = predict(soup.nn, testers, type = "raw"),
  logit = predict(soup.logit, testers, type = "response"))
pred$logit.class = factor(ifelse(pred$logit < .50, "over","under"), levels = c("over","under"))

confusionMatrix(pred$logit.class, testers$close.result)
roc(testers$close.result, pred$logit, plot = TRUE, main="LOGIT")

confusionMatrix(pred$gbm.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$gbm, plot = TRUE, main = "GBM")

confusionMatrix(pred$glm.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$glm, plot = TRUE, main = "GLM")

confusionMatrix(pred$knn.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$knn, plot = TRUE, main = "KNN")

confusionMatrix(pred$svm.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$svm, plot = TRUE, main = "SVM")
#confusionMatrix(testers$close.result[which(pred$svm.class=="")], pred$prf.class[which(pred$prf > .5)])

confusionMatrix(pred$nb.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$nb, plot = TRUE, main = "NB")

confusionMatrix(pred$rf.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$rf, plot = TRUE, main = "RF")

confusionMatrix(pred$rf1.class, testers$close.result, positive = "over")
roc(testers$close.result, pred$rf1, plot = TRUE, main = "RF1")

confusionMatrix(pred$nn.class, testers$close.result)
roc(testers$close.result, pred$nn, plot = TRUE, main="NN")
# confusionMatrix(testers$close.result, 
#                 as.factor(apply(pred[,c(2,4,8,9,10)], 1, function(x) names(which.max(table(x))))))
pred$x = apply(pred[,c(2,4,6,8,10)], 1, function(x) max(x, 1-x))
#pred$x.class = ifelse(pred$x %in% pred[,c(1,3,5,7,9)], "under", "over")
pred$x.class = apply(pred[,c(2,4,6,8,10)], 1, function(x) which.max(c(x, 1-x)))
pred$x.class = as.factor(ifelse(pred$x.class <= 5, "under", "over"))
confusionMatrix(testers$total.result, pred$x.class)
roc(testers$total.result, pred$x, plot = TRUE)

#### pca stuff ----
library(caret)
library(ggplot2)
library(ggfortify)
trainers.pp = trainers[,c("open.total","total.change","home.open","home.change")] #,"close.result" "week",
pca.pp = preProcess(trainers.pp, method = c("center","scale","pca"), outcome = "close.result")
pca.pp
autoplot(prcomp(trainers.pp), data = trainers, colour = 'close.result', loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3)
pca.pp = princomp(trainers.pp)
loadings(pca.pp)
summary(pca.pp)
biplot(pca.pp)
eigen(pca.pp)

eigen(cov(trainers.pp))$values
eigen(cov(trainers.pp))$vectors
