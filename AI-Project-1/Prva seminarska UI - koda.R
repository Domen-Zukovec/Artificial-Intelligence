########################################################
#         Regresija
########################################################
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(kknn)
library(nnet)
library(CORElearn)
library(lubridate)


podatki <- read.table("dataSem1.txt",sep=",",header=T, stringsAsFactors = T)

podatki$Mesec <- (month(as.Date(podatki$datum)))
podatki$Dan <- as.factor(weekdays(as.Date(podatki$datum)))
podatki$datum <- as.factor(podatki$datum)

sel <- podatki$Dan %in% c("nedelja","sobota")

podatki$Weekend <- FALSE
podatki$Weekend[sel] <- TRUE

podatki$Weekend <- as.factor(podatki$Weekend)

sel <- podatki$Mesec %in% c(1, 2, 3)
podatki$LetniCas[sel] <- "zima"
sel <- podatki$Mesec %in% c(4, 5, 6)
podatki$LetniCas[sel] <- "spomlad"
sel <- podatki$Mesec %in% c(7, 8, 9)
podatki$LetniCas[sel] <- "poletje"
sel <- podatki$Mesec %in% c(10, 11, 12)
podatki$LetniCas[sel] <- "jesen"
podatki$LetniCas <- as.factor(podatki$LetniCas)

sel <- podatki$regija %in% c("vzhodna")
podatki_vz <- podatki[sel,]
sel <- podatki$regija %in% c("zahodna")
podatki_za <- podatki[sel,]


summary(podatki)

for(x in 1:max(podatki$stavba))
{
  sel <- podatki$stavba == x
  
  for(i in 1:nrow(podatki[sel,]))
  {
    if(i <= 20)
    {
      podatki$avgNorm_Poraba[sel][i] <- names(sort(table(podatki$norm_poraba[sel][1:i]), decreasing = T))[1]
      podatki$avgPoraba[sel][i] <- mean(podatki$poraba[sel][1:i])
    }
    else
    {
      podatki$avgNorm_Poraba[sel][i] <- names(sort(table(podatki$norm_poraba[sel][i-20:i]), decreasing = T))[1]
      podatki$avgPoraba[sel][i] <- mean(podatki$poraba[sel][i-20:i])
    }
  }
}

test1 <- podatki[podatki$Mesec %in% c(1), ] 
train1 <- podatki[podatki$Mesec %in% c(2), ]
test2 <- podatki[podatki$Mesec %in% c(1, 2), ] 
train2 <- podatki[podatki$Mesec %in% c(3), ]
test3 <- podatki[podatki$Mesec %in% c(1, 2, 3), ] 
train3 <- podatki[podatki$Mesec %in% c(4), ]
test4 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4), ] 
train4 <- podatki[podatki$Mesec %in% c(5), ]
test5 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5), ] 
train5 <- podatki[podatki$Mesec %in% c(6), ]
test6 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5, 6), ] 
train6 <- podatki[podatki$Mesec %in% c(7), ]
test7 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5, 6, 7), ] 
train7 <- podatki[podatki$Mesec %in% c(8), ]
test8 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5, 6, 7, 8), ] 
train8 <- podatki[podatki$Mesec %in% c(9), ]
test9 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9), ] 
train9 <- podatki[podatki$Mesec %in% c(10), ]
test10 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), ] 
train10 <- podatki[podatki$Mesec %in% c(11), ]
test11 <- podatki[podatki$Mesec %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), ] 
train11 <- podatki[podatki$Mesec %in% c(12), ]


#train <- subset(train, select=-c(norm_poraba))
#test <- subset(test, select=-c(norm_poraba))


### Mere za ocenjevanje ucenja v regresiji

###srednja absolutna napaka
mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}

###srednja kvadratna napaka
mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

### Relativne mere ocenjujejo model v primerjavi s trivialno predikcijo

### relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

### relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

### tabele za shranjevanje doblenih kvalitet modela
tab_mae <- matrix(ncol=11,nrow=3)
tab_rmae <- matrix(ncol=11,nrow=3)
tab_mae_vz <- matrix(ncol=11,nrow=3)
tab_rmae_vz <- matrix(ncol=11,nrow=3)
tab_mae_za <- matrix(ncol=11,nrow=3)
tab_rmae_za <- matrix(ncol=11,nrow=3)

###trivialni model

meanVal <- mean(train11$poraba)
predTrivial <- rep(meanVal, nrow(test11))
observed <- test11$poraba

mae(observed, predTrivial)
mse(observed, predTrivial)

###linearna regresija [Dela] obe regiji

sel <- podatki$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki$Mesec == i
  
  train <- podatki[sel,]
  test <- podatki[ podatki$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  #model <- lm(poraba ~  povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train)
  #model <- lm(poraba ~  leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train)
  
  model <- lm(poraba ~  avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + regija, train)
  #model <- lm(poraba ~  ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + avgNorm_Poraba + avgPoraba, train)

  predicted <- predict(model, test)
  observed <- test$poraba
  
  tab_mae[[1,i]] <- mae(observed, predicted)
  tab_rmae[[1,i]] <- rmae(observed, predicted, mean(train$poraba))
  
  print(paste0("Od 11 sem jih naredil...", i))
  

  print(mae(observed, predicted))
  #print(rmae(observed, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
###linearna regresija [Dela] vzhodna regija

sel <- podatki_vz$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki_vz$Mesec == i
  train <- podatki_vz[sel,]
  test <- podatki_vz[ podatki_vz$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  #model <- lm(poraba ~  povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train)
  #model <- lm(poraba ~  leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train)
  
  model <- lm(poraba ~  avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend, train)
  #model <- lm(poraba ~  ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + avgNorm_Poraba + avgPoraba, train)
  
  predicted <- predict(model, test)
  observed <- test$poraba
  
  tab_mae_vz[[1,i]] <- mae(observed, predicted)
  tab_rmae_vz[[1,i]] <- rmae(observed, predicted, mean(train$poraba))
  
  print(paste0("Od 11 sem jih naredil...", i))
  
  
  print(mae(observed, predicted))
  #print(rmae(observed, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
###linearna regresija [Dela] zahodna regija

sel <- podatki_za$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki_za$Mesec == i
  
  train <- podatki_za[sel,]
  test <- podatki_za[ podatki_za$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  #model <- lm(poraba ~  povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train)
  #model <- lm(poraba ~  leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train)
  
  model <- lm(poraba ~  avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend, train)
  #model <- lm(poraba ~  ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + avgNorm_Poraba + avgPoraba, train)
  
  predicted <- predict(model, test)
  observed <- test$poraba
  
  tab_mae_za[[1,i]] <- mae(observed, predicted)
  tab_rmae_za[[1,i]] <- rmae(observed, predicted, mean(train$poraba))
  
  print(paste0("Od 11 sem jih naredil...", i))
  
  
  print(mae(observed, predicted))
  #print(rmae(observed, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}

### regresijsko drevo [Dela] obe regiji
sel <- podatki$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki$Mesec == i
  train <- podatki[sel,]
  test <- podatki[ podatki$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  
  #rt.model <- rpart(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, data=train)
  #rt.model <- rpart(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, data=train)
  
  rt.model <- rpart(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas + regija, data=train)
  #rt.model <- rpart(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, data=train)
  
  predicted <- predict(rt.model, test)
  observed <- test$poraba
  
  tab_mae[[2,i]] <- mae(observed, predicted)
  tab_rmae[[2,i]] <- rmae(observed, predicted, mean(train$poraba))
  print(paste0("Od 11 sem jih naredil...", i))
  
  print(mae(test$poraba, predicted))
  #print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
### regresijsko drevo [Dela] vzhodna regija
sel <- podatki_vz$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki_vz$Mesec == i
  train <- podatki_vz[sel,]
  test <- podatki_vz[ podatki_vz$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  
  #rt.model <- rpart(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, data=train)
  #rt.model <- rpart(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, data=train)
  
  rt.model <- rpart(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas + regija, data=train)
  #rt.model <- rpart(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, data=train)
  
  predicted <- predict(rt.model, test)
  observed <- test$poraba
  
  tab_mae_vz[[2,i]] <- mae(observed, predicted)
  tab_rmae_vz[[2,i]] <- rmae(observed, predicted, mean(train$poraba))
  print(paste0("Od 11 sem jih naredil...", i))
  
  print(mae(test$poraba, predicted))
  #print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
### regresijsko drevo [Dela] zahodna regija
sel <- podatki_za$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki_za$Mesec == i
  train <- podatki_za[sel,]
  test <- podatki_za[ podatki_za$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  
  #rt.model <- rpart(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, data=train)
  #rt.model <- rpart(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, data=train)
  
  rt.model <- rpart(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas, data=train)
  #rt.model <- rpart(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, data=train)
  
  predicted <- predict(rt.model, test)
  observed <- test$poraba
  
  tab_mae_za[[2,i]] <- mae(observed, predicted)
  tab_rmae_za[[2,i]] <- rmae(observed, predicted, mean(train$poraba))
  print(paste0("Od 11 sem jih naredil...", i))
  
  print(mae(test$poraba, predicted))
  #print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}

### do boljsega rezultat lahko pridemo z usreznim rezanjem drevesa [rab full full dolg cajta...ampak dobiš dobr rezultat (screenshot)]

### najprej zgradimo veliko drevo (nastavitev cp=0)
rt.model <- rpart(poraba ~ Mesec+Dan+Weekend+leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+avgPoraba+avgNorm_Poraba, data=train, cp=0)
rpart.plot(rt.model)

### rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
tab <- printcp(rt.model)

### izberemo vrednost parametra cp, ki ustreza minimalni napaki internega presnega preverjanja
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

### porezemo drevo z izbrano nastavitvijo
rt.model <- prune(rt.model, cp=th)
rpart.plot(rt.model)

predicted <- predict(rt.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))

### nakljucni gozd (runna po več minut zato samo po eno na enkrat)

#rf.model <- randomForest(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train1)
#rf.model <- randomForest(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train1)

rf.model <- randomForest(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas + regija, train11)
#rf.model <- randomForest(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, train5)

predicted <- predict(rf.model, test11)
mae(test11$poraba, predicted)
rmae(test11$poraba, predicted, mean(train11$poraba))

### svm ne runnat ker rab več kot 1h !!

svm.model <- svm(poraba ~ Mesec+Dan+Weekend+leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+avgPoraba+avgNorm_Poraba, train)
predicted <- predict(svm.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))

### k-najblizjih sosedov (rab relativno uredi cajta)

knn.model <- kknn(poraba ~ Mesec+Dan+Weekend+leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+avgPoraba+avgNorm_Poraba, train3, test3, k = 5)
predicted <- fitted(knn.model)
mae(test3$poraba, predicted)
rmae(test3$poraba, predicted, mean(train3$poraba))

### k-najblizjih sosedov (rab relativno uredi cajta)  funkcija čez vse mesece na obeh regijah
sel <- podatki$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki$Mesec == i
  train <- podatki[sel,]
  test <- podatki[ podatki$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  #knn.model <- kknn(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train, test, k = 5)
  #knn.model <- kknn(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train, test, k = 5)
  
  knn.model <- kknn(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas + regija, train, test, k = 5)
  #knn.model <- kknn(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, train, test, k = 5)
  
  predicted <- fitted(knn.model)
  observed <- test$poraba
  
  tab_mae[[3,i]] <- mae(observed, predicted)
  tab_rmae[[3,i]] <- rmae(observed, predicted, mean(train$poraba))
  print(paste0("Od 11 sem jih naredil...", i))
  
  #print(mae(test$poraba, predicted))
  #print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
### k-najblizjih sosedov (rab relativno uredi cajta)  funkcija čez vse mesece na vzhodni regiji
sel <- podatki_vz$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki_vz$Mesec == i
  train <- podatki_vz[sel,]
  test <- podatki_vz[ podatki_vz$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  #knn.model <- kknn(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train, test, k = 5)
  #knn.model <- kknn(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train, test, k = 5)
  
  knn.model <- kknn(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas, train, test, k = 5)
  #knn.model <- kknn(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, train, test, k = 5)
  
  predicted <- fitted(knn.model)
  observed <- test$poraba
  
  tab_mae_vz[[3,i]] <- mae(observed, predicted)
  tab_rmae_vz[[3,i]] <- rmae(observed, predicted, mean(train$poraba))
  print(paste0("Od 11 sem jih naredil...", i))
  
  #print(mae(test$poraba, predicted))
  #print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
### k-najblizjih sosedov (rab relativno uredi cajta)  funkcija čez vse mesece na zahodni regiji
sel <- podatki_za$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki_za$Mesec == i
  train <- podatki_za[sel,]
  test <- podatki_za[ podatki_za$Mesec == i+1,]
  train <- subset(train, select=-c(norm_poraba))
  test <- subset(test, select=-c(norm_poraba))
  
  #knn.model <- kknn(poraba ~ povrsina + leto_izgradnje + stavba + namembnost + ura + temp_rosisca + regija, train, test, k = 5)
  #knn.model <- kknn(poraba ~ leto_izgradnje+namembnost+povrsina+ura+stavba+temp_rosisca+regija+temp_zraka+oblacnost+padavine+pritisk+smer_vetra+hitrost_vetra, train, test, k = 5)
  
  knn.model <- kknn(poraba ~ avgPoraba + povrsina + avgNorm_Poraba + leto_izgradnje + stavba + namembnost + ura + Dan + Weekend + LetniCas, train, test, k = 5)
  #knn.model <- kknn(poraba ~ ura + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + Mesec + Dan + Weekend + LetniCas + avgNorm_Poraba + avgPoraba, train, test, k = 5)
  
  predicted <- fitted(knn.model)
  observed <- test$poraba
  
  tab_mae_za[[3,i]] <- mae(observed, predicted)
  tab_rmae_za[[3,i]] <- rmae(observed, predicted, mean(train$poraba))
  print(paste0("Od 11 sem jih naredil...", i))
  
  #print(mae(test$poraba, predicted))
  #print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}
tab_mae
tab_rmae
tab_mae_vz
tab_rmae_vz
tab_mae_za
tab_rmae_za


#
### OCENJEVANJE ATRIBUTOV
#


### Izbira podmnozice atributov s filter metodo
### atribute ocenimo z neko mero

sort(attrEval(poraba ~ ., train, "MSEofMean"), decreasing = TRUE)
sort(attrEval(poraba ~ ., train, "RReliefFexpRank"), decreasing = TRUE) # preveč cajta !
 
### model zgradimo s pomocjo nekaj najbolje ocenjenih

modelReduced <- train.kknn(poraba ~ avgPoraba + povrsina, train3, ks=5)
predicted <- predict(modelReduced, test3)
observed <- test3$poraba
print(mae(test3$poraba, predicted))
print(rmae(test3$poraba, predicted, mean(train3$poraba)))
#print(mse(observed, predicted))
print(rmse(observed, predicted, mean(train$poraba)))


#še z funkcijo

sel <- podatki$Mesec == 1
for(i in 1:11){
  sel <- sel | podatki$Mesec == i
  train <- podatki[sel,]
  test <- podatki[ podatki$Mesec == i+1,]
  
  modelReduced <- train.kknn(poraba ~ avgPoraba + povrsina + leto_izgradnje + stavba, train, ks=5)
  predicted <- predict(modelReduced, test)
  observed <- test$poraba
  
  print(mae(test$poraba, predicted))
  print(rmae(test$poraba, predicted, mean(train$poraba)))
  #print(mse(observed, predicted))
  #print(rmse(observed, predicted, mean(train$poraba)))
}

######################### Domen Žukovec ################

########################################################
#         Regresija Konec
########################################################

########################################################
#         Klasifikacija
########################################################


####### DODAJANJE PODATKOV 

library(CORElearn)
library(rpart)
library(kernlab)
library(e1071)
library(nnet)
library(randomForest)
library(lubridate)

podatki <- read.table("dataSem1.txt",sep=",",header=T, stringsAsFactors = T)


################ PRIPRAVA 

brierRez <- matrix(ncol=11,nrow=3)
caRez <- matrix(ncol=11,nrow=3)

########  FUNKCIJI CA & BRIER SCORE

CA <- function(observed, predicted)
{
  return(mean(observed == predicted))
}


brier.score <- function(observedMatrix, predictedMatrix)
{
  return(sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix))
}

################ VIZUALIZACIJA

pie(table(podatki$regija), names(table(podatki$regija)))
pie(table(podatki$namembnost), names(table(podatki$namembnost)))

plot(podatki$namembnost,podatki$poraba)
plot(podatki$ura,podatki$poraba)

plot(podatki$Mesec, podatki$poraba, type = "h")
plot(podatki$Dan, podatki$poraba)
plot(podatki$Weekend, podatki$poraba)
plot(podatki$LetniCas, podatki$poraba)
pie(table(podatki$avgNorm_Poraba))


################ OCENJEVANJE ORIGINALNIH PODATKOV (Klasifikacijska sprem.)

sort(attrEval(norm_poraba ~ .,podatki,"MDL"), decreasing=T)
sort(attrEval(norm_poraba ~ .,podatki,"InfGain"), decreasing=T)
sort(attrEval(norm_poraba ~ .,podatki,"ReliefFequalK"), decreasing=T)

################


################ DODAJANJE ATRIBUTOV


######## POVPRECNA PORABA IN POVPRECNA NORM PORABA

for(x in 1:max(podatki$stavba))
{
  sel <- podatki$stavba == x
  
  for(i in 1:nrow(podatki[sel,]))
  {
    if(i <= 20)
    {
      podatki$avgNorm_Poraba[sel][i] <- names(sort(table(podatki$norm_poraba[sel][1:i]), decreasing = T))[1]
      podatki$avgPoraba[sel][i] <- mean(podatki$poraba[sel][1:i])
    }
    else
    {
      podatki$avgNorm_Poraba[sel][i] <- names(sort(table(podatki$norm_poraba[sel][i-20:i]), decreasing = T))[1]
      podatki$avgPoraba[sel][i] <- mean(podatki$poraba[sel][i-20:i])
    }
  }
}

############ ATRIBUT DAN

podatki$Dan <- as.factor(weekdays(as.Date(podatki$datum)))

############ ATRIBUT MESEC

podatki$Mesec <- as.factor(month(as.Date(podatki$datum)))

############ ATRIBUT VIKEND 

sel <- podatki$Dan %in% c("Saturday","Sunday")

podatki$Weekend <- FALSE
podatki$Weekend[sel] <- TRUE

podatki$Weekend <- as.factor(podatki$Weekend)


############ Letni Casi 

podatki$LetniCas <- "nedoloceno"
sel <- podatki$Mesec %in% c(1,2, 3)
podatki$LetniCas[sel] <- "Zima"
sel <- podatki$Mesec %in% c(4,5, 6)
podatki$LetniCas[sel] <- "Pomlad"
sel <- podatki$Mesec %in% c(7,8, 9)
podatki$LetniCas[sel] <- "Poletje"
sel <- podatki$Mesec %in% c(10,11, 12)
podatki$LetniCas[sel] <- "Jesen"
podatki$LetniCas <- as.factor(podatki$LetniCas)

############


################### KLASIFIKACIJSKE FUNKCIJE


####### CORE Model Tree

treeFunc <- function(train,test,observed)
  
{

  tree <- CoreModel(norm_poraba ~  avgNorm_Poraba + leto_izgradnje + namembnost + povrsina + Dan + Mesec + Weekend + stavba, data = train, model = "tree")
  predicted <- predict(tree,test,type="class")
  predMat <- predict(tree,test,type="prob")
  rez <- c(CA(observed,predicted),brier.score(obsMat,predMat))
  return(rez)
}

####### Core Model Random Forest

rfFunc <- function(train,test,observed)
  
{

  rf <- CoreModel(norm_poraba ~  avgNorm_Poraba + leto_izgradnje + namembnost + povrsina + Dan + Mesec + Weekend + stavba ,data = train,model = "rf")
  predicted <- predict(rf,test,type="class")
  predMat <- predict(rf,test,type="prob")
  rez <- c(CA(observed,predicted),brier.score(obsMat,predMat))
  return(rez)
  
}

####### Core Model nb

nbFunc <- function(train,test,observed)
  
{
  
  nb <- CoreModel(norm_poraba ~ avgNorm_Poraba + leto_izgradnje + namembnost + povrsina + Dan + Mesec + Weekend + stavba,data = train,model = "bayes")
  predicted <- predict(nb,test,type="class")
  predMat <- predict(nb,test,type="prob")
  rez <- c(CA(observed,predicted),brier.score(obsMat,predMat))
  return(rez)  
}

##################



################## EVALVACIJA 

sel <- podatki$Mesec == 1
for(i in 1:11)
{
  set.seed(0)
  sel <- sel | podatki$Mesec == i
  
  train <- podatki[sel,]
  test <- podatki[podatki$Mesec == i+1,]
  observed <- test$norm_poraba
  obsMat <- class.ind(test$norm_poraba)
  
  rez <- treeFunc(train,test,observed)
  caRez[[1,i]] <- rez[1]
  brierRez[[1,i]] <- rez[2]
  print("Done tree")
  rez <- rfFunc(train,test,observed)
  caRez[[2,i]] <- rez[1]
  brierRez[[2,i]] <- rez[2]
  print("Done rf")
  
  rez <- nbFunc(train,test,observed)
  caRez[[3,i]] <- rez[1]
  brierRez[[3,i]] <- rez[2]
  print("Done nb")
  
  gc()
}


########## SELECTION ZA VZHODNA/ZAHODNA

sel <- podatki$regija %in% c("vzhodna")
podatki_vz <- podatki[sel,]
sel <- podatki$regija %in% c("zahodna")
podatki_za <- podatki[sel,]

################ SAMO ZA VZHODNO
sel <- podatki_vz$Mesec == 1
for(i in 1:11)
{
  set.seed(0)
  sel <- sel | podatki_vz$Mesec == i
  
  train <- podatki_vz[sel,]
  test <- podatki_vz[podatki_vz$Mesec == i+1,]
  observed <- test$norm_poraba
  obsMat <- class.ind(test$norm_poraba)
  
  rez <- treeFunc(train,test,observed)
  caRez[[1,i]] <- rez[1]
  brierRez[[1,i]] <- rez[2]
  print("Done tree")
  rez <- rfFunc(train,test,observed)
  caRez[[2,i]] <- rez[1]
  brierRez[[2,i]] <- rez[2]
  print("Done rf")
  
  rez <- nbFunc(train,test,observed)
  caRez[[3,i]] <- rez[1]
  brierRez[[3,i]] <- rez[2]
  print("Done nb")
  
  gc()
}

################ SAMO ZA ZAHODNO


sel <- podatki_za$Mesec == 1
for(i in 1:11)
{
  set.seed(0)
  sel <- sel | podatki_za$Mesec == i
  
  train <- podatki_za[sel,]
  test <- podatki_za[podatki_za$Mesec == i+1,]
  observed <- test$norm_poraba
  obsMat <- class.ind(test$norm_poraba)
  
  rez <- treeFunc(train,test,observed)
  caRez[[1,i]] <- rez[1]
  brierRez[[1,i]] <- rez[2]
  print("Done tree")
  rez <- rfFunc(train,test,observed)
  caRez[[2,i]] <- rez[1]
  brierRez[[2,i]] <- rez[2]
  print("Done rf")
  
  rez <- nbFunc(train,test,observed)
  caRez[[3,i]] <- rez[1]
  brierRez[[3,i]] <- rez[2]
  print("Done nb")
  
  gc()
}
##################### DRAGAN SPASOVSKI ########################

########################################################
#         Konec Klasifikacije
########################################################