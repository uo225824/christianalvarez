train<-read.csv("train.csv",header=T)
test<-read.csv("test.csv",header=T)
datos2<-read.csv("gender_submission.csv",header=T)
names(train)
summary(train)
plot(train)

#set2
train2<-train[-c(1,4,11)]
train2$Survived<-factor(train$Survived)
trainFinal$Survived<-factor(train$Survived)


set.seed(12345) 
ii <- sample(1:nrow(train2), 625) # muestra de 80 números entre 1 y 100
train2<-train2[ii,]
test2 <- train2[-ii, ] # muestra de entrenamiento


library(adabag)

# Analisis A priori
library(arules) # carga el paquete arules, que debe estar instalado previamente
library(car)

trainf<-train2


trainf$Survived<-factor(trainf$Survived)
trainf$Pclass<-factor(trainf$Pclass)
trainf$SibSp<-factor(trainf$SibSp)
trainf$Parch<-factor(trainf$Parch)
trainf$Age <- recode(trainf$Age, ' lo:18="menor"; 18:hi="mayor" ', as.factor=T)
q3<-quantile(trainf$Fare,0.66)
trainf$Fare<- recode(trainf$Fare, ' lo:q3="barato"; q3:hi="caro" ', as.factor=T)

supervivientes <- as(trainf, "transactions")

reglas <- apriori(supervivientes, parameter = list(support = 0.1, confidence = 0.90))
reglas.ordenadas <- sort(reglas, by = "confidence")
as(reglas.ordenadas, "data.frame")


#Adaboost
library(adabag)
modeloAdabag <- boosting(Survived ~ ., data = trainFinal[-b,])
modeloAdabag$importance
resultados.entrenamiento <- predict(modeloAdabag, newdata = testFinal[-a,], type = "class")
resultados.entrenamiento


#Random Forest
library(randomForest)
set.seed(12345)
train2$Survived<-factor(train2$Survived)
train2<-na.exclude(train2)
train2$Ticket<-factor(train2$Ticket)
modeloRF <- randomForest(Survived ~ Pclass+Sex+Parch+Embarked, data=train2)
prediccionesRF <- predict(modeloRF, testFinal) # con toda la muestra
test2<-na.exclude(test2)
with(test2, table(prediccionesRF, Survived))


#CART 

library(rpart)
modeloCART <- rpart(Survived ~ . , data = train2) 
resultadosCART <- predict(object = modeloCART, newdata = testFinal, type = "class")
t <- table(resultadosCART, test2$Survived)
t ; 100 * sum(diag(t)) / sum(t)


#C5.0

library(C50)
modeloC5 <- C5.0(Survived~Sex+Pclass+Age+SibSp+Parch+Fare,data = train2) 
summary(modeloC5)
plot(modeloC5)

resultadosC5 <- predict(modeloC5, newdata = test2, type = "class")
table(resultadosC5, test2$Survived)
resultadosC5
#SVM

library(e1071)
modeloSVM <- svm(Survived ~ ., data = train2)
summary(modeloSVM)
train2<-na.exclude(train2)
test2<-na.exclude(test2)
resultadosSVM <- predict(modeloSVM, newdata = train2, type = "class")
table(resultadosSVM, train2$Survived)


#Modelos finales CART y ADABOOST

#ADABOOST
trainFinal<-train[-c(1,4,11)]
trainFinal$Survived<-factor(trainFinal$Survived)
testFinal<-test[-c(1,3,10)]
library(adabag)
modeloAdabag <- boosting(Survived ~ ., data = trainFinal)
modeloAdabag$importance
resultados.entrenamiento <- predict(modeloAdabag, newdata = testFinal, type = "class")
resultados.entrenamiento

valores<-datos2
valores$Survived<-as.numeric(resultados.entrenamiento$class)
write.csv(valores,'valores.csv',row.names = F)

#CART

modeloCART <- rpart(Survived ~ . , data = trainFinal) 
resultadosCART <- predict(object = modeloCART, newdata = testFinal, type = "class")
valores<-datos2
valores$Survived<-as.numeric(resultadosCART)-1
write.csv(valores,'valores.csv',row.names = F)


#C5.0

library(C50)
modeloC5 <- C5.0(Survived~Sex+Pclass+Age+SibSp+Parch+Fare,data = trainFinal) 
summary(modeloC5)
plot(modeloC5)

resultadosC5 <- predict(modeloC5, newdata = testFinal, type = "class")
valores<-datos2
valores$Survived<-as.numeric(resultadosC5)-1
write.csv(valores,'valores.csv',row.names = F)

#RF

modeloRF <- randomForest(Survived ~ ., data=na.exclude(trainFinal))
prediccionesRF <- predict(modeloRF, (testFinal)) # con toda la muestra

jj<-which(is.na(as.numeric(prediccionesRF)))
modeloRF2 <- randomForest(Survived ~ Pclass+Sex+Parch+Embarked, data=train2)
prediccionesRF2 <- predict(modeloRF2, testFinal[jj,]) # con toda la muestra



valores<-datos2
valores$Survived<-5
valores$Survived<-as.numeric(prediccionesRF)-1
valores$Survived[jj]<-as.numeric(prediccionesRF2)-1
write.csv(valores,'valores.csv',row.names = F)



#Metodo combinado
solucionADA<-as.numeric(resultados.entrenamiento$class)
solucionCART<-as.numeric(resultadosCART)-1
solucionC5<-as.numeric(resultadosC5)-1
solucionRF<-as.numeric(prediccionesRF)-1
solucionRF[jj]<-as.numeric(prediccionesRF2)-1

solucionfinal<-round(0.25*solucionADA+0.25*solucionCART+0.25* solucionC5+ 0.25* solucionRF)

valores$Survived<-solucionfinal
write.csv(valores,'valores.csv',row.names = F)
