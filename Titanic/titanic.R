
library(neuralnet)

train<-read.csv("train.csv",header=T)
test<-read.csv("test.csv",header=T)
datos2<-read.csv("gender_submission.csv",header=T)
names(train)
modelo<-glm(Survived ~ Pclass + Sex + Age + SibSp,train,family  =binomial(link  =  "logit") ) 
summary(modelo)
plot(train$Survived~modelo$residuals)
plot(modelo)

solution<-predict(modelo,test)
solution<-exp(solution)/(1+exp(solution))
solution2<-cbind(solution,datos2$Survived)
solution2<-data.frame(solution2)
solucion3<-na.omit(solution2)

f=function(x) {if (x<0.5) {0} else {1}}

cc<-sapply(solucion3$solution, f)

solucionf<-cbind(solucion3,cc)

1-mean(abs(solucionf$V2 - solucionf$cc))


PassengerId+ Pclass + Name + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked  
train<-na.omit(cbind(train$Pclass,train$Sex,train$Age,train$SibSp,train$Survived))
test<-na.omit(cbind(test$Pclass,test$Sex,test$Age,test$SibSp,datos2$Survived))

colnames(train) <-c("Pclass","Sex","Age","SibSp","Survived")
colnames(test) <-c("Pclass","Sex","Age","SibSp","Survived")

rn<- neuralnet(Survived ~ Pclass + Sex + Age + SibSp,train, hidden = c(1))
output <- neuralnet::compute(rn, test[ , c("Pclass","Sex","Age","SibSp","Survived")])
result <- data.frame(
  Real = test$Species, 
  Predicted = levels(iris$Species)[round(output$net.result)])
result
