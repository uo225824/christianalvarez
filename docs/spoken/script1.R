library(seawaveQ)
library(audio)
library(fda.usc)
library(stats)


s10<-load.wave(where = "0_jackson_0.wav")
s10t<-fft(s10)
s10t<-abs(s10t)


temp = list.files(pattern="*wav")
myfiles = lapply(temp, load.wave)

M<-matrix(data=0,nrow = 50,ncol = 6273)
for (i in 1:50) {
 
  M[i,1:length(myfiles[[i]])]<-myfiles[[i]]
    
}

datosf<-fdata(fft(abs(as.matrix(M))))

train<-read.csv(file = "datos/train.csv")
head(train)

indice<-sample(ii,50)
for (i in 1:9) {
  jj<-which(train$label==i)
  indice<-c(indice,sample(jj,50))
}
ii<-which(train$label==0)
index<-sample(ii,50)
jj<-which(train$label==1)
indice<-sample(ii,50)
indice1<-sample(jj,50)
indices<-c(indice,indice1)
datosfN<-fdata(train[indices,2:ncol(train)])
plot(datosfN)

basisx <- create.pc.basis(datosf,1:50)
basisy<- create.pc.basis(datosfN,1:50)
res.fr = fregre.basis.fr(datosfN,datosf,basisx,basisy)
summary(res.fr)

res.fr = fregre.basis.fr(tempfd, lnprecfd, basis.s = basisx, basis.t = basisy)
plot(predict(res.fr))


pre<-predict(res.fr)

library(imager)

A1<-matrix(as.double(train$data[1,]),nrow = 28,byrow = T)
A2<-matrix(as.double(datos[2,2:ncol(datos)]),nrow = 28,byrow = T)

A1<-matrix(as.double(r),nrow = 28,byrow = T)



rotate <- function(x) t(apply(x, 2, rev))
image(A1)

r<-0
for (i in 1:length(apfou$data)) {
  if (apfou$data[i]<10) {
    r[i]=0
  }else{
    r[i]=apfou$data[i]
  }
}

image(rotate(A1),col=c(0,1))
image(rotate(A2),col=c(0,1))
