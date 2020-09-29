abs(t(fft(M)))

#Libreria principal del analisis de datos funcionales.

library(fda.usc)
y <-t(train[index,2:ncol(train)])
x<-abs(t(fft(M)))

#Base optima
opt.bsY<-optim.basis(fdata(t(y)),numbasis = seq(4,30,2),type.basis = "bspline")
opt.bsX<-optim.basis(fdata(t(x)),numbasis = seq(4,30,2),type.basis = "bspline")

#Número de elementos de la base óptima 
nbx<-opt.bsX$numbasis.opt
nby<-opt.bsY$numbasis.opt

nbx<-50
nby<-199

#Creamos las bases y formamos nuestro set de datos funcionales.  
basisx <- create.fourier.basis(c(0,6273),nbasis = nbx )
basisy<-  create.fourier.basis(c(0,784),nbasis = nby )
datosf<-Data2fd((x),seq(0,6273,length=6273),basisobj = basisx)
datosfN<-Data2fd((y),seq(0,784,length=784),basisobj = basisy)

plot(datosfN)
plot(datosf)
plot(datosf[49:51])

#Inicializacion de los parametros de penalización
lambdas<-1
lambdat<-1

#Estimación rápida del orden de la penalización
FMSE<-0
ll<-sample(1:50,10)
for (i in 1:10) {
  lambdas<-lambdas/10
  lambdat<-lambdat/10
  tini <- proc.time()
  res.fr = fregre.basis.fr(datosf[-ll],datosfN[-ll],basisx,basisy,lambda.s = lambdas,lambda.t = lambdat,Lfdobj.s = 1,Lfdobj.t = 1)
  proc.time() - tini
  prf<-predict(res.fr,datosf[ll],basisobj = basisy)
  pr<-eval.fd(prf,seq(0,784,length=784))
  FMSE[i]<-mean(colMeans((y[ll]-pr)^2))
}


prf<-predict(res.fr,basisobj = basisy)
pr<-eval.fd(prf,seq(0,784,length=784))

#FMSE mínimo
which.min(FMSE)

#Modelo final

lambdas<-1/10^which.min(FMSE)
lambdat<-1/10^which.min(FMSE)
res.fr = fregre.basis.fr(datosf[1:150],datosfN[1:150],basisx,basisy,lambda.s = lambdas,lambda.t = lambdat,Lfdobj.s = 1,Lfdobj.t = 1)
prf<-predict(res.fr,datosf,basisobj = basisx)
pr<-eval.fd(prf,seq(0,1,length=ttiempo))

#Representación Grafica

A1<-matrix(as.double(pr[,7]),nrow = 28,byrow = T)
A1<-matrix(as.double(train[24795,2:785]),nrow = 28,byrow = T)

rotate <- function(x) t(apply(x, 2, rev))
image(rotate(A1))

jj<-which(pr[,7]<50)
pr[jj,7]=0
