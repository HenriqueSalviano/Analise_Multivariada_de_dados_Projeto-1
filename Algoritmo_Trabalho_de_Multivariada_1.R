# funcao vetor de medias das colunas (covariaveis)
fmu <- function(X){
  mu <- rep(NA,ncol(X))
  for (j in 1:ncol(X)) {
    mu[j] <- sum(X[,j])/length(X[,j])
  }
  return(mu)
}
# funcao que gera a matriz VarCov
fvarcov <- function(X){
  mu <- fmu(X) #calcula o vetor de medias
  nc <- ncol(X) #numero de colunas
  nl <- nrow(X) #numero de linhas
  
  varcov <- matrix(0,nc,nc)
  for (j in 1:nc) {
    for (k in 1:nc) {
      varcov[j,k] <- sum((X[,j]-mu[j])*(X[,k]-mu[k]))/(nl-1)
    }
  }
  return(varcov)
}
# funcao que calcula a Variancia Total da matriz VarCov
# Variancia Total da matriz VarCov = tra¸co da matriz VarCov
fVartot <- function(X){
  traco <- 0
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
      if (i == j) {
        traco <- traco+X[i,j]
      }
    }
  }
  return(traco)
}
# funcao que calcula matriz de correla¸c~ao
corr <- function(X) {
  nc <- ncol(X) #numero de colunas
  nl <- nrow(X) #numero de linhas
  corr <- matrix(0,nc,nl)
  for (i in 1:nl) {
    for (j in 1: nc) {
      corr[i,j] <- X[i,j]*(sqrt(X[i,i]*X[j,j]))^-1
    }
  }
  return(corr)
}

#Calculando a dist^ancia estat´istica entre matriz dos dados e vetor de m´edias
mahalanobis(data2, c(119.7,70.5,64.9,-5.4,17.5,52.9,223.3,13.8,8.9,71.7), s) %>%
  as.data.frame()

# Resultados
mu <- fmu(X) #Vetor de medias obtido pela funcao
varcov <- fvarcov(X) # matriz varcov obtida pela funcao
Vartot<-fVartot(varcov)# variancia total obtida pela funcao

determinante <- det(varcov) # Determinante da matriz varcov
correlacao <- corr(varcov) # matriz de correla¸c~ao obtida pela funcao
determinante <- det(varcov)

# Imprimindo resultados
mu #Vetor de medias
round(fvarcov(X),2) # matriz varcov com arredondamento de 2 casas
Vartot # variancia total
determinante # Determinante da matriz varcov
round(correlacao,2) # Matriz de correla¸c~ao com arredondamento de 2 casas
#####################################

#QQ-Plots
#par(mfrow=c(2,5))
par(mfrow=c(2,2))
qqnorm(X[,1], pch = 1, frame = FALSE,main = "Normal Q-Q Plot bpm")
qqline(X[,1], col = "steelblue", lwd = 2)
qqnorm(X[,2], pch = 1, frame = FALSE,main = "Normal Q-Q Plot enrg")
qqline(X[,2], col = "steelblue", lwd = 2)
qqnorm(X[,3], pch = 1, frame = FALSE,main = "Normal Q-Q Plot danc")
qqline(X[,3], col = "steelblue", lwd = 2)
qqnorm(X[,4], pch = 1, frame = FALSE,main = "Normal Q-Q Plot dB")
qqline(X[,4], col = "steelblue", lwd = 2)
par(mfrow=c(2,2))
qqnorm(X[,5], pch = 1, frame = FALSE,main = "Normal Q-Q Plot viva")
qqline(X[,5], col = "steelblue", lwd = 2)
qqnorm(X[,6], pch = 1, frame = FALSE,main = "Normal Q-Q Plot vale")
qqline(X[,6], col = "steelblue", lwd = 2)
qqnorm(X[,7], pch = 1, frame = FALSE,main = "Normal Q-Q Plot comp")
qqline(X[,7], col = "steelblue", lwd = 2)
qqnorm(X[,8], pch = 1, frame = FALSE,main = "Normal Q-Q Plot acus")
qqline(X[,8], col = "steelblue", lwd = 2)
par(mfrow=c(2,2))
qqnorm(X[,9], pch = 1, frame = FALSE,main = "Normal Q-Q Plot disc")
qqline(X[,9], col = "steelblue", lwd = 2)
qqnorm(X[,10], pch = 1, frame = FALSE,main = "Normal Q-Q Plot popu")
qqline(X[,10], col = "steelblue", lwd = 2)
#####################################

# Testando normalidade e normalidade multivariada
library(nortest)

# Testando normalidade da variavel bpm
shapiro.test(X[,1])
print(ad.test(X[,1]))
print(lillie.test(X[,1]))

# Testando normalidade da variavel nrgy
shapiro.test(X[,2])
print(ad.test(X[,2]))
print(lillie.test(X[,2]))

# Testando normalidade da variavel dnce
shapiro.test(X[,3])
print(ad.test(X[,3]))
print(lillie.test(X[,3]))

# Testando normalidade da variavel dB
shapiro.test(X[,4])
print(ad.test(X[,4]))
print(lillie.test(X[,4]))

# Testando normalidade da variavel live
shapiro.test(X[,5])
print(ad.test(X[,5]))
print(lillie.test(X[,5]))

# Testando normalidade da variavel val
shapiro.test(X[,6])
print(ad.test(X[,6]))
print(lillie.test(X[,6]))

# Testando normalidade da variavel dur
shapiro.test(X[,7])
print(ad.test(X[,7]))
print(lillie.test(X[,7]))

# Testando normalidade da variavel acous
shapiro.test(X[,8])
print(ad.test(X[,8]))
print(lillie.test(X[,8]))

# Testando normalidade da variavel spch
shapiro.test(X[,9])
print(ad.test(X[,9]))
print(lillie.test(X[,9]))

# Testando normalidade da variavel pop
shapiro.test(X[,10])
print(ad.test(X[,10]))
print(lillie.test(X[,10]))

