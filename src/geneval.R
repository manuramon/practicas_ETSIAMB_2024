# Valoracion genética

library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Ejemplo del libro de Mrode

# Datos
dat1 <- data.frame(animal=c(4,5,6,7,8),
                   sexo=c("m","f","f","m","m"),
                   pheno=c(4.5,2.9,3.9,3.5,5.0),
                   padre=c(1,3,1,4,3),
                   madre=c(0,2,2,5,6))

# construimos el pedigrí a partir de la información de los datos

# ped1 <- data.frame(animal=1:8,
#                    padre=c(0,0,0,1,3,1,4,3),
#                    madre=c(0,0,0,0,2,2,5,6))
# 
# ped1 <- dat1[, c("animal","padre","madre")]

library(nadiv)
ped2 <- prepPed(dat1[, c("animal","padre","madre")])
Amat <- makeA(ped2)
image(Amat)
Ainv <- makeAinv(ped2)$Ainv

# Modelo lineal

boxplot(pheno ~ sexo, data=dat1)

mod1 <- lm(pheno ~ sexo, data=dat1)
summary(mod1)

# para obtener las medias mínimo cuadráticas para el factor sexo, podemos 
# usar la función lsmeans del paquete emmmeans (si no está, hay que instalarlo!)

require("emmeans")

emmeans::lsmeans(mod1, "sexo")


# Modelo mixto 

X <- model.matrix(~sexo-1, data=dat1)
Z <- model.matrix(~factor(animal)-1, data=dat1)
Z <- cbind(matrix(0, ncol=3, nrow=5), Z)
dimnames(Z) <- list(NULL, paste0("animal_",1:8))
y <- dat1$pheno

# variance component ratio
# varianza residual = 40
# variance genética aditiva = 20
vcr = 40/20

Xpy <- t(X)%*%y
Zpy <- t(Z)%*%y
XpX <- t(X)%*% X
XpZ <- t(X)%*%Z
ZpX <- t(Z)%*%X
ZpZ <- t(Z)%*%Z

LHS <- rbind( cbind(XpX, XpZ), cbind(ZpX, ZpZ+Ainv*vcr))
RHS <- c(Xpy, Zpy)

source("src/jor.R")
inits <- c(4.333,3.400,0,0,0,0.167,-0.5,0.5,-0.833,0.667)
inits <- c(3,3,0,0,0,0.1,0.1,0.1,0.1,0.1)
w <- c(1,1,rep(0.8,8)) # más peso a los efectos fijos, son más fáciles de estimar
ans <- jor(LHS, RHS, inits, w, disp = TRUE) # usa dips=FALSE y disp=TRUE
ans

ans[1:2] # efecto promedio de los 2 sexos (female, male)
ans[3:10] # valores genéticos de los 7 animales del pedigrí

ped2$EBV <- ans[3:10]
arrange(ped2, -EBV)

# ¿Qué animales usaríamos como reproducores si queremos aumentar el 
# peso al destete?



# Ejemplo para hacer en clase
dat2 <- dat1
dat2$pheno <- c(0,5,6.8,6,7.5)

