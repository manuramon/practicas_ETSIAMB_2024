
# Practicas de MGA 2023 - Valoracion genética -----------------------------


# 00 - Librerías ----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)



# 01 - Leemos datos -------------------------------------------------------

# Ejemplo de valoración genética en ovino de leche: 
# Los caracteres de interés son leche (L), % de grasa (G) y % proteina (P)
# los datos se encuentran en el fichero datos_mga.xlsx
# El pedigrí se encuentra en el fichero pedigri_mga.xlsx

ped <- readxl::read_excel("data/pedigri_mga.xlsx", 
                          sheet = 1,
                          col_types = c("text","text","text","numeric"),
                          na="NA") %>% 
  as.data.frame()

dim(ped) # numero de animales en el pedigrí
head(ped)
tail(ped)

library(nadiv)
p1 <- prepPed(ped[, 1:3])

matA <- makeA(p1)

# see A matrix
diag(matA) <- 1

dim(matA)
image(matA[1000:3000, 1000:3000])
image(matA[5001:7000, 5001:7000])


# fenotipos
datos <- readxl::read_excel("data/datos_mga.xlsx")

# vamos a explorar nuestros datos

head(datos, 6)  # 6 primeras filas de datos
tail(datos, 4)  # 4 ultimas filas de datos
nrow(datos)  # cuantos registros tenemos

# vemos que variables contiene nuestro fichero de datos. Para saber más de cada
# variable, usamos la funcion str()

str(datos)


# Los caracteres de interés (L, G y P) se miden varios días diferentes 
# durante la lactación de una oveja. Los registros corresponden a las producciones
# diarias de esos días

# Explora la distribución de esos datos

mean(datos$L) # en ml
sd(datos$L)
hist(datos$L)

# lo mismo para el resto (G y P)

mean(datos$G) # en %
sd(datos$G)
hist(datos$G)



# 02 - Factores ambientales -----------------------------------------------

# son el rebaño (reb), número de lactación (nolac), edad (edad) y días en 
# lactación (dim)

boxplot(L ~ reb, data=datos)
boxplot(L ~ nolac, data=datos) # 
boxplot(L ~ edad, data=datos)  # en meses
boxplot(L ~ dim, data=datos)

datos %>% 
  group_by(nolac, dim) %>% 
  summarise(L.avg=mean(L)) %>% 
  ggplot(aes(x=dim, y=L.avg, group=nolac)) + 
  geom_smooth(aes(color=factor(nolac)), se=FALSE)


# 03 - Efectos del ambiente sobre el caracter -----------------------------

regL0 <- lm(L ~ reb + factor(nolac) + factor(edad) + factor(control), data = datos)
anova(regL0)
summary(regL0)

regL1 <- lm(L ~ HY + factor(pa) + factor(control), data = datos)
anova(regL1)
summary(regL1)

anova(regL0, regL1) # mejor el modelo 2



# 04 - Damos formato a los datos para usar BLUPF90 ------------------------

write.table(datos[, c("P","HY","pa","control","ani")],
            file = "data/datos2blup.txt",
            row.names = FALSE,
            quote = FALSE)


p1$padre[is.na(p1$padre)] <- 0
p1$madre[is.na(p1$madre)] <- 0

write.table(p1,
            file = "data/ped2blup.txt",
            row.names = FALSE,
            quote = FALSE)


