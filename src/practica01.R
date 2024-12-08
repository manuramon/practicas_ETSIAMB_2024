# Practica 01. Cálculo de coeficientes de parentesco y consanguinidad -----

options(scipen = 9999)

# Librerías ---------------------------------------------------------------

# Cargamos las librerías que necesitamos

install.packages(c("kinship2", "nadiv"))

library(kinship2)
library(nadiv)

# vamos a instalar una libreria. Si preguntan si queremos actualizar, indicamos none (3)
devtools::install_github("luansheng/visPedigree")  
library(visPedigree)

# package GeneticsPed
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GeneticsPed")
#library(GeneticsPed)

# inbreedR
# (https://cran.r-project.org/web/packages/inbreedR/vignettes/inbreedR_step_by_step.html)
# devtools::install_github("mastoffel/inbreedR", build_vignettes = TRUE)
# library(inbreedR)


# Ejercicio 1 -------------------------------------------------------------

# disponemos de la siguiente información de genealogía

p1 <- data.frame(id = c(3,4,5,6),
                 padre = c(1,1,4,5),
                 madre = c(2,NA,3,2))

# para ver el contenido de un objeto de R, pon su nombre en la consola o
# clima en el en la pestaña de environment

p1

# ¿De cuántos animales hay información en el pedigrí?


# construimos el perdigrí: añade animales a la primera columna y
# ordena el pedigrí. Esto último es requisito para aplicar los 
# métodos recursivos de cálculo de parentesco y consanguinidad

p1a <- prepPed(p1)
print(p1)
print(p1a)

#  ¿Qué diferencias hay entre p1 y p1a? (NOTA: revisa número de id y orden)

# dibujamos el pedigrí

visped(tidyped(p1), cex=1)


# Calculamos la matriz de parentesco
# usamos el pedigrí ordenado

matC <- kinship(id = p1a$id, 
                dadid = p1a$padre, 
                momid = p1a$madre
                )
matC

# Calculamos la matriz de relaciones genéticas aditivas
# vamos a usar el pedigrí completo y ordenado: p1a

matA <- makeA(p1a)
matA

# La matriz A tiene puntos; ¿qué significado crees que tiene?

# Compara las matrices matA y matC. 
# ¿Se cumple la condición de que matA = 2*matC?


# Para calcular la consanguinidad, usamos la diagonal de las matrices
# de parentesco o de relaciones genéticas aditivas

# la función diag() nos permite obtener la diagonal de una matrix simétrica

diag(matC)
diag(matA)

inb_c <- (diag(matC)-0.5)*2
round(inb_c, 3)

inb_a <- diag(matA - 1)
round(inb_a, 3)

# usando la función inbreeding del paquete GeneticsPed
# hay que ponerlo en el formato que requiere la función

library(GeneticsPed)

p1b <- Pedigree(p1a, subject = "id", ascendant = c("padre","madre"))
inbreeding(p1b)

# podemos visualizar las relaciones de parentesco pintando la matriz

corrplot::corrplot(matC, 
                   method = "shade", tl.col = "gray20")

corrplot::corrplot(as.matrix(matA), 
                   is.corr = FALSE, # porque hay valores >1
                   method = "shade", tl.col = "gray20")



# Ejercicio 2 -------------------------------------------------------------

# para el pedigrí p2, calcula la matriz de parentesco, la matriz de relaciones
# genéticas aditivas y la consanguinidad de los indiviuos en el pedigrí.
# Explora el gráfico de relaciones de parentesco y relaciones genéticas aditivas

# vamos a generar un pedigrí con 3 generaciones y 5 individuos por generación

p2 <- generatePedigree(nId = 5, nGeneration = 3)

# información del pedigrí

summary.Pedigree(p2)

# visualizamos el pedigree
visped(tidyped(p2[, 1:3]), cex=1)


# podemos usar la función kinship del paquete kinship2 
# Nota: hemos puesto delante del nombre de la función el nombre del paquete
# separado por dobles ":" (kinship2::kinship) porque la función kinship
# ha sido sobreescrita por el paquete GeneticsPed

matC1 <- kinship2::kinship(id = p2$id, 
                           dadid = p2$father, 
                           momid = p2$mother)

# podéis visualizar parte de la matriz indicando las filas y columnas

matC1[1:4, 1:4]
matC1[11:15, 11:15]


# O podemos usar la función kinship del paquete GeneticsPed
matC2 <- kinship(p2)
matC2[1:4, 1:4]
matC2[11:15, 11:15]

# para verla gráficamente

corrplot::corrplot(matC1)

# mejor visualización

corrplot::corrplot(matC1, 
                   method = "shade", tl.col = "gray20")

# existe la posibilidad de ordenar a los animales por su parecido,
# lo que nos permite tener una idea de las familias en nuestro pedigrí.
# Para ello, usamos la opción order="hclust"

corrplot::corrplot(matC1, 
                   order = "hclust",
                   method = "shade", tl.col = "gray20")


# pasamos a calcular la matriz de relaciones genéticas aditivas
p2a <- prepPed(p2[, c("id","father","mother")])
matA <- makeA(p2a)


corrplot::corrplot(as.matrix(matA),
                   is.corr = FALSE,
                   order = "hclust",
                   method = "shade", tl.col = "gray20")

# para calcular la consanguinidad, podemos usar la diagonal de la matriz
# de paretesco o de relaciones genéticas aditivas, o la función inbreeding()

(diag(matC1)-0.5)*2

round(diag(matA) - 1, 3)

inbreeding(p2)

# ¿Qué animales tiene una consanguinidad >= 0.25?

which(inbreeding(p2)>=0.25)

which(inbreeding(p2)>=0.125)



# Ejercicio 3 -------------------------------------------------------------

# crea un pedigrí con la función GeneratePed() que tenga 4 generaciones y
# 2 individuos por generación. Calcula las matrices de parentesco y de
# relaciones genéticas  aditivas así como la consanguinidad de cada
# individuo. Representa gráficamente dichas matrices e identifica aquellos
# individuos con una consanguinidad mayor a 1/8
# Discute con tu compañero de al lado los resultados.




# Ejercicio 5 -------------------------------------------------------------

# Crea alguno de las genealogías que hemos visto en la clase y verifica
# que las matrices de parentesco y de relaciones genéticas aditivas,
# así como los valores de consanguinidad, están bien.

p2 <- data.frame(id=c("A","B","C","D","E"),
                 padre=c(NA,NA,NA,"B","B"),
                 madre=c(NA,"A","A","C","C"))

p2a = prepPed(p2)
c1 <- kinship(id= p2a$id, dadid=p2a$padre, momid=p2a$madre)
inbc1 <- diag(c1)*2 - 1


p3 <- data.frame(id=c("A","B","C","D","E", "X"),
                 padre=c(NA,NA,NA,"B","B", "D"),
                 madre=c(NA,"A","A","C","C", "E"))
p3a <- prepPed(p3)
c3 <- kinship(id=p3a$id, dadid=p3a$padre, momid=p3a$madre)


p4 <- data.frame(id =   c("B","C","D","E","F","G","X"),
                 padre = c("A","A","B","B","D",NA,"F"),
                 madre = c(NA,NA,NA,"C",NA,"E","G"))


p4a <- prepPed(p4)
makeA(p4a)











