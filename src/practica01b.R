
# Practica 01. Cálculo de coeficientes de parentesco y consanguinidad -----



# Librerías ---------------------------------------------------------------

# Cargamos las librerías que necesitamos

library(kinship2)
library(nadiv)

# vamos a instalar una libreria. Si preguntan si queremos actualizar, indicamos none (3)
devtools::install_github("luansheng/visPedigree")  
library(visPedigree)


# Ejercicio 1 -------------------------------------------------------------

p1 <- data.frame(id = c(3,4,5,6),
                 padre = c(1,1,4,5),
                 madre = c(2,NA,3,2))


# construimos el perdigrí

p1a <- prepPed(p1)
print(p1)
print(p1a)

#  ¿Qué diferencias hay entre p1 y p1a? (NOTA: revisa número de id y orden)

# dibujamos el pedigrí
visped(tidyped(p1))

# Calculamos la matriz de parentesco
matC <- kinship(pedigree(id = p1a$id, 
                         dadid = p1a$padre, 
                         momid = p1a$madre, 
                         sex = c(1,2,1,2,1,2)))

# Calculamos la matriz de relaciones genéticas aditivas
# vamos a usar el pedigrí completo y ordenado: p1a
matA <- makeA(p1a)
matA*0.5
