# Librería para leer los datos
library(tidyr)

# Librería para graficar
library(ggplot2)

# Librería para la moda
library(modeest)


cat(" =============================== Laboratorio N°2 Análisis de Datos =============================== \n\n")
cat(" Desarrolladores: Patricia Melo - Gustavo Hurtado\n\n")

#cat(" -------------- Tablas variables discretas --------------\n\n ")

#===================================================== Lectura y manejo de BD =====================================================# 


# URL de la base de datos
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/allhyper.data"

# Se leen los datos de la DB y se almacenan en data
data <- read.table(url, sep = "\t", dec = ",")

col_names <- c("age","sex","on thyroxine","query on thyroxine","on antithyroid medication","sick","pregnant","thyroid surgery",
               "I131 treatment","query hypothyroid","query hyperthyroid","lithium","goitre","tumor","hypopituitary", "psych",
               "TSH measured", "TSH","T3 measured","T3","TT4 measured","TT4","T4U measured","T4U","FTI measured","FTI","TBG measured",
               "TBG","referral source","class")

# Se le asignan nombres apropiados a las columnas de los datos  
sep_data <- separate(data, col = "V1", into = col_names, sep = ",")


#============================================== Limpieza de los datos =================================================#

# Se quitaron los números al final de la clase (|.232), ya que no se utilizan.
sep_data$class <- vapply(strsplit(sep_data$class,"\\."), `[`, 1, FUN.VALUE=character(1))

# Se obtiene el número total de individuos que se enceuntran en el estudio.
total_individuos <- nrow(sep_data)

# Dado que existe una edad 455, se optó por eliminar esa fila para que no altere los demás datos.
sep_data <- sep_data[-c(1365),]

# Dado que TGB medido es siempre falso, se quitará la columna TBG measured y TBG
sep_data$TBG <- NULL
sep_data$`TBG measured` <- NULL

# Esta variable auxiliar será utilizada para hacer cálculos más adelante
sep_data_aux <- sep_data

# A continuación se obtiene la cantidad de filas restantes que no contienen "?" por cada variable númerica encontrada.
# TSH
tsh <- sep_data$TSH
tsh[tsh == "?"] <- NA
tsh <- tsh[complete.cases(tsh)]
filas_tsh <- NROW(tsh)

# T3
t3 <- sep_data$T3
t3[t3 =="?"] <- NA
t3 <- t3[complete.cases(t3)]
filas_t3 <- NROW(t3)

# TT4
tt4 <- sep_data$TT4
tt4[tt4 =="?"] <- NA
tt4 <- tt4[complete.cases(tt4)]
filas_tt4 <- NROW(tt4)

# T4U
t4u <- sep_data$T4U
t4u[t4u =="?"] <- NA
t4u <- t4u[complete.cases(t4u)]
filas_t4u <- NROW(t4u)

# FTI
fti <- sep_data$FTI
fti[fti == "?"]<-NA
fti <- fti[complete.cases(fti)]
filas_fti <- NROW(fti)

# Para poder reemplazar los datos faltantes por la media o mediana es necesario que dichos datos no superen 
# el 5% del total de la población, siendo este 2800 (o 2799 ya que se eliminó uno antes).
# 140 corresponde al 5% de 2800.

# Comparando los resultados de las filas recién obtenidas, se ve que ninguna variable cumple con dicho requisito, 
# por eso se decide eliminar las filas del que más perdida tiene, y así volver a calcular.

# Eliminando "?" de T3
sep_data_aux$T3[sep_data_aux$T3 == "?"] <- NA
sep_data_aux <- sep_data_aux[complete.cases(sep_data_aux),]


# Se eliminan todas las filas que contengan un "?". Pasando de 2799 casos (2800 menos la edad de 455) a 1946 casos.
# Debido a la gran pérdida de datos que se tiene, se buscó eliminar todos los "?" de la columna con más de estos
# Datos vacíos, para luego rellenar los de las otras columnas con la media de su propia columna, pero al superar
# el 5% de datos que pueden ser rellenados, se descartó esa opción.
#sep_data[sep_data == "?"] <- NA
#sep_data <- sep_data[complete.cases(sep_data),]


# Se transforman todas las variables contínuas a numéricas.
#sep_data$age <- as.numeric(as.character(sep_data$age))
#sep_data$T3 <- as.numeric(as.character(sep_data$T3))
#sep_data$T4U <- as.numeric(as.character(sep_data$T4U))
#sep_data$TSH <- as.numeric(as.character(sep_data$TSH))
#sep_data$FTI <- as.numeric(as.character(sep_data$FTI))
#sep_data$TT4 <- as.numeric(as.character(sep_data$TT4))

#======================================================== Funciones =========================================================#


ejemplo <- c(1, "?", 3, 4, 5, 9, "?", 7, "?")
a <- c(99, 88, 77, "?", 55, 44, "?", 22, 11)

algo<-data.frame(cbind(ejemplo, a))
