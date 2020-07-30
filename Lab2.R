# Librería para leer los datos
library(tidyr)

# Librería para graficar
library(ggplot2)

# Librería para la moda
library(modeest)

#library(quantmod)


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

#======================================================== Funciones =========================================================#


# Se genera una función que calcula la media, mediana, moda, desviación standard, mínimo y máximo 
# de una columna perteneciente pertenenciente a un data frame.
# Recibe una columna de un data frame y el nombre de la columna.
# Retorna un data frame con la media, mediana, moda, mínimo, máximo y desviación standard de la columna.
get_col_measures <- function(col, name){
  measurements <- data.frame(
    mean = round(mean(col, na.rm = TRUE),3),
    median = round(median(col, na.rm = TRUE),3),
    mode = round(mfv1(col, na_rm = TRUE),3),
    min = round(min(col, na.rm = TRUE),3),
    max = round(max(col, na.rm = TRUE),3),
    sd = round(sd(col, na.rm = TRUE),3)
  )
  rownames(measurements) <- c(name)
  
  return(measurements)
  
}


# Se genera una función que calcula la media, mediana, moda y desviación standard de todas las columnas de un
# data frame con valores numéricos contínuos
# Recibeun data frame y el nombre de la columna.
# Retorna un data frame con la media, mediana, moda y desviación standard de cada columna del data frame.

get_all_measures <- function(data_frame){
  total_measurements <- rbind(get_col_measures(data_frame$age,"age"), get_col_measures(data_frame$TSH, "TSH"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$T3, "T3"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$TT4, "TT4"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$T4U, "T4U"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$FTI, "FTI"))
  return(total_measurements)
}


#======================================= Reconocimiento de datos ==========================================

# Se calcula datos de medida central para cada variable numérica, antes de la limpieza
# Para esto no se consideran los datos nulos y se crea un data frame para luego utilizar una función.

# Edad
age <- sep_data$age
age[age == "?"] <- NA
age <- as.numeric(as.character(age))
#age <- age[complete.cases(age)]

# TSH
tsh <- sep_data$TSH
tsh[tsh == "?"] <- NA
#tsh <- tsh[complete.cases(tsh)]
tsh <- as.numeric(as.character(tsh))

# T3
t3 <- sep_data$T3
t3[t3 =="?"] <- NA
#t3 <- t3[complete.cases(t3)]
t3 <- as.numeric(as.character(t3))

# TT4
tt4 <- sep_data$TT4
tt4[tt4 =="?"] <- NA
#tt4 <- tt4[complete.cases(tt4)]
tt4 <- as.numeric((as.character(tt4)))

# T4U
t4u <- sep_data$T4U
t4u[t4u =="?"] <- NA
#t4u <- t4u[complete.cases(t4u)]
t4u <- as.numeric(as.character(t4u))

# FTI
fti <- sep_data$FTI
fti[fti == "?"]<-NA
#fti <- fti[complete.cases(fti)]
fti <- as.numeric(as.character(fti))


# Se crea el data frame.
datos <- data.frame(
  "age" = age,
  "TSH" = tsh, 
  "T3" = t3,
  "TT4" = tt4,
  "T4U" = t4u, 
  "FTI" = fti
)

# Se calcula la media, mediana, moda, mínimo, máximo y desviación estándar
get_all_measures(datos)



#============================================ Limpieza de los datos ==============================================#

# Se quitaron los números al final de la clase (|.232), ya que no se utilizan.
sep_data$class <- vapply(strsplit(sep_data$class,"\\."), `[`, 1, FUN.VALUE=character(1))

# Se obtiene el número total de individuos que se enceuntran en el estudio.
total_individuos <- nrow(sep_data)

# Dado que TGB medido es siempre falso, se quitará la columna TBG measured y TBG
sep_data$TBG <- NULL
sep_data$`TBG measured` <- NULL

# Esta variable auxiliar será utilizada para hacer cálculos más adelante
sep_data_aux <- sep_data


#------------------------ Eliminando valores atipicos --------------------------------

# Dado que existe una edad 455, se optó por eliminar esa fila para que no altere los demás datos.
sep_data <- sep_data[-c(1365),]

