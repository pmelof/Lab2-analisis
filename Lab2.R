# Librería para leer los datos
library(tidyr)

# Librería para graficar
library(ggplot2)

# Librería para la moda
library(modeest)

# Librería para graficar componentes principales
library(factoextra)

# Librería PCA
library(FactoMineR)

# Librería para los cluster
library(cluster)


library(dplyr)


cat(" =============================== Laboratorio N°2 Análisis de Datos =============================== \n\n")
cat(" Desarrolladores: Patricia Melo - Gustavo Hurtado\n\n")


#===================================================== Lectura y manejo de BD =====================================================# 


# URL de la base de datos
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/allhyper.data"

# Se leen los datos de la DB y se almacenan en data
data <- read.table(url, sep = "\t", dec = ",")

col_names <- c("age","sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery",
               "I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary", "psych",
               "TSH_measured", "TSH","T3_measured","T3","TT4_measured","TT4","T4U_measured","T4U","FTI_measured","FTI","TBG_measured",
               "TBG","referral_source","class")

# Se le asignan nombres apropiados a las columnas de los datos  
sep_data <- separate(data, col = "V1", into = col_names, sep = ",")

#======================================================== Funciones =========================================================#


# Se genera una función que calcula la media, mediana, moda, desviación standard, mínimo y máximo 
# de una columna perteneciente a un data frame.
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
# Recibe un data frame.
# Retorna un data frame con la media, mediana, moda y desviación standard de cada columna del data frame.
get_all_measures <- function(data_frame){
  total_measurements <- rbind(get_col_measures(data_frame$age,"age"), get_col_measures(data_frame$TSH, "TSH"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$T3, "T3"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$TT4, "TT4"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$T4U, "T4U"))
  total_measurements <- rbind(total_measurements, get_col_measures(data_frame$FTI, "FTI"))
  return(total_measurements)
}


# Función que calcula las frecuencias de las clases.
# Recibe la columna que contiene las clases.
# Retorna un data frame con las frecuencias de las clases.
class_frequency <- function(col, name){
  frequencies <- data.frame(
    Negative = NROW(subset(col, col == 'negative')),
    T3_toxic = NROW(subset(col, col == "T3 toxic")), 
    Goitre = NROW(subset(col, col == "goitre")), 
    Hyperthyroid = NROW(subset(col, col == "hyperthyroid"))
  )
  rownames(frequencies) <- c(name)
  return(frequencies)
}


# Función que calcula las fecuencias de clases en los primeros 4 cluster.
# Recibe un dta frame.
# Retorna un data frame con las fecuencias de las clases en los primeros 4 cluster.
class_in_cluster_frequency <- function(data){
  frecuencies <- rbind(class_frequency(filter(data[21], data[22]==1), "1"))
  frecuencies <- rbind(frecuencies, class_frequency(filter(data[21], data[22]==2), "2"))
  frecuencies <- rbind(frecuencies, class_frequency(filter(data[21], data[22]==3), "3"))
  frecuencies <- rbind(frecuencies, class_frequency(filter(data[21], data[22]==4), "4"))
  return(frecuencies)
}


# Función que calcula la frecuencia de una columna perteneciente a un data frame.
# Recibe una columna de un data frame y el nombre de la columna.
# Retorna un data frame con la frecuencia de true y false que se encuentran en la columna.
get_col_frequency <- function(col, name){
  frequencies <- data.frame(
    True = NROW(subset(col, col == "t")),
    False = NROW(subset(col, col == "f"))
  )
  rownames(frequencies) <- c(name)
  return(frequencies)
}


# Función que calcula la frecuencia de todas las columnas de un data frame con variables discretas.
# Recibe un data frame.
# Retorna un data frame con las frecuencias de true y false de cada columna del data frame.
get_all_frequency <- function(data_frame){
  total_frequency <- rbind(get_col_frequency(data_frame$on_thyroxine, "on_thyroxine"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$query_on_thyroxine, "query_on_thyroxine"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$on_antithyroid_medication, "on_antithyroid_medication"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$sick, "sick"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$pregnant, "pregnant"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$thyroid_surgery, "thyroid_surgery"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$I131_treatment, "I131_treatment"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$query_hypothyroid, "query_hypothyroid"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$query_hyperthyroid, "query_hyperthyroid"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$lithium, "lithium"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$goitre, "goitre"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$tumor, "tumor"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$hypopituitary, "hypopituitary"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$psych, "psych"))
  # total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$class, "class"))
  return(total_frequency)
}


# Función que genera un dataframe con todos los valores normalizados.
# Recibe una columna del data frame.
# Entrega un data frame con los valores normalizados de la columna.
normalize <- function(col, name){
  max = max(col)
  min = min(col)
  data_normalized <- data.frame(
    value = ((col-min)/(max-min))
  )
  colnames(data_normalized) <- name
  return(data_normalized)
}

# Función que genera un dataframe con todos los valores normalizados de un data frame.
# Recibe un data frame.
# Entrega un data frame con los valores normalizados de todas las columnas.
normalize_all <- function(data_frame){
  data_final <- cbind(normalize(data_frame$age, "age"))
  data_final <- cbind(data_final, normalize(data_frame$sex, "sex"))
  data_final <- cbind(data_final, normalize(data_frame$on_thyroxine, "on_thyroxine"))
  data_final <- cbind(data_final, normalize(data_frame$query_on_thyroxine, "query_on_thyroxine"))
  data_final <- cbind(data_final, normalize(data_frame$on_antithyroid_medication, "on_antithyroid_medication"))
  data_final <- cbind(data_final, normalize(data_frame$sick, "sick"))
  data_final <- cbind(data_final, normalize(data_frame$pregnant, "pregnant"))
  data_final <- cbind(data_final, normalize(data_frame$thyroid_surgery, "thyroid_surgery"))
  data_final <- cbind(data_final, normalize(data_frame$I131_treatment, "I131_treatment"))
  data_final <- cbind(data_final, normalize(data_frame$query_hypothyroid, "query_hypothyroid"))
  data_final <- cbind(data_final, normalize(data_frame$query_hyperthyroid, "query_hyperthyroid"))
  data_final <- cbind(data_final, normalize(data_frame$lithium, "lithium"))
  data_final <- cbind(data_final, normalize(data_frame$goitre, "goitre"))
  data_final <- cbind(data_final, normalize(data_frame$tumor, "tumor"))
  data_final <- cbind(data_final, normalize(data_frame$psych, "psych"))
  data_final <- cbind(data_final, normalize(data_frame$TSH, "TSH"))
  data_final <- cbind(data_final, normalize(data_frame$T3, "T3"))
  data_final <- cbind(data_final, normalize(data_frame$TT4, "TT4"))
  data_final <- cbind(data_final, normalize(data_frame$T4U, "T4U"))
  data_final <- cbind(data_final, normalize(data_frame$FTI, "FTI"))
  return(data_final)
}

# Desnormaliza un valor, según los datos a la columna perteneciente.
# Recibe un valor, min de la columna y max de la misma columna.
# Retorna el valor desnormalizado.
desnormalization <- function(col, min, max, name){
  data <- data.frame(
    value = round(((col*(max-min))+min),3)
  )
  colnames(data) <- name
  return(data)
}


desnormalize_all_centers <- function(data, centers, k){
  data_final <- cbind(desnormalization(centers[1:k], min(data$age), max(data$age), "age"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,2], min(data$sex), max(data$sex), "sex"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,3], min(data$on_thyroxine), max(data$on_thyroxine), "on_thyroxine"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,4], min(data$query_on_thyroxine), max(data$query_on_thyroxine), "query_on_thyroxine"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,5], min(data$on_antithyroid_medication), max(data$on_antithyroid_medication), "on_antithyroid_medication"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,6], min(data$sick), max(data$sick), "sick"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,7], min(data$pregnant), max(data$pregnant), "pregnant"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,8], min(data$thyroid_surgery), max(data$thyroid_surgery), "thyroid_surgery"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,9], min(data$I131_treatment), max(data$I131_treatment), "I131_treatment"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,10], min(data$query_hypothyroid), max(data$query_hypothyroid), "query_hypothyroid"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,11], min(data$query_hyperthyroid), max(data$query_hyperthyroid), "query_hyperthyroid"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,12], min(data$lithium), max(data$lithium), "lithium"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,13], min(data$goitre), max(data$goitre), "goitre"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,14], min(data$tumor), max(data$tumor), "tumor"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,15], min(data$psych), max(data$psych), "psych"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,16], min(data$TSH), max(data$TSH), "TSH"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,17], min(data$T3), max(data$T3), "T3"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,18], min(data$TT4), max(data$TT4), "TT4"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,19], min(data$T4U), max(data$T4U), "T4U"))
  data_final <- cbind(data_final, desnormalization(centers[1:k,20], min(data$FTI), max(data$FTI), "FTI"))
  return(data_final)
}


# Función que genera un dataframe con todos los valores desnormalizados de un data frame.
# Recibe un data frame y los datos originales.
# Entrega un data frame con los valores desnormalizados de todo el data frame.
desnormalize_all_dataframe <- function(mydata,data){
  data_final <- cbind(desnormalization(mydata$age, min(data$age), max(data$age), "age"))
  data_final <- cbind(data_final, desnormalization(mydata$sex, min(data$sex), max(data$sex), "sex"))
  data_final <- cbind(data_final, desnormalization(mydata$on_thyroxine, min(data$on_thyroxine), max(data$on_thyroxine), "on_thyroxine"))
  data_final <- cbind(data_final, desnormalization(mydata$query_on_thyroxine, min(data$query_on_thyroxine), max(data$query_on_thyroxine), "query_on_thyroxine"))
  data_final <- cbind(data_final, desnormalization(mydata$on_antithyroid_medication, min(data$on_antithyroid_medication), max(data$on_antithyroid_medication), "on_antithyroid_medication"))
  data_final <- cbind(data_final, desnormalization(mydata$sick, min(data$sick), max(data$sick), "sick"))
  data_final <- cbind(data_final, desnormalization(mydata$pregnant, min(data$pregnant), max(data$pregnant), "pregnant"))
  data_final <- cbind(data_final, desnormalization(mydata$thyroid_surgery, min(data$thyroid_surgery), max(data$thyroid_surgery), "thyroid_surgery"))
  data_final <- cbind(data_final, desnormalization(mydata$I131_treatment, min(data$I131_treatment), max(data$I131_treatment), "I131_treatment"))
  data_final <- cbind(data_final, desnormalization(mydata$query_hypothyroid, min(data$query_hypothyroid), max(data$query_hypothyroid), "query_hypothyroid"))
  data_final <- cbind(data_final, desnormalization(mydata$query_hyperthyroid, min(data$query_hyperthyroid), max(data$query_hyperthyroid), "query_hyperthyroid"))
  data_final <- cbind(data_final, desnormalization(mydata$lithium, min(data$lithium), max(data$lithium), "lithium"))
  data_final <- cbind(data_final, desnormalization(mydata$goitre, min(data$goitre), max(data$goitre), "goitre"))
  data_final <- cbind(data_final, desnormalization(mydata$tumor, min(data$tumor), max(data$tumor), "tumor"))
  data_final <- cbind(data_final, desnormalization(mydata$psych, min(data$psych), max(data$psych), "psych"))
  data_final <- cbind(data_final, desnormalization(mydata$TSH, min(data$TSH), max(data$TSH), "TSH"))
  data_final <- cbind(data_final, desnormalization(mydata$T3, min(data$T3), max(data$T3), "T3"))
  data_final <- cbind(data_final, desnormalization(mydata$TT4, min(data$TT4), max(data$TT4), "TT4"))
  data_final <- cbind(data_final, desnormalization(mydata$T4U, min(data$T4U), max(data$T4U), "T4U"))
  data_final <- cbind(data_final, desnormalization(mydata$FTI, min(data$FTI), max(data$FTI), "FTI"))
  return(data_final)
}

#======================================= Reconocimiento de datos ==========================================

# Se calcula datos de medida central para cada variable numérica, antes de la limpieza
# Para esto no se consideran los datos nulos y se crea un data frame para luego utilizar una función.

# Edad
age <- sep_data$age
age[age == "?"] <- NA
age <- as.numeric(as.character(age))

# TSH
tsh <- sep_data$TSH
tsh[tsh == "?"] <- NA
tsh <- as.numeric(as.character(tsh))

# T3
t3 <- sep_data$T3
t3[t3 =="?"] <- NA
t3 <- as.numeric(as.character(t3))

# TT4
tt4 <- sep_data$TT4
tt4[tt4 =="?"] <- NA
tt4 <- as.numeric((as.character(tt4)))

# T4U
t4u <- sep_data$T4U
t4u[t4u =="?"] <- NA
t4u <- as.numeric(as.character(t4u))

# FTI
fti <- sep_data$FTI
fti[fti == "?"]<-NA
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

# Para variables categóricas se procede a calcular la frecuencia en cada caso
# Se tiene para sexo: F, M.
# Resto de variables categóricas: t, f.

# data frame con variables cualitativas 
data_cualitative <- data.frame(
  "on_thyroxine" = sep_data$on_thyroxine,
  "query_on_thyroxine" = sep_data$query_on_thyroxine,
  "on_antithyroid_medication" = sep_data$on_antithyroid_medication,
  "sick" = sep_data$sick,
  "pregnant" = sep_data$pregnant,
  "thyroid_surgery" = sep_data$thyroid_surgery,
  "I131_treatment" = sep_data$I131_treatment,
  "query_hypothyroid" = sep_data$query_hypothyroid,
  "query_hyperthyroid" = sep_data$query_hyperthyroid,
  "lithium" = sep_data$lithium,
  "goitre" = sep_data$goitre,
  "tumor" = sep_data$tumor,
  "hypopituitary" = sep_data$hypopituitary, 
  "psych" = sep_data$psych
)


# Sexo
sex <- sep_data$sex
female_quant <- NROW(subset(sex, sex == "F"))
male_quant <- NROW(subset(sex, sex == "M"))

# Resto de las variables cualitativas
get_all_frequency(data_cualitative)


#====================== Detección y tratamiento de datos faltantes y atípicos =======================#

# Se quitaron los números al final de la clase (|.232), ya que no se utilizan.
sep_data$class <- vapply(strsplit(sep_data$class,"\\."), `[`, 1, FUN.VALUE=character(1))

# Se obtiene el número total de individuos que se enceuntran en el estudio.
total_individuos <- nrow(sep_data)    # 2800

# Esta variable auxiliar guardará los datos sin modificar
sep_data_aux <- sep_data


# -------------------------------------------------------------------
#                    Disminución de variables 
#
# Se eliminan variables que no entregan un gran aporte al estudio

# Variables numéricas:
# Dado que TGB medido es siempre falso, se quitará la columna TBG
sep_data$TBG <- NULL

# Variables categóricas:
# Como se eliminó TBG, ya no es necesario TBG measured.
sep_data$TBG_measured <- NULL

# Además, al tener valores en el resto de las variables numéricas hace que todas las variables que tengan
# "measured" no entreguen mayor información, ya que si hay valor entonces el individuo se realizó el examen.
sep_data$TSH_measured <- NULL
sep_data$T3_measured <- NULL
sep_data$TT4_measured <- NULL
sep_data$T4U_measured <- NULL
sep_data$FTI_measured <- NULL

# Con el reconocimiento de datos previamente hecho, se vió que la variable hypopituitary no tenía una
# gran variación en los resultados, donde había 1 verdadero y 2799 falsos, por este motivo se elimina.
sep_data$hypopituitary <- NULL

# Otras variables discretas que no entregan mayor información al estudio con respecto a los calculos son
# referral source y class, eliminando así a ambas.
sep_data$referral_source<-NULL
#sep_data$class <- NULL


# -------------------------------------------------------------------
#             Detección  y tratamiento de datos faltantes 
#
# Se encuentran los datos nulos y se ve si es posibles intercambiarlos por la mediana o moda

# Variables continuas:
# A continuación se obtiene la cantidad de filas restantes que no contienen "?" (nulo)
# por cada variable númerica encontrada.

# Edad
age <- age[complete.cases(age)]
rows_age <- NROW(age)             # = 2799
# TSH
tsh <- tsh[complete.cases(tsh)]
rows_tsh <- NROW(tsh)             # = 2516
# T3
t3 <- t3[complete.cases(t3)]
rows_t3 <- NROW(t3)               # = 2215
# TT4
tt4 <- tt4[complete.cases(tt4)]
rows_tt4 <- NROW(tt4)             # = 2616
# T4U
t4u <- t4u[complete.cases(t4u)]
rows_t4u <- NROW(t4u)             # = 2503
# FTI
fti <- fti[complete.cases(fti)]
rows_fti <- NROW(fti)             # = 2505

# Para poder reemplazar los datos faltantes por la media, mediana o moda es necesario que dichos datos sean 
# cercanos al 5% del total de la población.
# 140 corresponde al 5% de 2800.

# Comparando los resultados de las filas recién obtenidas, se ve que ninguna variable cumple con dicho requisito, 
# por eso se decide eliminar las filas del que más pérdida tiene, y así volver a calcular.

# Eliminando "?" de T3
sep_data$T3[sep_data$T3 == "?"] <- NA

sep_data <- sep_data[complete.cases(sep_data),]

# Nuevo total de casos: 2215
# Probando con el resto de variables para ver si es cercano al 5% nuevo (110.75).

# Edad
age <- sep_data$age
age[age == "?"] <- NA
age <- age[complete.cases(age)]
rows_age <- NROW(age)             # = 2214

# TSH
tsh <- sep_data$TSH
tsh[tsh == "?"] <- NA
tsh <- tsh[complete.cases(tsh)]
rows_tsh <- NROW(tsh)             # = 2143

# TT4
tt4 <- sep_data$TT4
tt4[tt4 =="?"] <- NA
tt4 <- tt4[complete.cases(tt4)]
rows_tt4 <- NROW(tt4)             # = 2189

# T4U
t4u <- sep_data$T4U
t4u[t4u =="?"] <- NA
t4u <- t4u[complete.cases(t4u)]
rows_t4u <- NROW(t4u)             # = 2078    6,19%

# FTI
fti <- sep_data$FTI
fti[fti == "?"]<-NA
fti <- fti[complete.cases(fti)]
rows_fti <- NROW(fti)             # = 2080    6,09%


# Esta vez los datos si son cercanos al 5%, por lo que se procede a rellenar los datos nulos
# de cada variable con sus medianas.

# Edad
sep_data$age[sep_data$age == "?"] <- NA
sep_data$age <- as.numeric(as.character(sep_data$age))
sep_data$age[is.na(sep_data$age)] <- median(sep_data$age, na.rm = TRUE)

# TSH
sep_data$TSH[sep_data$tsh == "?"] <- NA
sep_data$TSH <- as.numeric(as.character(sep_data$TSH))
sep_data$TSH[is.na(sep_data$TSH)] <- median(sep_data$TSH, na.rm = TRUE)

# TT4
sep_data$TT4[sep_data$TT4 == "?"] <- NA
sep_data$TT4 <- as.numeric(as.character(sep_data$TT4))
sep_data$TT4[is.na(sep_data$TT4)] <- median(sep_data$TT4, na.rm = TRUE)

# T4U
sep_data$T4U[sep_data$T4U == "?"] <- NA
sep_data$T4U <- as.numeric(as.character(sep_data$T4U))
sep_data$T4U[is.na(sep_data$T4U)] <- median(sep_data$T4U, na.rm = TRUE)

# FTI
sep_data$FTI[sep_data$FTI == "?"] <- NA
sep_data$FTI <- as.numeric(as.character(sep_data$FTI))
sep_data$FTI[is.na(sep_data$FTI)] <- median(sep_data$FTI, na.rm = TRUE)


# Variables discretas:
# La única variable que contiene datos nulos es sexo, por lo que se procede a rellenar con la moda, 
# la cual es femenino.
sep_data$sex[sep_data$sex == "?"] <- "F"


# -------------------------------------------------------------------
#             Detección y tratamiento de datos atípicos 
#
# Los datos que esten fuera de un rango determinado serán considerados como atípicos y se procederan a 
# intercambiar por la mediana

# Variables continuas:
sep_data$age[sep_data$age > 110] <- median(sep_data$age)
sep_data$TSH[sep_data$TSH > 10] <- median(sep_data$TSH)
sep_data$T3[sep_data$T3 > 5] <- median(sep_data$T3)
sep_data$TT4[sep_data$TT4 > 220] <- median(sep_data$TT4)
sep_data$FTI[sep_data$FTI > 200] <- median(sep_data$FTI)


#Se utilizará para calcular Gower
sep_data_factors <- sep_data
sep_data_factors$T3 <- as.numeric(as.character(sep_data_factors$T3))

sep_data_factors$sex <- as.factor(sep_data_factors$sex)
sep_data_factors$on_thyroxine <- as.factor(sep_data_factors$on_thyroxine)
sep_data_factors$query_on_thyroxine <- as.factor(sep_data_factors$query_on_thyroxine)
sep_data_factors$on_antithyroid_medication <- as.factor(sep_data_factors$on_antithyroid_medication)
sep_data_factors$sick <- as.factor(sep_data_factors$sick)
sep_data_factors$pregnant <- as.factor(sep_data_factors$pregnant)
sep_data_factors$thyroid_surgery <- as.factor(sep_data_factors$thyroid_surgery)
sep_data_factors$I131_treatment <- as.factor(sep_data_factors$I131_treatment)
sep_data_factors$query_hypothyroid <- as.factor(sep_data_factors$query_hypothyroid)
sep_data_factors$query_hyperthyroid <- as.factor(sep_data_factors$query_hyperthyroid)
sep_data_factors$lithium <- as.factor(sep_data_factors$lithium)
sep_data_factors$goitre <- as.factor(sep_data_factors$goitre)
sep_data_factors$tumor <- as.factor(sep_data_factors$tumor)
sep_data_factors$psych <- as.factor(sep_data_factors$psych)


#===================================== Componentes principales ====================================#

# Pese a ya haber reducido las variables, se utilizará el método de componentes principales
# para ver si se puede reducir aún más.

# Primero se deben transformar las variables cualitativas a cuantitativas.
# False = 0     Masculino = 0
# True = 1      Femenino = 1

sep_data[sep_data == "M"] <- 0
sep_data[sep_data == "F"] <- 1
sep_data[sep_data == "f"] <- 0
sep_data[sep_data == "t"] <- 1

# Transformar todos los datos a numéricos
# Variables categóricas:
sep_data$sex <- as.numeric(as.character(sep_data$sex))
sep_data$on_thyroxine <- as.numeric(as.character(sep_data$on_thyroxine))
sep_data$query_on_thyroxine <- as.numeric(as.character(sep_data$query_on_thyroxine))
sep_data$on_antithyroid_medication <- as.numeric(as.character(sep_data$on_antithyroid_medication))
sep_data$sick <- as.numeric(as.character(sep_data$sick))
sep_data$pregnant <- as.numeric(as.character(sep_data$pregnant))
sep_data$thyroid_surgery <- as.numeric(as.character(sep_data$thyroid_surgery))
sep_data$I131_treatment <- as.numeric(as.character(sep_data$I131_treatment))
sep_data$query_hypothyroid <- as.numeric(as.character(sep_data$query_hypothyroid))
sep_data$query_hyperthyroid <- as.numeric(as.character(sep_data$query_hyperthyroid))
sep_data$lithium <- as.numeric(as.character(sep_data$lithium))
sep_data$goitre <- as.numeric(as.character(sep_data$goitre))
sep_data$tumor <- as.numeric(as.character(sep_data$tumor))
sep_data$psych <- as.numeric(as.character(sep_data$psych))

# Variables continuas:
sep_data$T3 <- as.numeric(as.character(sep_data$T3))


# Obtener componentes principales
pca_datos <- prcomp(sep_data[1:20], scale = TRUE)
#
pca2 <- PCA(sep_data[1:20], scale.unit = TRUE, ncp = 20, graph = FALSE)

# Cantidad de componentes principales distintas = 20
dim(pca_datos$rotation)

# Varianza de las componentes principales
pca_datos$sdev^2

#===================================== Normalización del rango de los datos ====================================#

# Cuando los datos estan en distintas unidades es recomendable normalizar.

# Data con los datos a normalizar

sep_data_normalized <- data.frame(
  age = sep_data$age, 
  sex = sep_data$sex, 
  on_thyroxine = sep_data$on_thyroxine, 
  query_on_thyroxine = sep_data$query_on_thyroxine, 
  on_antithyroid_medication = sep_data$on_antithyroid_medication, 
  sick = sep_data$sick, 
  pregnant = sep_data$pregnant, 
  thyroid_surgery = sep_data$thyroid_surgery, 
  I131_treatment = sep_data$I131_treatment, 
  query_hypothyroid = sep_data$query_hypothyroid, 
  query_hyperthyroid = sep_data$query_hyperthyroid, 
  lithium = sep_data$lithium, 
  goitre = sep_data$goitre,
  tumor = sep_data$tumor, 
  psych = sep_data$psych, 
  TSH = sep_data$TSH, 
  T3 = sep_data$T3, 
  TT4 = sep_data$TT4, 
  T4U = sep_data$T4U, 
  FTI = sep_data$FTI
)

sep_data_normalized <- normalize_all(sep_data_normalized)
sep_data_normalized <- data.frame(sep_data_normalized,sep_data$class)


#===================================== Obtención del Clúster ====================================#

# Primero se obtendrá la cantidad óptima de centroides a utilizar.
# Método del codo
#fviz_nbclust(sep_data_normalized[1:20], kmeans, method = "wss")

# Método codo para PAM
# elbow_pam <- fviz_nbclust(x = sep_data_normalized[1:20],FUNcluster = pam, method = "wss", k.max = 15,
#              diss = dist(sep_data_normalized[1:20], method = "manhattan"))

# Método silhouette
#fviz_nbclust(sep_data_normalized[1:20], kmeans, method = "silhouette")+theme_classic()

# Método silhouette PAM
# silhouette_pam <- fviz_nbclust(x = sep_data_normalized[1:20],FUNcluster = pam, method = "silhouette", k.max = 15,
#              diss = dist(sep_data_normalized[1:20], method = "manhattan"))


# Los valores de k a implementar son 4, 5 y 6, según los métodos anteriores.
set.seed(123)
k4_means <- kmeans(sep_data_normalized[1:20], centers = 4, nstart = 50)
#k5_means <- kmeans(sep_data_normalized[1:20], centers = 5, nstart = 50)
#k6_means <- kmeans(sep_data_normalized[1:20], centers = 6, nstart = 50)

pam4_cluster <- pam(x = sep_data_normalized[1:20], k = 4, metric = "manhattan")
#pam_cluster <- pam(x = sep_data_normalized[1:20], k = 6, metric = "manhattan")

# algo
#aggregate(sep_data_normalized[1:20],by=list(k4_means$cluster),FUN=mean)
aggregate(sep_data_normalized[1:20],by=list(pam4_cluster$clustering),FUN=mean)

# append cluster assignment

#mydata <- data.frame(sep_data_normalized, k4_means$cluster)
data_final_4k <- data.frame(sep_data_normalized, pam4_cluster$clustering)


frequcies_4cluster <- class_in_cluster_frequency(data_final_4k)


# Falta hacer el de 7, quieres igual los datos para anlaizar o est trabajo que despué no se ocupará?

#-------------------------------------------------------------------
#                      Desnormalizar variables 
#
# Para el clúster con k=4
  
# Desnormalizar e invertir data frame.
# k4_means_desnormalized <- data.frame(t(desnormalize_all_centers(sep_data[1:20], k4_means$centers, 4)))
# k6_means_desnormalized <- data.frame(t(desnormalize_all_centers(sep_data[1:20], k6_means$centers, 6)))

# Desnormalizar e invertir data frame.
#pam_4_desnormalized <- data.frame(t(desnormalize_all_centers(sep_data[1:20], pam_cluster$medoids, 4)))
#pam_6_desnormalized <- data.frame(t(desnormalize_all_centers(sep_data[1:20], pam_cluster$medoids, 6)))

# Desnormalizar mydata
#new_data <- desnormalize_all_dataframe(mydata, sep_data)
#new_data <- data.frame(new_data, mydata[21:23])

# ------------------------------------------------------------------
  
# Distancia gower
gower_dist <- daisy(sep_data_factors[1:20], metric = "gower",type = list(logratio = 3))
dis_datos<-as.matrix(gower_dist)


# ¿Se ocupa esto?
# Elbow con distancia gower
SSE = rep(0, 15)
for (k in 1:15) {
  set.seed(42)
  grupos = kmeans(dis_datos, k)
  SSE[k] = grupos$tot.withinss
}
plot(SSE)

# ----------------------------------------------------
# ¿ Se ocupa?
#asd <- kmeans(dis_datos,4)
sil_width <- c(NA)
for(i in 2:10){
  pam7 <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam7$silinfo$avg.width
}


# Gráfico sihouette para ver que k es mejor.
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#
data.frame(n_clusters = 1:10, media_silhouette = sil_width) %>%
  ggplot(aes(x = n_clusters, y = media_silhouette)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Silhouette Width", x = "Number of clusters k",
       title = "Optimal number of clusters")+
  theme_light()

#--------------------------------------------------------------------------

# PAM 7 cluster
pam7 <- pam(gower_dist, diss = TRUE, k = 7)

elbow_pam2 <- fviz_nbclust(x = sep_data_normalized[1:20],FUNcluster = pam, method = "wss", k.max = 10, diss = dis_datos)

km_clusters <- eclust(x = datos, FUNcluster = "kmeans", k = 3, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 


# ================================== Validación ======================================
#
# km_clusters <- eclust(x = datos, FUNcluster = "pam", k = 4, seed = 123,
#                       hc_metric = "euclidean", nstart = 50, graph = FALSE)
# fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
#                 ggtheme = theme_classic()) 
