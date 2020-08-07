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

library(cluster)


#library(GGally)
#library(plotly)

#library(tidyverse)


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


# ----------------------------------------
frecuencias <- function(col, name){
  frequencies <- data.frame(
    True = NROW(subset(col, col == 1)),
    False = NROW(subset(col, col == 0))
  )
  rownames(frequencies) <- c(name)
  return(frequencies)
}
#-----------------------------------------



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
  total_frequency <- rbind(get_col_frequency(data_frame$on_thyroxine, "on thyroxine"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$query_on_thyroxine, "query on thyroxine"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$on_antithyroid_medication, "on antithyroid medication"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$sick, "sick"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$pregnant, "pregnant"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$thyroid_surgery, "thyroid surgery"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$I131_treatment, "I131 treatment"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$query_hypothyroid, "query hypothyroid"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$query_hyperthyroid, "query hyperthyroid"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$lithium, "lithium"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$goitre, "goitre"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$tumor, "tumor"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$hypopituitary, "hypopituitary"))
  total_frequency <- rbind(total_frequency, get_col_frequency(data_frame$psych, "psych"))
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
  data_final <- cbind(data_final, normalize(data_frame$on_thyroxine, "on thyroxine"))
  data_final <- cbind(data_final, normalize(data_frame$query_on_thyroxine, "query on thyroxine"))
  data_final <- cbind(data_final, normalize(data_frame$on_antithyroid_medication, "on antithyroid medication"))
  data_final <- cbind(data_final, normalize(data_frame$sick, "sick"))
  data_final <- cbind(data_final, normalize(data_frame$pregnant, "pregnant"))
  data_final <- cbind(data_final, normalize(data_frame$thyroid_surgery, "thyroid surgery"))
  data_final <- cbind(data_final, normalize(data_frame$I131_treatment, "I131 treatment"))
  data_final <- cbind(data_final, normalize(data_frame$query_hypothyroid, "query hypothyroid"))
  data_final <- cbind(data_final, normalize(data_frame$query_hyperthyroid, "query hyperthyroid"))
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


on_thyroxine <- sep_data$`on thyroxine`
query_on_thyroxine <- sep_data$`query on thyroxine`
on_antithyroid_medication <- sep_data$`on antithyroid medication`
thyroid_surgery <- sep_data$`thyroid surgery`
I131_treatment <- sep_data$`I131 treatment`
query_hypothyroid <- sep_data$`query hypothyroid`
query_hyperthyroid <- sep_data$`query hyperthyroid`



# data frame con variables cualitativas 
data_cualitative <- data.frame(
  "on_thyroxine" = on_thyroxine,
  "query_on_thyroxine" = query_on_thyroxine,
  "on_antithyroid_medication" = on_antithyroid_medication,
  "sick" = sep_data$sick,
  "pregnant" = sep_data$pregnant,
  "thyroid_surgery" = thyroid_surgery,
  "I131_treatment" = I131_treatment,
  "query_hypothyroid" = query_hypothyroid,
  "query_hyperthyroid" = query_hyperthyroid,
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

# Esta variable auxiliar será utilizada para hacer cálculos más adelante
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
sep_data$`TBG measured` <- NULL

# Además, al tener valores en el resto de las variables numéricas hace que todas las variables que tengan
# "measured" no entreguen mayor información, ya que si hay valor entonces el individuo se realizó el examen.
sep_data$`TSH measured` <- NULL
sep_data$`T3 measured` <- NULL
sep_data$`TT4 measured` <- NULL
sep_data$`T4U measured` <- NULL
sep_data$`FTI measured` <- NULL

# Con el reconocimiento de datos previamente hecho, se vió que la variable hypopituitary no tenía una
# gran variación en los resultados, donde había 1 verdadero y 2799 falsos, por este motivo se elimina.
sep_data$hypopituitary <- NULL

# Otras variables discretas que no entregan mayor información al estudio con respecto a los calculos son
# referral source y class, eliminando así a ambas.
sep_data$`referral source`<-NULL
sep_data$class <- NULL


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



#===================================== Reducción de dimensionalidad ====================================#

# Pese a ya haber reducido las variables, se utilizará el método de componentes principales
# para ver si se puede reducir aún más.

# -------------------------------------------------------------------
#                    Componentes principales 
#

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
sep_data$`on thyroxine` <- as.numeric(as.character(sep_data$`on thyroxine`))
sep_data$`query on thyroxine` <- as.numeric(as.character(sep_data$`query on thyroxine`))
sep_data$`on antithyroid medication` <- as.numeric(as.character(sep_data$`on antithyroid medication`))
sep_data$sick <- as.numeric(as.character(sep_data$sick))
sep_data$pregnant <- as.numeric(as.character(sep_data$pregnant))
sep_data$`thyroid surgery` <- as.numeric(as.character(sep_data$`thyroid surgery`))
sep_data$`I131 treatment` <- as.numeric(as.character(sep_data$`I131 treatment`))
sep_data$`query hypothyroid` <- as.numeric(as.character(sep_data$`query hypothyroid`))
sep_data$`query hyperthyroid` <- as.numeric(as.character(sep_data$`query hyperthyroid`))
sep_data$lithium <- as.numeric(as.character(sep_data$lithium))
sep_data$goitre <- as.numeric(as.character(sep_data$goitre))
sep_data$tumor <- as.numeric(as.character(sep_data$tumor))
sep_data$psych <- as.numeric(as.character(sep_data$psych))

# Variables continuas:
sep_data$T3 <- as.numeric(as.character(sep_data$T3))


# Obtener componentes principales
pca_datos <- prcomp(sep_data, scale = TRUE)
#
pca2 <- PCA(sep_data, scale.unit = TRUE, ncp = 20, graph = FALSE)

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
  on_thyroxine = sep_data$`on thyroxine`, 
  query_on_thyroxine = sep_data$`query on thyroxine`, 
  on_antithyroid_medication = sep_data$`on antithyroid medication`, 
  sick = sep_data$sick, 
  pregnant = sep_data$pregnant, 
  thyroid_surgery = sep_data$`thyroid surgery`, 
  I131_treatment = sep_data$`I131 treatment`, 
  query_hypothyroid = sep_data$`query hypothyroid`, 
  query_hyperthyroid = sep_data$`query hyperthyroid`, 
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


#===================================== Obtención del Clúster ====================================#

# Primero se obtendrá la cantidad óptima de centroides a utilizar.
# Método del codo
fviz_nbclust(sep_data_normalized, kmeans, method = "wss")
# Método silhouette
fviz_nbclust(sep_data_normalized, kmeans, method = "silhouette")+theme_classic()

# Los valores de k a implementar son 4, 5 y 6, según los métodos anteriores.
set.seed(123)
k4_means <- kmeans(sep_data_normalized, centers = 4, nstart = 50)
k5_means <- kmeans(sep_data_normalized, centers = 5, nstart = 50)
k6_means <- kmeans(sep_data_normalized, centers = 6, nstart = 50)




fviz_cluster(k4_means, data = sep_data_normalized, frame.type = "convex")


### Usando pam
pam.res <- pam(sep_data_normalized, 4)
# Visualize
fviz_cluster(pam.res)


# Intento para graficar
aggregate(sep_data_normalized, by=list(k4_means$cluster), FUN = mean)
# Para visualizar
par(mfrow=c(1,2))
clusplot(sep_data_normalized, k4_means$cluster, color=TRUE, shade=TRUE, labels=0.1, lines=0, main="k medias")


d <- dist(sep_data_normalized, method = "euclidean")




#----------------------------------------------------

X <- 1:2215
Y <- 1:2215


sep_data_normalized$cluster <- k_means$cluster
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = sep_data_normalized, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = k_means$centers[, 1], y = k_means$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 4 / K-Medias') + 
  xlab('X') + ylab('Y')




#data <- sep_data_normalized %>% mutate(cluster = km_clusters$cluster)
#data <- sep_data_normalized %>% mutate(cluster = as.factor(cluster),
#                          grupo   = as.factor(grupo))

#ggplot(data = data, aes(x = x, y = y, color = grupo)) +
#  geom_text(aes(label = cluster), size = 5) +
#  theme_bw() +
#  theme(legend.position = "none")
