# Practica 1 : Introduction to R.
# 30/10/2026  
# Daniel Fuertes
# Oscar Moreno

# 1.1 swap v[2] and v[20]

v1 <- 1:20
v1[c(2, 20)] <- v1[c(20, 2)]
v1

# 1.2 replace NA by the mean

v0 <- c(1,2,3,NA)
seq1 <- rep(v0,5)
seq1 <- replace(seq1,is.na(seq1),mean(v0, na.rm = TRUE)) 
seq1

# 1.3 create Dataframe

df1 <- data.frame(Age = c(22,25,18,20), Name = c('James','Mathew','Olivia','Stella'), Gender = c("M","M","F","F"))

# 1.4 select rows where age >= 21

df1[df1$Age>= 21, ]

# 1.5 create attribute adult

df1$Adult <- rep(c(df1$Age >= 21)) # añado columnas simplemente definiendolas

# 1.6 use apply on rows and columns in dataset

summary(iris)
df2 <- iris[, sapply(iris, is.numeric)] # Quitar la columna Species que no se puede sumar

rowSums(df2)
apply(df2, MARGIN = 1, FUN = sum)   # 1 es para filas
rowMeans(df2)
apply(df2, MARGIN = 1, FUN = mean)

colSums(df2)
apply(df2, MARGIN = 2, FUN = sum)  # 2 es para columnas
colMeans(df2)
apply(df2, MARGIN = 2, FUN = mean) 

# 1.7 create function that returns means by row and column

f <- function(df) {
  df <- df[, sapply(df, is.numeric)] # Quitar columnas que no se pueden sumar
  list(rows = rowMeans(df), cols = colMeans(df))
}

f(df2)
f(iris)

# 1.8 fibonacci fuction

fibonacci <- function(n) {
  if(n <= 0 || n != round(n)) {
    return("n tiene que ser un entero positivo.")
  }
  else if(n == 1) {
    return(1)
  }
  else if(n == 2) {
    return(c(1,1))
  } else {
    fi <- numeric(n)
    fi[c(1,2)] = 1
    fi[2] = 1
    for(i in 3:n) {
      fi[i] = fi[i-1]+fi[i-2]
    }
    fi
  }
}

fibonacci(0)
fibonacci(2)
fibonacci(10)
fibonacci(2.5)

# 1.9 Explore dataset

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Por si tu wd es otro distinto al del script, que es donde estan los datos 
options(width = 400) # setting para que se me vea mejor la consola

tur <- read.table('turistics_oct_2026.csv', header = TRUE, sep = ",")

dim(tur) # Dimensiones rows x columns
# names(tur) # Columnas
# head(tur) # Primeros datos 
# tail(tur) # Últimos datos
str(tur) # Tipo de variables i una muestra
sapply(choices, class) # Clases de las columnas
summary(tur) #Resumen estadístico

levels(factor(tur$Comarca)) # select distinct comarca, los datos se ven bien
names(table(tur$Província)) # nombres de las provincias
table(tur$Tipus.establiment)

# Data Prep

campHotel <- tur[tur$Tipus.establiment %in% c('Hotels', 'Càmpings'), ]
notPisos <- tur[!tur$Tipus.establiment %in% c("Habitatges d'ús turístic", 'Apartaments Turístics'), ]
pisos <- tur[tur$Tipus.establiment %in% c("Habitatges d'ús turístic", 'Apartaments Turístics'), ] 

countPisosComarca <- sort(table(pisos$Comarca),decreasing = TRUE) # tabla de contingencia consigo mismo, ordenada por count
countPisosComarca <- countPisos[1:20] # top 20

countPisosProvincia <- table(pisos$Província)
countNotPisosProvincia <- table (notPisos$Província)

labelsP1 <- paste0(names(countNotPisosProvincia), " (", round(100 * countNotPisosProvincia / sum(countNotPisosProvincia), 1), "%)")
labelsP2 <- paste0(names(countPisosProvincia), " (", round(100 * countPisosProvincia / sum(countPisosProvincia), 1), "%)")

# Plotting

par(mfrow = c(2, 2)) # para hacer un 2x2 de graficos

pie(countPisosProvincia, main = "Pisos turísticos por provincia", labels = labelsP2)

pie(countNotPisosProvincia
  , main = "Hoteles, Rurales, uso compartido y campings " # caravanas es como un camping a mi entender
  , labels = labelsP1)

barplot(countPisos
, las = 2
, cex.names = 0.6
, col = "lightblue"
, main = "20 comarcas con más pisos turísticos"
, ylim = c(0,20000)) # no se como girar los labels a 45 grados sin ggplot

boxplot(Total.estances ~ Tipus.establiment, data = campHotel,
  main = 'Número de estancias en hoteles y campings',
  col = "lightblue",
  ylab = NA,
  xlab = NA,
  las = 1 # horizontal
)

# Exercice block 2

choices <- read.table('Choices dataset.csv', header = TRUE, sep = ";")

dim(choices)
sapply(choices, class) 
summary(choices)

table <- table(choices$INFO,choices$MEASURE)

tableNMargin<- addmargins(table) # añadir sumas 
tableNMargin

tableFMargin <- round(100 * addmargins(prop.table(table)), 2) # hacer la matriz de frecuencias/proporciones y añadir los márgines. Redondear
tableFMargin

chisq.test(table) # pvalue muy inferior a 0.05, hay que rechazar la hipotesis que son independientes
# INFO y MEASURE tienen una relación estadísticamente significativa