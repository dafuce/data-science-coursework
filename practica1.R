# Daniel Fuertes
# Oscar Moreno

# 1.1 swap v[2] and v[20]

v1 <- seq(from = 1, to = 20)
tmp <- v1[20]
v1[20] <- v1[2]
v1[2] <- tmp
rm(tmp)

# 1.2 replace NA by the mean

v0 <- c(1,2,3,NA)
seq1 <- rep(v0,5)
seq1 <- replace(seq1,is.na(seq1),mean(v0, na.rm = TRUE)) 
rm(v0)

# 1.3 create Dataframe

df1 <- data.frame(Age = c(22,25,18,20), Name = c('James','Mathew','Olivia','Stella'), Gender = c("M","M","F","F"))

# 1.4 select rows where age >= 21

df1[df1$Age>= 21, ]

# 1.5 create attribute adult

df1$Adult <- rep(c(df1$Age >= 21))

# 1.6 use apply on rows and columns in dataset

summary(iris)
df2 <- iris[, sapply(iris, is.numeric)] # Quitar la columna Species que no se puede sumar

rowSums(df2)
apply(df2, MARGIN = 1, FUN = sum)   # 1 is for rows
colMeans(df2)
apply(df2, MARGIN = 2, FUN = mean)  # 2 is for columns

# 1.7 create function that returns means by row and column

f <- function(df) {
  list(rows = rowMeans(df), cols = colMeans(df))
}
f(df2)

# 1.8 fibonacci fuction

fibonnaci <- function(n) {
  fi <- numeric(n)
  fi[c(1,2)] = 1
  fi[2] = 1
  for(i in 3:n) {
    fi[i] = fi[i-1]+fi[i-2]
  }
  fi
 }

fi <- fibonnaci(400)

# 1.9 Explore dataset

# http://opendata-ajuntament.barcelona.cat/data/es/dataset # no funciona ahora mismo
# https://analisi.transparenciacatalunya.cat/Turisme/Establiments-d-allotjament-tur-stic-inscrits-al-Re/t2h3-cgys/about_data # saco de aqui

tur <- read.table('turistics_oct_2026.csv', header = TRUE, sep = ",")
summary(tur)

levels(factor(tur$Categoria))
levels(factor(tur$Província))

counts <- table(tur$Comarca)
counts <- sort(counts, decreasing = TRUE)   # biggest first

barplot(counts,las = 2,cex.names = 0.7, main = "Establiments turístics per Comarca", ylim = c(0,20000))

library(ggplot2)
ggplot(tur, aes(x = Comarca)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exercice block 2

setwd("/home/daniel/Documents/Master/Data Science/Intro_R_2026")

choices <- read.table('Choices dataset.csv', header = TRUE, sep = ";")

class(choices$MEASURE)
class(choices$ID)
class(choices$INFO)
class(choices$COUNTRY)
class(choices$CHOICE_ID)

summary(choices)

tableCon <- table(choices$INFO,choices$MEASURE)

chisq.test(tableCon) # pvalue enano, hay que rechazar la hipotesis que son independientes
