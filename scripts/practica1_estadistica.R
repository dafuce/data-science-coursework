# david.aguilera.luzon@upc.edu

# Daniel Fuertes
# Oscar Moreno
# Marc Torres

# Sesión 1: Estadística Básica

# 1 

options(width = 400) # setting para que se me vea mejor la consola

precios <- c(7.80,12.60,15.96,13.50,8.25,31.29,16.46)
summary(precios, digits = 2, )
sd(precios)
mean(precios)

boxplot(precios)
pdf("../images/pr1_estad_boxplot1.pdf", width = 2, height = 4)
boxplot(precios)
dev.off()

# 2 Como no todo el mundo es experto, la muestra aleatoria representa mejor a la población. La muestra aleatoria es más representativa de la población que la muestra sesgada.


# 3

# 4 Teorema central del límite: La media de la muestra se aproxima a la media de la población a medida que aumenta el tamaño de la muestra. La distribución de la media muestral se aproxima a una distribución normal a medida que aumenta el tamaño de la muestra, independientemente de la distribución de la población.

# 5

pisos <- read.table("../data/bcn_pisos.txt", header = TRUE, stringsAsFactors = TRUE)

str(pisos)
summary(pisos)

anyDuplicated(pisos)       # cuenta los duplicados
pisos[duplicated(pisos), ] # muestra los duplicados
pisos <- pisos[!duplicated(pisos), ] # elimina los duplicados
str(pisos)

#renombrar los levels para que queden bonitos

levels(pisos$Dist) <- c("Ciutat Vella", "Eixample", "Gràcia","Horta","Les Corts","Nou Barris","Sant Andreu","Sant Martí","Sants","Sarrià")
levels(pisos$Tipus) <- c("Bloque", "Manzana")
levels(pisos$Ascens) <- c("No", "Sí")
levels(pisos$ExtInt) <- c("Exterior", "Interior")
levels(pisos$Reforma) <- c("En obra", "Obra Nueva", "Reciente", " > 20 años", " < 15 años", " < 20 años"," < 10 años", " < 5 años", "Sin reformar")
levels(pisos$Estat) <- c("Muy malo", "Malo", "Normal", "Bueno", "Muy bueno")


# 6 

pdf("../images/pr1_estad_histogramas.pdf")
par(mfrow = c(2, 2))

hist(pisos$Valor, main = "Precios de los pisos"
, xlab = "Valor (Pesetas)" , ylab = NA #"Frecuencia absoluta"
, col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$Valor))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))


hist(pisos$Superf, main = "Superfície de los pisos"
, xlab = "m²" , ylab = NA #"Frecuencia absoluta"
, col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$Superf))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

hist(pisos$Edat, main = "Edad de los pisos"
, xlab = "Años" , ylab = NA #"Frecuencia absoluta"
, col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$Edat))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

hist(pisos$ValSol, main = "Valor del m² de los pisos"
, xlab = "Valor (Pesetas)" , ylab = NA #"Frecuencia absoluta"
, col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$ValSol))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

dev.off()


pdf("../images/pr1_estad_barplots.pdf")
par(mfrow = c(3, 3))

barplot(table(pisos$Dorm), main = "Número de dormitorios", col = "lightgray", border = "black")

barplot(table(pisos$Banys), main = "Número de baños", col = "lightgray", border = "black")

barplot(table(pisos$Planta), main = "Planta del piso", col = "lightgray", border = "black")

barplot(table(pisos$Estat), main = "Estado del de piso", col = "lightgray", border = "black", las = 2) # las = 2 para labels verticales

barplot(table(pisos$Dist), main = "Barrio del piso", col = "lightgray", order = "black", las = 2)

barplot(table(pisos$Reforma), main = "Reformas del piso", col = "lightgray", border = "black", las = 2)

barplot(table(pisos$Tipus), main = "Tipo de piso", col = "lightgray", border = "black")

barplot(table(pisos$Ascens), main = "Tiene ascensor", col = "lightgray", border = "black")

barplot(table(pisos$ExtInt), main = "Exterior o interior", col = "lightgray", border = "black")


dev.off()
par(mfrow = c(1, 1))

# 7 : identify outliers on continuous variables

# Install once if needed: install.packages("DescTools")
#install.packages('httr')
#install.packages('curl')
library(DescTools)
library(ggplot2)

pisos_cont <- pisos[, c("Valor", "Superf", "Edat", "ValSol")] # me quedo solo con las variables continuas
outlier.scores <- LOF(scale(pisos_cont), k = 5) # aplicamos Local Outlier Factor

boxplot(outlier.scores) # vemos que dos estan claramente separados, y un tercero
outliers <- order(outlier.scores, decreasing=T)[1:2] # selecciono el top 2
cbind(outlier.scores[outliers],pisos_cont[outliers,]) # info sonbre los outliers

outliers.scores <- LOF(scale(pisos_cont), k = 5)
outlier <- order(outlier.scores, decreasing=T)[1:10]
cbind(outlier.scores[outliers], pisos_cont[outliers,])

pisos_cont_so <- pisos_cont[-outliers, ] # quito los outliers de pisos
pisos_so <- pisos[-outliers,]

# 8 Plot Valor vs Superf, correlation and covariance, dos continuas

# voy a mostrar como es con outliers y sin


pdf("../images/pr1_scaterplot1.pdf", width = 14, height = 7)

par(mfrow = c(1,2))
plot(pisos_cont$Superf, pisos$Valor/1000000,
     main = "Precio según superficie con outliers",
     xlab = "Superficie (m²)",
     ylab = "Precio (Millones de Pesetas)",
     pch = 19, col = rgb(0, 0, 0, 0.5))
#abline(lm(Valor ~ Superf, data = pisos), col = "blue", lwd = 2)

plot(pisos_cont_so$Superf, pisos_so$Valor/1000000,
     main = "Precio según superficie sin outliers",
     xlab = "Superficie (m²)",
     ylab = "Precio (Millones de Pesetas)",
     pch = 19, col = rgb(0, 0, 0, 0.5))
#abline(lm(Valor ~ Superf, data = pisos_so), col = "blue", lwd = 2)

dev.off()

# 269375175 covarianza positiva, más superficie más precio

cov(pisos_cont_so$Valor, pisos_cont_so$Superf) 
cor(pisos_cont_so$Valor, pisos_cont_so$Superf) 
# 0.8845801 es una correlación bastante cercana a 1

# 9 Asociación entre Barrio y Valor, una categorica y una continua, F de Fisher

pdf("../images/pr1_boxplot2.pdf", width = 14, height = 7)
boxplot(Valor/1000000 ~ Dist, data = pisos_so,
        main = "Precio según barrio",
        xlab = NULL,
        ylab = "Precio (Millones de Pesetas)",
        las = 2,
        cex.axis = 0.7
        )
dev.off()
# F de fisher

oneway.test(pisos_so$Valor ~ pisos_so$Dist, var.equal = TRUE) 

# ANOVA de un factor: prueba F de Fisher-Snedecor

# p-value sale muy muy pequeño. Hay que rechazar hipótesis nula y aceptar que el barrio si inluye en el precio

# 10 Asociación entre Estado y Distrito, dos categóricas, chi-squared

t <- table(pisos_so$Dist, pisos_so$Estat)
ct <- addmargins(t)

dimnames(ct)[[1]][nrow(ct)] <- "N_j (f_j)"
dimnames(ct)[[2]][ncol(ct)] <- "N_i (f_i)"

ft <- round(addmargins(prop.table(t)), 2)

ct_ft <- matrix(
     as.character(as.vector(ct)),
     nrow = nrow(ct),
     dimnames = dimnames(ct)
)
ct_ft[nrow(ct_ft), ] <- sprintf("%d (%.2f)", ct[nrow(ct), ], ft[nrow(ft), ])
ct_ft[, ncol(ct_ft)] <- sprintf("%d (%.2f)", ct[, ncol(ct)], ft[, ncol(ft)])
ct_ft


# Estas dos variables categoricas, se hacen con el chi-squared 

chisq.test(pisos_so$Dist, pisos_so$Estat) 

