# Daniel Fuertes

# Sesión 1: Estadística Básica

# 1 

options(width = 400) # setting para que se me vea mejor la consola

precios <- c(7.80,12.60,15.96,13.50,8.25,31.29,16.46)
summary(precios)
boxplot(precios)

# 5

pisos <- read.table("bcn_pisos.txt", header = TRUE, stringsAsFactors = TRUE)
class(pisos)
summary(pisos)

anyDuplicated(pisos)       # cuenta los duplicados
pisos[duplicated(pisos), ] # muestra los duplicados
pisos <- pisos[!duplicated(pisos), ] # elimina los duplicados

pisos$Valor <- pisos$Valor / 100 # creo que son céntimos
# 6 

head(pisos)


par(mfrow = c(2, 2))

hist(pisos$Valor, main = "Precios de los pisos"
, xlab = "Valor (€)", ylab = "Frecuencia absoluta", col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$Valor))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

hist(pisos$Superf, main = "Superfície de los pisos"
, xlab = "m²", ylab = "Frecuencia absoluta", col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$Superf))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

hist(pisos$Edat, main = "Edad de los pisos"
, xlab = "Años", ylab = "Frecuencia absoluta", col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$Edat))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

hist(pisos$ValSol, main = "Valor del suelo de los pisos"
, xlab = "Valor (€)", ylab = "Frecuencia absoluta", col = "lightblue"
, border = "black", breaks = 30, xaxt = "n", ylim = c(0, 800))
ticks <- pretty(range(pisos$ValSol))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))




par(mfrow = c(3, 3))

barplot(table(pisos$Dorm), main = "Número de dormitorios", col = "lightblue", border = "black")

barplot(table(pisos$Banys), main = "Número de baños", col = "lightblue", border = "black")

barplot(table(pisos$Estat), main = "Estado del de piso", col = "lightblue", border = "black")

barplot(table(pisos$Planta), main = "Planta del piso", col = "lightblue", border = "black")

barplot(table(pisos$Dist), main = "Barrio del piso", col = "lightblue", border = "black", las = 2)

barplot(table(pisos$Tipus), main = "Tipo de piso", col = "lightblue", border = "black")

barplot(table(pisos$Ascens), main = "Tiene ascensor", col = "lightblue", border = "black")

barplot(table(pisos$ExtInt), main = "Exterior o interior", col = "lightblue", border = "black")

barplot(table(pisos$Reforma), main = "Reformas del piso", col = "lightblue", border = "black", las = 2)

par(mfrow = c(1, 1))

