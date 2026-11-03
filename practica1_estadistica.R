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

#renombrar los levels de la variable Estat
levels(pisos$Estat) <- c("Muy malo", "Malo", "Normal", "Bueno", "Muy bueno")

pisos$Estat

levels(pisos$Dist) <- c("Ciutat Vella", "Eixample", "Gràcia","Horta","Les Corts","Nou Barris","Sant Andreu","Sant Martí","Sants","Sarrià")
levels(pisos$Tipus) <- c("Bloque", "Manzana")
levels(pisos$Ascens) <- c("No", "Sí")
levels(pisos$ExtInt) <- c("Exterior", "Interior")
levels(pisos$Reforma) <- c("En obra", "Obra Nueva", "Reciente", "Hace más de 20 años", "Hace más de 10 años", "Hace más de 15 años","Hace más de 5 años", "Hace menos de 5 años", "Sin reformar")
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

barplot(table(pisos$Estat), main = "Estado del de piso", col = "lightblue", border = "black", las = 2) # las = 2 para labels verticales

barplot(table(pisos$Planta), main = "Planta del piso", col = "lightblue", border = "black")

barplot(table(pisos$Dist), main = "Barrio del piso", col = "lightblue", border = "black", las = 2)

barplot(table(pisos$Tipus), main = "Tipo de piso", col = "lightblue", border = "black")

barplot(table(pisos$Ascens), main = "Tiene ascensor", col = "lightblue", border = "black")

barplot(table(pisos$ExtInt), main = "Exterior o interior", col = "lightblue", border = "black")

barplot(table(pisos$Reforma), main = "Reformas del piso", col = "lightblue", border = "black", las = 2)

par(mfrow = c(1, 1))

# 7 : identify outliers on continuous variables

boxplot(pisos$Valor)

# cual es el threshold para considerar un piso como outlier? 99 pctil?

quantile(pisos$Valor, probs = 0.99)

pisos_99 <-pisos[pisos$Valor < quantile(pisos$Valor, probs = 0.99), ]

hist(pisos_99$Valor, main = "Precios de los pisos"
, xlab = "Valor (€)", ylab = "Frecuencia absoluta", col = "lightblue"
, border = "black", breaks = 30, xaxt = "n")
ticks <- pretty(range(pisos_99$Valor))
axis(1, at = ticks, labels = format(ticks, big.mark = ".", decimal.mark = ","
, scientific = FALSE, trim = TRUE))

pisos_99 <- pisos_99[pisos_99$Superf < quantile(pisos_99$Superf, probs = 0.99), ]
pisos_99 <- pisos_99[pisos_99$Edat < quantile(pisos_99$Edat, probs = 0.99), ]
pisos_99 <- pisos_99[pisos_99$ValSol < quantile(pisos_99$ValSol, probs = 0.99), ]

length(pisos_99$Valor)
length(pisos$Valor) # me he quitado 96 de 2254 pisos

# 8 Plot Valor vs Superf, correlation and covariance

ggplot(pisos_99, aes(x = Superf, y = Valor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Precio según superficie",
    x = "Superficie (m²)",
    y = "Precio (€)"
  )

cov(pisos_99$Valor, pisos_99$Superf) # 1517380 covarianza positiva, más superficie más precio
cor(pisos_99$Valor, pisos_99$Superf) # 0.859 es bastante correlación, está claro

# 9 Asociación entre Barrio y Valor

ggplot(pisos_99, aes(x = Dist, y = Valor)) +
  geom_boxplot() +
  labs(
    title = "Precio según barrio",
    x = NULL,
    y = "Precio (€)"
  ) +
  scale_y_continuous(labels = function(x) format(
    x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE
  ))

# 10 Asociación entre Estado y Valor

  ggplot(pisos_99, aes(x = Estat, y = Valor)) +
  geom_boxplot() +
  labs(
    title = "Precio según estado del piso",
    x = NULL,
    y = "Precio (€)"
  ) +
  scale_y_continuous(labels = function(x) format(
    x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE
  ))
