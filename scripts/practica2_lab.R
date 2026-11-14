# Daniel Fuertes
# Oscar Moreno
# Marc Torres

# 1 Pipeline Operator |> 

# a. Squared root of squared 

5 |> (\(x) x^2)()
5 |> (\(x) x^2)() |> sqrt()

# b. 

rnorm(n = 1000, mean = 5, sd = 3) |> scale() |> max()

# c. 

rnorm(n = 1000, mean = 5, sd = 3) |> scale() |> as.numeric() |> (\(x) { plot(density(x), main = "Density of x"); max(x) })()

# 2 pisos dataset

# preparacion inicial

install.packages("here")
library(here)
#getwd()

options(width = 400) # consola mas ancha para ver bien el summary

pisos <- read.table("data/bcn_pisos.txt", header = TRUE, stringsAsFactors = TRUE)

#renombrar los levels para que queden bonitos

levels(pisos$Dist) <- c("Ciutat Vella", "Eixample", "Gràcia","Horta","Les Corts","Nou Barris","Sant Andreu","Sant Martí","Sants","Sarrià")
levels(pisos$Tipus) <- c("Bloque", "Manzana")
levels(pisos$Ascens) <- c("No", "Sí")
levels(pisos$ExtInt) <- c("Exterior", "Interior")
levels(pisos$Reforma) <- c("En obra", "Obra Nueva", "Reciente", " > 20 años", " < 15 años", " < 20 años"," < 10 años", " < 5 años", "Sin reformar")
levels(pisos$Estat) <- c("Muy malo", "Malo", "Normal", "Bueno", "Muy bueno")

str(pisos)

# a 

dim(pisos)
pisos[!duplicated(pisos),] |> (\(x){dim(x)})()

pisos[!duplicated(pisos),] |> (\(x){aggregate(Valor ~ Dist, data = x, FUN = mean)})()

# b


# str(pisos) # Valor, Superf, Dorm, Banys, Edat, ValSol ( Dorm i Banys como categorico?)

pisos[!duplicated(pisos),] |> (\(x){x[, vapply(x, is.numeric, logical(1)), drop = FALSE]})()

pisos[!duplicated(pisos),] |> (\(x){x[, vapply(x, is.numeric, logical(1)), drop = FALSE]})() |> (\(x){scale(x)})()

# c 


# usamos una lista para poderle pasar el df y los aggregates al siguiente paso de la pipeline

pisos[!duplicated(pisos),] |> (\(df) list(df = df,aggregates = aggregate(Valor ~ Dist, data = df, FUN = mean)))() 


pisos[!duplicated(pisos),] |> (\(df) list(df = df,aggregates = aggregate(Valor ~ Dist, data = df, FUN = mean)))() |> (\(x) {
	x$df$Greater_than_its_mean <- factor(
		x$df$Valor >= x$aggregates$Valor[match(x$df$Dist, x$aggregates$Dist)],
		levels = c(FALSE, TRUE), labels = c("N", "Y")
	)
	x$df
})()
