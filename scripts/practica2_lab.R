# Daniel Fuertes
# Oscar Moreno
# Marc Torres

install.packages("dplyr")
library(dplyr)
# 1 Pipeline Operator |> or %>% 

# a. Squared root of squared 

square <- function(x)(x^2)

5 |> square() |> sqrt()

square_and_root <- . %>% square() %>% sqrt() # el . es una referencia a un valor numerico

square_and_root(5)

# b. 


rnorm(n = 1000, mean = 5, sd = 3) |> scale() |> max()

get_sample_scaled <- function() {
	rnorm(n = 1000, mean = 5, sd = 3) %>% scale()
}

get_sample_scaled() %>% max()

# c. 

density_plot_and_max <- function(x) {
	plot(density(x), main = "Density of x")
	max(x)
}

rnorm(n = 1000, mean = 5, sd = 3) |> scale() |> as.numeric() |> (\(x) { plot(density(x), main = "Density of x"); max(x) })()

get_sample_scaled() %>% as.numeric() %>% density_plot_and_max()

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

remove_duplicates <- function(df) {df[!duplicated(df),]}
valor_mean_by_dist <- function(df) {aggregate(Valor ~ Dist, data = df, FUN = mean)}

pisos %>% remove_duplicates() %>% valor_mean_by_dist()

# b


# str(pisos) # Valor, Superf, Dorm, Banys, Edat, ValSol ( Dorm i Banys como categorico?)

pisos[!duplicated(pisos),] |> (\(x){x[, vapply(x, is.numeric, logical(1)), drop = FALSE]})()

pisos[!duplicated(pisos),] |> (\(x){x[, vapply(x, is.numeric, logical(1)), drop = FALSE]})() |> (\(x){scale(x)})()

keep_numerics <- function(df) {df[, vapply(df, is.numeric, logical(1)), drop = FALSE]}

pisos %>% remove_duplicates() %>% keep_numerics() %>% scale()

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

# alternativa con función de dos parámetros

add_greater_than_mean_col <- function(df, x) {
	df$Greater_than_its_mean <- factor(
		df$Valor >= x$Valor[match(df$Dist, x$Dist)],
		levels = c(FALSE, TRUE), labels = c("N", "Y")
	)
	df
}

pisos %>% remove_duplicates() %>% { add_greater_than_mean_col(., valor_mean_by_dist(.)) } # el . es referencia a pisos con remove duplicates

# 3

# preparacion del dataframe usando sql-like interface
install.packages("sqldf")
library(sqldf)

pisos <- pisos %>% remove_duplicates()
distinfosql <- sqldf("
SELECT 
	Dist													-- AS DistrictName
	, COUNT(CASE WHEN Dorm = 1 THEN 1 END)	 				-- AS [1Dorm] 
	, COUNT(CASE WHEN Dorm = 2 THEN 1 END)	 				-- AS [2Dorm]
	, COUNT(CASE WHEN Dorm = 3 THEN 1 END)	 				-- AS [3Dorm]
	, COUNT(CASE WHEN Dorm = 4 THEN 1 END)	 				-- AS [4Dorm]
	, COUNT(CASE WHEN Dorm = 5 THEN 1 END)	 				-- AS [5Dorm]
	, AVG(Valor) 											-- AS Valor
	, COUNT(CASE WHEN Ascens = 'Sí' THEN 1 END) 			-- AS AscS
	, COUNT(CASE WHEN Ascens = 'No' THEN 1 END) 			-- AS AscN
	, COUNT(CASE WHEN Planta = 'Atic' THEN 1 END)			-- AS Atics
	, COUNT(CASE WHEN Planta = 'Planta' THEN 1 END)			-- AS Planta
	, COUNT(CASE WHEN Planta = 'Bajos' THEN 1 END)			-- AS Bajos
	, COUNT(CASE WHEN Edat <= 10 THEN 1 END) 				-- AS Nous
	, COUNT(CASE WHEN Edat BETWEEN 11 AND 20 THEN 1 END) 	-- AS SemiNous
	, COUNT(CASE WHEN Edat BETWEEN 21 AND 49 THEN 1 END) 	-- AS Vells
	, COUNT(CASE WHEN Edat >= 50 THEN 1 END) 				-- AS MoltVells
	, AVG(Superf) 											-- AS Superf
	, COUNT(*)												-- AS Obs
FROM pisos
GROUP BY Dist
") # los aliases no van :/ hay que cambiar los names a mano

names(distinfosql) <- c(
	"DistrictName", "1Dorm", "2Dorm", "3Dorm", "4Dorm", "5Dorm",
	"Valor", "AscS", "AscN", "Atics", "Planta", "Bajos",
	"Nous", "SemiNous", "Vells", "MoltVells", "Superf", "Obs"
)

distinfosql

# Alternative grouping with a pipeline and base R data-frame functions

numericValues <- function(x) as.numeric(as.character(x)) # esto deberia ya estar en el tipo correcto en pisos?

distStats <- function(df) {
	dorm <- numericValues(df$Dorm)
	edat <- numericValues(df$Edat)
	valor <- numericValues(df$Valor)
	superf <- numericValues(df$Superf)
	data.frame(
		DistrictName = unique(df$Dist),
		`1Dorm` = sum(dorm == 1, na.rm = TRUE),
		`2Dorm` = sum(dorm == 2, na.rm = TRUE),
		`3Dorm` = sum(dorm == 3, na.rm = TRUE),
		`4Dorm` = sum(dorm == 4, na.rm = TRUE),
		`5Dorm` = sum(dorm == 5, na.rm = TRUE),
		Valor = mean(valor, na.rm = TRUE),
		AscS = sum(df$Ascens == "Sí", na.rm = TRUE),
		AscN = sum(df$Ascens == "No", na.rm = TRUE),
		Atics = sum(df$Planta == "Atic", na.rm = TRUE),
		Planta = sum(df$Planta == "Planta", na.rm = TRUE),
		Bajos = sum(df$Planta == "Bajos", na.rm = TRUE),
		Nous = sum(edat <= 10, na.rm = TRUE),
		SemiNou = sum(edat >= 11 & edat <= 20, na.rm = TRUE),
		Vells = sum(edat >= 21 & edat < 50, na.rm = TRUE),
		MoltVell = sum(edat >= 50, na.rm = TRUE),
		Superf = mean(superf, na.rm = TRUE),
		Obs = nrow(df),
		check.names = FALSE
	)
}

distinfo <- split(pisos, pisos$Dist, drop = TRUE) %>% lapply(distStats) %>% { do.call(rbind, .) }
rownames(distinfo) <- NULL # borrar los nombres de las filas y que ponga solo del 1 al 10

distinfo
distinfosql

# A pintar !!!


stacked_prop <- function(df, columns) {
	counts <- as.matrix(df[, columns, drop = FALSE])
	t(sweep(counts, 1, df$Obs, "/"))
}

old_par <- par(mfrow = c(1, 1), mar = c(6, 4, 3, 10) + 0.1)

barplot(
	distinfo %>% stacked_prop(c("1Dorm", "2Dorm", "3Dorm", "4Dorm", "5Dorm")),
	 beside = FALSE,
	 names.arg = distinfo$DistrictName,
	 col = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
	 ylim = c(0, 1),
	 las = 2,
	 main = "Distribución de número de dormitorios por barrio",
	 legend.text = c("1","2","3","4","5"),
	 args.legend = list(x = "topright", inset = c(-0.15, 0), xpd = NA, bty = "n")
)
	
barplot(
	distinfo %>% stacked_prop(c("Nous", "SemiNou", "Vells", "MoltVell")),
	 beside = FALSE,
	 names.arg = distinfo$DistrictName,
	 col = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
	 ylim = c(0, 1),
	 las = 2,
	 main = "Distribución de antigüedad de los pisos por barrio",
	 legend.text = c("Nous","Semi Nous","Vells","Molt Vells"),
	 args.legend = list(x = "topright", inset = c(-0.30, 0), xpd = NA, bty = "n")
)

barplot(
	distinfo %>% stacked_prop(c("Bajos", "Planta", "Atics")),
	 beside = FALSE,
	 names.arg = distinfo$DistrictName,
	 col = c("#1b9e77", "#d95f02", "#7570b3"),
	 ylim = c(0, 1),
	 las = 2,
	 main = "Distribución de las plantas de los pisos por barrio",
	 legend.text = c("Bajos", "Plantas", "Aticos"),
	 args.legend = list(x = "topright", inset = c(-0.25, 0), xpd = NA, bty = "n")
)

barplot(
	distinfo %>% stacked_prop(c("AscS", "AscN")),
	 beside = FALSE,
	 names.arg = distinfo$DistrictName,
	 col = c("#1b9e77", "#d95f02"),
	 ylim = c(0, 1),
	 las = 2,
	 main = "Pisos con ascensor o no por barrio",
	 legend.text = c("Si", "No"),
	 args.legend = list(x = "topright", inset = c(-0.15, 0), xpd = NA, bty = "n")
)

# Valor and Superf have different units, so plot them in separate panels.
precio_m2 <- with(distinfo, (Valor / 1e6) / Superf)
par(mfrow = c(3, 1), mar = c(8, 5, 2, 1) + 0.1)

barplot(
	distinfo$Valor/1000000,
	 names.arg = distinfo$DistrictName,
	 las = 2,
	 col = "#1b9e77",
	 ylab = "MPtas.",
	 main = "Valor medio por barrio"
)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")

barplot(
	distinfo$Superf,
	 names.arg = distinfo$DistrictName,
	 las = 2,
	 col = "#7570b3",
	 ylab = "m^2",
	 main = "Superficie media por barrio"
)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")

barplot(
	precio_m2,
	 names.arg = distinfo$DistrictName,
	 las = 2,
	 col = "#e1816c",
	 ylab = "MPtas. / m^2",
	 main = "Precio medio del metro cuadrado por barrio"
)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")

par(old_par)


