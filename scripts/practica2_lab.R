# Daniel Fuertes
# Oscar Moreno
# Marc Torres

#install.packages("dplyr")
library(dplyr)
# 1 Pipeline Operator |> o %>% con library

# a. 

square <- function(x)(x^2)

square_and_root <- . %>% square() %>% sqrt() # el . es una referencia a un valor numerico en este caso

square_and_root(5)

# b. 

get_sample_scaled <- function() {
	rnorm(n = 1000, mean = 5, sd = 3) %>% scale()
}

get_sample_scaled() %>% max()

# c. 

density_plot_and_max <- function(x) {
	plot(density(x), main = "Density of x")
	max(x)
}

get_sample_scaled() %>% as.numeric() %>% density_plot_and_max()

# 2 pisos dataset

# preparacion inicial

#install.packages("here")
#library(here)
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

remove_duplicates <- function(df) {df[!duplicated(df),]}

valor_mean_by_dist <- function(df) {aggregate(Valor ~ Dist, data = df, FUN = mean)}

pisos %>% remove_duplicates() %>% valor_mean_by_dist()

# distinct ya me hace lo mismo, para que voy crear una funcion yo, igual con el group by

pisos %>% distinct(.) %>% valor_mean_by_dist()

# b

# str(pisos) # Valor, Superf, Dorm, Banys, Edat, ValSol ( Dorm i Banys como categorico?)

keep_numerics <- function(df) {df[, vapply(df, is.numeric, logical(1))]} # logical(1) indica que is.numeric debe devolver un true o false por columna.

pisos %>% remove_duplicates() %>% keep_numerics() %>% scale()

# c 

# usamos una lista para poderle pasar el df y los aggregates al siguiente paso de la pipeline

add_greater_than_mean_col <- function(df, x) {
	df$Greater_than_its_mean <- factor(
		df$Valor >= x$Valor[match(df$Dist, x$Dist)],
		levels = c(FALSE, TRUE), labels = c("N", "Y")
	)
	df
}

pisos %>%
	remove_duplicates() %>%
	{ print(add_greater_than_mean_col(., valor_mean_by_dist(.))) } # %T% muestra el resultado, pero conserva el data frame original

# version con un pipeline mas fancy

pisos %>% distinct() %>% group_by(Dist) %>% mutate( `Greater than is mean` = factor( if_else(Valor > mean(Valor, na.rm=T), "Y", "N") ) )

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

# Alternativa con pipelines
 
distinfo <- pisos  %>% distinct() %>%
	group_by(DistrictName = Dist) %>%
	summarise(
		`1Dorm` = sum(as.numeric(as.character(Dorm)) == 1, na.rm = TRUE),
		`2Dorm` = sum(as.numeric(as.character(Dorm)) == 2, na.rm = TRUE),
		`3Dorm` = sum(as.numeric(as.character(Dorm)) == 3, na.rm = TRUE),
		`4Dorm` = sum(as.numeric(as.character(Dorm)) == 4, na.rm = TRUE),
		`5Dorm` = sum(as.numeric(as.character(Dorm)) == 5, na.rm = TRUE),
		Valor = mean(as.numeric(as.character(Valor)), na.rm = TRUE),
		AscS = sum(Ascens == "Sí", na.rm = TRUE),
		AscN = sum(Ascens == "No", na.rm = TRUE),
		Atics = sum(Planta == "Atic", na.rm = TRUE),
		Planta = sum(Planta == "Planta", na.rm = TRUE),
		Bajos = sum(Planta == "Bajos", na.rm = TRUE),
		Nous = sum(as.numeric(as.character(Edat)) <= 10, na.rm = TRUE),
		SemiNous = sum(as.numeric(as.character(Edat)) >= 11 & as.numeric(as.character(Edat)) <= 20, na.rm = TRUE),
		Vells = sum(as.numeric(as.character(Edat)) >= 21 & as.numeric(as.character(Edat)) < 50, na.rm = TRUE),
		MoltVells = sum(as.numeric(as.character(Edat)) >= 50, na.rm = TRUE),
		Superf = mean(as.numeric(as.character(Superf)), na.rm = TRUE),
		Obs = n(),
		.groups = "drop"
	)

distinfo
distinfosql

# A pintar !!!

stacked_prop <- function(df, columns) {
	counts <- as.matrix(df[, columns, drop = FALSE])
	t(sweep(counts, 1, df$Obs, "/"))
}

old_par <- par(mfrow = c(1, 1), mar = c(6, 4, 3, 10) + 0.1) # con huequito para la leyenda

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

# Valor and Superf en dos paneles separados
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


