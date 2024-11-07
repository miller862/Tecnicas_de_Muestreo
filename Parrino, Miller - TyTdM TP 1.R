# ---- Ejercicio 1 ----

# Crear un data frame con los datos de los alumnos
df <- data.frame(
  Alumno = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r"),
  X = c(6, 9, 5, 4, 2, 7, 10, 4, 12, 5, 8, 12, 5, 9, 2, 6, 11, 8),
  Y = c(14.0, 20.0, 12.0, 10.0, 5.0, 12.0, 24.0, 5.0, 21.0, 9.0, 18.0, 20.0, 8.0, 15.0, 2.5, 11.0, 20.0, 15.0)
)

# Mostrar el data frame
df

# Calcular los parámetros poblacionales para comparar con los resultados muestrales
X_media = mean(df$X)
X_media
Y_media = mean(df$Y)
Y_media
R = Y_media / X_media
R

# Calcular la varianza de X y Y
VARX <- var(df$X)
VARX
VARY <- var(df$Y)
VARY

# Número total de observaciones
N = nrow(df)

# Tamaño de la muestra
k = 9

# Establecer semilla para reproducibilidad
set.seed(999)

# Seleccionar una muestra aleatoria simple de 9 casos
s_mas <- df[sample(nrow(df), size = 9, replace = FALSE), ]
s_mas

# Calcular medias muestrales de X y Y
x_media = mean(s_mas$X)
x_media
y_media = mean(s_mas$Y)
y_media

# Estimador de razón muestral
r = y_media / x_media
r

# Calcular el número de muestras posibles
# Alternativa 1: Usando la función choose()
choose(18, 9)

# Alternativa 2: Fórmula combinatoria
sposibles <- factorial(N) / (factorial(k) * factorial(N - k))
sposibles

# Hallar la varianza y el coeficiente de variación (CV) de los estimadores de y_barra y x_barra
varMAS_xmedia <- (1 - k / N) * VARX / k
varMAS_xmedia
varMAS_x <- N^2 * varMAS_xmedia   # Recordar: Var(k*X) = k^2 * Var(X)
varMAS_x

varMAS_ymedia <- (1 - k / N) * VARY / k
varMAS_ymedia
varMAS_y <- N^2 * varMAS_ymedia   # Recordar: Var(k*X) = k^2 * Var(X)
varMAS_y

CVMAS_xmedia <- 100 * sqrt(varMAS_x) / N
CVMAS_xmedia
CVMAS_ymedia <- 100 * sqrt(varMAS_y) / N
CVMAS_ymedia

# Calcular la varianza aproximada y el CV aproximado del estimador r
# Calculo de la varianza del estimador r
VARr <- r^2 * (varMAS_ymedia / (y_media^2) + varMAS_xmedia / (x_media^2))

# Calculo del coeficiente de variación de r
CVr <- (sqrt(VARr) / r) * 100

# Resultados: varianza y coeficiente de variación del estimador de la razón
VARr
CVr

# Seleccionar 10,000 muestras aleatorias simples para estimar la varianza y el CV de r
# y comparar con el resultado anterior

# Definir una función para estimar R en una muestra de tamaño x
estimo_R_MAS <- function(x) {
  muestra <- df[sample(nrow(df), x, replace = FALSE), ]
  
  # Calcular medias muestrales y estimador de razón
  x_media <- mean(muestra$X)
  y_media <- mean(muestra$Y)
  r <- mean(muestra$Y / muestra$X)
  
  # Calcular varianza y coeficiente de variación
  varMAS_xmedia <- (1 - k / N) * VARX / k
  varMAS_x <- N^2 * varMAS_xmedia   
  
  varMAS_ymedia <- (1 - k / N) * VARY / k
  varMAS_y <- N^2 * varMAS_ymedia
  
  VARr <- r^2 * (varMAS_ymedia / (y_media^2) + varMAS_xmedia / (x_media^2))
  CVr <- (sqrt(VARr) / r) * 100
  
  # Devolver resultados
  a <- c(x, x_media, y_media, r, VARr, CVr)
  return(a)
}

# Crear una lista de tamaño muestral y aplicar la función a cada muestra
lista <- rep(9, 20000)
lista_estim <- lapply(lista, estimo_R_MAS)

# Crear un data frame con los resultados de las estimaciones
df_estimNUEVO <- data.frame(matrix(unlist(lista_estim), 
                                   nrow = length(lista_estim), byrow = TRUE))

# Asignar nombres a las columnas
colnames(df_estimNUEVO) <- c("n", "x_media", "y_media", "r", "VARr", "CVr")

# Mostrar las primeras filas del data frame
head(df_estimNUEVO)

# Medias de las estimaciones
mean(df_estimNUEVO$x_media)
mean(df_estimNUEVO$y_media)
mean(df_estimNUEVO$r)

# Calcular varianza y coeficiente de variación generales de r
vargeneral <- var(df_estimNUEVO$r)
vargeneral
cvgeneral <- (sqrt(vargeneral) / mean(r)) * 100
cvgeneral


# ---- Ejercicio 2 ----

# Cargar el archivo de datos
df_tabla <- read_excel("tabla_muestras_posibles.xlsx")

# Calcular los parámetros poblacionales para comparar con los resultados muestrales
Y_mean = mean(df_tabla$Y)
Y_mean

Y_median <- median(df_tabla$Y)
Y_median

Y_truncmean <- mean(sort(df_tabla$Y)[3:(length(df_tabla$Y) - 2)])
Y_truncmean

VARY <- var(df_tabla$Y)
VARY

# Establecer semilla para reproducibilidad
set.seed(999)

# Seleccionar una muestra aleatoria de 10 elementos y calcular su media muestral
mue <- sample(df_tabla$Y, 10, replace = FALSE)
mue

mue_mean <- mean(mue)
mue_mean

# Calcular la media muestral truncada eliminando el 10% inferior y el 10% superior
# (en este caso, se elimina el menor y el mayor valor de la muestra)
mue_ordenada <- sort(mue)
mue_truncada <- mue_ordenada[2:(length(mue_ordenada) - 1)]
mue_truncada
mue_truncada_mean <- mean(mue_truncada)
mue_truncada_mean

# Calcular la mediana de la muestra
mue_median <- median(mue)
mue_median

# Generar todas las muestras posibles y calcular la media, media truncada y mediana de cada una
df_muestras <- data.frame(matrix(unlist(combn(df_tabla$Y, 10, simplify = FALSE)),
                                 ncol = 10, byrow = TRUE))

# Calcular la media de cada muestra
media <- apply(df_muestras[, 1:10], 1, mean)
df_muestras$Media <- media

# Calcular la media truncada excluyendo el mínimo y el máximo de cada fila
df_muestras$Mediatrunc <- apply(df_muestras[, 1:10], 1, function(x) mean(x[x != min(x) & x != max(x)]))

# Calcular la mediana de cada muestra
mediana <- apply(df_muestras[, 1:10], 1, median)
df_muestras$Mediana <- mediana

# Mostrar las primeras filas del data frame
sample(df_muestras)

# Verificar si la media muestral es un estimador insesgado de la media poblacional
mean(df_muestras$Media) == Y_mean       # TRUE --> Media es insesgada
mean(df_muestras$Mediana) == Y_median   # FALSE --> Mediana es sesgada
mean(df_muestras$Mediatrunc) == Y_truncmean  # FALSE --> Media truncada es sesgada

# Graficar histogramas de las estimaciones de media, media truncada y mediana

# Histograma para la media muestral
pmean <- ggplot(df_muestras, aes(x = Media)) +
  geom_histogram(bins = 30, color = "black", fill = "lightgray") +
  coord_cartesian(xlim = c(-10, 100)) +
  geom_vline(aes(xintercept = mean(Media)), color = "blue", linetype = "dashed", linewidth = 1) + # Línea para la media de medias muestrales
  geom_vline(aes(xintercept = Y_mean), color = "green", linetype = "twodash", linewidth = 0.9) + # Línea para la media poblacional
  scale_color_manual(name = "Indicador", values = c("Parámetro Poblacional" = "blue", "Estimador Muestral" = "green")) +
  labs(title = "Distribución de las Medias Muestrales", x = "Medias Muestrales", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "top")
pmean

# Histograma para la media truncada
ptruncmean <- ggplot(df_muestras, aes(x = Mediatrunc)) +
  geom_histogram(bins = 30, color = "black", fill = "lightgray") +
  coord_cartesian(xlim = c(-10, 100)) +
  geom_vline(aes(xintercept = mean(Mediatrunc)), color = "blue", linetype = "dashed", linewidth = 1) + # Línea para la media de medias truncadas muestrales
  geom_vline(aes(xintercept = Y_truncmean), color = "green", linetype = "twodash", linewidth = 0.9) +
  scale_color_manual(name = "Indicador", values = c("Parámetro Poblacional" = "blue", "Estimador Muestral" = "green")) +
  labs(title = "Distribución de las Medias Truncadas Muestrales", x = "Medias Truncadas Muestrales", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "top")
ptruncmean

# Histograma para la mediana
pmedian <- ggplot(df_muestras, aes(x = Mediana)) +
  geom_histogram(bins = 30, color = "black", fill = "lightgray") +
  coord_cartesian(xlim = c(-10, 100)) +
  geom_vline(aes(xintercept = mean(Mediana)), color = "blue", linetype = "dashed", linewidth = 1) + # Línea para la media de medianas muestrales
  geom_vline(aes(xintercept = Y_median), color = "green", linetype = "twodash", linewidth = 0.9) +
  scale_color_manual(name = "Indicador", values = c("Parámetro Poblacional" = "blue", "Estimador Muestral" = "green")) +
  labs(title = "Distribución de las Medianas Muestrales", x = "Medianas Muestrales", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "top")
pmedian

# Calcular el Coeficiente de Variación (CV) y el Error Medio Cuadrático (EMC) para cada estimador

# Para la media muestral
var_media <- var(df_muestras$Media)
cv_media <- (sqrt(var_media) / mean(df_muestras$Media)) * 100
sesgomedia <- mean(df_muestras$Media) - Y_mean
emc_media <- (sesgomedia^2) + var_media

# Para la mediana
var_mediana <- var(df_muestras$Mediana)
cv_mediana <- (sqrt(var_mediana) / mean(df_muestras$Mediana)) * 100
sesgomediana <- mean(df_muestras$Mediana) - Y_median
emc_mediana <- (sesgomediana^2) + var_mediana

# Para la media truncada
var_media_truncada <- var(df_muestras$Mediatrunc)
cv_media_truncada <- (sqrt(var_media_truncada) / mean(df_muestras$Mediatrunc)) * 100
sesgomediatrunc <- mean(df_muestras$Mediatrunc) - Y_truncmean
emc_media_truncada <- (sesgomediatrunc^2) + var_media_truncada

# Crear tabla de resultados
resultados_estimadores <- data.frame(
  Estimador = c("Media Muestral", "Media Truncada", "Mediana"),
  CV = c(cv_media, cv_media_truncada, cv_mediana),
  EMC = c(emc_media, emc_media_truncada, emc_mediana)
)
resultados_estimadores

# ---- Ejercicio 3 ----

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(survey)

# Configuración de la semilla y lectura de datos
set.seed(999)
censoviv <- read_xlsx("cen2010_radios_tipo.xlsx")
censo <- read_xlsx("cen2010_radios_sexo.xlsx")

# Cálculo de variables derivadas
censoviv$Pob_radio <- censo$Varon + censo$Mujer
censoviv$Viv_radio <- censoviv$Casa + censoviv$Rancho + censoviv$Casilla +
  censoviv$Departamento + censoviv$Inquilinato + censoviv$Hotel_pension

# Extraemos el código de provincia y asignamos etiquetas
censoviv$prov <- floor(censoviv$Codigo / 10000000)
censoviv <- censoviv %>%
  mutate(Provincia = case_when(
    prov == 2 ~ 'CABA', prov == 6 ~ 'BsAs', prov == 10 ~ 'Catamarca', 
    prov == 14 ~ 'Cordoba', prov == 18 ~ 'Corrientes', prov == 22 ~ 'Chaco',
    prov == 26 ~ 'Chubut', prov == 30 ~ 'Entre Rios', prov == 34 ~ 'Formosa', 
    prov == 38 ~ 'Jujuy', prov == 42 ~ 'La Pampa', prov == 46 ~ 'La Rioja',
    prov == 50 ~ 'Mendoza', prov == 54 ~ 'Misiones', prov == 58 ~ 'Neuquen', 
    prov == 62 ~ 'Rio Negro', prov == 66 ~ 'Salta', prov == 70 ~ 'San Juan',
    prov == 74 ~ 'San Luis', prov == 78 ~ 'Santa Cruz', prov == 82 ~ 'Santa Fe',
    prov == 86 ~ 'Santiago', prov == 90 ~ 'Tucuman', prov == 94 ~ 'TdFuego'
  ))

# Eliminamos los radios sin viviendas
censoviv <- censoviv[censoviv$Viv_radio > 0, ]

# Crear variable Rancho_Casilla y proporción
censoviv$Rancho_Casilla <- censoviv$Rancho + censoviv$Casilla
censoviv$prop_RC <- censoviv$Rancho_Casilla / censoviv$Viv_radio

# Asignar N y n para la muestra
n <- 240
N <- nrow(censoviv)

# Selección de Muestra Aleatoria Simple
s_mas <- sample(N, n, replace = FALSE)
muestra_radios <- censoviv[s_mas, ]

# Cálculo de estimadores para Población, Casa, Rancho/Casilla y Proporción
# 1. Estimación de la población total
media_muestral_pob <- mean(muestra_radios$Pob_radio)
Pob_est_ht <- media_muestral_pob * N
Pob_param <- sum(censoviv$Pob_radio)
var_poblacion <- var(muestra_radios$Pob_radio)
var_mediaPobMAS <- (1 - n / N) * (var_poblacion / n)
var_total_poblacion <- N^2 * var_mediaPobMAS
cv_total_poblacion <- (sqrt(var_total_poblacion) / Pob_est_ht) * 100

# 2. Estimación de hogares en vivienda tipo Casa
media_muestral_casa <- mean(muestra_radios$Casa)
Casa_est_ht <- media_muestral_casa * N
Casa_param <- sum(censoviv$Casa)
var_casa <- var(muestra_radios$Casa)
var_mediaCasaMAS <- (1 - n / N) * (var_casa / n)
var_total_casa <- N^2 * var_mediaCasaMAS
cv_total_casa <- (sqrt(var_total_casa) / Casa_est_ht) * 100

# 3. Estimación de hogares en vivienda Rancho/Casilla
media_muestral_RC <- mean(muestra_radios$Rancho_Casilla)
RC_est_ht <- media_muestral_RC * N
RC_param <- sum(censoviv$Rancho_Casilla)
var_ranchocasilla <- var(muestra_radios$Rancho_Casilla)
var_mediaRCMAS <- (1 - n / N) * (var_ranchocasilla / n)
var_total_RC <- N^2 * var_mediaRCMAS
cv_total_RC <- (sqrt(var_total_RC) / RC_est_ht) * 100

# 4. Estimación de proporción de hogares en Rancho/Casilla
media_muestral_viv <- mean(muestra_radios$Viv_radio)
viv_est_ht <- media_muestral_viv * N
viv_param <- sum(censoviv$Viv_radio)
var_viv <- var(muestra_radios$Viv_radio)
var_mediavivMAS <- (1 - n / N) * (var_viv / n)
var_total_viv <- N^2 * var_mediavivMAS
cv_total_viv <- (sqrt(var_total_viv) / viv_est_ht) * 100

# Estimación de proporción y su error
propRC_est <- mean(muestra_radios$prop_RC)
propRC_Param <- sum(censoviv$Rancho_Casilla) / sum(censoviv$Viv_radio)

# Calculo de CV para la razón
varpropRC <- (media_muestral_RC)^2 * 
  (var_mediaRCMAS / (media_muestral_RC^2) + var_mediavivMAS / (media_muestral_viv^2))
CVr <- (sqrt(varpropRC) / media_muestral_RC) * 100

# Diseño de la muestra para estimaciones con survey
muestra_radios$pondera <- N / n
muestra_radios$fpc <- N

# Estimación con survey (Población, Casa, Rancho/Casilla)
disenopob <- svydesign(id = ~1, weights = ~pondera, data = muestra_radios, fpc = ~fpc)
EstPob <- svytotal(~Pob_radio, design = disenopob, deff = TRUE, cv = TRUE, ci = TRUE)

disenocasa <- svydesign(id = ~1, weights = ~pondera, data = muestra_radios, fpc = ~fpc)
EstCasa <- svytotal(~Casa, design = disenocasa, deff = TRUE, cv = TRUE, ci = TRUE)

disenoRC <- svydesign(id = ~1, weights = ~pondera, data = muestra_radios, fpc = ~fpc)
EstRC <- svytotal(~Rancho_Casilla, design = disenoRC, deff = TRUE, cv = TRUE, ci = TRUE)

disenopropRC <- svydesign(id = ~1, weights = ~pondera, data = muestra_radios, fpc = ~fpc)
EstRatio <- svyratio(~Rancho_Casilla, ~Viv_radio, disenopropRC, deff = TRUE, cv = TRUE, ci = TRUE)


summary(df_estim_pob$Estimacion)
summary(df_estim_casa$Estimacion)
summary(df_estim_RC$Estimacion)

# ---- Ejercicio 4 ----

# Parámetros para muestreo sistemático
I <- floor(N / n)
aa <- 3  # Arranque aleatorio

# Estrategia 1: Ordenado por Provincia-Total de Viviendas
censoviv <- censoviv[order(censoviv$Provincia, censoviv$Viv_radio), ]
s <- censoviv[seq(aa, N, I), ]

# Estimación de cada variable (Estrategia 1)
estimPob <- I * sum(s$Pob_radio)
estimCasa <- I * sum(s$Casa)
estimRC <- I * sum(s$Rancho_Casilla)

# Funciones de estimación para todas las muestras posibles (Estrategia 1)
estim_sistematico_pob <- function(aa) { I * sum(censoviv[seq(aa, N, I), ]$Pob_radio) }
estim_sistematico_casa <- function(aa) { I * sum(censoviv[seq(aa, N, I), ]$Casa) }
estim_sistematico_RC <- function(aa) { I * sum(censoviv[seq(aa, N, I), ]$Rancho_Casilla) }

# Listas de estimaciones (Estrategia 1)
lista_arranques <- 1:I
df_estim_pob <- data.frame(Estimacion = unlist(lapply(lista_arranques, estim_sistematico_pob)))
df_estim_casa <- data.frame(Estimacion = unlist(lapply(lista_arranques, estim_sistematico_casa)))
df_estim_RC <- data.frame(Estimacion = unlist(lapply(lista_arranques, estim_sistematico_RC)))
# Estrategia 2: Ordenado por número aleatorio
censoviv$aleatorio <- runif(nrow(censoviv))
censoviv_al <- censoviv[order(censoviv$aleatorio), ]
s_al <- censoviv_al[seq(aa, N, I), ]

# Estimación de cada variable (Estrategia 2)
estimPob_al <- I * sum(s_al$Pob_radio)
estimCasa_al <- I * sum(s_al$Casa)
estimRC_al <- I * sum(s_al$Rancho_Casilla)

# Funciones de estimación para todas las muestras posibles (Estrategia 2)
estim_sistematico_pob_al <- function(aa) { I * sum(censoviv_al[seq(aa, N, I), ]$Pob_radio) }
estim_sistematico_casa_al <- function(aa) { I * sum(censoviv_al[seq(aa, N, I), ]$Casa) }
estim_sistematico_RC_al <- function(aa) { I * sum(censoviv_al[seq(aa, N, I), ]$Rancho_Casilla) }

# Listas de estimaciones (Estrategia 2)
lista_arranques_al <- 1:I
df_estim_pob_al <- data.frame(Estimacion = unlist(lapply(lista_arranques_al, estim_sistematico_pob_al)))
df_estim_casa_al <- data.frame(Estimacion = unlist(lapply(lista_arranques_al, estim_sistematico_casa_al)))
df_estim_RC_al <- data.frame(Estimacion = unlist(lapply(lista_arranques_al, estim_sistematico_RC_al)))


# Cálculo de parámetros poblacionales (comunes a ambas estrategias)
parampob <- sum(censoviv$Pob_radio)
paramcasa <- sum(censoviv$Casa)
paramRC <- sum(censoviv$Rancho_Casilla)

# Calculos para Población
sesgopob <- mean(df_estim_pob$Estimacion) - parampob
Varianzapob <- var(df_estim_pob$Estimacion) * (N - 1) / N
DSpob <- sqrt(Varianzapob)
CVpob <- 100 * DSpob / parampob
S2pob <- var(censoviv$Pob_radio)
V_maspob <- N^2 * (1 - n / N) * S2pob / n
deffpob <- Varianzapob / V_maspob
EMCPob <- Varianzapob + sesgopob^2

# Calculos para Casa
sesgocasa <- mean(df_estim_casa$Estimacion) - paramcasa
Varianzacasa <- var(df_estim_casa$Estimacion) * (N - 1) / N
DScasa <- sqrt(Varianzacasa)
CVcasa <- 100 * DScasa / paramcasa
S2casa <- var(censoviv$Casa)
V_mascasa <- N^2 * (1 - n / N) * S2casa / n
deffcasa <- Varianzacasa / V_mascasa

# Calculos para Rancho/Casilla
sesgoRC <- mean(df_estim_RC$Estimacion) - paramRC
VarianzaRC <- var(df_estim_RC$Estimacion) * (N - 1) / N
DSRC <- sqrt(VarianzaRC)
CVRC <- 100 * DSRC / paramRC
S2RC <- var(censoviv$Rancho_Casilla)
V_masRC <- N^2 * (1 - n / N) * S2RC / n
deffRC <- VarianzaRC / V_masRC

#  Estrategia 2: Estadísticas de Muestreo Sistemático con Orden Aleatorio 

# Calculos para Población
sesgopob_al <- mean(df_estim_pob_al$Estimacion) - parampob
Varianzapob_al <- var(df_estim_pob_al$Estimacion) * (N - 1) / N
DSpob_al <- sqrt(Varianzapob_al)
CVpob_al <- 100 * DSpob_al / parampob
S2pob_al <- var(censoviv$Pob_radio)
V_maspob_al <- N^2 * (1 - n / N) * S2pob_al / n
deffpob_al <- Varianzapob_al / V_maspob
EMCPob_al <- Varianzapob_al + sesgopob_al^2

# Calculos para Casa
sesgocasa_al <- mean(df_estim_casa_al$Estimacion) - paramcasa
Varianzacasa_al <- var(df_estim_casa_al$Estimacion) * (N - 1) / N
DScasa_al <- sqrt(Varianzacasa_al)
CVcasa_al <- 100 * DScasa_al / paramcasa
S2casa_al <- var(censoviv_al$Casa)
V_mascasa_al <- N^2 * (1 - n / N) * S2casa_al / n
deffcasa_al <- Varianzacasa_al / V_mascasa_al

# Calculos para Rancho/Casilla
sesgoRC_al <- mean(df_estim_RC_al$Estimacion) - paramRC
VarianzaRC_al <- var(df_estim_RC_al$Estimacion) * (N - 1) / N
DSRC_al <- sqrt(VarianzaRC_al)
CVRC_al <- 100 * DSRC_al / paramRC
S2RC_al <- var(censoviv_al$Rancho_Casilla)
V_masRC_al <- N^2 * (1 - n / N) * S2RC_al / n
deffRC_al <- VarianzaRC_al / V_masRC_al

#  Tabla de Resultados Comparativa 

# Crear tabla resumen con los resultados de ambas estrategias
tabla_resultados <- data.frame(
  Estimador = c("Población", "Casa", "Rancho/Casilla"),
  `CV Estrategia 1` = c(CVpob, CVcasa, CVRC),
  `CV Estrategia 2` = c(CVpob_al, CVcasa_al, CVRC_al),
  `deff Estrategia 1` = c(deffpob, deffcasa, deffRC),
  `deff Estrategia 2` = c(deffpob_al, deffcasa_al, deffRC_al),
  `Sesgo Relativo Estrategia 1` = c(sesgopob / parampob, sesgocasa / paramcasa, sesgoRC / paramRC),
  `Sesgo Relativo Estrategia 2` = c(sesgopob_al / parampob, sesgocasa_al / paramcasa, sesgoRC_al / paramRC),
  `EMC Estrategia 1` = c(EMCPob, Varianzacasa + sesgocasa^2, VarianzaRC + sesgoRC^2),
  `EMC Estrategia 2` = c(EMCPob_al, Varianzacasa_al + sesgocasa_al^2, VarianzaRC_al + sesgoRC_al^2)
)
write_xlsx(x=tabla_resultados, "tabla.xlsx")
print(tabla_resultados)


# ---- Ejercicio 5 ----

# Ordenar el data frame por el código
censovivmad <- censoviv[order(censoviv$Codigo),]
censovivmad$pi_i <- n * censovivmad$Viv_radio / sum(censovivmad$Viv_radio)
pik <- censovivmad$pi_i
s <- sampling::UPsystematic(pik)

# Seleccionar la muestra y asignar pesos
muestra_radios <- censovivmad[s == 1,]
muestra_radios$pondera <- 1 / muestra_radios$pi_i

# Crear el diseño de encuesta
diseno <- svydesign(id = ~1, weights = ~pondera, data = muestra_radios)

# Estimación del total de población
EstTotalPob <- svytotal(~Pob_radio, diseno, deff = TRUE, cv = TRUE, ci = TRUE)
100 * cv(EstTotalPob)
survey::cv(EstTotalPob)
df_estim_totalpob <- data.frame(EstTotalPob)
colnames(df_estim_totalpob) <- c("Estimacion", "SE", "deff")
df_estim_totalpob$cv <- 100 * df_estim_totalpob$SE / df_estim_totalpob$Estimacion

# Estimación del total de viviendas tipo Casa
EstTotalCasa <- svytotal(~Casa, diseno, deff = TRUE, cv = TRUE, ci = TRUE)
100 * cv(EstTotalCasa)
survey::cv(EstTotalCasa)
df_estim_totalcasa <- data.frame(EstTotalCasa)
colnames(df_estim_totalcasa) <- c("Estimacion", "SE", "deff")
df_estim_totalcasa$cv <- 100 * df_estim_totalcasa$SE / df_estim_totalcasa$Estimacion

# Estimación del total de hogares en Rancho o Casilla
EstTotalRancho <- svytotal(~Rancho_Casilla, diseno, deff = TRUE, cv = TRUE, ci = TRUE)
deff(EstTotalRancho)
100 * cv(EstTotalRancho)
survey::cv(EstTotalRancho)
df_estim_totalrc <- data.frame(EstTotalRancho)
colnames(df_estim_totalrc) <- c("Estimacion", "SE", "deff")
df_estim_totalrc$cv <- 100 * df_estim_totalrc$SE / df_estim_totalrc$Estimacion

# Unir resultados de las estimaciones en un solo data frame
df_estim_total_ej5 <- rbind(df_estim_totalrc, df_estim_totalpob, df_estim_totalcasa)
write_xlsx(df_estim_total_ej5, "ej5tabla.xlsx")

# Función para estimar parámetros usando la selección de muestra mediante Madow
estimo_madow <- function(n) {
  censovivmad$pi_i <- n * censovivmad$Viv_radio / sum(censovivmad$Viv_radio)
  pik <- censovivmad$pi_i
  s <- UPsystematic(pik, eps = 1e-6)
  
  muestra_radios <- censovivmad[s == 1,]
  muestra_radios$pondera <- 1 / muestra_radios$pi_i
  diseno <- svydesign(id = ~1, weights = ~pondera, data = muestra_radios)
  
  # Estimación del total de población
  EstTotalPob <- svytotal(~Pob_radio, diseno, deff = TRUE, cv = TRUE, ci = TRUE)
  df_estim_pob <- data.frame(EstTotalPob)
  colnames(df_estim_pob) <- c("Estimacion", "SE", "deff")
  df_estim_pob$cv <- 100 * df_estim_pob$SE / df_estim_pob$Estimacion
  
  # Estimación del total de viviendas tipo Casa
  EstTotalCasa <- svytotal(~Casa, diseno, deff = TRUE, cv = TRUE, ci = TRUE)
  df_estim_casa <- data.frame(EstTotalCasa)
  colnames(df_estim_casa) <- c("Estimacion", "SE", "deff")
  df_estim_casa$cv <- 100 * df_estim_casa$SE / df_estim_casa$Estimacion
  
  # Estimación del total de hogares en Rancho o Casilla
  EstTotalRancho <- svytotal(~Rancho_Casilla, diseno, deff = TRUE, cv = TRUE, ci = TRUE)
  df_estim_rancho <- data.frame(EstTotalRancho)
  colnames(df_estim_rancho) <- c("Estimacion", "SE", "deff")
  df_estim_rancho$cv <- 100 * df_estim_rancho$SE / df_estim_rancho$Estimacion
  
  # Retornar resultados como una lista
  list(
    estimacion_pob = df_estim_pob,
    estimacion_casa = df_estim_casa,
    estimacion_rancho = df_estim_rancho
  )
}

# Repetir la función diez veces
n_repeticiones <- 10
resultados_repetidos <- lapply(1:n_repeticiones, function(x) estimo_madow(240))

# Extraer las estimaciones de cada repetición y organizar en un data.frame
df_madow <- data.frame(
  Estimacion_Pob = sapply(resultados_repetidos, function(res) res$estimacion_pob$Estimacion),
  SE_Pob = sapply(resultados_repetidos, function(res) res$estimacion_pob$SE),
  deff_Pob = sapply(resultados_repetidos, function(res) res$estimacion_pob$deff),
  cv_Pob = sapply(resultados_repetidos, function(res) res$estimacion_pob$cv),
  
  Estimacion_Casa = sapply(resultados_repetidos, function(res) res$estimacion_casa$Estimacion),
  SE_Casa = sapply(resultados_repetidos, function(res) res$estimacion_casa$SE),
  deff_Casa = sapply(resultados_repetidos, function(res) res$estimacion_casa$deff),
  cv_Casa = sapply(resultados_repetidos, function(res) res$estimacion_casa$cv),
  
  Estimacion_Rancho = sapply(resultados_repetidos, function(res) res$estimacion_rancho$Estimacion),
  SE_Rancho = sapply(resultados_repetidos, function(res) res$estimacion_rancho$SE),
  deff_Rancho = sapply(resultados_repetidos, function(res) res$estimacion_rancho$deff),
  cv_Rancho = sapply(resultados_repetidos, function(res) res$estimacion_rancho$cv)
)

# Calcular varianzas y coeficientes de variación
varpob <- var(df_madow$Estimacion_Pob)
varcasas <- var(df_madow$Estimacion_Casa)
varRC <- var(df_madow$Estimacion_Rancho)

cvpoblacion <- (sqrt(varpob) / mean(df_madow$Estimacion_Pob)) * 100
cvcasas <- (sqrt(varcasas) / mean(df_madow$Estimacion_Casa)) * 100
cvranchocasilla <- (sqrt(varRC) / mean(df_madow$Estimacion_Rancho)) * 100

# Agregar columna de diseño indicando que la estrategia es Madow
df_madow$diseno <- "Madow"

# Mostrar el data frame resultante
df_madow

# ---- Ejercicio 6 ----

# Parámetros de la encuesta
n <- 400
xi <- 212

# Cálculo de la proporción de votos y sus complementos
p <- xi / n
q <- 1 - p

# Cálculo de la varianza de la proporción
VarP <- p * q / n

# Cálculo del Coeficiente de Variación (CV) de la proporción en porcentaje
CVP <- sqrt(q / (p * n)) * 100

# Mostrar los resultados de la proporción, varianza y CV
p
VarP
CVP

# Intervalo de confianza para la proporción utilizando la librería `binom`
# Instalar la librería `binom` si no está instalada: install.packages("binom")
library(binom)

# Cálculo del intervalo de confianza al 95% usando diferentes métodos
dfbinom.confint <- binom.confint(xi, n, conf.level = 0.95, method = "all")

# Mostrar el intervalo de confianza
dfbinom.confint

# ---- Ejercicio 7 ----

# Parámetros del ejercicio
confianza <- 0.95
p <- 0.5
E <- 0.005

# Calcular Z para el nivel de confianza
Z <- qnorm((1 + confianza) / 2)

# Calcular el tamaño de muestra necesario para obtener la amplitud del intervalo deseada
n <- (Z^2 * p * (1 - p)) / (E^2)
n <- ceiling(n)  # Redondeo hacia arriba

# Mostrar el tamaño de muestra requerido
n


# ---- Ejercicio 8 ----
binom.confint(0,24,conf.level = .9, method= c('all'))
