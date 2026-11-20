# metodos_estadisticos
## cursos de metodos estadisticos agosto-diciembre  2026

## contenido del semestre
**12/08/2026: inicio de cursos de metodos estadisticos**
  + preparar mi area de trabajo.
  + crear cuenta en Github "bibiiolw".
  + crear mi primer repositorio.
  + modificar el archivo _README_.
  + activar _credenciales de github_ para sincronizar repositorio.
  + primera sincronización exitosa.
  + creal :file_folder: carpetas para organizar mi informacion.
:neckbeard:

# Fabiola Lucia Fernandez Ovalle
#2197150
#19/08/2026

#importar datos ---
#usar la funcion "read.cvs" para importar datos

exp <- read.csv("D:/repositorio/Met_est_2026/vivero.csv",
                header = TRUE)
exp$Tratamiento <- as.factor(exp$Tratamiento)
exp$Tratamiento

#Grafica----

#Boxplot de los datos

boxplot(exp$IE ~ exp$Tratamiento,
        xlab = "Factor Fertilizante",
        ylab = "Índice (IE)",
        col = "green",
        main = "Unidad experimental")
    
file.choose()

# Gastos totales

300 + 240 + 1527 + 400 + 1500 + 1833
5800
 
celular <- 300
celular
300
 
transporte <- 240
transporte
 240
 
comestibles <- 1527
comestibles
1527

gimnasio <- 400
gimnasio
400
 
alquiler <- 1500
alquiler
1500
 
otros <- 1833
otros
1833
 
gastos <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)
 barplot(gastos)
 
 
 costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94,
            80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89,
            82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01,
            80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29,
            75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

#Explique con sus propias palabras que es H0 y H1
# H0: es él valor total de peso en los costales
# H1: es él valor dél peso que permite determinar si la media es menor al peso de costales

#¿Por qué corresponde utilizar una prueba de una cola?
# porque se quiere determinar si es menor que 80kg

#Calcule la media y desviación estándar de los 44 costales.
mean(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94,
     80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89,
     82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01,
     80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29,
     75.52, 77.21, 75.99, 81.94, 80.41, 77.7)
# media: 87.7

sd(costal)
# desviacion estandar: 3.056023

t.test(costal, mu = 80, alternative = "less")

# estadístico t: -2.3644
#grados de libertad: 43
#valor de p: 0.01132
#media observada:78.91068

# Datos sin valor extremo
x1 <- c(1.2, 1.5, 1.7, 1.8)

# resumen general
summary(x1)
# media
mean(x1)
#mediana
median(x1)
# varianza
var(x1)
#desviacion estandar
sd(x1)

#datos con valor extremo
x2 <- c(1.2, 1.5, 1.7, 1.8, 4.8)

#resumen general
summary(x2)
#media
mean(x2)
#mediana
median(x2)
#varianza
var(x2)
#desviaciob estandar
sd(x2)


anillos <- data.frame(
  Arbol = 1:40,
  RW = c(1.42, 1.58, 1.71, 1.63, 1.85, 1.94, 2.06, 1.76, 1.68, 1.89, 2.14, 1.53, 1.79, 1.97, 2.23, 1.61, 1.82, 2.08, 1.73, 1.87, 1.91, 2.04, 1.76, 1.83, 2.12, 1.69, 1.57, 1.88, 2.21, 1.95, 1.74, 1.81, 2.09, 1.66, 1.92, 2.17, 1.72, 1.86, 2.02, 3.48)
)

# ==============================================================
# Evaluación III - Principios de estadística
# Anillos de crecimiento y temperatura
#
# Nombre:Fabiola LUcia Fernandez Ovalle
# Matrícula:2197150
# ==============================================================


# ==============================================================
# EJERCICIO 1
# ==============================================================

# --- a) Ingresar los datos en el objeto "anillos" ---------------
#        Variables: Temperatura (AFres, ACal) y Ancho (mm)
Afres <- c(2.45,1.98, 3.12, 2.67, 1.85, 2.98, 3.35, 2.20, 2.76, 2.58)
Acal <- c(2.51, 1.90, 3.20, 2.55, 1.92, 2.84, 3.41, 2.12, 2.70, 2.66)



# --- b) Hipótesis de partida ------------------------------------
# H0:las variables son iguales
# H1:las variables no son iguales


# --- c) Media de AFres y ACal -----------------------------------
mean(Afres)
mean(Acal)


# --- d) Desviación estándar de AFres y ACal ---------------------
sd(Afres)
sd(Acal)


# --- e) Diferencia media entre ambos años -----------------------
mean(Afres - Acal)


# --- f) Valor de t ----------------------------------------------
t.test(Afres, Acal, paired = TRUE, 
       alternative = "two.sided",
       conf.level = 0.95)


# --- g) ¿Diferencias significativas? / Parámetro / Valor --------
#No hay diferencias, el valor del parametro es 0.850


# --- h) Conclusión sobre la decisión del responsable ------------
# Respuesta:
#No esta protegida


# ==============================================================
# EJERCICIO 2
# ==============================================================

# --- Ingresar los datos -----------------------------------------
Afres2 <- c(2.45, 1.98, 3.12, 2.67, 1.85, 2.98, 3.35, 2.20, 2.76, 2.58)
Acal2 <- c(2.10, 1.85, 2.64, 2.48, 1.70, 2.41, 2.95, 2.15, 2.30, 2.35)



# --- Hipótesis --------------------------------------------------
# H0:
# H1:


# --- a) Media de AFres y ACal -----------------------------------
mean(Afres2)
mean(Acal2)


# --- b) Desviación estándar de AFres y ACal ---------------------
sd(Afres2)
sd(Acal2)


# --- c) Diferencia media entre ambos años -----------------------
mean(Afres2 - Acal2)


# --- d) Valor de t ----------------------------------------------
t.test(Afres2, Acal2, paired = TRUE, 
       alternative = "two.sided",
       conf.level = 0.95)


# --- e) ¿Diferencias significativas? / Parámetro / Valor --------
#si hay diferencias, el valor es p < 0.05

# --- f) Decisión de la Comisión Forestal ------------------------
# Respuesta: Lo mas recomendable es checar el sitio y tomar medidas de prevencion


# Analisis de varianza ANOVA

pinos <- read.csv("Datos_Rascon_Anova.csv")

boxplot(pinos$DAP ~ pinos$Paraje,
        col = "blue",
        xlab = "DAP (cm)",
        main = "Analisis de varianza")

tapply(pinos$DAP, pinos$Paraje, mean)
tapply(pinos$DAP, pinos$Paraje, var)


#Funcion aov
pin.aov <- aov(pinos$DAP ~ pinos$Paraje)
summary(pin.aov)

#la funcion de TukeyHSD() me ayuda a encontrar las diferencias
TukeyHSD(pin.aov)

plot(TukeyHSD(pin.aov))

