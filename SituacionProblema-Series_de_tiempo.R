
#install.packages("readxl")   # Read Excel Files
library(readxl)
#install.packages("dplyr")    # A Grammar of Data Manipulation
library(dplyr)

piezas = read_excel("./Datos_reto.xlsx", sheet=1, skip=2)
attach(piezas)

piezas = piezas[ -c(1,4,7) ]
piezas = rename(piezas, producidasA     = 1,
               defectuosasA     = 2,
               producidasB     = 3,
               defectuosasB     = 4,
               producidasC   = 5,
               defectuosasC       = 6)

# --------------------------------- PRODUCTO A --------------------------------- 

# Convertimos los datos a time series
productoA = ts(piezas$defectuosasA, start=c(2020, 6), frequency=12)

# Diagrama de caja y brazos
#boxplot(productoA ~ cycle(productoA)) 

pA = decompose(productoA)
#plot(pA, xlab='Año')
desestacionalizadosA = productoA-pA$seasonal
plot(desestacionalizadosA, main="Registros defectuosos del producto A")

# Eliminación de tendencia
xA = log(productoA) # Estabilización de la varianza
dif1A = diff(xA)
plot(dif1A)

dif2A = diff(dif1A, lag=12)
plot(dif2A)

yA = dif2A
acf(yA) # Función de autocorrelación
acf(yA, plot=FALSE)$acf # Altura de las líneas

# Verificar si tenemos o no ruido blanco
#plot.ts(productoA)
#mean(productoA)
#Box.test(productoA) # Prueba de hipótesis
                    #   H0: Ruido blanco
                    #   Ha: No es ruido blanco
                    #   Si p-value >= alpha     NO RECHAZO HIPÓTESIS NULA

# --------------------------------- PRODUCTO B --------------------------------- 

# Convertimos los datos a time series
productoB = ts(piezas$defectuosasB, start=c(2020, 6), frequency=12)

# Diagrama de caja y brazos
#boxplot(productoB ~ cycle(productoB)) 

pB = decompose(productoB)
#plot(pB, xlab='Año')
desestacionalizadosB = productoB-pB$seasonal
plot(desestacionalizadosB, main="Registros defectuosos del producto B")

# Eliminación de tendencia
xB = log(productoB) # Estabilización de la varianza
dif1B = diff(xB)
plot(dif1B)

dif2B = diff(dif1B, lag=12)
plot(dif2B)

yB = dif2B
acf(yB) # Función de autocorrelación
acf(yB, plot=FALSE)$acf # Altura de las líneas

# Verificar si tenemos o no ruido blanco
#plot.ts(productoB)
#mean(productoB)
#Box.test(productoB) # Prueba de hipótesis
#   H0: Ruido blanco
#   Ha: No es ruido blanco
#   Si p-value >= alpha     NO RECHAZO HIPÓTESIS NULA

# --------------------------------- PRODUCTO C --------------------------------- 

# Convertimos los datos a time series
productoC = ts(piezas$defectuosasC, start=c(2020, 6), frequency=12)

# Diagrama de caja y brazos
#boxplot(productoC ~ cycle(productoC)) 

pC = decompose(productoC)
#plot(pC, xlab='Año')
desestacionalizadosC = productoC-pC$seasonal
plot(desestacionalizadosC, main="Registros defectuosos del producto C")

# Eliminación de tendencia
xC = log(productoC) # Estabilización de la varianza
dif1C = diff(xC)
plot(dif1C)

dif2C = diff(dif1C, lag=12)
plot(dif2C)

yC = dif2C
acf(yC) # Función de autocorrelación
acf(yC, plot=FALSE)$acf # Altura de las líneas

# Verificar si tenemos o no ruido blanco
#plot.ts(productoC)
#mean(productoC)
#Box.test(productoC) # Prueba de hipótesis
#   H0: Ruido blanco
#   Ha: No es ruido blanco
#   Si p-value >= alpha     NO RECHAZO HIPÓTESIS NULA
