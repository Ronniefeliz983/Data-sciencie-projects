#Predicion de salarios Ronnie Isael Feiz Doñe

# Cargar paquetes necesarios

library(tidyverse)
library(caret) # Para particionar los datos y evaluar el modelo

library(ggplot2) #Para mi grafico

# Crear dataframe

empleados <- tibble(
  nombre = c("Ana", "Luis", "Marta", "Pedro", "Sofía", "Carlos", "María", "Jorge", "Laura", "Fernando"),
  edad = c(23, 30, 22, 34, 28, 45, 33, 29, 31, 40),
  experiencia = c(1, 8, 2, 10, 5, 20, 12, 7, 9, 15),
  ciudad = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Bilbao", "Madrid", "Barcelona", "Valencia",
             "Sevilla", "Bilbao"),
  salario = c(30000, 35000, 32000, 40000, 36000, 45000, 38000, 34000, 37000, 42000)
)

#es

print(empleados)

##################################################################################################################

#Paso 2: Preparación de los datos

#Antes de eso vere los tipos de datos que hay en mi dataframe

str(empleados)

#Hay dos variables categoricas pero la que voy a estar utilizando sera la variable ciudad para mi analisis

#Codificar la variable categórica ciudad.

#La primera forma me gusta mas hacerla pero la comentare ya que quiere que usemos tidyverse

#empleados$ciudad <- as.factor(empleados$ciudad)

empleados <- empleados %>% mutate(ciudad = as.factor(ciudad)) #Utilize mutate para sobrescribir la variable agregandole la ciudad pero en factor

#Me sobreescribe la variable ciudad en simple palabras pero en factor

#Recordar factor se usa en variables categoricas para guardar estas, si no esta de este tipo ocasionara errores en graficos o modelos estadisticos

str(empleados) #Ver estructura cambiada

###########################################################################################

#  Particionar los datos en conjuntos de entrenamiento y prueba

#Antes de esto estare analizando la correlacion de las variables

#Usare la funcion pairs que me permite hacer grafico de dispersion de una forma mas especifa y mas facil

pairs(empleados[,c('experiencia','salario')]) 
pairs(empleados[,c('edad','salario')]) 
pairs(empleados[,c('experiencia','edad')]) 
pairs(empleados[,c('ciudad','salario')]) 
pairs(empleados[,c('experiencia','ciudad')]) # NO hay correlacion
pairs(empleados[,c('ciudad','edad')]) #Tampoco

#Las variables experiencia y ciudad quedan decartadas

#Estares utilizando la funcion pairs ya que me ahorra codigo en mi modelo de regresion lineal

pairs(ejercicios)

set.seed(2) #Establecer una semilla 

#Puse la semilla en 2 ya que esta era la que me realizaba la particion de forma que mis datos estuvieran mas cercanos a los valores reales

#Hacer particion para el entrenamiento del modelo 

trainIndex <- createDataPartition(empleados$salario, p=0.5 ,list=FALSE)

trainData <- empleados[trainIndex,] #Datos de entrenamiento

TestData <- empleados[-trainIndex,] #Datos de prueba

##########################################################################################

# Paso 3: Entrenar el modelo de regresión lineal

regresion_modelo <-  lm(salario~ edad + experiencia , data = trainData)
#este simbolo ~ indica que salario es la variable dependiente y que edad, experiencia y ciudad son los que se van a analizar para la prediccion

#OJO: Se que puede dar multicolinealidad por que las dos variables tienen una correlacion fuerte entre si
#Pero experiencia es una buen variable asi que no quise desperdiciarla.

summary(regresion_modelo) #Me da un resumen de la regresion lineal

######################################################################################################################################################

# Paso 4: Evaluación del modelo


# Hacer predicciones usando el conjunto de datos de prueba
predicciones <- predict(regresion_modelo, TestData)

predicciones #Las predicciones se acercan

head(TestData)


ggplot(TestData, aes(x = salario, y = predicciones, color = ciudad)) +
  geom_point() +  # Puntos de datos reales
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "blue", size = 1) +  # Línea de regresión
  labs(x = "Valores real", y = "Predicciones", color = "Ciudad", title = "Regresión Lineal de Salario") +
  scale_color_manual(values = c("Madrid" = "red", "Barcelona" = "blue", "Valencia" = "green",
                                "Sevilla" = "purple", "Bilbao" = "orange")) +  # Colores por ciudad
  theme_minimal()

#OJO: Use geom_smooth ya que esta es la que se utilize para comparar correlaciones de variables



#Calcular métricas de rendimiento como el RMSE y el R²

# Calcular el RMSE y el R²
RMSE <- sqrt(mean((TestData$salario - predicciones)^2))

#RMSE es un calculo que se utiliza para evaluar la precision de un modelo, es decir que tanto se equivoco de las predicciones

#Mientras bajo el numero mejor, mas alto el modelo no sirve

R2 <- 1 - (sum((TestData$salario - predicciones)^2) / sum((TestData$salario - mean(TestData$salario))^2))

# R2 o R (R-cuadrado o coeficiente de determinación me explica la variabilidad de la variable dependiente en base a la independiente

# es decir que tanto las variable dependiente afecta a la independiente

#El coeficiente de determinacion nos ayuda a saber que tan preciso son los resultados de mi mi variable dependiente

# Imprimir las métricas

print(paste("RMSE:", RMSE))
print(paste("R²:", R2))


############################################################################################################################################

# Paso 5: Interpretación de los resultados


# Analiza los coeficientes del modelo y las métricas de rendimiento para entender cómo
# cada variable afecta al salario y la precisión del modelo.



coeficientes <- coef(regresion_modelo)

coeficientes

#Como intrepreto los coficientes? pues es bastante facil

#Intercept lo que me calcula la intercepcion cuando las dos variables independientes es 0, osea donde se va a encontrar en el grafico

#Edad y experiencia lo que me dice que tanto aumentara si aumenta la variable dependiente salario


