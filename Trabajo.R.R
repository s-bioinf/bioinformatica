
#1.Carga los datos y exáminalos en R. Emplea las funciones head(),summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos hay?

#Hay 5 tratamientos para 2 variables

#read.table (file):lee un archivo en formato de tabla y crea un marco de datos a partir de él; el separador sep="\t" para hacer que la separación sea exactamente un tabulador; use header=TRUE para leer la primera línea como encabezado de nombres de columnas
#Previsualización de  los primeros 6 datos de la tabla y de los 6 últimos con la función head y tail

datos <- read.table("dates1.txt", sep="\t", header = TRUE)
head(datos)
tail(datos)
#La funcion dim nos da informacion sobre las dimensiones,es decir el número de filas y columnas
dim(datos)
#La funcion summary nos da informacion sobre la distribución de los valores de las variables,muestra el minimo, 1er cuartil, mediana, media, 3er cuartil, y máximo
summary(datos)
#La funcion str nos sirve para conocer la estructura de los datos, nos muestra la clase de datos(data.frame) y el numero de dimensiones: 50 obs y 3 variables. Luego muestra el nombre y clase de cada variable, con una previsualización de los datos.
str(datos)


#2. Haz un boxplot para nuestros datos. Uno para cada variable.Colorea a Variable 1 y a Variable 2 de forma diferente (guarda esos colores para las siguientes gráficas)

boxplot(Variable1 ~ Tratamiento, data=datos, col=c("purple"), main="variable1-boxplots", xlab = "TRATAMIENTOS", ylab = "VARIABLE 1")
boxplot(Variable2 ~ Tratamiento, data=datos, col=c("aquamarine"), main="variable2-boxplots", xlab = "TRATAMIENTOS", ylab = "VAEIABLE 2")

#3. Haz un gráfico de dispersión con las dos variables. Cada tratamiento debe de ir de un color distinto. ¡Como en la siguiente imagen! Pista: usa col=datos$Tratamiento
#cex para poner el tamaño del símbolo y lwd para el ancho del borde

plot(datos$Variable2, datos$Variable1, pch = 9, cex = 2, lwd = 2, xlab = "Variable2", ylab = "Variable1", col=datos$Tratamiento, main = "Scatterplot")


#4.Ponle leyenda al gráfico del apartado anterior. En el margen inferior derecho. Pista: investiga sobre legend()

plot(datos$Variable2, datos$Variable1, pch = 9, cex = 2, lwd = 2, xlab = "Variable2", ylab = "Variable1", col=datos$Tratamiento, main = "Scatterplot")
legend("bottomright", legend = c("Tratamiento1", "Tratamiento2", "Tratamiento3", "Tratamiento4", "Tratamiento5"), fill = c("black", "red", "green", "cyan", "blue"), title = "Tratamientos")


#5. Haz un histograma para cada variable. Recuerda mantener los colores.

hist(datos$Variable1, col=c("purple"), main="variable1-histograma", xlab = "VARIABLE 1", ylab = "FRECUENCIA")
hist(datos$Variable2, col=c("aquamarine"), main="variable2-histograma", xlab = "VARIABLE 2", ylab = "FRECUENCIA")

#6. Haz un factor en la columna tratamiento y guárdalo en una variable. Pista: factor(factor$Tratamiento)

tratamiento_factor <- factor(datos$Tratamiento, levels=c ("1", "2", "3", "4", "5"))
tratamiento_factor
class(tratamiento_factor)

#7.Calcula la media y la desviación estándar para cada tratamiento. Recomendación: es más fácil si usas aggregate() o tapply(). 
# Lo he relaizado de las 2 maneras para corroborar que salen los mismos resultados
# Funcion mena para calcular la media y sd para la desviacion estandar
#Podemos ver que aggregate da el resultado como dta.frame y tapply en forma de array

aggregate(Variable1~tratamiento_factor,datos,mean) 
tapply(datos$Variable1,tratamiento_factor,mean)

aggregate(Variable2~tratamiento_factor,datos,mean) 
tapply(datos$Variable2,tratamiento_factor,mean)

aggregate(Variable1~tratamiento_factor,datos,sd) 
tapply(datos$Variable1,tratamiento_factor,sd)

aggregate(Variable2~tratamiento_factor,datos,sd) 
tapply(datos$Variable2,tratamiento_factor,sd)

# Aqui guardo los datos obtenidos en variables
media1 <- tapply(datos$Variable1,tratamiento_factor,mean)
media1

media2 <- tapply(datos$Variable2,tratamiento_factor,mean)
media2

de1 <- tapply(datos$Variable1,tratamiento_factor,sd)
de1

de2 <- tapply(datos$Variable2,tratamiento_factor,sd)
de2


#8.Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si usas table() con el factor

#Usamos table (x) que devuelve una tabla con los números de los diferentes valores de x

table(tratamiento_factor)

#9.Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variable diferente.
# Usamos la funcion subset para crear subconjuntos de nuestros datos originales

tto1 <- subset (datos, Tratamiento == 1,)
tto1

tto4 <- subset (datos, Tratamiento == 4,)
tto4

#10.Nuestra hipótesis nula es que las medias de tratamiento 1 y tratamiento 4 para la
#Variable 1 son iguales. ¿Puedes comprobarlo? Para ello, necesitarás comprobar
#primero si los datos se distribuyen de forma normal. En función del resultado de la
#prueba de normalidad, ¿qué test usarías? ** En general, asumimos que las muestras
#son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo a tus resultados.

#H0 : µ1 = µ2      H1 : µ1 ̸= µ2
#Debemos determinar si la distribución de nuetsra muestra es normal o no para ello emplearemos shapiro.test

#H0 : la distribución de la variable es normal
#H1 : la distribución de la variable es distinta a la normal

#El p value o p-valor nos muestra la probabilidad de haber obtenido el resultado que hemos obtenido si suponemos que la H0 es cierta
#Como p > 0,05 en los 2 tratamientos decimos que no hay diferencias significativas, por lo que no se rechaza la hipotesis nula, por lo que nuestra variable presenta una distribución normal.

tto1_Distribucion_normal <- shapiro.test(tto1$Variable1)
tto1_Distribucion_normal

tto4_Distribucion_normal <- shapiro.test(tto4$Variable1)
tto4_Distribucion_normal


#Como el test anterior indica que nuestras muestras tienen una distribución normal usaria el método de t-Student (t.test)

ttest <- t.test(tto1$Variable1, tto4$Variable1)
ttest

#Como el p value es muy pequeño e inferior a 0,05 y el intervalo de confianza no incluye nuestro valor podemos rechazar nuetsra hipotesis nula de igualdad de medias


#Para comparar las varianzas usamos var.test

varianzas <- var.test (tto1$Variable1, tto4$Variable1)
varianzas

#Como el p value es muy pequeño, rechazamos la hipoteis nula de igualdad de varianzas y concluimos que hay diferencias significativas entre los 2 tratamientos
 