#############################################################################
#
# PRACTICA 3
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
## ENTREGA EL 01 OCTUBRE 23:59
## Se requiere la entrega de este script completado con los códigos más las imágenes y las respuestas a las preguntas
## Adjuntar en la entrega el PDF final y el archivo con los genes
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")


# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))


# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data)
 "esto mide las dimensiones" 
head(data)
 "asi vemos las primeras filas"
tail(data)
 "asi vemos las ultimas filas"


# Hacemos un primer histograma para explorar los datos
hist (data)
hist(data, col = "gray", main="GSE5583 - Histogram")

# Transformamos los datos con un logaritmo 

# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?data
data_log=log2(data)
hist(data_log)
"Si hacemos una transformación logaritmica obtenemos una distribucion más normal de los datos para un mejor análisis"

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot? 
"Un boxplot es una representación mediante un diagrama de cajas de una variable cuantitativa que permite identificar valores  atípicos y observar como el 50% de los datos centrales están distribuidos.
boxplot(data_log)
boxplot(data_log, col=c("blue","blue","blue","orange","orange","orange")) 
"esto pone de un color azul los 3 primeros y de color naranja los 3 ultimos"

boxplot(data_log, col=c("blue","blue","blue","orange","orange","orange"), main="GSE5583-boxplots",las=2)
"main para poner el nombre a nuestro boxplot"

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación
# de los valores de expresión. ¿Es correcta la separación?
hc = hclust (as.dist(1-cor(data_log)))
plot(hc, main="GSE5583 - Hierarchical Clustering")
"si que nos ha hecho una buena clasificación, claramente hay 3 knok-out y 3 wild-type"

#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
"Nuestros datos ahora quedan organizados en una matriz"
wt <- data[,1:3]
ko <- data[,4:6]
class(wt)
head(wt)

# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean)
head(wt.mean)
ko.mean = apply(ko, 1, mean)
head(ko.mean)


# ¿Cuál es la media más alta?
max(wt.mean)
max(ko.mean)
"la media mas alta esta en los knok-out"


# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean)

plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO", main = "GSE5583 - Scatter")


# Añadir una línea diagonal con abline
abline(0, 1, col = "red")
"y=ax+b y=x a=1 b=0"
abline(h=2,col="blue")
"h nos indica horizontal"
abline(v=5,col="green")
"v nos indica vertical"
# ¿Eres capaz de añadirle un grid?
grid()
"con esto hacemos una cuadrícula"

# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean

# Hacemos un histograma de las diferencias de medias
hist(diff.mean)
hist(diff.mean,col="purple")
-> esto para cambiar el color


# Calculamos la significancia estadística con un t-test.
por cada fila de la matriz hacer un t-test
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué?
"Porque si lo hiciesemos con una transformación logarítmica dejarían de ser datos fiables , los lograitmos solo los usamos para realizar las gráficas.
# ¿Cuántas valores tiene cada condicion?
"Dentro de cada condicion hay 3 valores
Cada gen tiene 6 muestras y 2 condiciones, 3 muestras o replicas para cada condicion

pvalue = NULL
tstat = NULL
for(i in 1 : nrow(data)) { #Para cada gen
      x = wt[i, ] #gene wt numero i
      y = ko[i, ] #gene ko numero i

      # Hacemos el test
      t = t.test(x,y)

      #Añadimos el p-value a la lista, guarda los pvalues en lugar de mi vector p value
      pvalue[i] = t$p.value
       
      # Añadimos las estadisticas a la lista 
      tstat[i] = t$statistic
}
head(pvalue)
 para cada fila desde la 1 hasta el total de filas haz esto, la i se utiliza solo dentro del bucle, para cada fila de data voy a coger la variable x es un vector con 3 valores al igual que la y 
Hacemos el test de los 3 valores frente a los otros 3 valores de la otra condicion
Obtenemos tantos pvalues como numero de filas

length(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos


# Hacemos un histograma de los p-values.
hist(pvalue)

# ¿Qué pasa si le ponemos con una transformación de -log10?
hist(-log10(pvalue), col = "blue")
"Nos invierte como la distribucion,ha transformado nuestra distribucion (con sesgo positivo) haciendola más normal, expandiendo la parte izquierda y comprimiendo la derecha.
" De esta manera podemos ver mejor la grafica al cambiar las distribucion de los datos y dejar los p values significativos en la cola.

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")

" Los significativos se encuentran en la parte de arriba del volcano plot (debido a los valores del -log10)
# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
 
En este caso la diferencia de medias es 2 y el p value es 0,01 haciendolo mas restrictivo
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
Con abline marcamos donde hay significacion o no mediante una linea vertical, lwd es el ancho de linea
El p value seria en una linea horizontal
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3) 

#¿Puedes representarlo en el gráfico?

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias ( 2 y -2) (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff
dim(data[filter_by_diff.mean, ])


# Ahora el filtro de p-value (miramos cuantos valores superan el filtro)
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios? (combinamos ambos identificadores y nos quedamos con los genes comunes a los 2 filtros)filter_combined = filter_by_diff.mean & filter_by_pvalue
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)
"Cumplen los criterios 426 genes"

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2") 
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points(diff.mean[filter_combined & diff.mean < 0],
  -log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points(diff.mean[filter_combined & diff.mean > 0],
-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE)
heatmap(filtered)
"cexCol es el tamaño de letra del eje x"
"Colv y Rowv son los dendogramas"
"labRow= False vectores de caracteres con etiquetas de filas, ponemos false para que no aparezcan"
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col=hcl.colors(50))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col=cm.colors(12))

heatmap(filtered, col=cm.colors(12))



# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)

# Hacemos nuestro heatmap
heatmap.2 (filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col = rev(redgreen(256)), scale = "row")



# Lo guardamos en un archivo PDF


# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",
  quote = FALSE)

