#Generacion de data set
data.matrix <- matrix(nrow=100,ncol=10)
#Nombres de ejemplos
colnames(data.matrix) <- c(
    paste("dto",1:5, sep=""),
    paste("dat",1:5, sep=""))
rownames(data.matrix) <- paste ("muestra", 1:100, sep="")
#Ingreso de datos en la matriz
for(i in 1:100){
  
  dto.values <- rpois(5, lambda = sample(x=10:1000, size=1))
  dat.values <- rpois(5, lambda = sample(x=10:1000, size=1))
  
  data.matrix[i,]<- c(dto.values, dat.values)
}
#Imprime matriz con datos
head(data.matrix)
#Llamado al metodo para realizar pca a los datos (x,sdev,rotation)
#Por defecto el metodo necesita recibir los datos como filas
#las muestras como columnas, se usa la transpuesta
pca <- prcomp(t(data.matrix),scale=TRUE)
#X contiene los componentes principales para grafiCar
plot(pca$x[,1], pca$x[,2])
#sdev calcula cuanto varia la informacion actual de la original
pca.var <- pca$sdev^2
#Calcula el porcentaje
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
#Dibuja una barra de porcentajes
barplot(pca.var.per,main = "Barra de Porcentaje", xlab="Componentes Principales", 
        ylab="Porcentaje de variacion")
#Metodo para graficar
library(ggplot2)
#adaptar informacion para usar correctamente la libreria
pca.data <- data.frame(Sample =rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data
#Grafica PCA ggplot2 
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample))+
  geom_text()+
  xlab(paste("PC1 -", pca.var.per[1],"%", sep=""))+
  ylab(paste("PC2 -", pca.var.per[2],"%", sep=""))+
  theme_bw()+
  ggtitle("Grafico PCA")
#Se carga los componentes de esta manera se verifica
#cuales tienen un mayor efecto sobre el eje 1
#izquierda valores negativo, derecha valores positivos
loading_scores <- pca$rotation[,1]
#se usa el valor absoluto para ordenar por magnitud
tipo_scores <- abs(loading_scores)
#Se ordena los valores de mayor a menor
tipo_scores_ranked <- sort(tipo_scores, decreasing = TRUE)
#Se retira los 10 valores más altos con nombres
top_10_tipo <- names(tipo_scores_ranked[1:10])
#muestra los nombres de los 10 valores
top_10_tipo
#Muestra los nombres y los valores de las 10 muestras
pca$rotation [top_10_tipo,1]

