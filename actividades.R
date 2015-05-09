Sys.setenv("HADOOP_PREFIX"="/opt/hadoop")
Sys.setenv("HADOOP_CMD"="/opt/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/opt/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.4.0.jar")
library(rhdfs)
library(rmr2) 
hdfs.init()
rmr.options(backend="local") # Opciones "local" o "hadoop"

system("/opt/hadoop/bin/hdfs dfs -rm -r -skipTrash /user/root/wordcount/out")
system("/opt/hadoop/bin/hdfs dfs -rm -r -skipTrash /user/root/wordcount/data")
system("/opt/hadoop/bin/hdfs dfs -mkdir -p /user/root/wordcount/data")
system("/opt/hadoop/bin/hdfs dfs -copyFromLocal /home/hduser/Gutenberg/pg2000.txt /user/root/wordcount/data")
system("/opt/hadoop/bin/hdfs dfs -ls /user/root/wordcount/data")
system("whoami")

## self-defined R KMeans function
kmeans.mr = function( P, 
                      num.clusters, 
                      num.iter, 
                      combine, 
                      in.memory.combine ) {
  ## Sum of square distant function
  dist.fun =  function(C, P) { apply( C, 
                                      1, 
                                      function(x) colSums((t(P) - x)^2)) }
  
# ---------------------------- Map job --------------------- #
  kmeans.map = function(., P) {
    nearest = {
      if(is.null(C)) 
        sample( 1:num.clusters, nrow(P), replace = TRUE)
      else {
        D = dist.fun(C, P)
        nearest = max.col(-D)
      }
    }
    if(!(combine || in.memory.combine))
      keyval(nearest, P) 
    else 
      keyval(nearest, cbind(1, P))
  }
  
# ---------------------------------- Reduce job --------------------------------- #
  kmeans.reduce = {
    if (!(combine || in.memory.combine) ) 
      function(., P) t(as.matrix(apply(P, 2, mean)))
    else 
      function(k, P) keyval( k, t(as.matrix(apply(P, 2, sum))))
  }

# ------------------------------------------------------------------------------------#

  C = NULL
  for(i in 1:num.iter ) {
    calc <- mapreduce( P, 
                       map = kmeans.map,
                       reduce = kmeans.reduce,
                       verbose = FALSE )
    C = values( from.dfs( calc ) )
    if(combine || in.memory.combine)
      C = C[, -1]/C[, 1]
    #points(C, col = i + 1, pch = 19)
    if(nrow(C) < num.clusters) {
      C = rbind( C, 
                 matrix( rnorm( (num.clusters - nrow(C)) * nrow(C)), ncol = nrow(C)) %*% C) 
    }
  }   
  C
}

## Main
ignore <- rmr.options(backend = "local")
setwd(dir = "/mnt/live/memory/data/twitteR/Presentacion_Lab_07")
Datos = read.table(file = "23595856_23696989_22671778_twitter_moracarlos_logaritmo.csv", 
                   header=TRUE, 
                   sep = ";", 
                   dec = ",",
                   row.name = 1, 
                   encoding = "UTF-8")

Datos = subset(x = Datos, select = 2:8)

out <- kmeans.mr( to.dfs(Datos), 
                  num.clusters = 4, 
                  num.iter = 15,
                  combine = FALSE,
                  in.memory.combine = FALSE)
out

groupsDF = Datos[1,]
groupsDF = rbind(groupsDF, Datos[3,])
groupsDF1 = read.csv(row.names = 1, text="id,statusesCount,followersCount,favoritesCount,friendsCount,created,verified,relationship")
groupsDF1 = rbind(groupsDF1, Datos[3,])

cluster = rep(0, length(Datos[,1]))
for (j in 1:length(Datos[,1])){
  nearest = dist(rbind(Datos[j,], out[1,]))
  group = 1
  cluster[j]=group
  for(i in 2:length(out[,1])){
    if(dist(rbind(Datos[j,], out[i,]))[1] < nearest[1]){
      nearest = dist(rbind(Datos[j,], out[i,]))
      group = i
      cluster[j]=group
    }
  }  
}

cluster #listado de los grupos asignados a cada registro

Datos$cluster <- factor(cluster)

#Datos

write.csv(Datos, "23595856_23696989_22671778_twitter_moracarlos_grupos.csv")

cant = rep(0,length(out[,1]))
for (j in 1:length(cluster))
{
  if (cluster[j]==1) cant[1] = cant[1] + 1
  if (cluster[j]==2) cant[2] = cant[2] + 1
  if (cluster[j]==3) cant[3] = cant[3] + 1
  if (cluster[j]==4) cant[4] = cant[4] + 1
}

cant #cantidad de elementos en cada grupo

porc = lapply(cant, function(x) (x*100)/length(Datos[,1]))
porc #porcentaje que representa cada grupo

#--------------------------------------------------------------------------------------------------#
install.packages("FactoMineR")
install.packages("ppls")
library("ppls")
library(FactoMineR)

Datos = read.table(file = "23595856_23696989_22671778_twitter_moracarlos_logaritmo.csv", 
                   header=TRUE, 
                   sep = ";", 
                   dec = ",",
                   row.name = 1, 
                   encoding = "UTF-8")

prinComp<-PCA(Datos, scale.unit = TRUE, ncp=5, graph = FALSE) #Aplicamos el metodo de CP
#2.2
plot(prinComp, axes = c(1,2), choix = "ind", col.ind = "red", new.plot = TRUE)
#2.1
plot(prinComp, axes = c(1,2), choix = "var", col.var = "blue", new.plot = TRUE)
#2.4
cos2.ind <- (prinComp$ind$cos2[, 1] + prinComp$ind$cos2[, 2]) * 100
cos2.var <- (prinComp$var$cos2[, 1] + prinComp$var$cos2[, 2]) * 100
write("El grado de representacion de los individuos es:")
cos2.ind
write("El grado de representacion de las variables es:")
cos2.var
#graficar solo los que están bien representados
#Mostrar el grado de representaci?n en la gr?fica
par(mfrow = c(1, 2))
plot(prinComp, axes = c(1, 2), choix = "ind", col.ind = "red", new.plot = TRUE, select = "cos2 0.6")
plot(prinComp, axes = c(1, 2), choix = "var", col.var = "red", new.plot = TRUE, select = "cos2 0.6")

#3
res.hcpc<- HCPC(prinComp,
                nb.clust= -1,
                consol = TRUE,
                min=3,
                max=4,
                graph=FALSE)
plot(res.hcpc)
#4
salida <- res.hcpc$data.clust #Imprimirlo en el archivo
write.table(salida, file = "Servicio_al_cliente_sal.csv", row.names=TRUE , sep=";")

plot(res.hcpc,
     axes = c(1,2), 
     choice = "tree")

