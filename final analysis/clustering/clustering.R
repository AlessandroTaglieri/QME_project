#CLUSTER 2014
library(readxl)
df2014 <- read_excel("df2014.xlsx", col_types = c("text", 
                                                  "numeric", "numeric", "numeric"))
View(df2014)

regioni = df2019$regioni

df_2014_scl=scale(df2014[,-1])

#### Aggregazione gerarchiaca con distanza euclidea e metodo di Ward

d_2014 = dist(df_2014_scl, method="euclidean")
d_2014 # matrice delle distanze delle variabili standardizzati

aggregazione_2014 = hclust(d, method = "ward.D")
aggregazione_2014$merge
aggregazione_2014$height
aggregazione_2014$order
aggregazione_2014$labels

plot(aggregazione_2014, labels = regioni, main = "Cluster analysis 2014", ylab = "Distance")
num_clus = 4
gruppi2014 = cutree(aggregazione, k=num_clus)
gruppi2014

rect.hclust(aggregazione2, k=num_clus, border = c("red", "blue", "green", "yellow"))


##CLUSTER 2019
library(readxl)
df2019 <- read_excel("df2019.xlsx", col_types = c("text", 
                                                  "numeric", "numeric", "numeric"))
View(df2019)

regioni = df2019$regioni

df_2019_scl=scale(df2019[,-1])

#### Aggregazione gerarchiaca con distanza euclidea e metodo di Ward

d_2019 = dist(df_2019_scl, method="euclidean")
d_2019 # matrice delle distanze delle variabili standardizzati

aggregazione_2019 = hclust(d, method = "ward.D")
aggregazione_2019$merge
aggregazione_2019$height
aggregazione_2019$order
aggregazione_2019$labels

plot(aggregazione_2019, labels = regioni, main = "Cluster analysis 2019", ylab = "Distance")
num_clus = 4
gruppi2019 = cutree(aggregazione, k=num_clus)
gruppi2019

rect.hclust(aggregazione2, k=num_clus, border = c("red", "blue", "green", "yellow"))