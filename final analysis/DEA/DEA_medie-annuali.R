
#SIII - DEA-inv + pop vs letti -------------------------------

## Install libraries
library(FEAR)
library(corrplot)
library(dplyr)
library(plyr)



## Reading the dataset

##### Considering every year
df_csv_all <- read.csv("/Users/camilla/Downloads/final_DEA_v3.csv")
#View(df_csv_all)
attach(df_csv_all)

# computing the mean over the year
df_medie = ddply(df_csv_all, .(Territorio), summarize,  popolazione=mean(popolazione), investimento=mean(investimento), posti_letto=mean(posti_letto))

# taking out Lombardia which is an outlier
df_medie <- df_medie %>% filter(Territorio!="Lombardia")


## Variable Definition
###  Production System with: 3 inputs, 1 output

X1 <- as.numeric(df_medie$investimento)
X3 <- as.numeric(df_medie$popolazione)
X <- matrix(c(X1, X3), ncol=2)               # Matrix combining the 2 inputs
#View(X)
Y <- as.numeric(df_medie$posti_letto)
# View(data.frame(X, Y))
df <- data.frame(X, Y)
#View(df)




## Efficiency Analysis

library(Benchmarking)

## Testing Returns to Scale and Convexity Assumptions

x <- t(X)      # Transposing the X Matrix to work on FEAR
y <- t(Y)      # Transposing the Y vector to work on FEAR

set.seed(123)
#?test.rts #nonparametric test of constant versus variable returns to scale
test.rts(x, y, ORIENTATION = 1, METRIC = 1, NSPLIT = 1, NREP = 1000) 
"--> if < 0.05 use CRS"
#test.rts(x, y, ORIENTATION = 1, METRIC = 1, NSPLIT = 2, NREP = 1000)
?test.rts
"The test involves randomly splitting the sample into two independent subsamples,
and comparing the mean of DEA efficiency estimates from the first subsample where
constant returns are imposed against the mean of DEA efficiency estimates
from the second subsample allowing for variable returns to scale."

?test.convexity
test.convexity(x, y, ORIENTATION = 1, METRIC = 1, NSPLIT = 1, NREP = 1000)
#test.convexity(x, y, ORIENTATION = 1, METRIC = 1, NSPLIT = 2, NREP = 1000)


dea(X, Y, RTS = "crs", ORIENTATION = "in", SLACK = TRUE, DUAL = TRUE)
e <- dea(X, Y, RTS = "crs", ORIENTATION = "in")

efficienza = eff(e)
df_medie[[5]] = efficienza
colnames(df_medie)[5] <- "efficienza"
summary(e)                  # A nice summary of results

par(mfrow = c(1, 3))  # Visualization with 1 row, 3 columns
dea.plot(X, Y, RTS = "crs", main = "Constant Returns to Scale") # Efficient Frontier Plot under CRS Technology
dea.plot(X, Y, RTS = "vrs", main = "Variable Returns to Scale") # Efficient Frontier Plot under VRS Technology
dea.plot(X, Y, RTS = "fdh", main = "Free Disposal Hull")



## CLUSTER ON EFFICIENCY SCORE
library(miscTools)
regioni = df2014$Regioni
eff = as.matrix(efficienza)

insertCol(eff, 2, v = regioni, cName = "Regioni")

d_eff = dist(eff, method = "euclidean")
d_eff #matrix of the distance between efficiency scores


aggregation_eff=hclust(d_eff, method = "ward.D")
par(mfrow = c(1, 1))  # Visualization with 1 row, 1 columns
plot(aggregation_eff, labels = regioni, main = "Cluster analysis efficiency", ylab = "Distance")

clus = 3 #number of cluster selected
gruppi = cutree(aggregation_eff, k=clus)
gruppi

rect.hclust(aggregation_eff, k=clus, border = c("red", "blue", "green")) #cluster visualization
