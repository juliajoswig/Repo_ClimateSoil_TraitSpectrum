decreasing_perc_expl_with_increasing_PCA <- function(){
  
require(FactoMineR)  
  
input_6<- matrix(NA,ncol=6,nrow=1000)
  for(i in 1:ncol(input_6)){
    input_6[,i] <- rnorm(nrow(input_6))
  }

input_10 <- matrix(NA,ncol=10,nrow=1000)
for(i in 1:ncol(input_10)){
  input_10[,i] <- rnorm(nrow(input_10))
}

input_17 <- matrix(NA,ncol=17,nrow=1000)
for(i in 1:ncol(input_17)){
  input_17[,i] <- rnorm(nrow(input_17))
}

input_30 <- matrix(NA,ncol=30,nrow=1000)
for(i in 1:ncol(input_30)){
  input_30[,i] <- rnorm(nrow(input_30))
}


pca_6 <- PCA(input_6)
pca_10 <- PCA(input_10)
pca_17 <- PCA(input_17)
pca_30 <- PCA(input_30)

pca_6$eig[1:3,3]
pca_10$eig[1:3,3]
pca_17$eig[1:3,3]
pca_30$eig[1:3,3]
}