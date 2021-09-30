Tab_S1 <- function(origin){
  
  load(file.path(origin,"data","helper_files","fig_2","PCA3.RData"))
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                  "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")
  target_order=Rename_Vars(target_order1)[,3]
  ix=match(table=rownames(pca_FMr$var$coord),x = target_order1)
  
  res <- matrix(NA,ncol=5,nrow=18)
  colnames(res) <- c("Trait", "Group",	"PC1",	"PC2",	"PC3")
  res[1,3:5] <- round(pca_FMr$eig$`percentage of variance`[1:3],digits=2)
  res[1,1] <- "Variance explained"
  res[2:18,1] <- Rename_Vars(rownames(pca_FMr$var$coord))[,3]
  res[2:18,2] <- put_into_traitGroup(rownames(pca_FMr$var$coord))
  res[2:18,3:5] <- round(pca_FMr$var$coord[,1:3],digits=2)
#  res <- res[c(1,order(res[2:18,2])+1),]
  res <- res[c(1,ix+1),]
  
  write.csv(res, file=file.path(origin,"tables","Table_S1","Table_S1.csv"))
  
}