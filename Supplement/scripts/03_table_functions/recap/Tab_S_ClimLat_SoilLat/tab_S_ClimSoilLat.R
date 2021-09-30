#install.packages("likert")
#whichfig="3b"
plot_Figure_S3b <- function(origin,nruns,doPCA){
  require("likert")
  #-------------
  # load data
  #-------------
  climOsoil="soilAclimate"
   output_term="climLat"
  
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results")) 
  
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                   paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
    hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
    title <- "" 
  
  # data processing
      m <- matrix(NA,ncol=4,nrow=ncol(hp_now[[3]]))
      m[,1] <- Rename_Vars(colnames(hp_now[[3]]))[,3]
      m[,1] <- colnames(hp_now[[3]])#Rename_Vars()[,3]
      m[,2] <- colMeans(hp_now[[2]] - hp_now[[4]])*100# pure indep effect
      m[,3] <- colMeans(hp_now[[3]])*2*100# joint effect
      m[,4] <- colMeans(hp_now[[1]] - hp_now[[3]])*100# pure indep effect
      m[m<0]  = 0
      

      if(output_term=="soilLat"){
        colnames(m)<- c("Item","Independent_latitude","Joint","Independent_soil")
      df <- data.frame(m)
      df <- transform(df,Independent_latitude=as.numeric(as.vector(Independent_latitude)),
                      Independent_soil=as.numeric(as.vector(Independent_soil)),Joint=as.numeric(as.vector(Joint)))
      }  
      if(output_term=="climLat"){
        colnames(m)<- c("Item","Independent_climate","Joint","Independent_latitude")
       df <- data.frame(m)
       df <- transform(df,Independent_climate=as.numeric(as.vector(Independent_climate)),
                       Independent_latitude=as.numeric(as.vector(Independent_latitude)),Joint=as.numeric(as.vector(Joint)))
      }  
  
    dfs <- likert(summary = df)#,grouping = put_into_traitGroup(colnames(hp_now$r2_soil)))
    str(dfs)
    #The items are not present, but the likert object can still be summarised:
    summary(dfs)
    
    tab_out <- matrix(NA,ncol=4,nrow=10)
    tab_out[1,] <- c("Latitude independent effect","Joint effect","Climate independent effect","Soil independent effect") 
    tab_out[2,1] <- c("Total")
    tab_out[3,c(3:1)] <- round(as.numeric(apply(dfs$results,MARGIN = 2,FUN = median)[2:ncol(dfs$results)]),digits = 2)
    tab_out[5,1] <- c("Size")
    tab_out[6,c(3:1)] <- round(as.numeric(apply(dfs$results[put_into_traitGroup(dfs$results[,1])=="Size",],MARGIN = 2,FUN = median)[2:ncol(dfs$results)]),digits = 2)
    tab_out[8,1] <- c("Economics")
    tab_out[9,c(3:1)] <- round(as.numeric(apply(dfs$results[put_into_traitGroup(dfs$results[,1])=="Eco",],MARGIN = 2,FUN = median)[2:ncol(dfs$results)]),digits = 2)
    tab_out
    
    #-------------
    # load data
    #-------------
    climOsoil="soilAclimate"
    output_term="soilLat"
    # output_term="climLat"
    
    if(doPCA){pca_term="doPCA"}
    if(!doPCA){pca_term="no_PCA"}
    list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results")) 
    
    load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                   paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
    hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
    title <- "" 
    
    # data processing
    m <- matrix(NA,ncol=4,nrow=ncol(hp_now[[3]]))
    m[,1] <- Rename_Vars(colnames(hp_now[[3]]))[,3]
    m[,1] <- colnames(hp_now[[3]])#Rename_Vars()[,3]
    m[,2] <- colMeans(hp_now[[2]] - hp_now[[4]])*100# pure indep effect
    m[,3] <- colMeans(hp_now[[3]])*2*100# joint effect
    m[,4] <- colMeans(hp_now[[1]] - hp_now[[3]])*100# pure indep effect
    m[m<0]  = 0
    
    
    if(output_term=="soilLat"){
      colnames(m)<- c("Item","Independent_latitude","Joint","Independent_soil")
      df <- data.frame(m)
      df <- transform(df,Independent_latitude=as.numeric(as.vector(Independent_latitude)),
                      Independent_soil=as.numeric(as.vector(Independent_soil)),Joint=as.numeric(as.vector(Joint)))
    }  
    if(output_term=="climLat"){
      colnames(m)<- c("Item","Independent_climate","Joint","Independent_latitude")
      df <- data.frame(m)
      df <- transform(df,Independent_climate=as.numeric(as.vector(Independent_climate)),
                      Independent_latitude=as.numeric(as.vector(Independent_latitude)),Joint=as.numeric(as.vector(Joint)))
    }  
    
    dfs <- likert(summary = df)#,grouping = put_into_traitGroup(colnames(hp_now$r2_soil)))
    
    
    tab_out[4,c(1,2,4)] <- round(as.numeric(apply(dfs$results,MARGIN = 2,FUN = median)[2:ncol(dfs$results)]),digits = 2)
    tab_out[7,c(1,2,4)] <- round(as.numeric(apply(dfs$results[put_into_traitGroup(dfs$results[,1])=="Size",],MARGIN = 2,FUN = median)[2:ncol(dfs$results)]),digits = 2)
    tab_out[10,c(1,2,4)] <- round(as.numeric(apply(dfs$results[put_into_traitGroup(dfs$results[,1])=="Eco",],MARGIN = 2,FUN = median)[2:ncol(dfs$results)]),digits = 2)
    tab_out
    #Plots
    

    require("xtable")    
    print(xtable(tab_out), include.rownames=FALSE)
}
