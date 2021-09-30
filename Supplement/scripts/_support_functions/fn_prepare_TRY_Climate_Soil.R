prep_TRY_Env <- function(Appr_type_now,output_term,Rename_Vars,TRY_Env_data){
  
  if(output_term=="woody"){
    TRY_Env <- TRY_Env_data[,-c(grep(names(TRY_Env_data),pattern = "_nw"))]
    names(TRY_Env) <- gsub(names(TRY_Env),pattern = "_w",replacement = "")
  }
  if(output_term=="non_woody"){
    TRY_Env <- TRY_Env_data[,-c(grep(names(TRY_Env_data),pattern = "_w"))]
    names(TRY_Env) <- gsub(names(TRY_Env),pattern = "_nw",replacement = "")
  }

  if(output_term==""){TRY_Env <- TRY_Env_data}
  
  # ---------------------------------------------------------------------------------------
  # prepare Data
  # ---------------------------------------------------------------------------------------
  
    print("split into traits and environment")

   # split into traits and environment (soil & climate)
    traits <- TRY_Env[Rename_Vars(names(TRY_Env))[,2]=="trait"]
    soil <- TRY_Env[Rename_Vars(names(TRY_Env))[,2]=="soil"]
    climate <- TRY_Env[Rename_Vars(names(TRY_Env))[,2]=="climate"]
    info <- cbind(TRY_Env[names(TRY_Env)=="Kier_richness"|
                              names(TRY_Env)=="ECO_ID"|
                              names(TRY_Env)=="ECO_NAME"|
                              names(TRY_Env)=="BIOME"|
                              names(TRY_Env)=="species.count"|
                              names(TRY_Env)=="observation.count.tot"|
                              names(TRY_Env)=="Lat"|
                              names(TRY_Env)=="Lon"|
                              names(TRY_Env)=="min.lat"|
                              names(TRY_Env)=="max.lat"|
                              names(TRY_Env)=="min.lon"|
                              names(TRY_Env)=="max.lon"|
                              names(TRY_Env)=="Kier_area"],
                    TRY_Env[grep(names(TRY_Env),pattern = "observation.count.")]
    )
    
    climate_waterA<- TRY_Env[ix,Rename_Vars(names(TRY_Env))[,7]=="climate_water"]
    climate_energyA <- TRY_Env[ix,Rename_Vars(names(TRY_Env))[,7]=="climate_energy"]
    
    soil_physics <- TRY_Env[ix,Rename_Vars(names(TRY_Env))[,4]=="soil_physics"]
    soil_chemistry <- TRY_Env[ix,Rename_Vars(names(TRY_Env))[,4]=="soil_chemistry"]
    

    noise=as.data.frame(matrix(NA,ncol=ncol(soil),nrow=(nrow(soil))))
    for(i in 1:ncol(soil)){
      noise[,i]= rnorm(nrow(soil),
                       mean = sample(x = c(-5:+5),size = 1),
                       sd= sample(c(.5,.4,.3,.2,.1,5,4,3,2,1),size = 1)) 
    }
    colnames(noise) = paste0("noise",1:ncol(noise))
    
  
  out=list(info=info,
           trait=trait,
           soil=soil,
           climate=climate,
           noise=noise,
           soil_physics=soil_physics,
           soil_chemistry=soil_chemistry)
  
 return(out)
}