load_analysis <- function(origin,nruns,climOsoil,output_term,doPCA){
  
  #--------------------------------------------------------------------------------------
  # load data
  #--------------------------------------------------------------------------------------
  secondname=paste0(sub(".*A", "", climOsoil))
  firstname=paste0(substring(text = climOsoil,first = 1,last = regexpr("A", climOsoil)[1]-1))
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  
  #load first
  list.files(file.path( origin, "data", "_results","RidgeRegression",firstname, paste0(nruns,"Reps")))
  load(file = file.path( origin, "data", "_results","RidgeRegression",firstname, paste0(nruns,"Reps"),  
                         paste0("Res_",output_term,pca_term,".RData")))    
  first_res <- out
  rm("out")
  #load second
  load(file = file.path( origin, "data", "_results","RidgeRegression",secondname, paste0(nruns,"Reps"),  
                         paste0("Res_",output_term,pca_term,".RData")))    
  second_res <- out
  rm("out")
  
  #load firstAsecond
  if(climOsoil%in%c("soil_physicsAsoil_chemistry","soil_chemistryAsoil_physics",
                    "climate_waterAclimate_energy","climate_energyAclimate_water")){
    
    if(climOsoil%in%c("soil_physicsAsoil_chemistry","soil_chemistryAsoil_physics")&
       file.exists(file.path( origin, "data", "_results","RidgeRegression","soil", paste0(nruns,"Reps"),  
                              paste0("Res_",output_term,pca_term,".RData")))){
      load(file = file.path( origin, "data", "_results","RidgeRegression","soil", paste0(nruns,"Reps"),  
                             paste0("Res_",output_term,pca_term,".RData")))
      firstAsecond_res <- out
      rm("out")}
    
    if(climOsoil%in%c("climate_waterAclimate_energy","climate_energyAclimate_water")&
       file.exists(file.path( origin, "data", "_results","RidgeRegression","climate", paste0(nruns,"Reps"),  
                              paste0("Res_",output_term,pca_term,".RData")))){
      load(file = file.path( origin, "data", "_results","RidgeRegression","climate", paste0(nruns,"Reps"),  
                             paste0("Res_",output_term,pca_term,".RData")))
      firstAsecond_res <- out
      rm("out")}
  }else{
  if(file.exists(file.path( origin, "data", "_results","RidgeRegression",paste0(firstname,"A",secondname), paste0(nruns,"Reps"),  
                       paste0("Res_",output_term,pca_term,".RData")))){
    load(file = file.path( origin, "data", "_results","RidgeRegression",paste0(firstname,"A",secondname), paste0(nruns,"Reps"),  
                           paste0("Res_",output_term,pca_term,".RData")))}
  if(file.exists(file.path( origin, "data", "_results","RidgeRegression",paste0(secondname,"A",firstname), paste0(nruns,"Reps"),  
                            paste0("Res_",output_term,pca_term,".RData")))){
    load(file = file.path( origin, "data", "_results","RidgeRegression",paste0(secondname,"A",firstname), paste0(nruns,"Reps"),  
                           paste0("Res_",output_term,pca_term,".RData")))}
  firstAsecond_res <- out
    rm("out")
  }

  
  out <- list(r2_first=first_res$r2,
              r2_second=second_res$r2,
              r2_firstAsecond=firstAsecond_res$r2)
  names(out)[1] <- paste0("r2_",firstname)
  names(out)[2] <- paste0("r2_",secondname)
  names(out)[3] <- paste0("r2_",climOsoil) 
  
  return(out)
}
