load_analysis <- function(origin,nruns,climOsoil,output_term,doPCA){
    
  #--------------------------------------------------------------------------------------
  # load data
  #--------------------------------------------------------------------------------------
    secondname=paste0(sub(".*A", "", climOsoil))
    firstname=paste0(substring(text = climOsoil,first = 1,last = regexpr("A", climOsoil)[1]-1))
    

    if(doPCA){pca_term="PCA"}
    if(!doPCA){pca_term="no_PCA"}
  #load first
  load(file = file.path( origin, "data", "_results","PLS",firstname, paste0(nruns,"Reps"),
                         paste0("Res_",output_term, pca_term,".RData")))    
  first_res <- out
  rm("out")
  #load second
  load(file = file.path( origin, "data", "_results","PLS",secondname, paste0(nruns,"Reps"),
                         paste0("Res_",output_term, pca_term,".RData")))    
  second_res <- out
  rm("out")
  
  #load firstAsecond
  if(file.exists(file.path( origin, "data", "_results","PLS",paste0(firstname,"A",secondname), paste0(nruns,"Reps"),
                       paste0("Res_",output_term, pca_term,".RData")))){
    load(file = file.path( origin, "data", "_results","PLS",paste0(firstname,"A",secondname), paste0(nruns,"Reps"),
                           paste0("Res_",output_term, pca_term,".RData")))}
  if(file.exists(file.path( origin, "data", "_results","PLS",paste0(secondname,"A",firstname), paste0(nruns,"Reps"),
                            paste0("Res_",output_term, pca_term,".RData")))){
    load(file = file.path( origin, "data", "_results","PLS",paste0(secondname,"A",firstname), paste0(nruns,"Reps"),
                           paste0("Res_",output_term, pca_term,".RData")))}
  firstAsecond_res <- out
  rm("out")
  
  out <- list(r2_first=first_res$r2,
              r2_second=second_res$r2,
              r2_firstAsecond=firstAsecond_res$r2)
  
  names(out)[1] <- paste0("r2_",firstname)
  names(out)[2] <- paste0("r2_",secondname)
  names(out)[3] <- paste0("r2_",climOsoil) 
  
  return(out)
}
