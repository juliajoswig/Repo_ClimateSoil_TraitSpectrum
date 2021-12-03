# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement" # please add your local path here 
list.files(file.path(origin,"scripts/_master"))

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
# packages
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))


if(!file.exists(file.path(origin,"figures","Supplement_Fig_42"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_42"))}


  # load data
  output_term=""
  load(file = file.path(origin,"data","master_matrix",paste0("X2",output_term,".RData")))
  info=TRY_Env$info
  trait=TRY_Env$trait

  list.files(file.path(origin,"data","master_matrix"))
  load(file = file.path(origin,"data","master_matrix","TRY_Env2_Kier_20200424.RData"))# maybe update when data issue
  TRY_Env <- TRY_Env2_Kier
  
  output_term=""
  pca_term="doPCA"
  nruns=50
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_", output_term,pca_term,".RData")))
  r2_l <- r2_l$soilAclimate
  
  #----------------------------------
  TRY_Envc <- TRY_Env[TRY_Env$ECO_ID%in%info$ECO_ID,]
  TRY_Env_obs <- TRY_Envc[,grep(names(TRY_Envc),pattern = "_obs")]
  TRY_Env_gf <- TRY_Envc[,grep(names(TRY_Envc),pattern = "_gf")]

  trait_names = names(TRY_Envc)[grep(names(TRY_Envc),pattern = "sd.unique_spec.")]
  trait_names = gsub(trait_names,pattern = "sd.unique_spec.",replacement = "")
  trait_names = gsub(trait_names[grep(trait_names,pattern = "_gf")],pattern = "_gf",replacement = "")
  
  dim(TRY_Env_obs)
  boxplot(TRY_Env_obs[,grep(names(TRY_Env_obs),pattern = "sd.unique_spec.")],las=2,
        col=color_to_traits(trait_names))

  
  pdf(file=file.path(origin,"figures","Supplement_Fig_42","figure_S_CoefAggreg.pdf"),width = 8,height = 6)
  
    log10_traits = log10(TRY_Envc[,Rename_Vars(gsub(names(TRY_Envc),pattern = "_gf",replacement = ""))[,2]=="trait"])
    scaled_median_traits <- apply(log10_traits, 2, FUN = sd)
    dat_plot = (colMeans(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")],na.rm = T)/
                  scaled_median_traits)
    
    names(dat_plot) <- gsub(names(dat_plot),pattern = "sd.unique_spec.",replacement = "")
    names(dat_plot) <- gsub(names(dat_plot),pattern = "_gf",replacement = "")
    ord.ix <- order(put_into_traitGroup(names(dat_plot)))
    
    par(mfrow=c(1,1),mar=c(5,5,2,2))
    plot(log10(dat_plot),colMeans(r2_l$r2_soilAclimate),las=2,pch=16,cex=2,cex.lab=2,
         col=color_to_traits(names(dat_plot)),ylab="Variance explained",xlab="Coefficient of variation",xlim=c(-.1,2),ylim=c(0,.9)) 
    points(log10(dat_plot),colMeans(r2_l$r2_soil),las=2,pch=6,cex=2,
           col=color_to_traits(names(dat_plot)),ylab="r2_soil",xlab="coefficient of variation") 
    points(log10(dat_plot),colMeans(r2_l$r2_climate),las=2,pch=8,cex=2,
           col=color_to_traits(names(dat_plot)),ylab="r2_climate",xlab="coefficient of variation") 
    max_now=apply(X=rbind(colMeans(r2_l$r2_soilAclimate),colMeans(r2_l$r2_soil),colMeans(r2_l$r2_climate)), MARGIN=2, FUN=function(x) max(x, na.rm=T) )
    min_now=apply(X=rbind(colMeans(r2_l$r2_soilAclimate),colMeans(r2_l$r2_soil),colMeans(r2_l$r2_climate)), MARGIN=2, FUN=function(x) min(x, na.rm=T) )
    segments(x0 = log10(dat_plot), y0 = max_now,
             x1 = log10(dat_plot), y1 = min_now,col=color_to_traits(names(dat_plot)))
    text(log10(dat_plot)+.05,(min_now-.05),las=2,pch=16,labels = names(dat_plot),
         col=color_to_traits(names(dat_plot))) 
    legend(1.3,.8, c("Climate and soil", "Climate", "Soil"),  pch = c(16,8,6),pt.cex = 2)
    
  dev.off()
  
  #--------------------------------------------------------------------------------
  
    ord.ix <- match(target_order1,trait_names)
    boxplot(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")][,ord.ix],las=2,
            col=color_to_traits(trait_names)[ord.ix],xlab="",xaxt="n",main="Standard deviation per ecoregions & (gap-filled) trait (log_scaled)",ylim=c(0,2))
    abline(mean(colMeans(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")][,put_into_traitGroup(trait_names)=="Eco"],na.rm = T)),0,col="gray",lwd=4.5)
    abline(mean(colMeans(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")][,put_into_traitGroup(trait_names)=="Eco"],na.rm = T)),0,col=color_to_traits("LeN"),lwd=4)
    abline(mean(colMeans(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")][,put_into_traitGroup(trait_names)=="Size"],na.rm = T)),0,col="gray",lwd=4.5)
    abline(mean(colMeans(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")][,put_into_traitGroup(trait_names)=="Size"],na.rm = T)),0,col=color_to_traits("LeArea"),lwd=4)
    boxplot(TRY_Env_gf[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")][,ord.ix],las=2,
            col=color_to_traits(trait_names)[ord.ix],add = T,xlab="",xaxt="n")
    axis(1,labels = trait_names[ord.ix],at = 1:17,las=2)
  

  ord.ix <- match(target_order1,trait_names)
  ix<- as.vector(colSums(!is.na(TRY_Env_obs[,grep(names(TRY_Env_obs),pattern = "sd.unique_spec.")]))!=0)
  dat_plot <- TRY_Env_obs[,grep(names(TRY_Env_obs),pattern = "sd.unique_spec.")]
  names(dat_plot) <- gsub(names(dat_plot),pattern = "sd.unique_spec.",replacement = "")
  names(dat_plot) <- gsub(names(dat_plot),pattern = "_obs",replacement = "")
  
  boxplot(dat_plot[,ord.ix],las=2,
          col=color_to_traits(names(dat_plot))[ord.ix],
          xlab="",xaxt="n",main="Standard deviation per ecoregions & (observed) trait (log_scaled)",ylim=c(0,2))
  
  abline(median(colMeans(dat_plot[,put_into_traitGroup(names(dat_plot))=="Eco"],na.rm=T),na.rm = T),0,col="gray",lwd=4.5)
  abline(median(colMeans(dat_plot[,put_into_traitGroup(names(dat_plot))=="Eco"],na.rm=T),na.rm = T),0,col=color_to_traits("LeN"),lwd=4)
  abline(median(colMeans(dat_plot[,put_into_traitGroup(names(dat_plot))=="Size"],na.rm=T),na.rm = T),0,col="gray",lwd=4.5)
  abline(median(colMeans(dat_plot[,put_into_traitGroup(names(dat_plot))=="Size"],na.rm=T),na.rm = T),0,col=color_to_traits("LeArea"),lwd=4)
  
  boxplot(dat_plot[,ord.ix],las=2,
          col=color_to_traits(names(dat_plot))[ord.ix],
          xlab="",xaxt="n",add=TRUE,ylim=c(0,2))
  axis(1,labels = trait_names[ord.ix],at = 1:length(ord.ix),las=2)


#-------------------------------------------------------------------------------------------------------------

  traits1 = TRY_Env_obs[,Rename_Vars(gsub(names(TRY_Env_obs),pattern = "_obs",replacement = ""))[,2]=="trait"]
  log10_traits=traits1
  log10_traits[!is.na(log10_traits)] = log10(traits1[!is.na(log10_traits)])
  
  scaled_median_traits <- apply(X=log10_traits, MARGIN=2, FUN=function(x) sd(x, na.rm=T) )
  dat_plot = (colMeans(TRY_Env_obs[,grep(names(TRY_Env_gf),pattern = "sd.unique_spec.")],na.rm = T)/
                scaled_median_traits)

  names(dat_plot) <- gsub(names(dat_plot),pattern = "sd.unique_spec.",replacement = "")
  names(dat_plot) <- gsub(names(dat_plot),pattern = "_obs",replacement = "")
  ord.ix <- order(put_into_traitGroup(names(dat_plot)))

  par(mfrow=c(1,1))
  plot(log10(dat_plot),colMeans(r2_l$r2_soilAclimate),las=2,pch=16,cex=2,
       col=color_to_traits(names(dat_plot)[!is.na(dat_plot)]),ylab="Variance explained",xlab="Coefficient of variation (observed data)",xlim=c(-.4,2),ylim=c(0,.9))  
  points(log10(dat_plot),colMeans(r2_l$r2_soil),las=2,pch=6,cex=2,
         col=color_to_traits(names(dat_plot)),ylab="r2_soil",xlab="coefficient of variation (observed data)") 
  points(log10(dat_plot),colMeans(r2_l$r2_climate),las=2,pch=8,cex=2,
         col=color_to_traits(names(dat_plot)),ylab="r2_climate",xlab="coefficient of variation") 
  max_now=apply(X=rbind(colMeans(r2_l$r2_soilAclimate),colMeans(r2_l$r2_soil),colMeans(r2_l$r2_climate)), MARGIN=2, FUN=function(x) max(x, na.rm=T) )
  min_now=apply(X=rbind(colMeans(r2_l$r2_soilAclimate),colMeans(r2_l$r2_soil),colMeans(r2_l$r2_climate)), MARGIN=2, FUN=function(x) min(x, na.rm=T) )
  segments(x0 = log10(dat_plot), y0 = max_now,
           x1 = log10(dat_plot), y1 = min_now,col=color_to_traits(names(dat_plot)))
  text(log10(dat_plot)+.05,(min_now-.05),las=2,pch=16,labels = names(dat_plot),
       col=color_to_traits(names(dat_plot))) 




}