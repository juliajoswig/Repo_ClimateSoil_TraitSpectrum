
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

  doPCA=FALSE
  nruns=50
  type_analysis="lm"
  output_term=""
  climOsoil="soilAclimate"
  
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
  load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                      paste0("Res_",output_term,pca_term,".RData")))
  library("gplots")
  colz <- Rename_Vars(colnames(out$r2_vars))[,2]
  colz[colz=="soil"]=soil_col
  colz[colz=="climate"]=climate_col
  colnames(out$r2_vars) <- Rename_Vars(colnames(out$r2_vars))[,3]
  
  nbrs=round(t(out$r2_vars)*100,digits = 0)
  nbrs[nbrs==0]=""
  my_palette <- colorRampPalette(c( "white","turquoise"))(n = 199)
  col_breaks = c(seq(0,50,length=100), 
                 seq(51,100,length=100))             
  
    pdf(file=file.path(origin,"figures","Supplement_Fig_16",paste0("Figure_S_TraitsEnv_lm",output_term,".pdf")),
          width=15,height=35)
      heatmap.2(t(out$r2_vars)*100,
                cellnote = nbrs,  # same data set for cell labels
                ColSideColors  = color_to_traits(rownames(out$r2_vars)),
                RowSideColors  = colz,
                notecol="black",      # change font color of cell labels to black
                density.info="none",  # turns off density plot inside color legend
                trace="none",         # turns off trace lines inside the heat map
                margins =c(18,20),     # widens margins around plot
                cexRow = 2,
                cexCol =  2,
                notecex = 1.5,
                lhei=c(1,6,1),
              #  lhei=c(1,6,1),
                col=my_palette,       # use on color palette defined earlier
                breaks=col_breaks,    # enable color transition at specified limits
                dendrogram="col",     # only draw a row dendrogram
                Rowv="NA"            # turn off column clustering
      )
    dev.off()
  


