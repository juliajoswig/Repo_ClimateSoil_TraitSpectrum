

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
if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}


target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")

# please select:
output_term="woody"
#output_term="non_woody"


  # read data, produced earlier.
# load data of species per ecoregion aggregation level
if(output_term=="woody"){TRY_Env1 <- read.csv(file.path(origin,"data","master_matrix","X1_woody.csv"))}
if(output_term=="non_woody"){TRY_Env1 <- read.csv(file.path(origin,"data","master_matrix","X1_non_woody.csv"))}

# select data for trait-trait relationships: traits that are imputed.
  X1_trait <- TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_pred",replacement = ""))[,2]=="trait"]
  colnames(X1_trait) <- gsub(colnames(X1_trait),pattern = "_pred",replacement = "")
  dim(X1_trait)
  dat_cor <- cor(X1_trait)
# dat_cor <- read.csv(file=file.path(origin_new,"data","helper_files","fig_S1",output_term,"trait_correlations.csv"))
#  rownames(dat_cor) <- dat_cor[,1]
#  dat_cor <- dat_cor[,-which(colnames(dat_cor)=="X")]
#  colnames(dat_cor) <- rownames(dat_cor)
  dat_cor_m <- as.matrix(dat_cor)
#  for(i in 1:ncol(dat_cor)){dat_cor_m[,i] <- as.numeric(dat_cor_m[,i])}
  
  # creates an own color palette from red to green
  my_palette <- colorRampPalette(c("white","#edf8fb","#b3cde3","#8c96c6","#88419d"))(n = 499)# Plum
  my_palette <- colorRampPalette(c("white","#fef0d9","#fdcc8a","#fc8d59","#d7301f"))(n = 499)# Terra Cotta
  my_palette <- colorRampPalette(c("white","#f7fcb9","#addd8e","#31a354"))(n = 499)# summer green
  
  col_breaks = c(seq(0,0.1,length=100),  # for red
                 seq(.11,.2,length=100),
                 seq(.21,+.35,length=100),
                 seq(0.36,0.5,length=100), # for yellow
                 seq(0.51,1,length=100))     # for blue
  

  dat_cor_p <- round(abs(dat_cor_m),digits=2)
  dat_cor_p[dat_cor_p==1] <- NA
  
# create figure folder
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_7"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_7"))}
  
# plot.  
  pdf(file=file.path(origin,"figures","Supplement_Fig_7",paste0("figure_b",output_term,".pdf")),height=8,width=9)

  heatmap.2(dat_cor_p,
            cellnote = dat_cor_p,  # same data set for cell labels
            notecol="black",      # change font color of cell labels to black
            #            Colv = c(3:7,1,4:17),#c(9,16,8,7,14,1,15,2,4,11,6,10,3,5,17,12,13),
            RowSideColors = color_to_traits(colnames(dat_cor_p)), # links to original trait group
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,9),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
            breaks=col_breaks,    # enable color transition at specified limits
            dendrogram="row"     # only draw a row dendrogram
  )            
  dev.off()



  