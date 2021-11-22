

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

# Please select
output_term="woody"
#output_term="non_woody"

if (!require("gplots")) {
    install.packages("gplots", dependencies = TRUE)
  }
  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer", dependencies = TRUE)
  }
  library(gplots)
  library(RColorBrewer)
  
  trait_names=c("LeArea","SSD","SLA","LeC","LeN","LeP","PlantHeight", "SeedMass","SeLen","LeNArea","LeNP",
                "Led15N","SenbU","LeFMass","ConduitDens" ,"DispULen","VesLen"   )
  # read data
  list.files(file.path(origin,"data","master_matrix"))
  X1_tot <- as.data.frame(read.csv(file.path(origin,"data","master_matrix",paste0("X1_",output_term,".csv"))))
  X1_gf <- X1_tot[,grep(colnames(X1_tot),pattern = "_gf")]
  colnames(X1_gf) <- gsub(colnames(X1_gf),pattern = "_gf",replacement = "")
  head(X1_gf)
  X1 <- X1_gf[,colnames(X1_gf)%in%trait_names]
  dat_cor <- cor(log(X1))
  dat_cor_m <- as.matrix(dat_cor)

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

  #rename
  nms <- colnames(dat_cor_p)
  colnames(dat_cor_p) <- Rename_Vars(colnames(dat_cor_p))[,3]
  rownames(dat_cor_p) <- Rename_Vars(rownames(dat_cor_p))[,3]

# create a folder for figure
  if(!dir.exists(file.path(origin,"figures","Supplement_fig_8"))){
    dir.create(file.path(origin,"figures","Supplement_fig_8"))}
  
# plot.  
  pdf(file=file.path(origin,"figures","Supplement_fig_8",paste0("Supplement_fig_8",output_term,".pdf")),height=8,width=9)
  
  heatmap.2(dat_cor_p,
            cexRow = 2,
            cexCol = 2,
            cellnote = dat_cor_p,  # same data set for cell labels
            notecol="black",      # change font color of cell labels to black
            #ColSideColors = color_to_traits(colnames(dat_cor_p)),
            RowSideColors = color_to_traits(nms),
            #            Colv = c(3:7,1,4:17),#c(9,16,8,7,14,1,15,2,4,11,6,10,3,5,17,12,13),
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,11.5),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
            breaks=col_breaks,    # enable color transition at specified limits
            dendrogram="row"     # only draw a row dendrogram
  )            
  dev.off()

  


