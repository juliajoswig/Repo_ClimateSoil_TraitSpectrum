
plot_Figure_1a <- function(origin){
  
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
  load(file.path(origin,"data","master_matrix","X1.RData"))
  X1_gf <- TRY_Env1
  colnames(X1_gf)
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
  
  if(!dir.exists(file.path(origin,"figures","figure_1"))){
    dir.create(file.path(origin,"figures","figure_1"))}
  
  pdf(file=file.path(origin,"figures","figure_1","figure_1a.pdf"),height=8,width=9)

  heatmap.2(dat_cor_p,
            notecex=1.0,
            cexRow =1.9,# 0.2 + 1/log10(nrow(dat_cor_p)),#2,
            cexCol =1.9,# 0.2 + 1/log10(nrow(dat_cor_p)),#2,
            cellnote = dat_cor_p,  # same data set for cell labels
            notecol="black",      # change font color of cell labels to black
            #ColSideColors = color_to_traits(colnames(dat_cor_p)),
            RowSideColors = color_to_traits(nms),
            srtRow = NULL,
            #            Colv = c(3:7,1,4:17),#c(9,16,8,7,14,1,15,2,4,11,6,10,3,5,17,12,13),
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,11.5),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
            breaks=col_breaks,    # enable color transition at specified limits
            dendrogram="row",     # only draw a row dendrogram
            # color key + density info
            key = TRUE,
            keysize = 1.5
            
  )            
  dev.off()

  
  png(file=file.path(origin,"figures","figure_1","figure_1a.png"),res = 400,width = 1800*2,height = 1600*2)
  
  heatmap.2(dat_cor_p,
            notecex=1.0,
            cexRow =1.9,# 0.2 + 1/log10(nrow(dat_cor_p)),#2,
            cexCol =1.9,# 0.2 + 1/log10(nrow(dat_cor_p)),#2,
            cellnote = dat_cor_p,  # same data set for cell labels
            notecol="black",      # change font color of cell labels to black
            #ColSideColors = color_to_traits(colnames(dat_cor_p)),
            RowSideColors = color_to_traits(nms),
            srtRow = NULL,
            #            Colv = c(3:7,1,4:17),#c(9,16,8,7,14,1,15,2,4,11,6,10,3,5,17,12,13),
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,11.5),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
            breaks=col_breaks,    # enable color transition at specified limits
            dendrogram="row",     # only draw a row dendrogram
            # color key + density info
            key = TRUE,
            keysize = 1.5
            
  )            
  dev.off()
  
  
}


