
plot_Figure_S_WoNb <- function(origin,output_term){
  
  if (!require("gplots")) {
    install.packages("gplots", dependencies = TRUE)
    library(gplots)
  }
  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer", dependencies = TRUE)
    library(RColorBrewer)
  }

  # read data
  dat_cor <- read.csv(file=file.path(origin_new,"data","helper_files","fig_S1",output_term,"trait_correlations.csv"))
  rownames(dat_cor) <- dat_cor[,1]
  dat_cor <- dat_cor[,-which(colnames(dat_cor)=="X")]
  colnames(dat_cor) <- rownames(dat_cor)
  dat_cor_m <- as.matrix(dat_cor)
  for(i in 1:ncol(dat_cor)){dat_cor_m[,i] <- as.numeric(dat_cor_m[,i])}
  
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

  pdf(file=file.path(origin,"figures","figure_S_WoN",paste0("figure_b",output_term,".pdf")),height=8,width=9)

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

  
}


