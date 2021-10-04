Tab_S2 <- function(origin){
  
  
  ### TERMINAL:
  setwd("/Net/Groups")
  ### LOCAL
  orig_loctem =  "BGI"#"/Volumes/BGI" 
  #orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  output_term="_sel20_"#_minmaxmed
  spec_count=20 # if output_term == "_sel20_"
  NA_now="NA_mnNbr"#"NA_stays_NA"
  
  # ---------------------------------------------------------------------------------------
  # define the origin path
  origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
  
  load(file.path(origin, "data","Results","trait_correlations","dat_cor.RData"))
  load(file.path(origin,"data","helper_files","PCA_Fig1",paste0("PCA_agg1.RData")))
  
  # ---------------------------------------------------------------------------------------
  # correl plot
  
  my_palette <- colorRampPalette(c("white","#8ab6e8","#5884b4","#cd6787","#f8a0bb"))(n = 499)#"#e6cf6c",
  col_breaks = c(seq(0,0.1,length=100),  # for red
                 seq(.11,.2,length=100),
                 seq(.21,+.35,length=100),
                 seq(0.36,0.5,length=100), # for yellow
                 seq(0.51,1,length=100))     # for blue
  dat_cor_p <- round(abs(dat_cor),digits=2)
  dat_cor_p[dat_cor_p==1] <- NA
  
  ix=order(put_into_traitGroup(colnames(dat_cor_p)))
  dat_cor_ordered <- dat_cor_p[ix,ix]
  
#  colnames(dat_cor_ordered)<- Rename_Vars(colnames(dat_cor_p))[,3]
#  rownames(dat_cor_ordered)<- Rename_Vars(rownames(dat_cor_p))[,3]
  dat_cor_final <- dat_cor_ordered
  dat_cor_final <- cbind(rep(0,nrow(dat_cor_ordered)),
                         dat_cor_ordered[,1:((which(put_into_traitGroup(colnames(dat_cor_ordered))=="Other")[1])-1)],
                         rep(0,nrow(dat_cor_ordered)),dat_cor_ordered[,((which(put_into_traitGroup(colnames(dat_cor_ordered))=="Other")[1])):((which(put_into_traitGroup(colnames(dat_cor_ordered))=="Size")[1])-1)],
                         rep(0,nrow(dat_cor_ordered)),rep(0,nrow(dat_cor_ordered)),rep(0,nrow(dat_cor_ordered)), rep(0,nrow(dat_cor_ordered)), rep(0,nrow(dat_cor_ordered)),
                         dat_cor_ordered[,((which(put_into_traitGroup(colnames(dat_cor_ordered))=="Size")[1])):ncol(dat_cor_ordered)])
  i=18
  j=11 # row
  if (!require("RColorBrewer")) {install.packages("RColorBrewer", dependencies = TRUE);library(RColorBrewer)}
  dat_cor_final[is.na(dat_cor_final)] = 1
  colnames(dat_cor_final)
  for(j in 1:nrow(dat_cor_final)){
  pdf(file=file.path(origin,"plots","Fig_S4x",paste0("Correls_",rownames(dat_cor_final)[j],".pdf")),width=22,height = 8)
  par(mfrow=c(3,8),mar=c(0,0,0,0))
#  par(mar=c(0,0,0,0))
  for(i in 1:ncol(dat_cor_final)){
    print(which(dat_cor_final[j,i]<=col_breaks)[1])
    if(i==1){
      plot(1,col="white",frame=F,xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=5,col.lab=color_to_traits("LeN"))
      text(1,1,cex=6,labels="Eco",col=color_to_traits("LeN"),srt=90)
    }
    if(i==9){
      plot(1,col="white",frame=F,xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=5,col.lab=color_to_traits("VesLen"))
      text(1,1,cex=6,labels="Other",col=color_to_traits("VesLen"),srt=90)
    }
    if(i==18){
      plot(1,col="white",frame=F,xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=5,col.lab=color_to_traits("LeArea"))
      text(1,1,cex=6,labels="Size",col=color_to_traits("LeArea"),srt=90)
    }
    if(i!=1&i!=9&i!=15){plot(1,col="white",frame=F,xlab="",ylab="",xaxt="n",yaxt="n")}

  #  title(Rename_Vars(colnames(dat_cor_final))[i,3], cex.main=2,cex=2,col.main=color_to_traits(colnames(dat_cor_final)[i]))
    if(colnames(dat_cor_final)[i]!=rownames(dat_cor_final)[j]&i!=1&i!=9&i!=17){
      points(1,1,pch=16,cex=25,col=my_palette[which(dat_cor_final[j,i]<=col_breaks)[1]])
      text(1,1,cex=3.2,col="white",dat_cor_final[j,i])
      text(1,1.3,cex=2.5,labels=Rename_Vars(colnames(dat_cor_final)[i])[,3],col=color_to_traits(colnames(dat_cor_final)[i]))
    }
    if(colnames(dat_cor_final)[i]==rownames(dat_cor_final)[j]){
      points(1,1,pch=15,cex=20,col="gray")
      text(1,1,cex=3.2,col="white",dat_cor_final[j,i])
      text(1,1.3,cex=2.5,labels=Rename_Vars(colnames(dat_cor_final))[i,3],col=color_to_traits(colnames(dat_cor_final)[i]))
    }
         
  }
  dev.off()
  }

  ncol(soil_2)+ncol(climate_2)
nms<-   c(
          "ORCDRC_M_sl1_1km_ll.tif",
          "ORCDRC_M_s2_1km_ll.tif", 
          "ORCDRC_M_sl3_1km_ll.tif", 
          "ORCDRC_M_sl4_1km_ll.tif", 
          "ORCDRC_M_sl5_1km_ll.tif", 
          "ORCDRC_M_sl6_1km_ll.tif", 
          "ORCDRC_M_sl7_1km_ll.tif", 
          "OCDENS_M_sl1_1km_ll.tif",
          "OCDENS_M_sl2_1km_ll.tif",
          "OCDENS_M_sl3_1km_ll.tif",
          "OCDENS_M_sl4_1km_ll.tif",
          "OCDENS_M_sl5_1km_ll.tif",
          "OCDENS_M_sl6_1km_ll.tif",
          "OCDENS_M_sl7_1km_ll.tif",
          "CECSOL_M_sl1_1km_ll.tif",
          "CECSOL_M_sl2_1km_ll.tif",
          "CECSOL_M_sl3_1km_ll.tif",
          "CECSOL_M_sl4_1km_ll.tif",
          "CECSOL_M_sl5_1km_ll.tif",
          "CECSOL_M_sl6_1km_ll.tif",
          "CECSOL_M_sl7_1km_ll.tif",
          "CLYPPT_M_sl1_1km_ll.tif",
          "CLYPPT_M_sl2_1km_ll.tif",
          "CLYPPT_M_sl3_1km_ll.tif",
          "CLYPPT_M_sl4_1km_ll.tif",
          "CLYPPT_M_sl5_1km_ll.tif",
          "CLYPPT_M_sl6_1km_ll.tif",
          "CLYPPT_M_sl7_1km_ll.tif",
          "OCSTHA_M_sd1_1km_ll.tif",
          "OCSTHA_M_sd2_1km_ll.tif",
          "OCSTHA_M_sd3_1km_ll.tif",
          "OCSTHA_M_sd4_1km_ll.tif",
          "OCSTHA_M_sd5_1km_ll.tif",
          "OCSTHA_M_sd6_1km_ll.tif",
          "OCSTHA_M_30cm_1km_ll.tif",
          "OCSTHA_M_100cm_1km_ll.tif",
          "OCSTHA_M_200cm_1km_ll.tif",
          "BLDFIE_M_sl1_1km_ll.tif",
          "BLDFIE_M_sl2_1km_ll.tif",
          "BLDFIE_M_sl3_1km_ll.tif",
          "BLDFIE_M_sl4_1km_ll.tif",
          "BLDFIE_M_sl5_1km_ll.tif",
          "BLDFIE_M_sl6_1km_ll.tif",
          "BLDFIE_M_sl7_1km_ll.tif",
          "CRFVOL_M_sl1_1km_ll.tif",
          "CRFVOL_M_sl2_1km_ll.tif",
          "CRFVOL_M_sl3_1km_ll.tif",
          "CRFVOL_M_sl4_1km_ll.tif",
          "CRFVOL_M_sl5_1km_ll.tif",
          "CRFVOL_M_sl6_1km_ll.tif",
          "CRFVOL_M_sl7_1km_ll.tif",
          "PHIKCL_M_sl1_1km_ll.tif",
          "PHIKCL_M_sl2_1km_ll.tif",
          "PHIKCL_M_sl3_1km_ll.tif",
          "PHIKCL_M_sl4_1km_ll.tif",
          "PHIKCL_M_sl5_1km_ll.tif",
          "PHIKCL_M_sl6_1km_ll.tif",
          "PHIKCL_M_sl7_1km_ll.tif",
          "SNDPPT_M_sl1_1km_ll.tif",
          "SNDPPT_M_sl2_1km_ll.tif",
          "SNDPPT_M_sl3_1km_ll.tif",
          "SNDPPT_M_sl4_1km_ll.tif",
          "SNDPPT_M_sl5_1km_ll.tif",
          "SNDPPT_M_sl6_1km _ll.tif",
          "SNDPPT_M_sl7_1km _ll.tif",
          "SLTPPT_M_sl1_1km_ll.tif",
          "SLTPPT_M_sl2_1km_ll.tif",
          "SLTPPT_M_sl3_1km_ll.tif",
          "SLTPPT_M_sl4_1km_ll.tif",
          "SLTPPT_M_sl5_1km_ll.tif",
          "SLTPPT_M_sl6_1km_ll.tif",
          "SLTPPT_M_sl7_1km_ll.tif",
          "AWCh1_M_sl1_1km_ll.tif",
          "AWCh1_M_sl2_1km_ll.tif",
          "AWCh1_M_sl3_1km_ll.tif",
          "AWCh1_M_sl4_1km_ll.tif",
          "AWCh1_M_sl5_1km_ll.tif",
          "AWCh1_M_sl6_1km_ll.tif",
          "AWCh1_M_sl7_1km_ll.tif",
          "AWCh2_M_sl1_1km_ll.tif",
          "AWCh2_M_sl2_1km_ll.tif",
          "AWCh2_M_sl3_1km_ll.tif", 
          "AWCh2_M_sl4_1km_ll.tif", 
          "AWCh2_M_sl5_1km_ll.tif", 
          "AWCh2_M_sl6_1km_ll.tif", 
          "AWCh2_M_sl7_1km_ll.tif", 
          "AWCh3_M_sl1_1km_ll.tif", 
          "AWCh3_M_sl2_1km_ll.tif", 
          "AWCh3_M_sl3_1km_ll.tif", 
          "AWCh3_M_sl4_1km_ll.tif", 
          "AWCh3_M_sl5_1km_ll.tif", 
          "AWCh3_M_sl6_1km_ll.tif", 
          "AWCh3_M_sl7_1km_ll.tif", 
          "AWCtS_M_sl1_1km_ll.tif",
          "AWCtS_M_sl2_1km_ll.tif",
          "AWCtS_M_sl3_1km_ll.tif",
          "AWCtS_M_sl4_1km_ll.tif",
          "AWCtS_M_sl5_1km_ll.tif",
          "AWCtS_M_sl6_1km_ll.tif",
          "AWCtS_M_sl7_1km_ll.tif",
          "WWP_M_sl1_1km_ll.tif",
          "WWP_M_sl2_1km_ll.tif",
          "WWP_M_sl3_1km_ll.tif",
          "WWP_M_sl4_1km_ll.tif",
          "WWP_M_sl5_1km_ll.tif",
          "WWP_M_sl6_1km_ll.tif",
          "WWP_M_sl7_1km_ll.tif")

which(duplicated(nms))
length(nms)
ncol(soil_2)
nms[!nms%in%names(soil_2)]
names(soil_2)[!names(soil_2)%in%nms]
  
  sum(names(soil_2)%in%c("WWP_M_sl1_1km_ll.tif","))
dim(soil_2)
  res <- matrix(NA,ncol=5,nrow=17)
  colnames(res) <- c("Trait name","")
  res[,1] <- Rename_Vars(trait_names2)[,3]
  for(i in 1:17){
  res[i,5] <- sum(info_2[[paste0("observation.count.",trait_names2[i])]])
  }
  res <- res[order(res[,2]),]
  require("xlsx")
  write.xlsx(res, file=file.path(origin,"data","Results","tables","Tab_S2","Table_S2.xls"), sheetName="Sheet1", 
             col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
  
}
