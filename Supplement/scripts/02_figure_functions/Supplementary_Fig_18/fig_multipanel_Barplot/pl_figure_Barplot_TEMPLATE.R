
plot_figure_S4_barplot <- function(origin){
  

  trait_now=1

    trait_name_now = "Trait name"
    
    pdf(file = file.path(origin,"figures","figure_S3",paste0("Template_Barplot.pdf")),pointsize = 11,height=4,width=10)
    par(mfrow=c(1,3),mar=c(2.5,5,1.5,1))
    
    s=as.matrix(c(.2,.2,.25))
    s[s<0]=0
    c=as.matrix(c(.25,.2,.2))
    c[c<0]=0
    tot=as.matrix(c(.1,.6,.1))
    tot[tot<0]=0

    rownames(tot) <- c("Independent Effect Climate","Joint Climate and Soil Effect","Independent Effect Soil")
    rownames(c) <- c("Independent Effect Climate Water","Joint Climate Water and Energy","Independent Effect Climate Energy")
    rownames(s) <- c("Independent Effect Soil Chemistry","Joint Soil Chemistry and Physics","Independent Effect Soil Physics") 
    
    barplot(as.matrix(tot[3:1]),
            las=3,ylim=c(0,1),ylab="Variance explained",
            col=c(soil_S4,"lightgray",climate_S4),cex.lab=2,cex.axis=2)
    axis(1,labels = "Variance Explained by Climate and Soil",at = .7,tick = F,cex.axis=1.5)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(tot)), col = c(climate_S4,"lightgray",soil_S4),cex=1.5,box.col = "lightgray",
                       text.col = "black",  pch = c(15, 15, 15),bg = "white")
           
    barplot(as.matrix(c[3:1]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",main= "",
            col=c(climtype2_S4,"lightgray",climtype1_S4))
    axis(1,labels = "Variance Explained by Climate",at = .7,tick = F,cex.axis=1.5)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(c)), col =c(climtype1_S4,"lightgray",climtype2_S4),cex=1.5,box.col = "lightgray",
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
    barplot(as.matrix(s[3:1]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",
            col=c(soiltype2_S4,"lightgray",soiltype1_S4))
    axis(1,labels = "Variance Explained by Soil",at = .7,tick = F,cex.axis=1.5)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(s)), col =c(soiltype1_S4,"lightgray",soiltype2_S4),cex=1.5,box.col = "lightgray",
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
     dev.off()
}