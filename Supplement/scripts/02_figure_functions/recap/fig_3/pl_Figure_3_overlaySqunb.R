plot_overlayNB <- function(edg,axis_cex,climate_col,soil_col){
  
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
            rgb(x[1], x[2], x[3], alpha=alpha))  
  }  
  
  soil_col2= add.alpha(soil_col,.02)#.02#"#ffff96" #"#ffff96" #"#cd6787"#"#cd6787"
  climate_col2 =climate_col# add.alpha(climate_col,.2)#0.01# "#00a488" #"#5884b4"

  
    plot(x=c(0,1),y=c(1,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1,#c(min(atm[!is.na(soil)])*1.1,max(atm[!is.na(soil)])*1.1),
       xlim=c(0,edg),axes=FALSE,ylab="",xlab="",cex.axis=3)
  axis(1,cex=2,cex.axis=axis_cex)
  axis(2,cex=2,cex.axis=axis_cex)
  xpd=NA
  n=100
  seqs=seq(min(0),max(1),length.out = n)
   for(i in seqs){
    y <- c(0,0,0+i,1,1)
    x <- c(1,0+i,0+i,0+i,1)
    #polygon(x, y, col = add.alpha(as.vector(soil_col2),1/(n*2)) , lty = 1, lwd = 2, border = NA)
    polygon(x, y, col = add.alpha(as.vector(soil_col2),1/(n*2)) , lty = 1, lwd = 2, border = NA)
    #    polygon(x, y, col = soil_col2 , lty = 1, lwd = 2, border = NA)
   }

  for(i in seqs){
    x2 <- c(0,0,0+i,1,1)
    y2 <- c(1,0+i,0+i,0+i,1)
    #polygon(x2, y2, xpd = xpd, col = add.alpha(as.vector(climate_col2),1/(n*2)) , lty = 1, lwd = 2, border = NA)
    polygon(x2, y2, xpd = xpd, col = add.alpha(as.vector(climate_col2),1/(n*2)) , lty = 1, lwd = 2, border = NA)
    #    polygon(x2, y2, xpd = xpd, col = climate_col2 , lty = 1, lwd = 2, border = NA)
  }
  
  y3 <- c(1, 0, 1.1)
  x3 <- c(0, 1, 1.1)
  polygon(x3, y3, xpd = xpd, col = "white" , lty = 1, lwd = 2, border = NA)
  

  abline(a = 0.2, b = 0, col = "white",lty=3,lwd=3)
  abline(a = 0.4, b = 0, col = "white",lty=3,lwd=3)
  abline(a = 0.6, b = 0, col = "white",lty=3,lwd=3)
  abline(a = 0.8, b = 0, col = "white",lty=3,lwd=3)

  abline(v = .2, col = "white",lty=3,lwd=3)
  abline(v = .4, col = "white",lty=3,lwd=3)
  abline(v = .6, col = "white",lty=3,lwd=3)
  abline(v = .8, col = "white",lty=3,lwd=3)
  
}