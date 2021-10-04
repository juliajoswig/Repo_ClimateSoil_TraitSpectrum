Tab_S5 <- function(origin,Agg_type,sel_now){

    atmObio="soilAatmosphere"
    meanOsd="median"
    nruns=100
    sel_now=""
    
        #load test
        load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),paste0(atmObio,"_trait_",meanOsd,"test_",Agg_type,sel_now,".RData")))
        relI <- test$df_final2
        
        #cut to non0
        relI_W <- relI[relI$rel_Importance!=0,]
        
        relI_W$EnvVar <- paste0(rename_what_can_be_renamed(relI_W$driver)," ",rename_what_can_be_renamed(relI_W$relationship))

        head(relI_W[,c(4,7,9,3,2,1)])
        relI_c <- relI_W[order(relI_W$r2, relI_W$rel_Importance,decreasing = T),c(4,7,9,3,2,1)]
        relI_c$rel_Importance <- round(as.numeric(as.vector(unlist(relI_c$rel_Importance))),digits = 3)
        relI_c$reg.coef <- round(as.numeric(as.vector(unlist(relI_c$reg.coef))),digits = 2)
        relI_c$r2 <- round(as.numeric(as.vector(unlist(relI_c$r2))),digits = 2)
        
        write.csv(relI_c,file=file.path(origin,"tabs","Tab_S5","Tab_S5.csv"))
      }
}


      


  