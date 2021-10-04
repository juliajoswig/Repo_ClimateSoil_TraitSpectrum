

Rename_VarsXtoTRYname <- function(input){

  #input=names(TRY_obs_o)
  transl_s <-matrix(NA,ncol=2,nrow=length(input)) 
  transl_t <- transl_s # trait names

  head(trait_names_tab)
  trait_names_tab[,1] <- paste0("X",trait_names_tab[,1])
  
  transl_t[,1] <- input
  i=1
  for(i in 1:length(input)){
    t_now=input[i]
    if(sum(trait_names_tab[,1]==t_now)!=0){
   # traits 1/4 
    transl_t[which(transl_t[,1]%in%t_now),2] <-  trait_names_tab[trait_names_tab[,1]==t_now,2]
    }
  }
  
  
  # traits 1/4 
  transl_t[which(transl_t[,1]%in%"X11"),2] <- "Leaf area per leaf dry mass (specific leaf area, SLA)" 
  transl_t[which(transl_t[,1]%in%"X18"),2] <- "Plant height, vegetative" 
  transl_t[which(transl_t[,1]%in%"X784"),2] <- "unknownX784" 
  transl_t[which(transl_t[,1]%in%"X1"),2] <- "Leaf area" 
  
  
  return(transl_t)
}

Rename_VarsTRYnametoX <- function(input){
  
  #input=names(TRY_obs_o)
  transl_s <-matrix(NA,ncol=2,nrow=length(input)) 
  transl_t <- transl_s # trait names
  
  trait_names_tab <- read.table(file.path(origin,"data","helper_files","TRY","Trait_names","tde202161411136.txt"),sep="\t",header = TRUE)
  head(trait_names_tab)
  
  transl_t[,1] <- input
  i=1
  for(i in 1:length(input)){
    t_now=input[i]
    if(sum(trait_names_tab[,2]==t_now)!=0){
      # traits 1/4 
      transl_t[which(transl_t[,1]%in%t_now),2] <-  trait_names_tab[trait_names_tab[,2]==t_now,1]
    }
  }
  
  transl_t[which(transl_t[,1]%in%"Leaf area per leaf dry mass (specific leaf area, SLA)"),2] <- "X11"
  transl_t[which(transl_t[,1]%in%"Plant height, vegetative"),2] <- "X18"
  transl_t[which(transl_t[,1]%in%"unknownX784"),2]   <- "X784"
  transl_t[which(transl_t[,1]%in%"leaf area"),2]   <- "X1"#changed Julia from X2 20210922
  transl_t[which(transl_t[,1]%in%"Leaf area"),2]   <- "X1"#changed Julia from X2 20210922
  transl_t[which(transl_t[,1]%in%"Plant height, reproductive"),2] <- "X3107"
  transl_t[which(transl_t[,1]%in%"Stem dry mass per stem fresh volume (stem specific density, SSD)"),2]   <- "X4"
  transl_t[which(transl_t[,1]%in%"Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)"),2]   <- "X47"
  transl_t[which(transl_t[,1]%in%"Rooting depth"),2]   <- "X6"
  transl_t[which(transl_t[,1]%in%"Leaf dry mass"),2]   <- "X55"
  transl_t[which(transl_t[,1]%in%"Leaf carbon (C) content per leaf dry mass"),2]   <- "X13" #added Julia from 20210922
  transl_t[which(transl_t[,1]%in%"Leaf nitrogen (N) content per leaf dry mass"),2]   <- "X14" #added Julia from 20210922
  transl_t[which(transl_t[,1]%in%"Seed dry mass"),2]   <- "X26" #added Julia from 20210922
  transl_t[which(transl_t[,1]%in%"Leaf fresh mass"),2]   <- "X163" #added Julia from 20210922
  
  return(transl_t)
}



Rename_VarsXtoShort <- function(input){
  
  #input=names(TRY_obs_o)
  transl_s <-matrix(NA,ncol=2,nrow=length(input)) 
  transl_t <- transl_s # trait names
  
  trait_names_tab <- read.table(file.path(origin,"data","helper_files","TRY","Trait_names","tde202161411136.txt"),sep="\t",header = TRUE)
  head(trait_names_tab)
  trait_names_tab[,1] <- paste0("X",trait_names_tab[,1])
  
  transl_t[,1] <- input
  
  # traits 1/4 
  transl_t[which(transl_t[,1]%in%"X1"),2] <- "LeArea"# not confired 
  transl_t[which(transl_t[,1]%in%"X4"),2] <- "SSD" 
  transl_t[which(transl_t[,1]%in%"X11"),2] <- "SLA" 
  transl_t[which(transl_t[,1]%in%"X13"),2] <- "LeC"
  transl_t[which(transl_t[,1]%in%"X14"),2] <- "LeN" 
  transl_t[which(transl_t[,1]%in%"X15"),2] <- "LeP" 
  transl_t[which(transl_t[,1]%in%"X18"),2] <- "PlantHeight" 
  transl_t[which(transl_t[,1]%in%"X26"),2] <- "SeedMass" 
  transl_t[which(transl_t[,1]%in%"X27"),2] <- "SeLen" 
  transl_t[which(transl_t[,1]%in%"X50"),2] <- "LeNArea" 
  transl_t[which(transl_t[,1]%in%"X56"),2] <- "LeNP" 
  transl_t[which(transl_t[,1]%in%"X78"),2] <- "Led15N" 
  transl_t[which(transl_t[,1]%in%"X138"),2] <- "SenbU" 
  transl_t[which(transl_t[,1]%in%"X163"),2] <- "LeFMass" 
  transl_t[which(transl_t[,1]%in%"X169"),2] <- "ConduitDens" 
  transl_t[which(transl_t[,1]%in%"X237"),2] <- "DispULen" 
  transl_t[which(transl_t[,1]%in%"X282"),2] <- "VesLen" 
  
  
  
  return(transl_t)
}


Rename_VarsTRYnametoShort <- function(input){
  
  #input=names(TRY_obs_o)
  transl_s <-matrix(NA,ncol=2,nrow=length(input)) 
  transl_t <- transl_s # trait names
  
  trait_names_tab <- read.table(file.path(origin,"data","helper_files","TRY","Trait_names","tde202161411136.txt"),sep="\t",header = TRUE)
  head(trait_names_tab)
  trait_names_tab[,1] <- paste0("X",trait_names_tab[,1])
  
  transl_t[,1] <- input
  
  # traits 1/4 
  transl_t[which(transl_t[,1]%in%"Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)"),2] <- "SSD" 
  transl_t[which(transl_t[,1]%in%"Leaf area per leaf dry mass (specific leaf area, SLA)"),2] <- "SLA" 
  transl_t[which(transl_t[,1]%in%"Leaf carbon (C) content per leaf dry mass"),2] <- "LeC"
  transl_t[which(transl_t[,1]%in%"Leaf nitrogen (N) content per leaf dry mass"),2] <- "LeN" 
  transl_t[which(transl_t[,1]%in%"Leaf phosphorus (P) content per leaf dry mass"),2] <- "LeP" 
  transl_t[which(transl_t[,1]%in%"Plant height, vegetative"),2] <- "PlantHeight" 
  transl_t[which(transl_t[,1]%in%"Seed dry mass"),2] <- "Seed dry mass" 
  transl_t[which(transl_t[,1]%in%"Seed length"),2] <- "SeLen" 
  transl_t[which(transl_t[,1]%in%"Leaf nitrogen (N) content per leaf area"),2] <- "LeNArea" 
  transl_t[which(transl_t[,1]%in%"Leaf nitrogen/phosphorus (N/P) ratio"),2] <- "LeNP" 
  transl_t[which(transl_t[,1]%in%"Leaf nitrogen (N) isotope signature (delta 15N)"),2] <- "Led15N" 
  transl_t[which(transl_t[,1]%in%"Seed number per reproducton unit"),2] <- "SenbU" 
  transl_t[which(transl_t[,1]%in%"Leaf fresh mass"),2] <- "LeFMass" 
  transl_t[which(transl_t[,1]%in%"Stem conduit density (vessels and tracheids)"),2] <- "ConduitDens" 
  transl_t[which(transl_t[,1]%in%"Dispersal unit length"),2] <- "DispULen" 
  transl_t[which(transl_t[,1]%in%"Wood vessel element length; stem conduit (vessel and tracheids) element length"),2] <- "VesLen" 
  transl_t[which(transl_t[,1]%in%"Plant height, reproductive"),2] <- "PlantHeightR" 
  transl_t[which(transl_t[,1]%in%"Leaf area"),2] <- "LeArea" 
  
  return(transl_t)
}


#input="SSD"
Rename_VarsShorttoX <- function(input){
  
  transl_s <-matrix(NA,ncol=2,nrow=length(input)) 
  transl_t <- transl_s # trait names
  transl_t[,1] <- input
  
  transl_t[which(transl_t[,1]%in%"LeArea"),2] <-    "X1"
  transl_t[which(transl_t[,1]%in%"SSD"),2] <-      "X4"
  transl_t[which(transl_t[,1]%in%"SLA"),2] <-       "X11"
  transl_t[which(transl_t[,1]%in%"LeC"),2] <-       "X13"
  transl_t[which(transl_t[,1]%in%"LeN"),2] <-       "X14"
  transl_t[which(transl_t[,1]%in%"LeP"),2] <-       "X15"
  transl_t[which(transl_t[,1]%in%"PlantHeight"),2] <-"X18"
  transl_t[which(transl_t[,1]%in%"SeedMass"),2] <-   "X26"
  transl_t[which(transl_t[,1]%in%"SeLen"),2] <-      "X27"
  transl_t[which(transl_t[,1]%in%"LeNArea"),2] <-    "X50"
  transl_t[which(transl_t[,1]%in%"LeNP"),2] <-       "X56"
  transl_t[which(transl_t[,1]%in%"Led15N"),2] <-     "X78"
  transl_t[which(transl_t[,1]%in%"SenbU"),2] <-      "X138"
  transl_t[which(transl_t[,1]%in%"LeFMass"),2] <-    "X163"
  transl_t[which(transl_t[,1]%in%"ConduitDens"),2] <-"X169"
  transl_t[which(transl_t[,1]%in%"DispULen"),2] <-   "X237"
  transl_t[which(transl_t[,1]%in%"VesLen"),2] <-     "X282"
  
  return(transl_t)
}
