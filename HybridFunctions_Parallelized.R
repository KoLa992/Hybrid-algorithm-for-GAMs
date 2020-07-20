#----------------------------------------------------------Hybrid metaheuristic functions--------------------------
ModellEpit=function(egyed, X, Y, csalad, faktorok, konkurv_strict, magok){
  genszam=length(egyed)
  
  kvektor<-rep(10,sum(egyed))
  SpecStatus<-FALSE
  felsokorlat<-1
  
  alapnevek<-names(as.data.frame(X))
  nevek<-character(sum(egyed))
  szamlalo=1
  for(i in 1:genszam) {
    if(egyed[i]==1){
      nevek[szamlalo]<-alapnevek[i]
      szamlalo=szamlalo+1
    }
  }
  
  adatok=as.data.frame(matrix(nrow=nrow(X),ncol = sum(egyed)))
  szamlalo=1
  for(i in 1:genszam){
    if(egyed[i]==1){
      adatok[,szamlalo]=X[,i]
      szamlalo=szamlalo+1
    }
  }
  colnames(adatok)<-nevek
  adatok$target<-Y
  
  #library(parallel)
  #cl <- makeCluster(magok)
  
  while ((!SpecStatus)&(felsokorlat<=2)) {
    for (i in 1:length(kvektor)) {
      if (length(unique(adatok[,i]))<10) {
        kvektor[i]=length(unique(adatok[,i]))
      }
    }
    
    modelstring<-"target~"
    szamlalo=1
    for(i in 1:genszam) {
      if(egyed[i]==1){
        if (szamlalo==length(kvektor)) {
          if (alapnevek[i] %in% faktorok) {
            modelstring<-paste(modelstring, alapnevek[i], sep="")
          } else {
            modelstring<-paste(modelstring, "s(",alapnevek[i],",k=",kvektor[szamlalo],")", sep="")
          }
        } else {
          if (alapnevek[i] %in% faktorok) {
            modelstring<-paste(modelstring,alapnevek[i],"+", sep="")
          } else {
            modelstring<-paste(modelstring,"s(",alapnevek[i],",k=",kvektor[szamlalo],")+", sep="")
          }
        }
        szamlalo=szamlalo+1
      }
    }
    
    library(mgcv)
    gam.mod<-bam(as.formula(modelstring),family=csalad,data=adatok, method="fREML")
    ellen<-mgcv:::k.check(gam.mod)
    
    SpecStatus<-TRUE
    szamlalo<-1
    for (i in 1:length(kvektor)) {
      if (!(alapnevek[i]%in% faktorok)) {
        if (!is.null(ellen[szamlalo,1])&!is.null(ellen[szamlalo,2])&!is.null(ellen[szamlalo,4])) {
          tryCatch({
            if (((ellen[szamlalo,1]-ellen[szamlalo,2])<3)&(ellen[szamlalo,4]<0.05)) {
              SpecStatus<-FALSE
              kvektor[i]=kvektor[i]+5
            }
          },
          error=function(cond) {
            message(cond)
          },
          warning=function(cond) {
            message(cond)
          },
          finally={
            
          })
        }
        szamlalo=szamlalo+1
      }
    }
    felsokorlat=felsokorlat+1
  }
  
  szumma<-summary(gam.mod)
  
  akaike<-szumma$r.sq
  
  szignif<-TRUE
  if (length(szumma$p.pv)>0) {
    for(i in 1:length(szumma$p.pv)){
      if(szumma$p.pv[i]>0.05){szignif<-FALSE
      break}
    }
  }
  if (length(szumma$s.pv)>0) {
    for(i in 1:length(szumma$s.pv)){
      if(szumma$s.pv[i]>0.05){szignif<-FALSE
      break}
    }
  }
  
  vifre<-TRUE
  tryCatch({
    konkurv<-concurvity(gam.mod)[konkurv_strict,]
    for(i in 1:length(konkurv)){
      if(konkurv[i]>0.5) {vifre<-FALSE
      break}
    }
  }, warning = function(w) {
    
  }, error = function(e) {
    vifre<-FALSE
  }, finally = {
    
  })
  
  #stopCluster(cl)
  return(c(akaike,szignif, vifre))
}

Hibrid=function(genszam,pop_meret,maxlepes,mutacio, HMCR, vegmutacio, vegHMCR, konvergKrit, X, Y, csalad, faktorok, konkurv_strict, magok){
  
  library(foreach)
  library(parallel)
  library(doParallel)
  
  populacio<-matrix(nrow = pop_meret,ncol = 4)
  
  cl <- makeCluster(magok)
  registerDoParallel(cl)
  
  populacio <- foreach (i = 1:pop_meret, .combine='rbind', .export="ModellEpit") %dopar% {
    egyed.akt=rbinom(genszam,1,1/2)
    #print(egyed.akt)
    #flush.console()
    if(sum(egyed.akt)==0){
      c(toString(egyed.akt),-200000,0,0)
    } else{
      mod.out<-ModellEpit(egyed.akt,X,Y, csalad, faktorok, konkurv_strict, magok)
      c(toString(egyed.akt),mod.out[1],mod.out[2],mod.out[3])
    }
  }
  stopCluster(cl)
  
  populacio<-populacio[order(populacio[,2],decreasing=TRUE),]
  
  fitneszeddig=0
  konvergszamlalo=0
  HMCRszorzo=(vegHMCR/HMCR)^(1/(maxlepes-1))
  mutacioszoro=(vegmutacio/mutacio)^(1/(maxlepes-1))
  
  for(index in 1:maxlepes){
    
    legjobb<-populacio[1,]
    
    if(as.numeric(legjobb[2])==fitneszeddig){
      konvergszamlalo=konvergszamlalo+1
      if(konvergszamlalo==konvergKrit){
        break
      }
    } else{
      fitneszeddig=as.numeric(legjobb[2])
      konvergszamlalo=0
    }
    
    if(sum(as.numeric(populacio[,3]))==0){
      atlagfit<-mean(as.numeric(populacio[,2]))
    } else{
      atlagfit<-sum(as.numeric(populacio[,2])*as.numeric(populacio[,3])*as.numeric(populacio[,4]))/sum(as.numeric(populacio[,3])*as.numeric(populacio[,4]))
    }
    ujpop<-matrix(nrow = pop_meret,ncol = 4)
    
    cl <- makeCluster(magok)
    registerDoParallel(cl)
    ujpop <- foreach (j = 1:pop_meret, .combine='rbind') %dopar% {
      if(as.numeric(populacio[j,2])>atlagfit && as.numeric(populacio[j,3])==1 && as.numeric(populacio[j,4])==1){
        c(populacio[j,])
      } else{
        c(NA,NA,NA,NA)
      }
    }
    stopCluster(cl)
    
    ujpop <- ujpop[order(ujpop[,4],decreasing=TRUE),]
    
    startIndex=min(which(is.na(ujpop)))
    
    cl <- makeCluster(magok)
    registerDoParallel(cl)
    ujpop[startIndex:pop_meret,] <- foreach (j = startIndex:pop_meret, .combine='rbind', .export="ModellEpit") %dopar% {
      if(runif(1,0,1)<HMCR){
        valasztott=floor(runif(1, 1,startIndex))
        if(startIndex==1){
          ujegyed=legjobb[1]
        } else {
          ujegyed=populacio[valasztott,1]
        }
        ujegyed<-strsplit(ujegyed,",")[[1]]
        for(k in 1:genszam){
          if(runif(1,0,1)<mutacio){
            if(ujegyed[k]==0){ujegyed[k]<-1}else{ujegyed[k]<-0}
          }
        }
        ujegyed=toString(ujegyed)
      } else{
        ujegyed=toString(rbinom(genszam,1,1/2))
      }
      egyed.akt=as.numeric(strsplit(ujegyed,",")[[1]])
      #print(j)
      #print(egyed.akt)
      #flush.console()
      if(sum(egyed.akt)==0){
        c(toString(egyed.akt),-200000,0,0)
      } else{
        mod.out<-ModellEpit(egyed.akt,X,Y, csalad, faktorok, konkurv_strict, magok)
        c(toString(egyed.akt),mod.out[1],mod.out[2],mod.out[3])
      }
    }
    stopCluster(cl)
    
    populacio<-ujpop[order(ujpop[,4],ujpop[,3],ujpop[,2],decreasing=TRUE),]
    
    print(index)
    flush.console()
    
    HMCR=HMCR*HMCRszorzo
    mutacio=mutacio*mutacioszoro
  }
  return(c(legjobb, konvergszamlalo))
}

ModellEpit_B=function(egyed, X, Y, csalad, faktorok, magok){
  genszam=length(egyed)
  
  kvektor<-rep(10,sum(egyed))
  SpecStatus<-FALSE
  felsokorlat<-1
  
  alapnevek<-names(as.data.frame(X))
  nevek<-character(sum(egyed))
  szamlalo=1
  for(i in 1:genszam) {
    if(egyed[i]==1){
      nevek[szamlalo]<-alapnevek[i]
      szamlalo=szamlalo+1
    }
  }
  
  adatok=as.data.frame(matrix(nrow=nrow(X),ncol = sum(egyed)))
  szamlalo=1
  for(i in 1:genszam){
    if(egyed[i]==1){
      adatok[,szamlalo]=X[,i]
      szamlalo=szamlalo+1
    }
  }
  colnames(adatok)<-nevek
  adatok$target<-Y
  
  library(parallel)
  cl <- makeCluster(magok)
  
  while ((!SpecStatus)&(felsokorlat<=2)) {
    for (i in 1:length(kvektor)) {
      if (length(unique(adatok[,i]))<10) {
        kvektor[i]=length(unique(adatok[,i]))
      }
    }
    
    modelstring<-"target~"
    szamlalo=1
    for(i in 1:genszam) {
      if(egyed[i]==1){
        if (szamlalo==length(kvektor)) {
          if (alapnevek[i] %in% faktorok) {
            modelstring<-paste(modelstring, alapnevek[i], sep="")
          } else {
            modelstring<-paste(modelstring, "s(",alapnevek[i],",k=",kvektor[szamlalo],")", sep="")
          }
        } else {
          if (alapnevek[i] %in% faktorok) {
            modelstring<-paste(modelstring,alapnevek[i],"+", sep="")
          } else {
            modelstring<-paste(modelstring,"s(",alapnevek[i],",k=",kvektor[szamlalo],")+", sep="")
          }
        }
        szamlalo=szamlalo+1
      }
    }
    
    library(mgcv)
    gam.mod<-bam(as.formula(modelstring),family=csalad,data=adatok, method="fREML", cluster=cl)
    ellen<-mgcv:::k.check(gam.mod)
    
    SpecStatus<-TRUE
    szamlalo<-1
    for (i in 1:length(kvektor)) {
      if (!(alapnevek[i]%in% faktorok)) {
        if (!is.null(ellen[szamlalo,1])&!is.null(ellen[szamlalo,2])&!is.null(ellen[szamlalo,4])) {
          tryCatch({
            if (((ellen[szamlalo,1]-ellen[szamlalo,2])<3)&(ellen[szamlalo,4]<0.05)) {
              SpecStatus<-FALSE
              kvektor[i]=kvektor[i]+5
            }
          },
          error=function(cond) {
            message(cond)
          },
          warning=function(cond) {
            message(cond)
          },
          finally={
            
          })
        }
        szamlalo=szamlalo+1
      }
    }
    felsokorlat=felsokorlat+1
  }
  
  stopCluster(cl)
  return(gam.mod)
}