#----------------------------------------------------------Hybrid metaheuristic functions--------------------------
ModellEpit=function(egyed, X, Y){
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
      colnames(adatok)<-nevek
      szamlalo=szamlalo+1
    }
  }
  adatok$target<-Y
  
  while ((!SpecStatus)&(felsokorlat<=4)) {
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
          modelstring<-paste(modelstring, "s(",alapnevek[i],",k=",kvektor[szamlalo],")", sep="")
        } else {
          modelstring<-paste(modelstring,"s(",alapnevek[i],",k=",kvektor[szamlalo],")+", sep="")
        }
        szamlalo=szamlalo+1
      }
    }
    
    library(mgcv)
    gam.mod<-gam(as.formula(modelstring),family="gaussian",data=adatok, method="REML")
    ellen<-mgcv:::k.check(gam.mod)
    
    SpecStatus<-TRUE
    for (i in 1:length(kvektor)) {
      if (((ellen[i,1]-ellen[i,2])<3)&(ellen[i,4]<0.05)) {
        SpecStatus<-FALSE
        kvektor[i]=kvektor[i]+5
      }
    }
    felsokorlat=felsokorlat+1
  }
  
  szumma<-summary(gam.mod)
  
  akaike<-szumma$r.sq
  pvals<-szumma$s.pv[3]
  szignif<-TRUE
  for(i in 1:length(szumma$s.pv)){
    if(szumma$s.pv[i]>0.05){szignif<-FALSE
    break}
  }
  vifre<-TRUE
  tryCatch({
    konkurv<-concurvity(gam.mod)[1,]
    for(i in 2:length(konkurv)){
      if(konkurv[i]>0.5) {vifre<-FALSE
      break}
    }
  }, warning = function(w) {
    
  }, error = function(e) {
    vifre<-FALSE
  }, finally = {
    
  })
  
  return(c(akaike,szignif, vifre))
}

ModellEpit_Fix_k=function(egyed, X, Y, Default_k){
  genszam=length(egyed)
  
  kvektor<-rep(Default_k,sum(egyed))
  SpecStatus<-FALSE
  
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
      colnames(adatok)<-nevek
      szamlalo=szamlalo+1
    }
  }
  adatok$target<-Y
  
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
        modelstring<-paste(modelstring, "s(",alapnevek[i],",k=",kvektor[szamlalo],")", sep="")
      } else {
        modelstring<-paste(modelstring,"s(",alapnevek[i],",k=",kvektor[szamlalo],")+", sep="")
      }
      szamlalo=szamlalo+1
    }
  }
    
  library(mgcv)
  gam.mod<-gam(as.formula(modelstring),family="gaussian",data=adatok, method="REML")
  
  szumma<-summary(gam.mod)
  
  akaike<-szumma$r.sq
  pvals<-szumma$s.pv[3]
  szignif<-TRUE
  for(i in 1:length(szumma$s.pv)){
    if(szumma$s.pv[i]>0.05){szignif<-FALSE
    break}
  }
  vifre<-TRUE
  tryCatch({
    konkurv<-concurvity(gam.mod)[1,]
    for(i in 2:length(konkurv)){
      if(konkurv[i]>0.5) {vifre<-FALSE
      break}
    }
  }, warning = function(w) {
    
  }, error = function(e) {
    vifre<-FALSE
  }, finally = {
    
  })
  
  return(c(akaike,szignif, vifre))
}

Hibrid=function(genszam,pop_meret,maxlepes,mutacio, HMCR, vegmutacio, vegHMCR, X, Y, Default_k){
  
  populacio<-matrix(nrow = pop_meret,ncol = 4)
  
  for (i in 1:pop_meret){
    egyed.akt=rbinom(genszam,1,1/2)
    #print(egyed.akt)
    #flush.console()
    if(sum(egyed.akt)==0){
      populacio[i,1]=toString(egyed.akt)
      populacio[i,2]=-200000
      populacio[i,3]=0
      populacio[i,4]=0
    } else{
      mod.out<-ModellEpit_Fix_k(egyed.akt,X,Y,Default_k)
      populacio[i,1]=toString(egyed.akt)
      populacio[i,2]=mod.out[1]
      populacio[i,3]=mod.out[2]
      populacio[i,4]=mod.out[3]
    }
  }
  populacio<-populacio[order(populacio[,2],decreasing=TRUE),]
  
  fitneszeddig=0
  konvergszamlalo=0
  HMCRszorzo=(vegHMCR/HMCR)^(1/(maxlepes-1))
  mutacioszoro=(vegmutacio/mutacio)^(1/(maxlepes-1))
  
  for(index in 1:maxlepes){
    
    legjobb<-populacio[1,]
    
    if(as.numeric(legjobb[2])==fitneszeddig){
      konvergszamlalo=konvergszamlalo+1
      if(konvergszamlalo==5){
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
    szamlalo=1
    for(j in 1:pop_meret){
      if(as.numeric(populacio[j,2])>atlagfit && as.numeric(populacio[j,3])==1 && as.numeric(populacio[j,4])==1){
        ujpop[szamlalo,1]=populacio[j,1]
        ujpop[szamlalo,2]=populacio[j,2]
        ujpop[szamlalo,3]=populacio[j,3]
        ujpop[szamlalo,4]=populacio[j,4]
        szamlalo=szamlalo+1
      }
    }
    
    startIndex=szamlalo
    
    for(j in startIndex:pop_meret){
      if(runif(1,0,1)<HMCR){
        valasztott=floor(runif(1, 1,startIndex))
        if(startIndex==1){
          ujpop[j,1]=legjobb[1]
        } else {
          ujpop[j,1]=populacio[valasztott,1]
        }
        ujegyed<-strsplit(ujpop[j,1],",")[[1]]
        for(k in 1:genszam){
          if(runif(1,0,1)<mutacio){
            if(ujegyed[k]==0){ujegyed[k]<-1}else{ujegyed[k]<-0}
          }
        }
        ujpop[j,1]=toString(ujegyed)
      } else{
        ujpop[j,1]=toString(rbinom(genszam,1,1/2))
      }
      egyed.akt=as.numeric(strsplit(ujpop[j,1],",")[[1]])
      #print(j)
      #print(egyed.akt)
      #flush.console()
      if(sum(egyed.akt)==0){
        ujpop[j,2]=-200000
        ujpop[j,3]=0
        ujpop[j,4]=0
      } else{
        mod.out<-ModellEpit_Fix_k(egyed.akt,X,Y,Default_k)
        ujpop[j,2]=mod.out[1]
        ujpop[j,3]=mod.out[2]
        ujpop[j,4]=mod.out[3]
      }
    }
    
    populacio<-ujpop[order(ujpop[,4],ujpop[,3],ujpop[,2],decreasing=TRUE),]
    
    print(index)
    flush.console()
    
    HMCR=HMCR*HMCRszorzo
    mutacio=mutacio*mutacioszoro
  }
  return(c(legjobb, konvergszamlalo))
}

ModellEpit_B_Fix_k=function(egyed, X, Y, Default_k){
  genszam=length(egyed)
  
  kvektor<-rep(10,sum(egyed))
  SpecStatus<-FALSE
  
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
      colnames(adatok)<-nevek
      szamlalo=szamlalo+1
    }
  }
  adatok$target<-Y
  
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
        modelstring<-paste(modelstring, "s(",alapnevek[i],",k=",kvektor[szamlalo],")", sep="")
      } else {
        modelstring<-paste(modelstring,"s(",alapnevek[i],",k=",kvektor[szamlalo],")+", sep="")
      }
      szamlalo=szamlalo+1
    }
  }
  
  library(mgcv)
  gam.mod<-gam(as.formula(modelstring),family="gaussian",data=adatok, method="REML")
  
  return(gam.mod)
}

ModellEpit_B=function(egyed, X, Y){
  genszam=length(egyed)
  
  kvektor<-rep(10,sum(egyed))
  SpecStatus<-FALSE
  
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
      colnames(adatok)<-nevek
      szamlalo=szamlalo+1
    }
  }
  adatok$target<-Y
  
  while (!SpecStatus) {
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
          modelstring<-paste(modelstring, "s(",alapnevek[i],",k=",kvektor[szamlalo],")", sep="")
        } else {
          modelstring<-paste(modelstring,"s(",alapnevek[i],",k=",kvektor[szamlalo],")+", sep="")
        }
        szamlalo=szamlalo+1
      }
    }
    
    library(mgcv)
    gam.mod<-gam(as.formula(modelstring),family="gaussian",data=adatok, method="REML")
    ellen<-mgcv:::k.check(gam.mod)
    
    SpecStatus<-TRUE
    for (i in 1:length(kvektor)) {
      if (((ellen[i,1]-ellen[i,2])<3)&(ellen[i,4]<0.05)) {
        SpecStatus<-FALSE
        kvektor[i]=kvektor[i]+5
      }
    }
  }
  
  return(gam.mod)
}