readSS.txt<-function(repfiles="Report08_310.SSO",qt=1,spawnTime=1,useWt_Mid=TRUE){
#################
################# 2010/4/5 SS3の出力にも対応した
#################

                     #filenames=list(data="Alb06D1rev.d01",naa="NAA.TXT",caa="CAA.TXT",faa="FAA.TXT",maa="MAA.TXT",ind="IND.TXT"),firstYear=1,age0=1){
  #config<-read.table(file=filenames$data,nrows=6,fill=T,header=FALSE,as.is=TRUE)
  report<-getReport.sso(repfile=repfiles[1])
  biom <- getBabs.ss3.x(report)[[1]]
  nma <- getNMA.ss3.x(report)[[1]]

  firstYear<-as.numeric(biom$Yr[biom$Era=="TIME"][1])
  lastYear<- as.numeric(rev(biom$Yr[biom$Era=="TIME"])[1])
  nyear<-lastYear-firstYear+1
  Seas<-sort(unique(biom$Seas))
  nseas<-length(biom$Seas)
  biom$YQ<-as.numeric(biom$Yr)+(as.numeric(biom$Seas)-1)/nseas

  firstYQ<-biom$YQ[biom$Era=="TIME"][1]
  lastYQ<- rev(biom$YQ[biom$Era=="TIME"])[1]
  nyq<-lastYQ-firstYQ+1
  if(is.null(nma$age)){nma$age<-nma$Age}

  age0 <- min(nma$Age)#as.numeric(config[2,1])
  lastAge <- max(nma$Age)#as.numeric(config[2,2])
  nage<-lastAge-age0+1
  plusgroup<- max(nma$Age)#as.numeric(config[2,3])
  Seas<-sort(unique(nma$Seas))
  nindices<- 0 #  this is dummy
  spawnTime<- spawnTime #  same as VPA


  maturity<-nma$"Age_Mat"
  if(any(nma$Age_Mat<0)){maturity<-nma$'Mat*Fecund'/nma$Wt_Beg}
  modelName<-0 # dummy

#####################################################################
#  aa<-readLines(con=filenames$data)
#  blankLines<- grep(x=aa,pattern="^-1$")
  #waa<-read.table(file=filenames$data,nrows=nyear,header=F,skip=blankLines[5],col.names=c("Year",paste("age_",age0:(nage-age0+1),sep="")))
  #WAA<-as.matrix(waa[,2:(nage+1)])
### read weight at age
  if(useWt_Mid){
    Wt<-nma$"Wt_Mid"
  }else{
    Wt<-nma$"Wt_Beg"
  }
  nma<-cbind(nma,Wt)
  waa<-as.array(xtabs(formula=nma$Wt~nma$Seas+nma$Age+nma$Gender))
  waa<-1:nyear %o%  waa # 要修正
#  browser()
#  waa <- rep(Wt,nyear)   # A3_finalでは、SSBでは、Wt_Begが使用されている！要検討

#  dim(waa) <- c(length(Wt),nyear)
  WAA <- waa
  rownames(WAA)<-firstYear-1+1:nyear
  dim1<-dim(waa)[1]*dim(waa)[2]
  dim2<-dim(waa)[3]
  dim3<-dim(waa)[4]
  dim(WAA)<-c(dim1,dim2,dim3)
  CWAA<-WAA
#####################################################################
  reports<-getReport.sso(repfiles,oldStyle=FALSE)
  NAA<-readNAA.boot(reports=reports,firstYear=firstYear,age0=age0)
  CAA<-readCAA.boot(reports=reports,firstYear=firstYear,age0=age0)
  FAA<-readFAA.boot(reports=reports,firstYear=firstYear,age0=age0,qt=qt)
  # browser()
  #
  MAA<-NAA#readVPA.boot(filenames$maa,firstYear=firstYear,age0=age0)
  tmp<-as.array(xtabs(nma$M~nma$Seas+nma$Age+nma$Gender))
  tmp<-1:nyear %o% tmp
  dim(tmp)<-c(dim1,dim2,dim3)
  MAA[[1]]<-tmp
 # browser()

# 新しい順番 nyear*nseason*nage*ngender
# 古い順番 boot box year age
  IND<-0 # dummy
#######################################################################
#   tmp<-WAA*NAA$array[1,1,,]/1000
  wnaa<-sweep(NAA$array,c(1,2),WAA,FUN="*")/1000
  totBiomass<-apply(wnaa,c(2,3,4),FUN="sum")

  ZAA<-FAA
  ZAA$array<-MAA$array+FAA$array
  NAA.spawn<-NAA$array*exp(-ZAA$array*(spawnTime-1)/12)
  wnaa.spawn<-sweep(NAA.spawn,c(1,2),WAA,FUN="*")/1000
  spawner<-sweep(wnaa.spawn,2,maturity,FUN="*")
#  browser()
  SSB<-apply(spawner,c(2,3,4),FUN="sum")

  return(invisible(list(NAA=NAA,CAA=CAA,FAA=FAA,MAA=MAA,IND=IND,WAA=WAA,CWAA=CWAA,
                        maturity=maturity,SSB=SSB,totBiomass=totBiomass,nindices=nindices,spawnTime=spawnTime)))
}

readVPA<-function(binary=FALSE,filenames=NULL,datafile="Alb06D1rev.d01",
    waafile="WD1rev.txt",waa.spawn=NULL,waa.yirlf=NULL,nbox=1,nboot=500,spawnTime=5){

#readVPA<-function(ctl="Alb06D1.c01",waa.spawn=NULL)
#  ctl.config<-read.table(file=ctl,fill=T,header=FALSE,as.is=TRUEcol.names=paste("V",1:6))
  if(is.null(filenames)){
    if(!binary){
      filenames<-list(naa="NAA.TXT",caa="CAA.TXT",faa="FAA.TXT",maa="MAA.TXT",ind="IND.TXT")
    }else{
      filenames<-list(naa="NAA.OUT",caa="CAA.OUT",faa="FAA.OUT",maa="MAA.OUT",ind="IND.OUT")
    }
  }
  config<-read.table(file=datafile,nrows=6,fill=T,header=FALSE,as.is=TRUE)
  firstYear<-as.numeric(config[1,1])
  lastYear<-as.numeric(config[1,2])
  nyear<-lastYear-firstYear+1
  age0<-as.numeric(config[2,1])
  lastAge<-as.numeric(config[2,2])
  nage<-lastAge-age0+1
  plusgroup<-as.numeric(config[2,3])
  nindices<-as.numeric(config[3,1])
  if(is.null(spawnTime))spawnTime<-as.numeric(config[4,1])
  maturity<-as.numeric(config[5,1:nage])
  modelName<-config[6,1]

#####################################################################
  aa<-readLines(con=datafile)
  blankLines<- grep(x=aa,pattern="^-1$")
### read weight at age
  if(is.null(waafile)){
    waa<-read.table(file=datafile,nrows=nyear,header=FALSE,skip=blankLines[5],col.names=c("Year",paste("age_",seq(from=age0,by=1,length.out=nage),sep="")))
    if(is.null(waa.yield))waa.yield<-waa
  }else{
    waa<-read.table(file=waafile,header=FALSE,col.names=c("dum1","dum2","Year",paste("age_",seq(from=age0,by=1,length.out=nage),sep="")))
    waa.yield<-waa[nyear+1:nyear,-1:-2]
    waa<-waa[1:nyear,-1:-2]

  }
  WAA<-as.matrix(waa[,2:(nage+1)])
  CWAA<-as.matrix(waa.yield[,2:(nage+1)])
### setup weight at age for spawners
  if(!is.null(waa.spawn)){
    if(length(waa.spawn)==nage){
      WAA<-rep(1,nyear)%o%waa.spawn
    }else{
      stop("waa.spawn must be a vector of its length=nage")
    }
  }
############################################
  dimnames(WAA)[[1]]<-firstYear-1+1:nyear
  dimnames(CWAA)[[1]]<-firstYear-1+1:nyear
#####################################################################
  if(!binary){
    NAA<-readVPA.boot.ascii(filenames$naa,firstYear=firstYear,age0=age0)
    CAA<-readVPA.boot.ascii(filenames$caa,firstYear=firstYear,age0=age0)
    FAA<-readVPA.boot.ascii(filenames$faa,firstYear=firstYear,age0=age0)
    MAA<-readVPA.boot.ascii(filenames$maa,firstYear=firstYear,age0=age0)
    IND<-readVPA.boot.ascii(filenames$ind,firstYear=firstYear,age0=1)
  }else{
#    browser()
    NAA<-readVPA.boot.bin(filenames$naa,firstYear=firstYear,age0=age0,nage=nage,nbox=nbox,nboot=nboot,nyear=nyear)
    CAA<-readVPA.boot.bin(filenames$caa,firstYear=firstYear,age0=age0,nage=nage,nbox=nbox,nboot=nboot,nyear=nyear)
    FAA<-readVPA.boot.bin(filenames$faa,firstYear=firstYear,age0=age0,nage=nage,nbox=nbox,nboot=nboot,nyear=nyear)
    MAA<-readVPA.boot.bin(filenames$maa,firstYear=firstYear,age0=age0,nage=nage,nbox=nbox,nboot=nboot,nyear=nyear)
    IND<-readVPA.boot.bin(filenames$ind,firstYear=firstYear,age0=1,nage=nindices,nbox=nbox,nboot=nboot,nyear=nyear)
  }

#######################################################################
#   tmp<-WAA*NAA$array[1,1,,]/1000
  wnaa<-sweep(NAA$array,c(1,2),WAA,FUN="*")/1000
  totBiomass<-apply(wnaa,c(2,3,4),FUN="sum")
# 新しい順番 nyear*nage*nbox*nboot
# 古い順番 boot box year age
  ZAA<-FAA
  ZAA$array<-MAA$array+FAA$array
  NAA.spawn<-NAA$array*exp(-ZAA$array*(spawnTime-1)/12)
  wnaa.spawn<-sweep(NAA.spawn,c(1,2),WAA,FUN="*")/1000
  spawner<-sweep(wnaa.spawn,2,maturity,FUN="*")
  SSB<-apply(spawner,c(2,3,4),FUN="sum")

  return(invisible(list(NAA=NAA,CAA=CAA,FAA=FAA,MAA=MAA,IND=IND,WAA=WAA,CWAA=CWAA,
    maturity=maturity,SSB=SSB,totBiomass=totBiomass,nindices=nindices,spawnTime=spawnTime,
    WAA=WAA)))
}


readNAA.boot<-function(reports=NULL,bootfiles){#filename,firstYear=1,age0=1){
  if(is.null(reports))reports<-getReport.sso(repfile=bootfiles,oldStyle=FALSE)
  nboot<-reports[[2]]
  res.tmp <- getNAA.ss3.x(report=reports[[1]][[1]])
  info<-res.tmp$info
  res.tmp<-res.tmp[[1]]
  if(is.null(res.tmp$Year))res.tmp$Year<-res.tmp$Yr
  if(is.null(res.tmp$Per))res.tmp$Per<-res.tmp$Era
  years <- res.tmp$Year[y.range <- (res.tmp$"Per"=="TIME")]
  #tmp <- as.numeric(names(res.tmp))
  #ages <- tmp[a.range <- !is.na(tmp)]
  ages<-info$Ages
  Gender<-info$Gender

  # res.tmp$YQ<-as.numeric(res.tmp$Yr)+(as.numeric(res.tmp$Seas)-1)/nseas
  res.tmp$YQ<-as.numeric(res.tmp$Time)

  firstYQ<-res.tmp$YQ[res.tmp$Era=="TIME"][1]
  lastYQ<- rev(res.tmp$YQ[res.tmp$Era=="TIME"])[1]
  nyq<-(lastYQ-firstYQ)*length(info$Seasons)+1

  tmp.fn<-function(report){
    res.tmp<-getNAA.ss3.x(report=report)[[1]]
    tmp.names<-as.numeric(names(res.tmp))
    names(res.tmp)[!is.na(tmp.names)]<-paste("Age",tmp.names[!is.na(tmp.names)],sep=".")
    tmp.long<-reshape(res.tmp,direction="long",idvar=c("Area","Gender","Morph","Yr","Seas","Era"),varying=paste("Age",sum(!is.na(tmp.names)),sep="."))
    names(tmp.long)[9]<-"YQ"
    names(tmp.long)[11:12]<-c("Age","Number")
    array1<-as.array(xtabs(data=tmp.long,formula=Number~YQ     +Age+Gender))
    array2<-as.array(xtabs(data=tmp.long,formula=Number~Yr+Seas+Age+Gender))
    return(list(array1=array1,array2=array2))
  }
  lists<-lapply(reports[[1]],tmp.fn)
  return(invisible(list(lists1=lapply(lists,FUN=function(x){return(x$array1)}),
                        lists2=lapply(lists,FUN=function(x){return(x$array2)}),
                        nboot=nboot,nbox=1,nyear=length(years),nage=length(ages),
                        nseas=length(info$Seasons),ngender=length(Gender),firstYear=min(years),age0=min(ages))))
}

readCAA.boot<-function(reports=NULL,bootfiles){#filename,firstYear=1,age0=1){
  if(is.null(reports))reports<-getReport.sso(repfile=bootfiles,oldStyle=FALSE)
  nboot<-reports[[2]]
  res.tmp <- getCAA.ss3.x(report=reports[[1]][[1]])
  info<-res.tmp$info
  res.tmp<-res.tmp[[1]]
  if(is.null(res.tmp$Year))res.tmp$Year<-res.tmp$Yr
  if(is.null(res.tmp$Per))res.tmp$Per<-res.tmp$Era
  years <- res.tmp$Year[y.range <- !is.na(res.tmp$"YQ")]
#  tmp <- as.numeric(names(res.tmp))
#  ages <- tmp[a.range <- !is.na(tmp)]
  Ages<-info$Ages
  Gender<-info$Gender

  tmp.fn<-function(report){
    res.tmp<-getCAA.ss3.x(report=report)[[1]]

    tmp.names<-as.numeric(names(res.tmp))
#    names(res.tmp)[!is.na(tmp.names)]<-paste("Age",tmp.names)[!is.na(tmp.names)],sep=".")
    tmp.long<-reshape(res.tmp,direction="long",idvar=c("Area","Fleet","Gender","Morph","Yr","Seas","Era"),varying=paste("Age",Ages,sep="."))
#    tmp.long<-reshape(caa,direction="long",idvar=c("Area","Fleet","Morph","Yr","Seas","Era"),
#    varying=paste("Age",Ages,sep="."))

    names(tmp.long)[9]<-"YQ"
    names(tmp.long)[11:12]<-c("Age","Number")
    array1<-as.array(xtabs(data=tmp.long,formula=Number~YQ+Age+Gender))
    array2<-as.array(xtabs(data=tmp.long,formula=Number~YQ+Age+Gender+Fleet))
    return(list(array1=array1,array2=array2))
  }
  lists<-lapply(reports[[1]],tmp.fn)
  return(invisible(list(lists1=lapply(lists,FUN=function(x){return(x$array1)}),
                        lists2=lapply(lists,FUN=function(x){return(x$array2)}),
                        nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),
                        nseas=length(info$Seasons),ngender=length(Gender),firstYear=min(years),age0=min(ages))))

}

readFAA.boot<-function(reports=NULL,bootfiles){#filename,firstYear=1,age0=1){
  if(is.null(reports))reports<-getReport.sso(repfile=bootfiles,oldStyle=FALSE)
  nboot<-reports[[2]]
  res.tmp <- calFAA.ss3.x(reports[[1]][[1]])
  info<-res.tmp$info
  res.tmp<-res.tmp[[1]]
  if(is.null(res.tmp$Year))res.tmp$Year<-res.tmp$Yr
  if(is.null(res.tmp$Per))res.tmp$Per<-res.tmp$Era
  years <- res.tmp$naa$Year[res.tmp$naa$Per=="TIME"]
  Ages<-info$Ages
  Gender<-info$Gender

  tmp.fn<-function(report){
    res.tmp<-calFAA.ss3.x(report=report)
    tmp.names<-as.numeric(names(res.tmp))
    return(list(array1=res.tmp$faa,array2=res.tmp$faa.array))
  }
  lists<-lapply(reports[[1]],tmp.fn)
  return(invisible(list(lists1=lapply(lists,FUN=function(x){return(x$array1)}),
                        lists2=lapply(lists,FUN=function(x){return(x$array2)}),
                        nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),
                        nseas=length(info$Seasons),ngender=length(Gender),firstYear=min(years),age0=min(ages))))
}

makeProjection2<-function(report,Fyear=2002:2004,Ryear=1966:1998,nyear.proj=25,
  proj1stYr=2005,Fmult=1,nrandsim=10,geomean=TRUE,singleF=FALSE,nfixedCyear=2,fixedC=c(66000,85000),nreplRYear=1,saveResults=FALSE){



}





makeProjection<-function(boot.out=NULL,Fyear=2002:2004,Ryear=1966:1998,nyear.proj=25,
  proj1stYr=2005,Fmult=1,nrandsim=10,geomean=TRUE,singleF=FALSE,nfixedCyear=2,fixedC=c(66000,85000),nreplRYear=1,saveResults=FALSE){
################
##
##  Fyear : vector of year to calculate current F
##  Ryear : vector of year to resample recruitments
##  nyear.proj : number of year to be projected from last year of VPA
##               i.e., future projection be done from proj1stYr to firstYear+nyear-1+nyear.proj
##  proj1stYr  : 1st year of future projection i.e., numbers at age at the beginning of proj1stYr is assumed to be known
##  nfixedCyear : number of years to be projected with constant yield
##
# require(snowfall)
  require(foreach)
#  require(doMC)
#  require(multicore)
#  registerDoMC(cores=4)
#  registerDoSEQ()
#  require(snow)
#  require(doSNOW)
#  cl<-makeSOCKcluster(rep("localhost",length=4))
#  on.exit(stopCluster(cl))
#  registerDoSNOW(cl)


  if(is.null(boot.out))stop("boot.out is required")  # boot.out<-readVPA.txt()

  NAA<-boot.out$NAA
  CAA<-boot.out$CAA
  nboot<-NAA$nboot
  nbox<-NAA$nbox
  nyear<-NAA$nyear
  nage<-NAA$nage
  age0<-NAA$age0
  nseas<-NAA$nseas
  ngender<-NAA$ngender

  spawnTime<-boot.out$spawnTime
  firstYear<-NAA$firstYear
#  dim2<-dim(NAA$array)[4]
#  browser()
#  browser()
  NAA.proj<-CAA.proj<-array(0,dim=c(nyear+nyear.proj*nseas,nage,ngender))
# dimnames(NAA.proj)<-list(boot=0:nboot,box=1:nbox,year=1:(nyear+nyear.proj)+firstYear-1,age=age0:(nage-age0+1))
  dimnames(NAA.proj)<-list(year=1:(nyear+nyear.proj)+firstYear-1,age=seq(from=age0,by=1,length=nage),gender=1:ngender)

#  CAA.proj<-array(0,dim=c(nyear+nyear.proj,nage,nbox,dim2,nrandsim))
# dimnames(CAA.proj)<-list(boot=0:nboot,box=1:nbox,year=1:(nyear+nyear.proj)+firstYear-1,age=age0:(nage-age0+1))
#        dimnames(CAA.proj)<-list(randsim=1:nrandsim,boot=ifelse(dim2==(nboot+1),0,1):nboot,box=1:nbox,year=1:(nyear+nyear.proj)+firstYear-1,
#                                 age=seq(from=age0,by=1,length=nage))
        dimnames(CAA.proj)<-dimnames(NAA.proj)
  FAA<-boot.out$FAA
  F<-FAA$array[paste(Fyear),,,]
#  browser()
  if(length(Fyear)!=1){
    if(nboot==0){  # 次元が退化した場合に対応
      F.dum<-F%o%rep(1,1)
#      F<-aperm(F.dum,c(3,1,2))
    }
#    browser()
    if(geomean){
      F<-exp(apply(log(F),if(nbox==1){c(2,3)}else{c(2,3,4)},"mean"))
    }else{
      F<-apply(F,if(nbox==1){c(2,3)}else{c(2,3,4)},"mean")
    }
  }

  if(nbox==1){
    F<-F%o%rep(1,1)
    F<-aperm(F,c(1,3,2))
  }

  F<-F*Fmult
################################################# ダミー次元を加えた 2010/4/3
  F<-F%o%rep(1,nrandsim)
#  F<-aperm(F.dum,c(4,1,2,3))

################################################# ダミー次元を加えた 2010/4/3
#  NAA.arry.dum0<-NAA$array%o%rep(1,nrandsim)
#  NAA.arry.dum<-aperm(NAA.arry.dum0,c(5,1,2,3,4))
#  CAA.arry.dum0<-CAA$array%o%rep(1,nrandsim)
#  CAA.arry.dum<-aperm(CAA.arry.dum0,c(5,1,2,3,4))

  NAA.arry.dum<-NAA$array%o%rep(1,nrandsim)
  CAA.arry.dum<-CAA$array%o%rep(1,nrandsim)

  NAA.proj[1:(proj1stYr-firstYear+1),,,,]<-NAA.arry.dum[1:(proj1stYr-firstYear+1),,,,]
  CAA.proj[1:(proj1stYr-firstYear+1),,,,]<-CAA.arry.dum[1:(proj1stYr-firstYear+1),,,,]

#  NAA.proj[,,,1:(proj1stYr-firstYear+1),]<-NAA$array[,,1:(proj1stYr-firstYear+1),]
#  CAA.proj[,,,1:(proj1stYr-firstYear+1),]<-CAA$array[,,1:(proj1stYr-firstYear+1),]
##############################################

##  Set up future recruitment

# browser()
  sizeFutureR<-nyear-(proj1stYr-firstYear+1)+nyear.proj+nreplRYear
#  futureR<-array(0,dim=c(nrandsim,dim2,nbox,sizeFutureR))

  futureR<-array(0,dim=c(sizeFutureR,nbox,dim2,nrandsim))

  for(s in 1:nrandsim){
    for(i in 1:dim2){
      for(b in 1:nbox){
        futureR[,b,i,s]<-sample(x=NAA$array[paste(Ryear),1,b,i],size=sizeFutureR,replace=TRUE)
      }
    }
  }

#  futureR<-
#  foreach(s=1:nrandsim,.combine=rbind)%:%
#    foreach(i=1:dim2,.combine=rbind)%:%
#      foreach(b=1:nbox,rbind)%do{
#        sample(x=NAA$array[i,b,paste(Ryear),1],size=sizeFutureR,replace=TRUE)
#      }

#  dim1<-if(length(dim(NAA$array[,,paste(Ryear),1]))==1){1}else{c(1,2)}
#  futureR<-apply(NAA$array[,,paste(Ryear),1],dim1,sample,size=nyear-(proj1stYr-firstYear+1)+nyear.proj,replace=TRUE)
# dim(futureR)<-dim(NAA.proj[,,,(proj1stYr-firstYear+2):(nyear+nyear.proj),1])

  NAA.proj[(proj1stYr-firstYear+2):(nyear+nyear.proj),1,,,]<-futureR[(1+nreplRYear):sizeFutureR,,,]
############## MAAにも次元を加える
  MAA<-boot.out$MAA
  MAA$array<-MAA$array%o%rep(1,nrandsim)
#  MAA$array<-aperm(MAA.dum,c(5,1,2,3,4))
#  MAA<-boot.out$MAA
###############################
## Calculate biomass and SSB time series
  WAA<-array(0,dim=c(nyear+nyear.proj,nage))
  WAA[1:nyear,]<-boot.out$WAA
  WAA[nyear+1:nyear.proj,]<-rep(1,nyear.proj)%o%WAA[nyear,]

#  browser()
##### Function to calculate yield given diffent F-multiplier
### 一次元版
calcYield.0<-function(Fmult,F,NAA,MAA,WAA){
  Fnew<-Fmult*F
  CAA<-NAA*(1-exp(-Fnew-MAA))*Fnew/(Fnew+MAA)
  WCAA<-CAA*WAA/1000
  Yield<-WCAA
  return(Yield)
}

#################################
### 多次元版
calcYield<-function(Fmult,F,NAA,MAA,WAA){
  Fnew<-Fmult*F
  dim(Fnew)<-dim(F)
  CAA<-NAA*(1-exp(-Fnew-MAA))*Fnew/(Fnew+MAA)
  dim(CAA)<-dim(NAA)
  WCAA<-CAA*WAA/1000
  Yield<-sum(WCAA)
  return(Yield)
}


solveByYield<-function(NAA,MAA,WAA,FAA,Yield){
  f<-function(x){
    return(Yield-calcYield(Fmult=x,F=FAA,NAA=NAA,MAA=MAA,WAA=WAA))
  }
##########################################################################
  temp<-uniroot(f=f,interval=c(0.1,10),tol=1.0e-6)
#  browser()
  return(temp$root)
}

    y<-proj1stYr-firstYear+2
#      F1<-FAA$array[,,y-1,]
      F1<-F
      dim(F1)<-dim(F)
      NAA.proj[y-1,1,,,]<-futureR[1,,,]  ##  今のところ1年しか過去の加入を置き換えない
 #     browser()
      NAA.proj[y,2:nage,,,]<-NAA.proj[y-1,1:(nage-1),,,]*exp(-F1[1:(nage-1),,,]-MAA$array[nyear,1:(nage-1),,,])
      NAA.proj[y,nage,,,]<-NAA.proj[y-1,nage,,,]*exp(-F1[nage,,,]-MAA$array[nyear,nage,,,])+NAA.proj[y,nage,,,]
      CAA.proj[y-1,1:nage,,,]<-NAA.proj[y-1,1:nage,,,]*
        (1-exp(-F1[1:nage,,,]-MAA$array[nyear,1:nage,,,]))*F1[1:nage,,,]/(F1[1:nage,,,]+MAA$array[nyear,1:nage,,,])
## MAA.projも定義した方がいい？要検討
#############################################
#  browser()
  if(nfixedCyear>0){  ##
#    FAA.fixedY<-array(0,dim=c(nrandsim,dim2,nbox,nfixedCyear,nage))
#    ZAA.fixedY<-array(0,dim=c(nrandsim,dim2,nbox,nfixedCyear,nage))
     FAA.fixedY<-ZAA.fixedY<-array(0,dim=c(nfixedCyear,nage,nbox,dim2,nrandsim))
    for(r in 1:nrandsim){
      for(bt in 1:dim2){
#      foreach(bt=1:nboot) %dopar% (
        for(b in 1:nbox){
          i<-0
#          browser()
          for(y in (proj1stYr-firstYear+3):(proj1stYr-firstYear+3+nfixedCyear-1)){
###################### determin Fmult #################################
            i<-i+1
### NAA.proj<-array(0,dim=c(nrandsim,dim2,nbox,nyear+nyear.proj,nage))
            Fmult.tmp<-solveByYield(NAA=NAA.proj[y-1,,b,bt,r],MAA=MAA$array[nyear,,b,bt,r],WAA=WAA[y,],FAA=F[,b,bt,r],Yield=fixedC[i])
###################### multiply F by Fmult ############################
            F.tmp<-F[,b,bt,r]*Fmult.tmp
#             browser()
            FAA.fixedY[i,,b,bt,r]<-F.tmp
            ZAA.fixedY[i,,b,bt,r]<-F.tmp+MAA$array[nyear,,b,bt,r]
#            cat(r,",",bt,",",b,",",y,",",Fmult.tmp,"\n")
            NAA.proj[y,2:nage,b,bt,r]<-NAA.proj[y-1,1:(nage-1),b,bt,r]*exp(-F.tmp[1:(nage-1)]-MAA$array[nyear,1:(nage-1),b,bt,r])
            NAA.proj[y,nage,b,bt,r]<-NAA.proj[y-1,nage,b,bt,r]*exp(-F.tmp[nage]-MAA$array[nyear,nage,b,bt,r])+NAA.proj[nyear,nage,b,bt,r]
###################### project N to next year #########################
            CAA.proj[y-1,,b,bt,r]<-NAA.proj[y-1,,b,bt,r]*(1-exp(-F.tmp[1:nage]-MAA$array[nyear,,b,bt,r]))*
              F.tmp/(F.tmp+MAA$array[nyear,,b,bt,r])
          }
        }
#      )
      }
    }
  }

  gc()
  gc()

  if(!singleF){
    for(y in (proj1stYr-firstYear+3+nfixedCyear):(nyear+nyear.proj)){
#     browser()
#      cat("y=",y,"\n")
      NAA.proj[y,2:nage,,,]<-NAA.proj[y-1,1:(nage-1),,,]*exp(-F[1:(nage-1),,,]-MAA$array[nyear,1:(nage-1),,,])
      NAA.proj[y,nage,,,]<-NAA.proj[y-1,nage,,,]*exp(-F[nage,,,]-MAA$array[nyear,nage,,,])+NAA.proj[y,nage,,,]
    }

  #### チェックする必要あり 2010/05/04
    for(y in (proj1stYr-firstYear+2+nfixedCyear):(nyear+nyear.proj)){
      CAA.proj[y,1:nage,,,]<-NAA.proj[y,1:nage,,,]*
        (1-exp(-F[1:nage,,,]-MAA$array[nyear,1:nage,,,]))*F[1:nage,,,]/(F[1:nage,,,]+MAA$array[nyear,1:nage,,,])
    }
  }else{
    F1<-F
    dim(F1)<-dim(F)
    for(i in 1:dim2){
      F1[,,i,]<-F[,,1,]
    }
    dim(F1)<-dim(F)
    for(y in (proj1stYr-firstYear+3+nfixedCyear):(nyear+nyear.proj)){
#    foreach(y=(proj1stYr-firstYear+3+nfixedCyear):(nyear+nyear.proj)) %do%
      NAA.proj[y,2:nage,,,]<-NAA.proj[y-1,1:(nage-1),,,]*exp(-F1[1:(nage-1),,,]-MAA$array[nyear,1:(nage-1),,,])
      NAA.proj[y,nage,,,]<-NAA.proj[y-1,nage,,,]*exp(-F1[nage,,,]-MAA$array[nyear,nage,,,])+NAA.proj[y,nage,,,]

    }
    for(y in (proj1stYr-firstYear+2+nfixedCyear):(nyear+nyear.proj)){
      CAA.proj[y,1:nage,,,]<-NAA.proj[y,1:nage,,,]*
        (1-exp(-F1[1:nage,,,]-MAA$array[nyear,1:nage,,,]))*F1[1:nage,,,]/(F1[1:nage,,,]+MAA$array[nyear,1:nage,,,])
    }
  }
  gc()
  gc()
# browser()
#  wnaa<-sweep(NAA.proj,c(3,4),WAA,FUN="*")/1000
#  totBiomass.proj<-apply(wnaa,c(1,2,3),FUN="sum")

  wnaa<-sweep(NAA.proj,c(1,2),WAA,FUN="*")/1000
  totBiomass.proj<-apply(wnaa,c(2,3,4,5),FUN="sum")

  maturity<-boot.out$maturity
  ZAA.proj<-array(0,dim=c(nyear+nyear.proj,nage,nbox,dim2,nrandsim))
#  ZAA.proj[,,,1:nyear,]<-aperm((boot.out$FAA$array+boot.out$MAA$array)%o%rep(1,nrandsim),c(5,1,2,3,4))
  ZAA.proj[1:nyear,,,,]<-(boot.out$FAA$array+boot.out$MAA$array)%o%rep(1,nrandsim)
#  tmp<-(F[,,1:nage]+MAA$array[,,nyear,1:nage])%o%rep(1,nyear-(proj1stYr-firstYear+1)+nyear.proj)
#  tmp<-aperm(tmp,c(1,3,2))
  if(!singleF){
    tmp<-(F[1:nage,,,]+MAA$array[nyear,1:nage,,,])%o%rep(1,nyear-(proj1stYr-firstYear+1)+nyear.proj)
#    tmp<-aperm(tmp,c(4,1,2,3))
  }else{
    F1<-F
    dim(F1)<-dim(F)
    for(i in 1:(nboot+1)){
      F1[,,i,]<-F[,,1,]
    }
    dim(F1)<-dim(F)
    tmp<-(F1[1:nage,,,]+MAA$array[nyear,1:nage,,,])%o%rep(1,nyear-(proj1stYr-firstYear+1)+nyear.proj)

  }
#  browser()
  if(length(dim(tmp))==4){
    tmp<-aperm(tmp,c(4,1,2,3))
  }else if(length(dim(tmp))==3){
    tmp<-aperm(tmp,c(3,1,2))
  }else{
    stop("ERROR")
  }
#  browser()
  if(nboot==0){  # 確認が必要
    tmp1<-tmp%o%rep(1,1)
#    tmp1<-aperm(tmp1,c())
    if(nrandsim==1){
      tmp<-tmp1%o%rep(1,1)
#      tmp1<-aperm(tmp2,c(1,2,3,4))
    }else{
      tmp<-aperm(tmp1,c(1,2,4,3))
    }
  }else{
  # browser()
#   if(singleF){
#     tmp<-tmp%o%rep(1,nboot+1)
#     tmp<-aperm(tmp,c(3,1,2))
#   }
 #  browser()
    if(nrandsim==1){
      tmp<-tmp%o%rep(1,1)
#      tmp<-aperm(tmp2,c(4,1,2,3))
    }
#    tmp<-aperm(tmp,c(1,4,2,3))
  }
#  browser()
#  tmp<-aperm(tmp,c(1,3,2,4))  順序があっているか要確認
  if(nbox==1){
    tmp<-tmp%o%rep(1,1)
#    tmp<-aperm(tmp,c(1,2,5,3,4))
    dim(tmp)<-c(dim(tmp)[1:2],1,dim(tmp)[3:4])
  }
#  browser()

#  ZAA.proj[,,(proj1stYr-firstYear+2):(nyear+nyear.proj),]<-tmp
#  NAA.spawn<-NAA.proj*exp(-ZAA.proj*(spawnTime-1)/12)
#  wnaa.spawn<-sweep(NAA.spawn,c(3,4),WAA,FUN="*")/1000
#  spawner<-sweep(wnaa.spawn,4,maturity,FUN="*")
#  SSB.proj<-array(0,dim=c(nboot+1,nbox,nyear+nyear.proj))
##   SSB.proj[,,1:nyear]<-boot.out$SSB
#  SSB.proj<-apply(spawner,c(1,2,3),FUN="sum")

  ZAA.proj[(proj1stYr-firstYear+2):(nyear+nyear.proj),,,,]<-tmp
### 固定の漁獲量の年はZを置き換える必要あり
#  ZAA.fixedY[r,bt,b,i,]
  if(nfixedCyear>0){
    for(r in 1:nrandsim){
      for(bt in 1:dim2){
        for(b in 1:nbox){
          i<-0
#          browser()
          for(y in (proj1stYr-firstYear+2):(proj1stYr-firstYear+2+nfixedCyear-1)){
###################### determin Fmult #################################
            i<-i+1
            ZAA.proj[y,,b,bt,r]<-ZAA.fixedY[i,,b,bt,r]
          }
        }
      }
    }
  }

  NAA.spawn<-NAA.proj*exp(-ZAA.proj*(spawnTime-1)/12)
  wnaa.spawn<-sweep(NAA.spawn,c(1,2),WAA,FUN="*")/1000
#  browser()
  spawner<-sweep(wnaa.spawn,2,maturity,FUN="*")
  SSB.proj<-array(0,dim=c(nyear+nyear.proj,nbox,dim2,nrandsim))
#  browser()
  dimnames(SSB.proj)[[3]]<-ifelse(dim2==(nboot+1),0,1):nboot
#   SSB.proj[,,1:nyear]<-boot.out$SSB
  SSB.proj<-apply(spawner,c(1,3,4,5),FUN="sum")
# browser()
  gc()
  gc()
  rt<-list(NAA=NAA.proj,CAA=CAA.proj,ZAA=ZAA.proj,SSB=SSB.proj,
    totBiomass=totBiomass.proj,WAA=WAA,F=if(singleF){F1}else{F},
    nyear=nyear,nyear.proj=nyear.proj,Fmult=Fmult,nboot=nboot)
  if(saveResults)save(rt,compress=TRUE,file=paste(Fmult,".r",sep=""))
  return(invisible(rt))
}

calFssb.alb <- function(projs,debug=FALSE,percentile=10,pyears=25){
  nboot<-projs[[1]]$nboot

  if(dim(projs[[1]]$NAA)[4]==nboot){
    offset<-0
  }else{
    offset<-1
  }

  res.mat <- array(0,dim=c(length(projs),2+length(percentile),length(pyears)))
  for(k in 1:length(projs)){
  #    proj.out <- projs[[k]]
    SSB<-projs[[k]]$SSB
    Fmult<-projs[[k]]$Fmult
    nrandsim<-dim(SSB)[4]
    nyear<-projs[[k]]$nyear
    nbox<-dim(SSB)[2]
#    browser()
    res.mat[k,1,] <- sapply(pyears,function(x){tmp<-sum(
      apply(SSB[1:nyear,,1:nboot+offset,1],if(nbox==1||nboot==1){2}else{c(2,3)},min) >
        apply(SSB[nyear+1:x,,1:nboot+offset,],if(nrandsim==1||nboot==1){2}else{c(2,3)},min))/(nrandsim*nboot);return(tmp)})
#    browser()

#    res.mat[k,1,pyears] <- sapply(pyears,function(x){sum(
#      apply(proj.out$SSB[,1:nboot+offset,1,1:nyear],if(nrandsim==1||nboot==1){1}else{2},min) >
#        apply(proj.out$SSB[,1:nboot+offset,1,nyear+1:x],if(nrandsim==1||nboot==1){1}else{2},min))/(nrandsim*nboot)})
#
#    browser()
#    if(debug)
#    browser()

    res.mat[k,1+1:length(percentile),]<-sapply(pyears,function(y){sapply(percentile,function(x){sum(
      apply(SSB[1:nyear,1,1:nboot+offset,],if(nrandsim==1||nboot==1){2}else{c(2,3)},quantile,probs=x/100,type=1) >
        apply(SSB[nyear+1:y,1,1:nboot+offset,],if(nrandsim==1||nboot==1){2}else{c(2,3)},min))/(nrandsim*nboot)})})
#    browser()
#   res.mat[k,1+1:length(percentile),paste(pyears)]


#    res.mat[k,1+1:length(percentile)] <- sapply(percentile,function(x){sum(
#      apply(proj.out$SSB[,1:nboot+offset,1,1:nyear],if(nrandsim==1||nboot==1){1}else{2},quantile,probs=x/100,type=1) >
#        apply(proj.out$SSB[,1:nboot+offset,1,(nyear+1):dim(proj.out$SSB)[[4]]],if(nrandsim==1||nboot==1){1}else{2},min))/(nrandsim*nboot)})
#    browser()
    res.mat[k,dim(res.mat)[2],] <-sapply(pyears, function(x){sum(apply(SSB[1:nyear,1,1:nboot+offset,],
          if(nrandsim==1||nboot==1){2}else{c(2,3)},function(x){mean(sort(x)[1:10])}) >
             apply(SSB[nyear+1:x,1,1:nboot+offset,],if(nrandsim==1||nboot==1){2}else{c(2,3)},min))/(nrandsim*nboot)})
#    browser()
#             apply(SSB[1:nyear,1:nboot+offset,],2,sort)
#    browser()
    cat("done ",k,"th Fmult:",Fmult,"\n")
  }
  dimnames(res.mat)[[1]]<-lapply(projs,"[[","Fmult")
#  browser()
  dimnames(res.mat)[[2]]<-c("F_SSB_min",paste("F_SSB",percentile,"%",sep=""),"F_SSB_ATHL")
  dimnames(res.mat)[[3]]<-pyears
  return(res.mat)
}

######################################## ここまで 2010/5/5 #############################
# 新しい順番 nyear*nage*nbox*nboot (*nrandsim)
# 古い順番 boot box year age


make.projs<-function(boot.out,interval=c(0.6,1.5),step=0.02,
  nrandsim=1,Ryear=1966:1998,singleF=TRUE,Fyear=2002:2004,fixedC=c(66000,85000),proj1stYr=2005,geomean=TRUE,nfixedCyear=2,
  parallelRun=TRUE,cpus=4,saveResults=FALSE,nyear.proj=25){
  require(snowfall)
  on.exit(sfStop())
  F<-function(x,...){
    cat("Fmult=",x,"\n")
    makeProjection(Fmult=x,boot.out=boot.out,nrandsim=nrandsim,Ryear=Ryear,singleF=singleF,Fyear=Fyear,fixedC=fixedC,
      proj1stYr=proj1stYr,geomean=geomean,nfixedCyear=nfixedCyear,saveResults=saveResults,nyear.proj=nyear.proj)
  }
  Fmults<-seq(from=interval[1],to=interval[2],by=step)
  sfInit(parallel=parallelRun, cpus=cpus)
  if(sfParallel()){
    sfExport("makeProjection","F","boot.out","singleF","Fyear",
      "fixedC","proj1stYr","geomean","nfixedCyear","nyear.proj",local=TRUE)
  }
  projs<-sfLapply(Fmults,fun=F,nrandsim=nrandsim,Ryear=Ryear)
  return(invisible(projs))
}

readVPA.boot.ascii<-function(filename,firstYear=1,age0=1){
  mat<-read.table(filename)
  nage<-ncol(mat)-3
#  nyear*nage*nbox*nboot
  colnames(mat)<-c("boot_no","box","year",paste("age_",age0:(nage-age0+1),sep=""))
  nboot<-max(mat$boot_no)
  nbox<-max(mat$box)
  nyear<-max(mat$year)
# NAA.array<-array(0,dim=c(nboot,nbox,nyear,nage),dimnames=c("boot","box","year","age"))
# dum<-sapply(1:nrow(NAA),function(i){NAA.array[NAA[i,1],NAA[i,2],NAA[i,3],]<-NAA[i,1:nage+3]})
# dum<-sapply(1:(nboot+1),
#   function(i){NAA.array[NAA[i,1],NAA[(i-1)*nyear*nbox+1:(nbox*nyear),2],NAA[(i-1)*nyear*nbox+1:(nbox*nyear),3],]<-
#     NAA[(i-1)*nyear*nbox+1:(nbox*nyear),1:nage+3];invisible(return(i))})

  mat2<-reshape(mat,varying=1:nage+3,direction="long",idvar=c("boot_no","box","year"),v.names="value")
  colnames(mat2)[4]<-"age"
# browser()
  mat.array<-array(0,dim=c(nyear,nbox,nboot+1,nage),dimnames=c("year","box","boot","age"))
  mat.array[1:length(mat2$value)]<-mat2$value
  dim(mat.array)<-c(nyear,nbox,nboot+1,nage)
  dimnames(mat.array)<-list(year=1:nyear+firstYear-1,box=1:nbox,boot=0:nboot,age=age0:(nage-age0+1))
#  mat.array<-aperm(mat.array,c(3,2,1,4))
  mat.array<-aperm(mat.array,c(1,4,2,3))
  return(invisible(list(array=mat.array,nboot=nboot,nbox=nbox,nyear=nyear,nage=nage,firstYear=firstYear,age0=age0)))
}

readVPA.boot.bin<-function(filename,firstYear=1,age0=1,nage,nbox,nboot,nyear){
  nobs <- nyear*nage*nbox*nboot
#  mat<-read.table(filename)
  mat<-readBin(con=filename,what="numeric",size=4,n=nobs)
  dim(mat) <- c(nyear,nage,nbox,nboot)

#  mat.array<-aperm(mat,c(4,3,1,2))
  mat.array<-mat
#  browser()
  dimnames(mat.array) <-
    list(seq(from=firstYear,length.out=nyear,by=1),seq(from=age0,length.out=nage,by=1),1:nbox,seq(from=1,length.out=nboot,by=1))
#  dimnames(mat.array) <- list(seq(from=1,length.out=nboot,by=1),1:nbox,seq(from=firstYear,length.out=nyear,by=1),seq(from=age0,length.out=nage,by=1))
#  colnames(mat2)<-c("boot_no","box_no","year","age")

  return(invisible(list(array=mat.array,nboot=nboot,nbox=nbox,nyear=nyear,nage=nage,firstYear=firstYear,age0=age0)))
}
 SSBX<-function(x){
  SSBx<-ssb[ssb$run==x,]
  ssbx<-as.matrix(SSBx[,-1:-2])
  ATHL<-apply(t(apply(ssbx[,1:40],1,sort))[,1:10],1,mean)
  write.csv(ssbx,paste("ssb",x,".csv",sep=""))
  write.csv(ATHL,paste("ATHL",x,".csv",sep=""))
  return(invisible(list(ssb=ssbx,ATHL=ATHL)))
}

readSS.new<-function(repfiles="Report08_310.SSO",qt=4,spawnTime=4,useWt_Mid=TRUE){
#################
################# 2010/4/5 SS3の出力にも対応した
#################

                     #filenames=list(data="Alb06D1rev.d01",naa="NAA.TXT",caa="CAA.TXT",faa="FAA.TXT",maa="MAA.TXT",ind="IND.TXT"),firstYear=1,age0=1){
  #config<-read.table(file=filenames$data,nrows=6,fill=T,header=FALSE,as.is=TRUE)
  biom <- getBabs.ss2(repfiles[1])[[1]]
  nma <- getNMA.ss2(repfiles[1],qt=qt)[[1]]

  firstYear<-biom$year[biom$period=="TIME"][1]
  lastYear<- rev(biom$year[biom$period=="TIME"])[1]
  nyear<-lastYear-firstYear+1
  if(is.null(nma$age)){nma$age<-nma$Age}
  age0 <- min(nma$age)#as.numeric(config[2,1])
  lastAge <- max(nma$age)#as.numeric(config[2,2])
  nage<-lastAge-age0+1
  plusgroup<- max(nma$age)#as.numeric(config[2,3])
  nindices<- 0 #  this is dummy
  spawnTime<- spawnTime #  same as VPA


  maturity<-nma$"Age_Mat"[1:nage]
  if(any(nma$Age_Mat[1:nage]<0)){maturity<-nma$'Mat*Fecund'[1:nage]/nma$Wt_Beg[1:nage]}
  modelName<-0 # dummy

#####################################################################
#  aa<-readLines(con=filenames$data)
#  blankLines<- grep(x=aa,pattern="^-1$")
  #waa<-read.table(file=filenames$data,nrows=nyear,header=F,skip=blankLines[5],col.names=c("Year",paste("age_",age0:(nage-age0+1),sep="")))
  #WAA<-as.matrix(waa[,2:(nage+1)])
### read weight at age
  if(useWt_Mid){
    Wt<-nma$"Wt_Mid"[1:nage+nage*(spawnTime-1)]
  }else{
    Wt<-nma$"Wt_Beg"[1:nage+nage*(spawnTime-1)]
  }
  waa <- rep(Wt,nyear)   # A3_finalでは、SSBでは、Wt_Begが使用されている！要検討
  dim(waa) <- c(length(Wt),nyear)
  WAA <- t(waa)
  rownames(WAA)<-firstYear-1+1:nyear
  CWAA<-WAA
#####################################################################
  NAA<-readSS.boot.x(repfiles,firstYear=firstYear,age0=age0,FUN=getNAA.y.ss3)
  CAA<-readSS.boot.x(repfiles,firstYear=firstYear,age0=age0,FUN=getCAA.y.ss3)
  FAA<-readSS.boot.x(repfiles,firstYear=firstYear,age0=age0,FUN=getFAA.y.ss3)

#  NAA<-readNAA.boot(repfiles,firstYear=firstYear,age0=age0)
#  CAA<-readCAA.boot(repfiles,firstYear=firstYear,age0=age0)
#  FAA<-readFAA.boot(repfiles,firstYear=firstYear,age0=age0,qt=qt)
  # browser()
  #
  MAA<-NAA#readVPA.boot(filenames$maa,firstYear=firstYear,age0=age0)
#  for(i in 1:dim(MAA[[1]])[4]){
#    for(j in 1:dim(MAA[[1]])[3]){
#      for(k in 1:dim(MAA[[1]])[1]){
#        for(m in 1:dim(MAA[[1]][2]))
#        MAA[[1]][k,m,j,i] <- nma$M[m]
#      }}}
  MAA[[1]]<-lapply(MAA[[1]],function(x){return(nma$M[1:nage])})
 # browser()
  IND<-0 # dummy
# 新しい順番 nyear*nage*nbox*nboot
# 古い順番 boot box year age

#######################################################################
#   tmp<-WAA*NAA$array[1,1,,]/1000
#  wnaa<-sweep(NAA$array,c(1,2),WAA,FUN="*")/1000
# browser()
  wnaa<-lapply(NAA$list,FUN=function(x){sweep(x,c(1,2),WAA,FUN="*")})
#  totBiomass<-apply(wnaa,c(2,3,4),FUN="sum")
  totBiomass<-lapply(wnaa,FUN=function(x){apply(x,2,sum)})
# browser()
  ZAA<-FAA
#  ZAA$array<-MAA$array+FAA$array
  ZAA$list<-mapply(FUN=function(i){MAA$list[[i]]+FAA$list[[i]]},1:length(MAA$list),SIMPLIFY=FALSE)
#  NAA.spawn<-NAA$array*exp(-ZAA$array*(spawnTime-1)/12)
  NAA.spawn<-mapply(FUN=function(i){NAA$list[[i]]*exp(-ZAA$list[[i]]*(spawnTime-1)/4)},1:length(MAA$list),SIMPLIFY=FALSE)  ## 要検討　2010/05/09
#browser()
#  wnaa.spawn<-sweep(NAA.spawn,c(1,2),WAA,FUN="*")/1000
  wnaa.spawn<-lapply(NAA.spawn,FUN=function(x){sweep(x,c(1,2),WAA,FUN="*")})
# browser()
#  spawner<-sweep(wnaa.spawn,2,maturity,FUN="*")
  spawner<-lapply(wnaa.spawn,FUN=function(x){sweep(x,2,maturity,FUN="*")})

#  browser()
#  SSB<-apply(spawner,c(2,3,4),FUN="sum")
  SSB<-lapply(spawner,function(x){apply(x,2,FUN="sum")})

  return(invisible(list(NAA=NAA,CAA=CAA,FAA=FAA,MAA=MAA,IND=IND,WAA=WAA,CWAA=CWAA,
                        maturity=maturity,SSB=SSB,totBiomass=totBiomass,nindices=nindices,spawnTime=spawnTime)))
}

readSS.boot.x<-function(bootfiles,firstYear=1,age0=1,to.array=FALSE,FUN){#filename,firstYear=1,age0=1){
#  res.tmp <- getNAA.ss2(bootfiles[1])[[1]]
  res.tmp <-FUN(bootfiles[1])
#  years <- res.tmp$Year[y.range <- (res.tmp$"Per"=="TIME")]
  years<-as.numeric(dimnames(res.tmp)[[1]])
#  tmp <- as.numeric(names(res.tmp))
#  ages <- tmp[a.range <- !is.na(tmp)]
  ages<-as.numeric(dimnames(res.tmp)[[2]])
  if(is.ss3(bootfiles[1])){ #真偽が逆？ 2010/05/06
    nboot<-length(bootfiles)
    offset<- 0
  }else{
    nboot<-length(bootfiles)-2
    offset<-2
  }

  if(to.array){
    mat.array <- array(0,dim=c(nboot,1,length(years),length(ages)))
    dimnames(mat.array) <- list(1:nboot,1,years,ages)

    for(i in 1:nboot){
      res.tmp <- getNAA.ss2(bootfiles[i+offset])[[1]]
      x <- res.tmp[y.range,a.range]
      mat.array[i,1,,] <- as.matrix(as.data.frame((lapply(x,as.numeric))))
    }
    #  nyear*nage*nbox*nboot
    mat.array<-aperm(mat.array,c(1,4,2,3))
    return(invisible(list(array=mat.array,nboot=nboot,
                          nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }else{
    mat.list<-lapply(1:nboot+offset,FUN=function(x){FUN(bootfiles[x+offset])})
    return(invisible(list(list=mat.list,nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }
}



readNAA.boot.x<-function(bootfiles,firstYear=1,age0=1,to.array=TRUE){#filename,firstYear=1,age0=1){
#  res.tmp <- getNAA.ss2(bootfiles[1])[[1]]
  res.tmp <-getNAA.y.ss3(bootfiles[1])
#  years <- res.tmp$Year[y.range <- (res.tmp$"Per"=="TIME")]
  years<-as.numeric(dimnames(res.tmp)[[1]])
#  tmp <- as.numeric(names(res.tmp))
#  ages <- tmp[a.range <- !is.na(tmp)]
  ages<-as.numeric(dimnames(res.tmp)[[2]])
  if(is.ss3(bootfiles[1])){ #真偽が逆？ 2010/05/06
    nboot<-length(bootfiles)
    offset<- 0
  }else{
    nboot<-length(bootfiles)-2
    offset<-2
  }

  if(to.array){
    mat.array <- array(0,dim=c(nboot,1,length(years),length(ages)))
    dimnames(mat.array) <- list(1:nboot,1,years,ages)

    for(i in 1:nboot){
      res.tmp <- getNAA.ss2(bootfiles[i+offset])[[1]]
      x <- res.tmp[y.range,a.range]
      mat.array[i,1,,] <- as.matrix(as.data.frame((lapply(x,as.numeric))))
    }
    #  nyear*nage*nbox*nboot
    mat.array<-aperm(mat.array,c(1,4,2,3))
    return(invisible(list(array=mat.array,nboot=nboot,
                          nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }else{
    mat.list<-lapply(1:nboot+offset,getNAA.y.ss3(bootfiles[i+offset]))
    return(invisible(list(list=mat.list,nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }
}

readCAA.boot.x<-function(bootfiles,firstYear=1,age0=1,to.array=TRUE){#filename,firstYear=1,age0=1){
#  res.tmp <- getCAA.ss2(bootfiles[1])[[1]]
  res.tmp <-getCAA.y.ss3.x(bootfiles[1])
#  years <- res.tmp$Year[y.range <- !is.na(res.tmp$"YQ")]
  years<-as.numeric(dimnames(res.tmp)[[1]])
#  tmp <- as.numeric(names(res.tmp))
#  ages <- tmp[a.range <- !is.na(tmp)]
  ages<-as.numeric(dimnames(res.tmp)[[2]])
  if(is.ss3(bootfiles[1])){
    nboot<-length(bootfiles)
    offset<-0
  }else{
    nboot<-length(bootfiles)-2
    offset<-2
  }

  if(to.array){
    mat.array <- array(0,dim=c(nboot,1,length(years),length(ages)))
    dimnames(mat.array) <- list(1:nboot,1,years,ages)

    for(i in 1:nboot){
      res.tmp <- getCAA.ss2(bootfiles[i+offset])[[1]]
      x <- res.tmp[y.range,a.range]
      mat.array[i,1,,] <- as.matrix(as.data.frame((lapply(x,as.numeric))))
    }
#  browser()
 #  nyear*nage*nbox*nboot
    mat.array<-aperm(mat.array,c(1,4,2,3))
    return(invisible(list(array=mat.array,nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }else{
    mat.list<-lapply(1:nboot+offset,getCAA.y.ss3(bootfiles[i+offset]))
    return(invisible(list(list=mat.list,nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }
}

readFAA.boot.x<-function(bootfiles,firstYear=1,age0=1,qt=1,to.array=TRUE){#filename,firstYear=1,age0=1){
#  browser()

#  res.tmp <- calFAA.ss2(bootfiles[1],qt=qt)
  res.tmp<-getFAA.y.ss3(bootfiles[1])
#  browser()
#  years <- res.tmp$naa$Year[res.tmp$naa$Per=="TIME"]
  years<-as.numeric(dimnames(res.tmp)[[1]])
#  res.tmp$faa[match(years,c(dimnames(res.tmp$faa)[[1]],2008))]
#  tmp <- as.numeric(names(res.tmp$naa))
#  ages <- tmp[a.range <- !is.na(tmp)]
  ages<-as.numeric(dimnames(res.tmp)[[2]])

  if(is.ss3(bootfiles[1])){
    nboot<-length(bootfiles)
    offset<-0
  }else{
    nboot<-length(bootfiles)-2
    offset<-2
  }
  if(to.array){
    mat.array <- array(0,dim=c(nboot,1,length(years),length(ages)))
    dimnames(mat.array) <- list(1:nboot,1,years,ages)
  }
  browser()
  if(to.array){
    for(i in 1:nboot){
      if(is.ss3(bootfiles[i+offset])){
        mat.array[i,1,,] <- calFAA.ss2(bootfiles[i+offset],qt=qt)$faa[-1,]
      }else{
        mat.array[i,1,,] <- calFAA.ss2(bootfiles[i+offset],qt=qt)$faa
      }
    }
  }else{
    mat.list<-lapply(1:nboot+offset,getFAA.y.ss3(bootfiles[i+offset]))
  }
#  nyear*nage*nbox*nboot
  if(to.array){
    mat.array<-aperm(mat.array,c(1,4,2,3))
    return(invisible(list(array=mat.array,nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }else{
    return(invisible(list(list=mat.list,nboot=nboot,
                        nbox=1,nyear=length(years),nage=length(ages),firstYear=min(years),age0=min(ages))))
  }
}


makeProjection2<-function(boot.out=NULL,Fyear=2002:2004,Ryear=1966:1998,nyear.proj=25,
  proj1stYr=2005,Fmult=1,nrandsim=10,geomean=TRUE,singleF=FALSE,nfixedCyear=2,fixedC=c(66000,85000),nreplRYear=1,saveResults=FALSE){
################
##
##  Fyear : vector of year to calculate current F
##  Ryear : vector of year to resample recruitments
##  nyear.proj : number of year to be projected from last year of VPA
##               i.e., future projection be done from proj1stYr to firstYear+nyear-1+nyear.proj
##  proj1stYr  : 1st year of future projection i.e., numbers at age at the beginning of proj1stYr is assumed to be known
##  nfixedCyear : number of years to be projected with constant yield
##
# require(snowfall)
  require(foreach)
#  require(doMC)
#  require(multicore)
#  registerDoMC(cores=4)
#  registerDoSEQ()
#  require(snow)
#  require(doSNOW)
#  cl<-makeSOCKcluster(rep("localhost",length=4))
#  on.exit(stopCluster(cl))
#  registerDoSNOW(cl)


  if(is.null(boot.out))stop("boot.out is required")  # boot.out<-readVPA.txt()

  NAA<-boot.out$NAA
  CAA<-boot.out$CAA
  nboot<-NAA$nboot
  nbox<-NAA$nbox
  nyear<-NAA$nyear
  nage<-NAA$nage
  age0<-NAA$age0
#  browser()
  if(dim(NAA$array)[4]==nboot){
    offset<-0
  }else{
    offset<-1
  }

  spawnTime<-boot.out$spawnTime
  firstYear<-NAA$firstYear
  dim2<-dim(NAA$array)[4]
#  browser()
#  browser()
  NAA.proj<-CAA.proj<-array(0,dim=c(nyear+nyear.proj,nage,nbox,dim2,nrandsim))
# dimnames(NAA.proj)<-list(boot=0:nboot,box=1:nbox,year=1:(nyear+nyear.proj)+firstYear-1,age=age0:(nage-age0+1))
  dimnames(NAA.proj)<-list(year=1:(nyear+nyear.proj)+firstYear-1,age=seq(from=age0,by=1,length=nage),
            box=1:nbox,boot=ifelse(dim2==(nboot+1),0,1):nboot,randsim=1:nrandsim)

#  CAA.proj<-array(0,dim=c(nyear+nyear.proj,nage,nbox,dim2,nrandsim))
# dimnames(CAA.proj)<-list(boot=0:nboot,box=1:nbox,year=1:(nyear+nyear.proj)+firstYear-1,age=age0:(nage-age0+1))
#        dimnames(CAA.proj)<-list(randsim=1:nrandsim,boot=ifelse(dim2==(nboot+1),0,1):nboot,box=1:nbox,year=1:(nyear+nyear.proj)+firstYear-1,
#                                 age=seq(from=age0,by=1,length=nage))
        dimnames(CAA.proj)<-dimnames(NAA.proj)
  FAA<-boot.out$FAA
  F<-FAA$array[paste(Fyear),,,]
#  browser()
  if(length(Fyear)!=1){
    if(nboot==0){  # 次元が退化した場合に対応
      F.dum<-F%o%rep(1,1)
#      F<-aperm(F.dum,c(3,1,2))
    }
#    browser()
    if(geomean){
      F<-exp(apply(log(F),if(nbox==1){c(2,3)}else{c(2,3,4)},"mean"))
    }else{
      F<-apply(F,if(nbox==1){c(2,3)}else{c(2,3,4)},"mean")
    }
  }

  if(nbox==1){
    F<-F%o%rep(1,1)
    F<-aperm(F,c(1,3,2))
  }

  F<-F*Fmult
################################################# ダミー次元を加えた 2010/4/3
  F<-F%o%rep(1,nrandsim)
#  F<-aperm(F.dum,c(4,1,2,3))

################################################# ダミー次元を加えた 2010/4/3
#  NAA.arry.dum0<-NAA$array%o%rep(1,nrandsim)
#  NAA.arry.dum<-aperm(NAA.arry.dum0,c(5,1,2,3,4))
#  CAA.arry.dum0<-CAA$array%o%rep(1,nrandsim)
#  CAA.arry.dum<-aperm(CAA.arry.dum0,c(5,1,2,3,4))

  NAA.arry.dum<-NAA$array%o%rep(1,nrandsim)
  CAA.arry.dum<-CAA$array%o%rep(1,nrandsim)

  NAA.proj[1:(proj1stYr-firstYear+1),,,,]<-NAA.arry.dum[1:(proj1stYr-firstYear+1),,,,]
  CAA.proj[1:(proj1stYr-firstYear+1),,,,]<-CAA.arry.dum[1:(proj1stYr-firstYear+1),,,,]

#  NAA.proj[,,,1:(proj1stYr-firstYear+1),]<-NAA$array[,,1:(proj1stYr-firstYear+1),]
#  CAA.proj[,,,1:(proj1stYr-firstYear+1),]<-CAA$array[,,1:(proj1stYr-firstYear+1),]
##############################################

##  Set up future recruitment

# browser()
  sizeFutureR<-nyear-(proj1stYr-firstYear+1)+nyear.proj+nreplRYear
#  futureR<-array(0,dim=c(nrandsim,dim2,nbox,sizeFutureR))

  futureR<-array(0,dim=c(sizeFutureR,nbox,dim2,nrandsim))

  for(s in 1:nrandsim){
    for(i in 1:dim2){
      for(b in 1:nbox){
        futureR[,b,i,s]<-sample(x=NAA$array[paste(Ryear),1,b,i],size=sizeFutureR,replace=TRUE)
      }
    }
  }

#  futureR<-
#  foreach(s=1:nrandsim,.combine=rbind)%:%
#    foreach(i=1:dim2,.combine=rbind)%:%
#      foreach(b=1:nbox,rbind)%do{
#        sample(x=NAA$array[i,b,paste(Ryear),1],size=sizeFutureR,replace=TRUE)
#      }

#  dim1<-if(length(dim(NAA$array[,,paste(Ryear),1]))==1){1}else{c(1,2)}
#  futureR<-apply(NAA$array[,,paste(Ryear),1],dim1,sample,size=nyear-(proj1stYr-firstYear+1)+nyear.proj,replace=TRUE)
# dim(futureR)<-dim(NAA.proj[,,,(proj1stYr-firstYear+2):(nyear+nyear.proj),1])

  NAA.proj[(proj1stYr-firstYear+2):(nyear+nyear.proj),1,,,]<-futureR[(1+nreplRYear):sizeFutureR,,,]
############## MAAにも次元を加える
  MAA<-boot.out$MAA
  MAA$array<-MAA$array%o%rep(1,nrandsim)
#  MAA$array<-aperm(MAA.dum,c(5,1,2,3,4))
#  MAA<-boot.out$MAA
###############################
## Calculate biomass and SSB time series
  WAA<-array(0,dim=c(nyear+nyear.proj,nage))
  WAA[1:nyear,]<-boot.out$WAA
  WAA[nyear+1:nyear.proj,]<-rep(1,nyear.proj)%o%WAA[nyear,]

#  browser()
##### Function to calculate yield given diffent F-multiplier
### 一次元版
calcYield.0<-function(Fmult,F,NAA,MAA,WAA){
  Fnew<-Fmult*F
  CAA<-NAA*(1-exp(-Fnew-MAA))*Fnew/(Fnew+MAA)
  WCAA<-CAA*WAA/1000
  Yield<-WCAA
  return(Yield)
}

#################################
### 多次元版
calcYield<-function(Fmult,F,NAA,MAA,WAA){
  Fnew<-Fmult*F
  dim(Fnew)<-dim(F)
  CAA<-NAA*(1-exp(-Fnew-MAA))*Fnew/(Fnew+MAA)
  dim(CAA)<-dim(NAA)
  WCAA<-CAA*WAA/1000
  Yield<-sum(WCAA)
  return(Yield)
}


solveByYield<-function(NAA,MAA,WAA,FAA,Yield){
  f<-function(x){
    return(Yield-calcYield(Fmult=x,F=FAA,NAA=NAA,MAA=MAA,WAA=WAA))
  }
##########################################################################
  temp<-uniroot(f=f,interval=c(0.1,10),tol=1.0e-6)
#  browser()
  return(temp$root)

}

    y<-proj1stYr-firstYear+2
#      F1<-FAA$array[,,y-1,]
      F1<-F
      dim(F1)<-dim(F)
      NAA.proj[y-1,1,,,]<-futureR[1,,,]  ##  今のところ1年しか過去の加入を置き換えない
 #     browser()
      NAA.proj[y,2:nage,,,]<-NAA.proj[y-1,1:(nage-1),,,]*exp(-F1[1:(nage-1),,,]-MAA$array[nyear,1:(nage-1),,,])
      NAA.proj[y,nage,,,]<-NAA.proj[y-1,nage,,,]*exp(-F1[nage,,,]-MAA$array[nyear,nage,,,])+NAA.proj[y,nage,,,]
      CAA.proj[y-1,1:nage,,,]<-NAA.proj[y-1,1:nage,,,]*
        (1-exp(-F1[1:nage,,,]-MAA$array[nyear,1:nage,,,]))*F1[1:nage,,,]/(F1[1:nage,,,]+MAA$array[nyear,1:nage,,,])
## MAA.projも定義した方がいい？要検討
#############################################
#  browser()
  if(nfixedCyear>0){  ##
#    FAA.fixedY<-array(0,dim=c(nrandsim,dim2,nbox,nfixedCyear,nage))
#    ZAA.fixedY<-array(0,dim=c(nrandsim,dim2,nbox,nfixedCyear,nage))
     FAA.fixedY<-ZAA.fixedY<-array(0,dim=c(nfixedCyear,nage,nbox,dim2,nrandsim))
    for(r in 1:nrandsim){
      for(bt in 1:dim2){
#      foreach(bt=1:nboot) %dopar% (
        for(b in 1:nbox){
          i<-0
#          browser()
          for(y in (proj1stYr-firstYear+3):(proj1stYr-firstYear+3+nfixedCyear-1)){
###################### determin Fmult #################################
            i<-i+1
### NAA.proj<-array(0,dim=c(nrandsim,dim2,nbox,nyear+nyear.proj,nage))
            Fmult.tmp<-solveByYield(NAA=NAA.proj[y-1,,b,bt,r],MAA=MAA$array[nyear,,b,bt,r],WAA=WAA[y,],FAA=F[,b,bt,r],Yield=fixedC[i])
###################### multiply F by Fmult ############################
            F.tmp<-F[,b,bt,r]*Fmult.tmp
#             browser()
            FAA.fixedY[i,,b,bt,r]<-F.tmp
            ZAA.fixedY[i,,b,bt,r]<-F.tmp+MAA$array[nyear,,b,bt,r]
#            cat(r,",",bt,",",b,",",y,",",Fmult.tmp,"\n")
            NAA.proj[y,2:nage,b,bt,r]<-NAA.proj[y-1,1:(nage-1),b,bt,r]*exp(-F.tmp[1:(nage-1)]-MAA$array[nyear,1:(nage-1),b,bt,r])
            NAA.proj[y,nage,b,bt,r]<-NAA.proj[y-1,nage,b,bt,r]*exp(-F.tmp[nage]-MAA$array[nyear,nage,b,bt,r])+NAA.proj[nyear,nage,b,bt,r]
###################### project N to next year #########################
            CAA.proj[y-1,,b,bt,r]<-NAA.proj[y-1,,b,bt,r]*(1-exp(-F.tmp[1:nage]-MAA$array[nyear,,b,bt,r]))*
              F.tmp/(F.tmp+MAA$array[nyear,,b,bt,r])
          }
        }
#      )
      }
    }
  }

  gc()
  gc()

  if(!singleF){
    for(y in (proj1stYr-firstYear+3+nfixedCyear):(nyear+nyear.proj)){
#     browser()
#      cat("y=",y,"\n")
      NAA.proj[y,2:nage,,,]<-NAA.proj[y-1,1:(nage-1),,,]*exp(-F[1:(nage-1),,,]-MAA$array[nyear,1:(nage-1),,,])
      NAA.proj[y,nage,,,]<-NAA.proj[y-1,nage,,,]*exp(-F[nage,,,]-MAA$array[nyear,nage,,,])+NAA.proj[y,nage,,,]
    }

  #### チェックする必要あり 2010/05/04
    for(y in (proj1stYr-firstYear+2+nfixedCyear):(nyear+nyear.proj)){
      CAA.proj[y,1:nage,,,]<-NAA.proj[y,1:nage,,,]*
        (1-exp(-F[1:nage,,,]-MAA$array[nyear,1:nage,,,]))*F[1:nage,,,]/(F[1:nage,,,]+MAA$array[nyear,1:nage,,,])
    }
  }else{
    F1<-F
    dim(F1)<-dim(F)
    for(i in 1:dim2){
      F1[,,i,]<-F[,,1,]
    }
    dim(F1)<-dim(F)
    for(y in (proj1stYr-firstYear+3+nfixedCyear):(nyear+nyear.proj)){
#    foreach(y=(proj1stYr-firstYear+3+nfixedCyear):(nyear+nyear.proj)) %do%
      NAA.proj[y,2:nage,,,]<-NAA.proj[y-1,1:(nage-1),,,]*exp(-F1[1:(nage-1),,,]-MAA$array[nyear,1:(nage-1),,,])
      NAA.proj[y,nage,,,]<-NAA.proj[y-1,nage,,,]*exp(-F1[nage,,,]-MAA$array[nyear,nage,,,])+NAA.proj[y,nage,,,]

    }
    for(y in (proj1stYr-firstYear+2+nfixedCyear):(nyear+nyear.proj)){
      CAA.proj[y,1:nage,,,]<-NAA.proj[y,1:nage,,,]*
        (1-exp(-F1[1:nage,,,]-MAA$array[nyear,1:nage,,,]))*F1[1:nage,,,]/(F1[1:nage,,,]+MAA$array[nyear,1:nage,,,])
    }
  }
  gc()
  gc()
# browser()
#  wnaa<-sweep(NAA.proj,c(3,4),WAA,FUN="*")/1000
#  totBiomass.proj<-apply(wnaa,c(1,2,3),FUN="sum")

  wnaa<-sweep(NAA.proj,c(1,2),WAA,FUN="*")/1000
  totBiomass.proj<-apply(wnaa,c(2,3,4,5),FUN="sum")

  maturity<-boot.out$maturity
  ZAA.proj<-array(0,dim=c(nyear+nyear.proj,nage,nbox,dim2,nrandsim))
#  ZAA.proj[,,,1:nyear,]<-aperm((boot.out$FAA$array+boot.out$MAA$array)%o%rep(1,nrandsim),c(5,1,2,3,4))
  ZAA.proj[1:nyear,,,,]<-(boot.out$FAA$array+boot.out$MAA$array)%o%rep(1,nrandsim)
#  tmp<-(F[,,1:nage]+MAA$array[,,nyear,1:nage])%o%rep(1,nyear-(proj1stYr-firstYear+1)+nyear.proj)
#  tmp<-aperm(tmp,c(1,3,2))
  if(!singleF){
    tmp<-(F[1:nage,,,]+MAA$array[nyear,1:nage,,,])%o%rep(1,nyear-(proj1stYr-firstYear+1)+nyear.proj)
#    tmp<-aperm(tmp,c(4,1,2,3))
  }else{
    F1<-F
    dim(F1)<-dim(F)
    for(i in 1:(nboot+1)){
      F1[,,i,]<-F[,,1,]
    }
    dim(F1)<-dim(F)
    tmp<-(F1[1:nage,,,]+MAA$array[nyear,1:nage,,,])%o%rep(1,nyear-(proj1stYr-firstYear+1)+nyear.proj)

  }
#  browser()
  if(length(dim(tmp))==4){
    tmp<-aperm(tmp,c(4,1,2,3))
  }else if(length(dim(tmp))==3){
    tmp<-aperm(tmp,c(3,1,2))
  }else{
    stop("ERROR")
  }
#  browser()
  if(nboot==0){  # 確認が必要
    tmp1<-tmp%o%rep(1,1)
#    tmp1<-aperm(tmp1,c())
    if(nrandsim==1){
      tmp<-tmp1%o%rep(1,1)
#      tmp1<-aperm(tmp2,c(1,2,3,4))
    }else{
      tmp<-aperm(tmp1,c(1,2,4,3))
    }
  }else{
  # browser()
#   if(singleF){
#     tmp<-tmp%o%rep(1,nboot+1)
#     tmp<-aperm(tmp,c(3,1,2))
#   }
 #  browser()
    if(nrandsim==1){
      tmp<-tmp%o%rep(1,1)
#      tmp<-aperm(tmp2,c(4,1,2,3))
    }
#    tmp<-aperm(tmp,c(1,4,2,3))
  }
#  browser()
#  tmp<-aperm(tmp,c(1,3,2,4))  順序があっているか要確認
  if(nbox==1){
    tmp<-tmp%o%rep(1,1)
#    tmp<-aperm(tmp,c(1,2,5,3,4))
    dim(tmp)<-c(dim(tmp)[1:2],1,dim(tmp)[3:4])
  }
#  browser()

#  ZAA.proj[,,(proj1stYr-firstYear+2):(nyear+nyear.proj),]<-tmp
#  NAA.spawn<-NAA.proj*exp(-ZAA.proj*(spawnTime-1)/12)
#  wnaa.spawn<-sweep(NAA.spawn,c(3,4),WAA,FUN="*")/1000
#  spawner<-sweep(wnaa.spawn,4,maturity,FUN="*")
#  SSB.proj<-array(0,dim=c(nboot+1,nbox,nyear+nyear.proj))
##   SSB.proj[,,1:nyear]<-boot.out$SSB
#  SSB.proj<-apply(spawner,c(1,2,3),FUN="sum")

  ZAA.proj[(proj1stYr-firstYear+2):(nyear+nyear.proj),,,,]<-tmp
### 固定の漁獲量の年はZを置き換える必要あり
#  ZAA.fixedY[r,bt,b,i,]
  if(nfixedCyear>0){
    for(r in 1:nrandsim){
      for(bt in 1:dim2){
        for(b in 1:nbox){
          i<-0
#          browser()
          for(y in (proj1stYr-firstYear+2):(proj1stYr-firstYear+2+nfixedCyear-1)){
###################### determin Fmult #################################
            i<-i+1
            ZAA.proj[y,,b,bt,r]<-ZAA.fixedY[i,,b,bt,r]
          }
        }
      }
    }
  }

  NAA.spawn<-NAA.proj*exp(-ZAA.proj*(spawnTime-1)/12)
  wnaa.spawn<-sweep(NAA.spawn,c(1,2),WAA,FUN="*")/1000
#  browser()
  spawner<-sweep(wnaa.spawn,2,maturity,FUN="*")
  SSB.proj<-array(0,dim=c(nyear+nyear.proj,nbox,dim2,nrandsim))
#  browser()
  dimnames(SSB.proj)[[3]]<-ifelse(dim2==(nboot+1),0,1):nboot
#   SSB.proj[,,1:nyear]<-boot.out$SSB
  SSB.proj<-apply(spawner,c(1,3,4,5),FUN="sum")
# browser()
  gc()
  gc()
  rt<-list(NAA=NAA.proj,CAA=CAA.proj,ZAA=ZAA.proj,SSB=SSB.proj,
    totBiomass=totBiomass.proj,WAA=WAA,F=if(singleF){F1}else{F},
    nyear=nyear,nyear.proj=nyear.proj,Fmult=Fmult,nboot=nboot)
  if(saveResults)save(rt,compress=TRUE,file=paste(Fmult,".r",sep=""))
  return(invisible(rt))
}