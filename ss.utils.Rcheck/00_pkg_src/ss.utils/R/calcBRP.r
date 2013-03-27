##
## 2011/06/07 new option : nyears.Floss : numbers of years to calculate mean RPS corresponding to  Floss were added
##

calcBRP<-calcBRP.20110315_2<-function(runname,repfile="report.sso",faa=NULL,multiFAA=FALSE,SR=NULL,
  SRfn=NULL,year,spawnseason=4,fmodifier,Fmax=TRUE,FMSY=FALSE,
  FSPR=c(10,20,30,40),F0.1=TRUE,Fmed=TRUE,Floss=TRUE,tol=0.01,range=c(0,5),debug=FALSE,geomean=TRUE,
  inverse=FALSE,calcB=TRUE,debug2=FALSE,nyears.Floss=3){
  if(missing(year))stop("year is missing")
  if(missing(runname))runname<-""
  if(!is.null(faa) && !multiFAA){
#    cat("HERE\n")
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
    if(is.null(SR) && !file.exist(repfile)){
      report<-getReport.sso(repfile=repfile)
      SR<-read.spawner_recruit(report=report,plot=FALSE)
    }else{
      stop(paste(repfile," does not exist"))
    }
    if(!is.null(SRfn))SR$SRfn<-SRfn
    R0<-ifelse(calcB,SR$R0,1)
    yprs<-calcBRP.0(runname=faa$repfile,faa=faa,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
      Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug,SR=SR,
      geomean=geomean,inverse=inverse,calcB=calcB,R0=R0,debug2=debug2,nyears.Floss=nyears.Floss)
    BRPmatF<-yprs$BRPvectF
    if(calcB){BRPmatB<-yprs$BRPvectB}else{BRPmatB<-NULL}
    cat("done\n")
  }else{
    if(is.null(faa) | is.null(SR)) reports<-getReport.sso(repfile=repfile,oldStyle=TRUE)
    if(is.null(faa)){
      if(class(reports)=="report.trad.list"){
        faas<-lapply(reports[[1]],FUN=function(report){
        #  report1<-list(report=list(report=report),n=1)
        #  class(report1)<-"report.list"
          calFAA.ss3.x(report=report,is.plot=FALSE)})
      }else{
        faas<-calFAA.ss3.x(report=reports,is.plot=FALSE)
      }
    }else{
      faas<-faa
    }
    if(is.null(SR)){
      if(class(reports)=="report.trad.list"){
        SRs<-lapply(reports[[1]],FUN=function(report){
          report1<-list(report=list(report=report),n=1)
          class(report1)<-"report.list"
          read.spawner_recruit(report=report1,plot=FALSE)})
      }else{
        SRs<-read.spawner_recruit(report=reports,plot=FALSE)
      }
    }
    if(!is.null(SRfn)){
      if(length(SRfn)==length(SRs)){
        for(i in 1:length(SRfn))SRs[[i]]$SRfn<-SRfn[i]
      }
    }
    if(class(reports)=="report.trad.list"){
      if(missing(fmodifier))fmodifier<-lapply(faas,FUN=function(faa){rep(1,dim(faa$faa.array)[3])})
    }else{
      if(missing(fmodifier))fmodifier<-rep(1,dim(faas$faa.array)[3])
    }
    BRPmatF<-NULL
    BRPmatB<-NULL
    BRPmatY<-NULL
    yprs<-list()
    if(class(reports)!="report.trad.list"){faas<-list(faa=faas);SRs<-list(SR=SRs)}
    for(i in 1:length(faas)){
      if(debug)browser()
      R0<-ifelse(calcB,SRs[[i]]$R0,1)
      yprs[[i]]<-calcBRP.0(runname=faas[[i]]$repfile,faa=faas[[i]],year=year,spawnseason=spawnseason,fmodifier=fmodifier[[i]],
        Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug,SR=SRs[[i]],geomean=geomean,
        inverse=inverse,R0=R0,calcB=calcB,debug2=debug2,nyears.Floss=nyears.Floss)
    #    browser()
      BRPmatF<-rbind(BRPmatF,yprs[[i]]$BRPvectF)
      BRPmatB<-rbind(BRPmatB,yprs[[i]]$BRPvectB)
      BRPmatY<-rbind(BRPmatY,yprs[[i]]$BRPvectY)
      if(length(faas)>1)rownames(BRPmatF)[i]<-rownames(BRPmatB)[i]<-rownames(BRPmatY)[i]<-repfile[i]
      cat(paste("Done", repfile[i],"\n"))
    }
  }
  BRP<-list(runname=runname,repfile=repfile,year=year,spawnseason=spawnseason,
    BRPmatF=BRPmatF,BRPmatB=BRPmatB,BRPmatY=BRPmatY,ypr=yprs)
  class(BRP)<-"BRPnew"
#  browser()
  return(BRP)
}

print.BRPnew<-function(BRP,digits= max(3, getOption("digits") - 3)){
  cat("Fishing mortality:\n")
  print.default(formatC(BRP$BRPmatF,digits=digits),quote=FALSE)
  if(!is.null(BRP$BRPmatB)){
    cat("\nSSB:\n")
    print.default(formatC(BRP$BRPmatB,digits=6),quote=FALSE)
  }
  if(!is.null(BRP$BRPmatY)){
    cat("\nequiribrium Yield:\n")
    print.default(formatC(BRP$BRPmatY,digits=6),quote=FALSE)
  }
}

