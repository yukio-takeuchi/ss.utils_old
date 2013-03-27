#
# Last updated 2012/11/29
#
calcFloss<-function(year,report=NULL,spawnseason=NA,fmult=1,fmodifier=NULL,SR=NULL,range=c(0,5),tol=0.01,debug=FALSE,
              faa=NULL,nboot=1,fmort.list=NULL,geomean=TRUE,SRfn=NULL,calcB=FALSE,debug2=FALSE,nyears=1){
  if(is.null(faa) && is.null(fmort.list)   && !is.null(report)){
    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
  }
  if(is.null(fmort.list) && !is.null(faa)){
    fmort.list<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,year=year,
      spawnseason=spawnseason,R0=1,debug=debug,geomean=geomean)
  }
  if(is.null(fmodifier))fmodifier<-rep(1,fmort.list$nfleet)

  if(is.null(SR)){
    if(!is.null(report)){
      SR<-read.spawner_recruit(report=report,plot=FALSE)
    }else{
      stop("either SR or report is necessary for calcFloss")
    }
  }
  if(!is.null(SRfn))SR$SRfn<-SRfn
#  browser()
  calcFloss.0<-function(SR,faa,year,spawnseason,range,tol,debug=FALSE,geomean,nyears=1,debug2=FALSE){
    orderSSB<-order(SR$sr[SR$sr[,9] %in% c("Main","Early"),"spawn_bio"])
    lowestRPS<-mean(SR$sr[SR$sr[,9] %in% c("Main","Early"),][orderSSB[1:nyears],"RPS"])
#    lowestRPS<-mean(SR$sr[orderSSB[1:nyears],"RPS"])
    cat("HERE27 in calcFloss\n")
#    browser()
    
    if(debug2)cat("lowestRPS:",SR$sr[SR$sr[,9] %in% c("Main","Early"),][orderSSB[1:nyears],"RPS"],"\n")
#    lowestSSB<-min(SR$sr[SR$sr[,9]=="Main","spawn_bio"])
#    lowestYear<-unique(as.numeric(SR$sr[SR$sr[,2]==lowestSSB,"year"]))
#    lowestRPS<-unique(SR$sr[SR$sr[,2]==lowestSSB,"RPS"])
    lowestSPR<-1/lowestRPS
    FmultFloss<-SPR2fmult(spr=lowestSPR,faa=faa,year=year,R0=1,spawnseason=spawnseason,fmort.list=fmort.list,
      fmodifier=fmodifier,range=range,tol=tol,debug=debug,geomean=geomean,debug2=debug2) #lowestSPRÇÃéûÇÃFÇåvéZ
    return(FmultFloss)
  }

  FmultFloss<-calcFloss.0(SR=SR,faa=faa,year=year,spawnseason=spawnseason,
        range=range,tol=tol,debug=debug,geomean=geomean,nyears=nyears,debug2=debug2)
  if(nboot>1){
    FUNC.finally<-function(SR){
      lowestSSB<-min(SR$sr[SR$sr[,9]=="Main","spawn_bio"])
      lowestYear<-unique(as.numeric(SR$sr[SR$sr[,2]==lowestSSB,"year"]))
      lowestRPS<-unique(SR$sr[SR$sr[,2]==lowestSSB,"RPS"])
      lowestSPR<-1/lowestRPS
      return(list(lowestSSB,lowestYear,lowestRPS,lowestSPR))
    }
    FmultFloss.vect<-sapply(1:nboot,FUN=function(i){
        sr.tmp<-SR$sr[SR$sr[,9]=="Main",]
        sr.new<-sr.tmp[sample(1:nrow(sr.tmp),replace=TRUE),]
        SR.new<-list(sr=sr.new)

        return.val<-try(
          calcFloss.0(SR=SR.new,faa=faa,year=year,spawnseason=spawnseason,
              range=range,tol=tol,debug=debug,geomean=geomean,nyears=nyears),
          TRUE)
        cat(i,return.val,"\n")
        return(return.val)
      }
    )
  }
  R0<-SR$R0
  ypr<-getFmort.ypr.0(fmult=FmultFloss,fmodifier=fmodifier,year=year,faa=faa,fmort.list=fmort.list,R0=R0,spawnseason=spawnseason)
  if(calcB){
    ypr<-calcB(SR=SR,ypr=ypr,debug2=debug2,tol=tol)
#    cnt<-0
#    R<-SR$SRfn(ypr$spr*R0)
#    diff<-abs(R0-R)
#    if(debug2)cat(paste("R0,R,SSB,diff:",R0,R,ypr$spr*R,diff,"\n"))
#    while(cnt<100 && diff>tol){
#      Rnew<-SR$SRfn(ypr$spr*R)
#      diff<-abs(R-Rnew)
#      if(debug2)cat(paste("R,Rnew,SSB,diff:",R,Rnew,ypr$spr*R,diff,"\n"))
#      R<-Rnew
#      cnt<-cnt+1
#    }
#    if(cnt>99){cat(paste("cnt=",cnt," in calcFloss"));browser()}
#    ypr$SSB<-ypr$spr*R
#    ypr$Yield<-ypr$ypr*R
#    ypr$Req<-R
  }else{
    ypr$SSB<-NA
    ypr$Yield<-NA
    ypr$Req<-NA
  }
  if(nboot>1){
#    return(list(FmultFloss=FmultFloss,FmultFloss.vect=FmultFloss.vect))
    return(list(ypr=ypr,FmultFloss.vect=FmultFloss.vect))
  }else{
#  return(FmultFmed)
    return(ypr)
#    return(FmultFloss)
  }
}