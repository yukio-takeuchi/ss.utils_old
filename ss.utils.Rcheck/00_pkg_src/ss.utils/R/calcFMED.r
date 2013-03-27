###################################################################################
##
##  Fmed ‚ðŒvŽZ‚·‚é
##

calcFmed<-function(year,report=NULL,spawnseason,fmult=1,fmodifier=NULL,SR=NULL,range=c(0,5),
  tol=0.01,debug=FALSE,faa=NULL,fmort.list=NULL,geomean=TRUE,calcB=FALSE,SRfn=NULL,debug2=FALSE){

  if(is.null(faa) && is.null(fmort.list)){
    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
  }
  if(is.null(fmort.list) && !is.null(faa)){
    fmort.list<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,year=year,
      spawnseason=spawnseason,R0=R0,debug=debug,geomean=geomean,debug2=debug2)
  }

  if(is.null(fmodifier))fmodifier<-rep(1,fmort.list$nfleet)
  if(is.null(SR)){
    if(!is.null(report)){
      SR<-read.spawner_recruit(report=report,plot=FALSE)
    }else{
      stop("either SR or report is necessary for calcFmed")
    }
  }
  if(!is.null(SRfn))SR$SRfn<-SRfn
  R0<-SR$R0
  SSB0<-SR$SSB0

  RPSmed<-median(SR$sr[SR$sr[,9]=="Main","RPS"])
  SPRmed<-1/RPSmed
#  if(debug2){cat("in calcFmed\n");browser()}
  FmultFmed<-SPR2fmult(spr=SPRmed,faa=faa,year=year,R0=1,spawnseason=spawnseason,fmort.list=fmort.list,
    fmodifier=fmodifier,range=range,tol=tol,debug=debug,geomean=geomean,debug2=debug2)
  ypr<-getFmort.ypr.0(fmult=FmultFmed,fmodifier=fmodifier,year=year,faa=faa,fmort.list=fmort.list,R0=R0,spawnseason=spawnseason,debug2=debug2)
#  return(FmultFmed)
  if(calcB){
    ypr<-calcB(SR=SR,ypr=ypr,debug2=debug2,tol=tol)
#
#    cnt<-0
#    R<-SR$SRfn(ypr$spr*R0)
#    diff<-abs(R0-R)
#    if(debug2)cat(paste("R0,R,SSB,diff:",R0,R,ypr$spr*R,diff,"\n"))
#    while(cnt<100 && diff>tol){
#      Rnew<-SR$SRfn(ypr$spr*R)
#      diff<-abs(R-Rnew)
#      if(debug2)cat(paste("R,Rnew,SSB,diff:",R,Rnew,ypr$spr*R,diff))
#       R<-Rnew
#      cnt<-cnt+1
#    }
#    if(cnt>99){cat(paste("cnt=",cnt," in calcFmed"));browser()}
#    ypr$SSB<-SPRmed*R
#    ypr$Yield<-ypr$ypr*R
#    ypr$Req<-R
  }else{
    ypr$SSB<-NA
    ypr$Yield<-NA
    ypr$Req<-NA
  }
  return(ypr)
}
