################################################################################################
##
## FMSY‚ðŒvŽZ‚·‚é
##
##

calcFMSY<-function(year,report=NULL,R0=1,spawnseason,faa=NULL,fmodifier=ifelse(!is.null(faa),rep(1,dim(faa$faa.array)[3]),NULL),
  fmort.list=NULL,debug=FALSE,SR=NULL,calcB=FALSE,geomean=TRUE,interval=c(0,5),tol=0.01,SRfn=NULL,debug2=FALSE,fmult=1){
 # browser()
  if(is.null(faa) && is.null(fmort.list) && !is.null(report)){
    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
  }
  if(is.null(fmort.list) && !is.null(faa)){
    fmort.list<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,year=year,
      spawnseason=spawnseason,R0=R0,debug=debug,geomean=geomean)
  }
  if(is.null(fmodifier))fmodifier<-rep(1,fmort.list$nfleet)

  if(is.null(SR)){
    if(is.null(report))stop("either object SR or report is required to calculate MSY" )
    SR<-read.spawner_recruit(report=report,plot=FALSE)
  }
#  browser()
  if(!is.null(SRfn))SR$SRfn<-SRfn
#  R0msy<-SR$R0
  SSB0<-SR$SSB0
  spr0<-calcSPR.ypr(faa=faa,year=year,fmult=0,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier)$spr
  if(debug){
    spr1<-calcSPR.ypr(faa=faa,year=year,fmult=0,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier)
#    browser()
  }
  if(debug)cat("SSB0=",SSB0," spr0=",spr0,"\n")  #;browser()
#  SRfn<-SR$SRfn
##############################################################################################
#  Function to calculate equiribrium Yield given Fmultiplier
#
#  if(0){
  tmp.fn<-function(x){
    spr<-calcSPR.ypr(faa=faa,year=year,fmult=x,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier)$spr
    tmp.fn1<-if(debug){
#      function(SSB){cat(paste("\nSSB=",prettyNum(SSB)," RPS=",prettyNum(SR$SRfn(SSB))," SSB/RPS=",prettyNum(SSB/SR$SRfn(SSB))," spr=",prettyNum(spr)));tmp<-SSB/SR$SRfn(SSB)-spr;cat(" diff=",tmp);cat(" ; x=",x);return(tmp)}
      function(SSB){cat(paste("\nSSB=",prettyNum(SSB)," RPS=",prettyNum(SR$SRfn(SSB))," R*spr=",prettyNum(spr*SR$SRfn(SSB))," spr=",prettyNum(spr)));tmp<-SSB-spr*SR$SRfn(SSB);cat(" diff=",tmp);cat(" ; x=",x);return(tmp)}
    }else{
#      function(SSB){SSB/SR$SRfn(SSB)-spr}
      function(SSB){SSB-spr*SR$SRfn(SSB)}
    }
    tmp1<-tmp.fn1(0.00000000000001)
    tmp2<-tmp.fn1(SSB0)
    if(debug)cat("\ntmp.fn1 at ends",tmp1," ; ",tmp2,"\n")
    if(debug)browser()

    if(tmp1*tmp2<0){
    SSBeq<-uniroot(f=tmp.fn1,interval=c(0.00000000000001,SSB0),tol=min(tol,1.0e-6))$root
    Req<-SR$SRfn(SSBeq)
    ## return value : equiribrium yield given Fmult
    ypr.0<-getFmort.ypr.0(faa=faa,year=year,fmult=x,R0=Req,spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier)
    ypr<-ypr.0$Yield
    }else{
      ypr<--1000
    }
    if(debug){cat("\nypr=",ypr,"\n");browser()}
    return(ypr)
  }
#  }
#  tmp.fn<-function(x){
#    spr1<-calcSPR.ypr(faa=faa,year=year,fmult=x,R0=SR$R0,spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier)

#  }
##############################################################################################
  SY.maximizer<-optimize(f=tmp.fn,interval=interval,maximum=TRUE)
  ypr.MSY<-getFmort.ypr.0(faa=faa,year=year,fmult=SY.maximizer$maximum,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,
    fmodifier=fmodifier,debug=debug)

  if(calcB){
#    browser()
    ypr.MSY$SSB<-ypr.MSY$SSB_MSY<-calcB(SR=SR,ypr=ypr.MSY,debug2=debug2,tol=tol)$SSB
#    ypr.MSY$Yield<-ypr.MSY$MSY<-ypr.MSY$ypr*SR$SRfn(ypr.MSY$SSB_MSY)
    ypr.MSY$Yield<-ypr.MSY$MSY<-SY.maximizer$objective
    ypr.MSY$R<-ypr.MSY$R_MSY<-SR$SRfn(ypr.MSY$SSB_MSY)
  }else{
    ypr.MSY$SSB<-NA
    ypr.MSY$Yield<-NA
    ypr.MSY$Req<-NA
  }
  ypr.MSY$rel_ypr<-tmp.fn(x=1)/ypr.MSY$MSY
#################################################################################

  return(ypr.MSY)
}

