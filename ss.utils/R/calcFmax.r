################################################################################################
##
## Fmax‚ðŒvŽZ‚·‚é
##
##

calcFmax<-function(year,report=NULL,R0=1,spawnseason,faa=NULL,fmodifier=ifelse(!is.null(faa),rep(1,dim(faa$faa.array)[3]),NULL),
  fmort.list=NULL,stepsize=0.001,debug=FALSE,geomean=TRUE,interval=c(0,5),calcB=FALSE,SR=NULL,SRfn=NULL,tol=0.01,debug2=FALSE,fmult=1){
 # browser()
  if(is.null(faa) && is.null(fmort.list)  && !is.null(report) ){
    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
  }
  if(is.null(fmort.list) && !is.null(faa)){
    fmort.list<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,year=year,
      spawnseason=spawnseason,R0=R0,debug=debug,geomean=geomean)
  }
  if(is.null(fmodifier))fmodifier<-rep(1,fmort.list$nfleet)

  if(is.null(SR) && calcB){
    if(is.null(report))stop("either object SR or report is required with calcB in calcF0.1" )
    SR<-read.spawner_recruit(report=report,plot=FALSE)
  }
  if(!is.null(SRfn))SR$SRfn<-SRfn
#  R0msy<-SR$R0
  SSB0<-SR$SSB0
  SRfn<-SR$SRfn

######################################################################################################################
########################################################################################################################

  ## fmult==0‚Ìê‡
  ## f-multiplier‚ÍAcurrent-F*fmult‚É‘Î‚·‚éf-multiplier‚Å‚Í‚È‚­AcurrentF‚É‘Î‚·‚éf-multiplier
#  cat("Enter Fmax\n")
  if(debug)browser()
  ypr.maximizer<-optimize(f=function(x){
      fmultx<-x;
      temp<-getFmort.ypr.0(fmort.list=fmort.list,fmult=fmultx,fmodifier=fmodifier,
      year=year,R0=R0,spawnseason=spawnseason,geomean=geomean,debug2=debug2);

      return(temp$ypr)
    },interval=interval,maximum=TRUE)
  ypr<-getFmort.ypr.0(faa=faa,year=year,fmult=ypr.maximizer$maximum,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,
    fmodifier=fmodifier,debug=debug,geomean=geomean)
  if(calcB){
     ypr<-calcB(SR=SR,ypr=ypr,debug2=debug2,tol=tol)
#    cnt<-0
#    R<-SRfn(ypr$spr*R0)
#    diff<-abs(R0-R)
#    if(debug2)cat(paste("R0,R,SSB,diff:",prettyNum(R0),prettyNum(R),prettyNum(ypr$spr*R),prettyNum(diff),"\n"))
#    while(cnt<100 && diff>tol){
#      Rnew<-SR$SRfn(ypr$spr*R)
#      if(debug2)cat(paste("R,Rnew,SSB:",prettyNum(R),prettyNum(Rnew),prettyNum(ypr$spr*R)),"\n")
#      diff<-abs(R-Rnew)
#      R<-Rnew
#      cnt<-cnt+1
#    }
#    if(cnt>99){cat(paste("cnt=",cnt," in calcFmax"));browser()}
#    ypr$SSB<-ypr$spr*R
#    ypr$Yield<-ypr$ypr*R
#    ypr$Req<-R
  }else{
    ypr$SSB<-NA
    ypr$Yield<-NA
    ypr$Req<-NA
  }
#    ypr$rel_ypr<-ypr$ypr/ypr$ypr
#  cat("Done Fmax\n")
###############################################################################################################################
#################################################################################################################################

  return(ypr)
}
