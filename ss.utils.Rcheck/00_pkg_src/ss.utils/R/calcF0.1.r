################################################################################################
##
## F0.1‚ðŒvŽZ‚·‚é
##
##

calcF0.1<-function(year,report=NULL,R0=1,spawnseason,faa=NULL,fmodifier=ifelse(!is.null(faa),rep(1,dim(faa$faa.array)[3]),NULL),
  fmort.list=NULL,stepsize=0.001,debug=FALSE,geomean=TRUE,calcB=FALSE,SR=NULL,SRfn=NULL,tol=0.01,debug2=FALSE,fmult.Fmax=NA){
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

#  debug2<-T
  ypr0<-getFmort.ypr.0(fmult=0,fmodifier=fmodifier,year=year,faa=faa,fmort.list=fmort.list,R0=R0,spawnseason=spawnseason)
  if(debug2){cat("ypr0=");print(ypr0$ypr)}
  maximize<-ifelse(is.na(fmult.Fmax),TRUE,FALSE)
  ypr1<-getFmort.ypr(fmult=stepsize,fmodifier=fmodifier,year=year,faa=faa,fmort.list=fmort.list,
    R0=R0,spawnseason=spawnseason,maximize=maximize)
  if(debug2){cat("ypr1=");print(ypr1$ypr)}
# browser()
  slopeOrigin<-(ypr1$ypr-ypr0$ypr)/stepsize
  slope01<-slopeOrigin*0.1
  if(debug2)cnt<-0
  slope<-function(x){
    fmultx<-x
 #    browser()
    ypr0<-getFmort.ypr.0(faa=faa,fmort.list=fmort.list,fmult=fmultx,fmodifier=fmodifier,year=year,R0=R0,spawnseason=spawnseason)
    fmultx<-(x+stepsize)
    ypr1<-getFmort.ypr.0(faa=faa,fmort.list=fmort.list,fmult=fmultx,fmodifier=fmodifier,year=year,R0=R0,spawnseason=spawnseason)
    slope<-(ypr1$ypr-ypr0$ypr)/stepsize
    if(debug2)cnt<-cnt+1
    if(debug2)cat("slope=",slope," counter=",cnt,"\n")
    if(debug2)cat("x=",x," x+stepsize=",x+stepsize,"\n")
    if(debug2)cat("slope-slope01=",slope-slope01,"\n")
    return(slope-slope01)
  }
  if(debug)browser()
  #browser()
  fmult.upper<-ifelse(maximize,ypr1$ypr.opt$fmult,fmult.Fmax)
#  browser()
  temp<-uniroot(f=slope,interval=c(0,fmult.upper))
  ypr<-getFmort.ypr.0(fmult=temp$root,fmodifier=fmodifier,year=year,faa=faa,fmort.list=fmort.list,R0=R0,spawnseason=spawnseason)
  if(calcB){
    ypr<-calcB(SR=SR,ypr=ypr,debug2=debug2,tol=tol)
#    cnt<-0
#    R<-SR$SRfn(ypr$spr*R0)
#    diff<-abs(R0-R)
#    if(debug2)cat(paste("R0,R,SSB,diff:",R0,R,ypr$spr*R,diff,"\n"))
#    while(cnt<100 && diff>tol){
#      Rnew<-SR$SRfn(ypr$spr*R)
#      diff<-abs(R-Rnew)
#      if(debug2)cat(paste("R,Rnew,SSB,diff:",R,Rnew,ypr$spr*R,diff))
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
  return(ypr)
}
