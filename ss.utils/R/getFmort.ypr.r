#
#  YPRをss2での漁獲量の計算方法に基づいて計算する
#  年単位、あるいは年の範囲で指定する
#
#  fmult : Fへのmultiplier すべての漁業に一律にかける
#  fmodifier : 漁法ごとに、Fを何倍するかを指定する
#  例 : getFmort.ypr(year=2004,faa=faa)
#  2008/5/4 コードの整理とfmultをfmultとfmodifierに分けた
#  faa : calcFAA.ss2の返りオブジェクト、
#
#  2009/09/12 defaultの引数を変更した
#  faa とfmot.listの両方が与えられた際には、fmort.listを優先する
##
## 2009/10/10  report を与えられた場合にはそれを最優先する
##
## 2011/03/16  calcFAA.yprでfmort.listを計算するようにした
##
getFmort.ypr<-function(report=NULL,faa=NULL,year,fmult=1,R0=1,spawnseason=4,maximize=FALSE,fmult.opt=NA,fmort.list=NULL,
  fmodifier=ifelse(!is.null(faa),rep(1,dim(faa$faa.array)[3]),fmort.list[["fmodifier"]]),
  stepsize=0.001,interval=c(0,5),F0.1=FALSE,debug=FALSE,FMSY=FALSE,SR=NULL,
  tol=0.001,geomean=TRUE,calcB=FALSE,SRfn=NULL,debug2=FALSE,FMAX=FALSE){
#  cat("Enter getFmort.ypr\n")
#  browser()
  if(is.null(year))stop("year is required for getFmort.ypr\n")
  if(!is.null(report)){
#   report<-getReport.sso(repfile=repfile)
    faa<-calcFAA(report=report,is.plot=FALSE)
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
  }
  if(is.null(fmort.list)){

#    browser()
    fmort.list<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,
      year=year,spawnseason=spawnseason,R0=R0,debug=debug,geomean=geomean)
#    browser()
  }
  if((FMSY || calcB) && is.null(SR)){
    if(is.null(report))stop("either object SR or report is required to calculate MSY" )
    SR<-read.spawner_recruit(report=report,plot=FALSE)
    if(!is.null(SRfn))SR$SRfn<-SRfn
  }

  ypr<-getFmort.ypr.0(faa=faa,year=year,fmult=fmult,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,
    fmodifier=fmodifier,debug=debug,geomean=geomean,debug2=debug2)

  if(maximize || FMAX){
  ## fmult==0の場合
  ## f-multiplierは、current-F*fmultに対するf-multiplierではなく、currentFに対するf-multiplier
#  cat("Enter Fmax\n")
        ypr.opt<-calcFmax(fmodifier=fmodifier,year=year,R0=R0,spawnseason=spawnseason,
      fmort.list=ypr$fmort,stepsize=stepsize,SR=SR,calcB=calcB,debug=debug,debug2=debug2,fmult=fmult,interval=interval)

#######################################
    if(0){

    if(debug)browser()
    ypr.maximizer<-optimize(f=function(x){
        fmultx<-x;
        temp<-getFmort.ypr.0(fmort.list=ypr$fmort,fmult=fmultx,fmodifier=fmodifier,
        year=year,R0=R0,spawnseason=spawnseason,geomean=geomean,debug2=debug2);

        return(temp$ypr)
      },interval=interval,maximum=TRUE)
    ypr.opt<-getFmort.ypr.0(faa=faa,year=year,fmult=ypr.maximizer$maximum,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,
      fmodifier=fmodifier,debug=debug,geomean=geomean)
    if(calcB){
      cnt<-0
      R<-SR$SRfn(ypr.opt$spr*R0)
      diff<-abs(R0-R)
      if(debug2)cat(paste("R0,R,SSB,diff:",R0,R,ypr$spr*R,diff,"\n"))
      while(cnt<100 && diff>tol){
        Rnew<-SR$SRfn(ypr.opt$spr*R)
        if(debug2)cat(paste("R,Rnew,SSB:",R,Rnew,ypr.opt$spr*R))
        diff<-abs(R-Rnew)
        R<-Rnew
        cnt<-cnt+1
      }
      if(cnt>99){cat(paste("cnt=",cnt," in calcFmax"));browser()}
      ypr.opt$SSB<-ypr.opt$spr*R
      ypr.opt$Yield<-ypr.opt$ypr*R
      ypr.opt$Req<-R
    }else{
      ypr.opt$SSB<-NA
      ypr.opt$Yield<-NA
      ypr.opt$Req<-NA
    }
    }
    ypr.opt$rel_ypr<-ypr$ypr/ypr.opt$ypr
    cat("Done Fmax\n")
  }else{
    ypr.opt<-NULL
  }
  if(F0.1){
    ypr.0.1<-calcF0.1(fmodifier=fmodifier,year=year,R0=R0,spawnseason=spawnseason,
      fmort.list=ypr$fmort,stepsize=stepsize,SR=SR,calcB=calcB,fmult.Fmax=ifelse(maximize,ypr.opt$fmult,NA),debug=debug,debug2=debug2)
    cat("Done F0.1\n")
  }else{
    ypr.0.1<-NULL
  }
# New 2010/10/09 Calculation of MSY
# Maximize equiribrium Yield
# In each step given Stock-Recruitment function and F, claculate equiribrium SSB, Recruitment and Yield

  if(FMSY){
    ypr.MSY<-calcFMSY(year=year,R0=R0,spawnseason=spawnseason,faa=faa,fmodifier=fmodifier,
      fmort.list=fmort.list,debug=debug,SR=SR,calcB=calcB,geomean=geomean,interval=interval,tol=tol,SRfn=SRfn)
    cat("Done FMSY\n")
  }else{
    ypr.MSY<-NULL
  }

  ypr.obj<-c(ypr,list(ypr.opt=ypr.opt,ypr.0.1=ypr.0.1,ypr.MSY=ypr.MSY))
  class(ypr.obj)<-"ypr"
  return(ypr.obj)
}


print.ypr<-function(ypr.obj){
 print(c(ypr.obj$fmult,ypr.obj$ypr,ypr.obj$perc_spr))
}

