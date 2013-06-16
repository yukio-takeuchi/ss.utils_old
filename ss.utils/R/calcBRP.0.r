#### 2010/02/23 Floss の計算も組み込んだ
#### 2010/10/09 Inplemented calculation of FMSY given S-R relationship
#### 2011/03/21 chenged return value to list of F-BRPs, B-BRPs and associated results of calculations
calcBRP.0<-function(runname,report=NULL,year,spawnseason=4,fmodifier,Fmax=TRUE,FSPR=c(10,20,30,40),F0.1=TRUE,
  Fmed=TRUE,Floss=TRUE,FMSY=TRUE, tol=0.01,range=c(0,5),debug=FALSE,debug2=FALSE,faa=NULL,fmult=1,
  geomean=TRUE,R0=1,SR=NULL,inverse=FALSE,calcB=FALSE,SRfn=NULL,nyears.Floss=1){
  if(is.null(faa)){
    if(!is.null(report)){
      faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
    }else{
      stop("Either faa or report is needed in calcBRP.0")
    }
  }
  if(is.null(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
  if(missing(runname))runname<-""
  cnt<-0
  if(Fmax)cnt<-cnt+1
  if(FMSY)cnt<-cnt+1
  if(F0.1)cnt<-cnt+1
  if(Fmed)cnt<-cnt+1
  if(Floss)cnt<-cnt+1
  if(!is.logical(FSPR))cnt<-cnt+length(FSPR)
  fmort.list<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,year=year,spawnseason=spawnseason,R0=R0,debug=debug,geomean=geomean)
  if((FMSY||Fmed||Floss||!is.logical(FSPR)) && is.null(SR)){
    SR<-read.spawner_recruit(report=report,plot=FALSE)
    if(!is.null(SRfn))SR$SRfn<-SRfn
  }
 # Fmax, F0.1 and Fmsy
  if(Fmax || F0.1 || FMSY){
#    cat("in calcBRP.0\n")
#    browser()
    ypr1<-getFmort.ypr(fmult=fmult,fmort.list=fmort.list,year=year,spawnseason=spawnseason,
      fmodifier=fmodifier,maximize=Fmax,F0.1=F0.1,FMSY=FMSY,debug=debug,interval=range,SR=SR,R0=R0,calcB=calcB,debug2=debug2)
#    cat("Done getFmort.ypr\n")
  }
  if(Fmed){
#    browser()
    yprFmed<-calcFmed(fmult=fmult,year=year,report=report,spawnseason=spawnseason,
      fmort.list=fmort.list,fmodifier=fmodifier,tol=tol,debug=debug,debug2=debug2,
        faa=faa,geomean=geomean,SR=SR,SRfn=SRfn,calcB=calcB,range=range)
    cat("Done Fmed\n")
  }
  if(Floss){
    yprFloss<-calcFloss(fmult=fmult,year=year,report=report,spawnseason=spawnseason,fmodifier=fmodifier,
      fmort.list=fmort.list,range=range,tol=tol,debug=debug,faa=faa,geomean=geomean,SR=SR,SRfn=SRfn,calcB=calcB,debug2=debug2,nyears=nyears.Floss)
    cat("Done Floss\n")
  }

 #  browser()
  Fmax.value<-ifelse(Fmax,ypr1$ypr.opt$fmult,NA)
  FMSY.value<-ifelse(FMSY,ypr1$ypr.MSY$fmult,NA)
  F0.1.value<-ifelse(F0.1,ypr1$ypr.0.1$fmult,NA)
  Fmed.value<-ifelse(Fmed,yprFmed$fmult,NA)
  Floss.value<-ifelse(Floss,yprFloss$fmult,NA)
  if(!is.logical(FSPR)){
    F_Perc_SPR<-numeric(length(FSPR))
    yprSPR<-list()
    for (i in 1:length(FSPR)){
      cat(paste("F",FSPR[i],"%\n",sep=""))
      F_Perc_SPR[i]<-SPR2fmult(perc_spr=FSPR[i]/100.0,faa=faa,year=year,R0=R0,spawnseason=spawnseason,
        fmodifier=fmodifier,range=range,tol=tol,debug=debug,geomean=geomean,percent=TRUE,debug2=debug2)
      yprSPR[[i]]<-getFmort.ypr.0(fmult=F_Perc_SPR[i],fmodifier=fmodifier,year=year,faa=faa,
        fmort.list=fmort.list,R0=R0,spawnseason=spawnseason)
      if(calcB){
        yprSPR[[i]]<-calcB(SR=SR,ypr=yprSPR[[i]],debug2=debug2,tol=tol)
#        cnt1<-0
#        R<-SR$SRfn(yprSPR[[i]]$spr*R0)
#        diff<-abs(R0-R)
#        if(debug2)cat(paste("R0,R,SSB,diff:",R0,R,yprSPR[[i]]$spr*R,diff,"\n"))
#        while(cnt1<100 && diff>tol){
#          Rnew<-SR$SRfn(yprSPR[[i]]$spr*R)
#          diff<-abs(R-Rnew)
#          if(debug2)cat(paste("R,Rnew,SSB,diff:",R,Rnew,yprSPR[[i]]$spr*R,diff))
#           R<-Rnew
#          cnt1<-cnt1+1
#        }
#        if(cnt1>99){cat(paste("cnt1=",cnt1," in calculation of SSB%spr"));browser()}
#        yprSPR[[i]]$SSB<-yprSPR[[i]]$spr*R
#        yprSPR[[i]]$Yield<-yprSPR[[i]]$ypr*R
#        yprSPR[[i]]$Req<-R
      }else{
        yprSPR[[i]]$SSB<-NA
        yprSPR[[i]]$Yield<-NA
        yprSPR[[i]]$Req<-NA
      }
      cat("Done PERC_SPR2fmult ",FSPR[i],"\n")
    }
    names(F_Perc_SPR)<-paste("F",FSPR,"%",sep="")
    cat("Done F_SPR\n")
  }else{
    F_Perc_SPR<-NA
  }
#  cat(paste("cnt=",cnt,"\n"))
  BRPvect<-numeric(cnt)
#  names(BRPvect)<-paste(1:cnt)
  BRPvectB<-numeric(cnt)
  BRPvectY<-numeric(cnt)
  yprList<-list()
#  browser()
  i<-1
  if(Fmax){
    names(BRPvectY)[i]<-names(BRPvectB)[i]<-names(BRPvect)[i]<-"Fmax"
    BRPvect[i]<-Fmax.value
    yprList[[i]]<-ypr1$ypr.opt
    BRPvectB[i]<-ypr1$ypr.opt$SSB
    BRPvectY[i]<-ypr1$ypr.opt$Yield
    i<-i+1
  }
  if(FMSY){
    names(BRPvectY)[i]<-names(BRPvectB)[i]<-names(BRPvect)[i]<-"FMSY"
    BRPvect[i]<-FMSY.value
    yprList[[i]]<-ypr1$ypr.MSY
    BRPvectB[i]<-ypr1$ypr.MSY$SSB
    BRPvectY[i]<-ypr1$ypr.MSY$Yield
    i<-i+1
  }

  if(F0.1){
    names(BRPvectY)[i]<-names(BRPvectB)[i]<-names(BRPvect)[i]<-"F0.1"
    BRPvect[i]<-F0.1.value
    yprList[[i]]<-ypr1$ypr.0.1
    BRPvectB[i]<-ypr1$ypr.0.1$SSB
    BRPvectY[i]<-ypr1$ypr.0.1$Yield
    i<-i+1
  }
  if(Fmed){
    names(BRPvectY)[i]<-names(BRPvectB)[i]<-names(BRPvect)[i]<-"Fmed"
    BRPvect[i]<-Fmed.value
    yprList[[i]]<-yprFmed
    BRPvectB[i]<-yprFmed$SSB
    BRPvectY[i]<-yprFmed$Yield
    i<-i+1
  }
  if(Floss){
    names(BRPvectY)[i]<-names(BRPvectB)[i]<-names(BRPvect)[i]<-"Floss"
    BRPvect[i]<-Floss.value
    yprList[[i]]<-yprFloss
    BRPvectB[i]<-yprFloss$SSB
    BRPvectY[i]<-yprFloss$Yield
    i<-i+1
  }
  if(!is.logical(FSPR)){
    names(BRPvectY)[i+(1:length(FSPR))-1]<-names(BRPvectB)[i+(1:length(FSPR))-1]<-names(BRPvect)[i+(1:length(FSPR))-1]<-names(F_Perc_SPR)
    BRPvect[i+(1:length(FSPR))-1]<-F_Perc_SPR[1:length(FSPR)]
    yprList[i+(1:length(FSPR))-1]<-yprSPR[1:length(FSPR)]
    for(j in i+(1:length(FSPR))-1){
      BRPvectB[j]<-yprList[[j]]$SSB
      BRPvectY[j]<-yprList[[j]]$Yield
    }
  }
  if(inverse)BRPvect<-1/BRPvect
  return(list(BRPvectF=BRPvect,BRPvectB=BRPvectB,BRPvectY=BRPvectY,yprList=yprList))
#   return(list(runname=runname,Fmax=Fmax.value,F0.1=F0.1.value,Fmed=Fmed.value,FSPR=F_Perc_SPR))
}

