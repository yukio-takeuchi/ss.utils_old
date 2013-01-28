##############
##############
##############
getMorphFun<-function(repfile="report.sso",report=NULL,RecrDist=NULL,MorphIndexing=NULL){
  if(is.null(RecrDist)||is.null(MorphIndexing)){
    if(is.null(report))report<-getReport(repfile=repfile)
    if(is.null(RecrDist))RecrDist<-getRecruitmentDist(report=report)
    if(is.null(MorphIndexing))Morpindexing<-getMorphIndexing(report=report)
  }
#  browser()
  Morph<-function(Index){
    tmp<-MorphIndexing[as.numeric(MorphIndexing$Index)==as.numeric(Index),]
    Bseas<-BirthSeason<-as.numeric(tmp$Bseas)
    Gpattern<-as.numeric(tmp$Gpattern)
    Gender<-as.numeric(tmp$Gender)
    Sub_Morph<-tmp$Sub_Morph
    Sub_Morph_Dist<-tmp$Sub_Morph_Dist
    Gpattern_Gender<-tmp$Gpattern_Gender
    tmp2<-RecrDist[as.numeric(rownames(RecrDist))==as.numeric(Index),]
    Rvalue<-tmp2$Value
    Used<-as.logical(tmp2$"Used?")
    Area<-tmp2$Area
    Seas<-tmp2$Seas
    return(list(Bseas=Bseas,BirthSeason=BirthSeason,Gpattern=Gpattern,
      Gender=Gender,SubMorph=Sub_Morph,Sub_Morph_Dist=Sub_Morph_Dist,Gpattern_Gender=Gpattern_Gender,
      Rvalue=Rvalue,Used=Used,Area=Area,Seas=Seas))
  }
  return(Morph)
}






##### calcFAA.yprの返り値を、fmultとR0で修正する
#
# argument
#
# fmult        : multiplier of F
# fmort.list   :
# R0           : number of fish at age 0, usually defined as 1
# debug        : Logical value if debug is on (default : FALSE)
#
# returns
# 以下を要素に持つlist
#　fmort.matrix : 2 dimensional array (nageseason x nfleet+3+1+nfleet+1), storing F@A and partial F@A,
#                 Z, M, Spawning output at age, W@A by fleet and number of survivor at age conditioned as R0=1 for YPR
# nage         : nage as defined in SS
# nseason      : number of season
# nfleet       : number of fleet
# nageseason   : (nage+1)*nseason
# nageseason3  : 3*(nage+1)*nseason
# spawnseason  : Spawning season
# fmult        : multiplier of F
# fmodifier    : modifier of F intensity by fleet
# R0           : number of fish at age 0, usually defined as 1
# faa          : object returned by cal
# year         : vector of integer to calculate average F at age
# geomean      : average F to be calculated by geometricmean (TRUE) or arithmetic mean (FALSE),default :TRUE
#
#
modifyFAA.ypr<-function(fmult=1,fmort.list=NULL,R0=1,debug=FALSE,geomean=TRUE){
  if(is.null(fmort.list)){cat("fmort.list is missing");browser()}
#  browser()
  fmort.matrix<-fmort.list[["fmort.matrix"]]
  nage<-fmort.list[["nage"]]
  nseason<-fmort.list[["nseason"]]
  nfleet<-fmort.list[["nfleet"]]
  nageseason<-fmort.list[["nageseason"]]
  nageseason3<-fmort.list[["nageseason3"]]
  fmodifier<-fmort.list[["fmodifier"]]
  faa<-fmort.list[["faa"]]
  spawnseason<-fmort.list[["spawnseason"]]
  year<-fmort.list[["year"]]
  nmorph<-fmort.list$nmorph
  info<-fmort.list$info
  Morph.fn<-fmort.list$Morph.fn
#  cat("in modifyFAA.ypr\n")
#  browser()
  if(fmult!=fmort.list[["fmult"]] && fmort.list[["fmult"]]!=0){
#    cat("here365\n")
#    Morph.fn<-getMorphFun(RecrDist=fmort.list$faa$RecrDist,MorphIndexing=fmort.list$faa$MorphIndexing)
#    browser()
    if(nmorph==1){
  ## To do set appropriate recruitment season
      Bseas<-Morph.fn(info$Morph[1])$Bseas
      Rdist<-Morph.fn(info$Morph[1])$Rvalue
      fmort.matrix[faa$info$Seas,"N"]<-0
      fmort.matrix[as.numeric(Bseas),"N"]<-R0
    }else{
      Bseas<-sapply(info$Morph,FUN=function(index){return(Morph.fn(index)$Bseas)},simplify=TRUE)
      Rdist<-sapply(info$Morph,FUN=function(index){return(Morph.fn(index)$Rvalue)},simplify=TRUE)
      fmort.matrix[info$Seas,"N",]<-0
  #    browser()
      for(i in 1:nmorph){
        fmort.matrix[Morph.fn(info$Morph[i])$Bseas,"N",i]<-R0*Rdist[i]
      }
    }
#    browser()
  # fmort.df$N[1]<-R0
    if(nmorph==1){
      fmort.matrix[,1:(nfleet+1)]<-fmort.matrix[,1:(nfleet+1)]*fmult/fmort.list[["fmult"]]
      fmort.matrix[,"Z"]<-fmort.matrix[,"M"]+fmort.matrix[,1]
      for(age in Morph.fn(info$Morph[1])$Bseas:(nageseason3-1)){
    #   fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
        fmort.matrix[age+1,"N"]<-fmort.matrix[age,"N"]*exp(-fmort.matrix[age,"Z"])
      }
    }else{
      for(i in 1:nmorph){
        fmort.matrix[,1:(nfleet+1),]<-fmort.matrix[,1:(nfleet+1),]*fmult/fmort.list[["fmult"]]
        fmort.matrix[,"Z",]<-fmort.matrix[,"M",]+fmort.matrix[,1,]
        for(age in Morph.fn(info$Morph[i])$Bseas:(nageseason3-1)){
    #     fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
          fmort.matrix[age+1,"N",i]<-fmort.matrix[age,"N",i]*exp(-fmort.matrix[age,"Z",i])
        }
      }
    }
  }else if(fmort.list[["fmult"]]==0){
    if(length(faa)==0){cat("length(faa)=",length(faa));browser()}
    if(is.null(year)){cat("year=",year);browser()}
    fmort.tmp<-calcFAA.ypr(faa=faa,year=year,fmult=fmult,fmodifier=fmodifier,R0=R0,spawnseason=spawnseason,debug=debug,geomean=geomean)
    fmort.matrix<-fmort.tmp[["fmort.matrix"]]
  }
#  cat("in modiryFAA.ypr\n")
#  browser()

  if(nmorph>1){
    for(i in 1:nmorph){
      maxN<-max(fmort.matrix[info$Seas,"N",i])
      if(any(maxN<fmort.matrix[(nseason+1):nageseason3,"N",i])){cat("error in calculation of N in modifyFAA.ypr\n");browser()}
    }
  }

  return(list(fmort.matrix=fmort.matrix,nage=nage,nseason=nseason,nfleet=nfleet,
    nageseason=nageseason,nageseason3=nageseason3,spawnseason=spawnseason,
    fmult=fmult,fmodifier=fmodifier,R0=R0,faa=faa,year=year,nmorph=nmorph,info=info,Morph.fn=Morph.fn))
}

##########################################
## fmortを修正する
# To-do 要チェック : modifyFAA.yprと同じ？
# Argument
#
# fmort :
# fmult        : multiplier of F
# R0           : number of fish at age 0, usually defined as 1
# spawnseason  : Spawning season
# debug        : Logical value if debug is on (default : FALSE)
#
# Return
#
# fmort        : list returned calFAA.ypr
#
modifyFmort.ypr<-function(fmort,fmult=1,R0=1,spawnseason,debug=FALSE){
  if(debug)browser()
  if(fmult!=fmort$fmult){
    fmult.old<-fmort$fmult
    fmort$fmult<-fmult
    info<-fmort$info
    nmorph<-length(info$Morph)
    Morph.fn<-getMorphFun(RecrDist=fmort$faa$RecrDist,MorphIndexing=fmort$faa$MorphIndexing)
    if(nmorph==1){
      fmort$fmort.matrix[,1:(fmort$nfleet+1)]<-fmort$fmort.matrix[,1:(fmort$nfleet+1)]*fmult/fmult.old
      fmort$fmort.matrix[,"Z"]<-fmort$fmort.matrix[,"M"]+fmort$fmort.matrix[,1]
    }else{
      fmort$fmort.matrix[,1:(fmort$nfleet+1),]<-fmort$fmort.matrix[,1:(fmort$nfleet+1),]*fmult/fmult.old
      fmort$fmort.matrix[,"Z",]<-fmort$fmort.matrix[,"M",]+fmort$fmort.matrix[,1,]
    }
  }
  if(nmorph==1){
    if(R0!=fmort$fmort.matrix[1,"N"]){
      fmort$R0<-R0
      fmort$fmort.matrix[1,"N"]<-R0
    }
  }else{
#    browser()
    tmp<-sum(sapply(1:nmorph,FUN=function(i){return(fmort$fmort.matrix[Morph.fn(info$Morph[i])$Bseas,"N",i])}))
    if(!identical(R0,tmp))
    fmort$R0<-R0
  }
#  browser()
  if(nmorph==1){
    for(age in Morph.fn(info$Morph[1])$Bseas:(fmort$nageseason3-1)){
      fmort$fmort.matrix[age+1,"N"]<-fmort$fmort.matrix[age,"N"]*exp(-fmort$fmort.matrix[age,"Z"])
    }
  }else{
    for(i in 1:nmorph){
 #     cat("here463\n")
 #     browser()
      for(age in as.numeric(Morph.fn(info$Morph[i])$Bseas):(fmort$nageseason3-1)){
        fmort$fmort.matrix[age+1,"N",i]<-fmort$fmort.matrix[age,"N",i]*exp(-fmort$fmort.matrix[age,"Z",i])
      }
    }
  }
#  browser()
  if(nmorph==1){
    for(age in Morph.fn(info$Morph[1])$Bseas:(fmort$nageseason3-1)){
  #   fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
      fmort$fmort.matrix[age+1,"N"]<-fmort$fmort.matrix[age,"N"]*exp(-fmort$fmort.matrix[age,"Z"])
    }
  }else{
    for(i in 1:nmorph){
      for(age in as.numeric(Morph.fn(info$Morph[i])$Bseas):(fmort$nageseason3-1)){
  #     fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
        fmort$fmort.matrix[age+1,"N",i]<-fmort$fmort.matrix[age,"N",i]*exp(-fmort$fmort.matrix[age,"Z",i])
      }
    }
  }
#  cat("here\n")
#  browser()

  if(spawnseason!=fmort$spawnseason) fmort$spawnseason<-spawnseason
  return(fmort)
}


# 四半期年齢別、漁法別漁獲重量を計算する
calcCW.ypr<-function(fmort.list){
  fmort<-fmort.list[["fmort.matrix"]]
  nage<-fmort.list[["nage"]]
  nseason<-fmort.list[["nseason"]]
  nfleet<-fmort.list[["nfleet"]]
  nageseason<-fmort.list[["nageseason"]]
  nageseason3<-fmort.list[["nageseason3"]]
  info<-fmort.list$info
  nmorph<-fmort.list$nmorph
  Morph.fn<-getMorphFun(RecrDist=fmort.list$faa$RecrDist,MorphIndexing=fmort.list$faa$MorphIndexing)
  if(nmorph==1){
    cw_fl<-array(0,dim=c(nageseason3,nfleet))
  }else{
    cw_fl<-array(0,dim=c(nageseason3,nfleet,nmorph))
  }
  dimnames(cw_fl)[[2]]<-paste("FL",1:nfleet,sep="")

  if(nmorph==1){
    OneMinusExpZ<-1-exp(-fmort[,"Z"])
    OneMinusExpZperZ<-OneMinusExpZ/fmort[,"Z"]
 #   browser()
  ## partial F と catch weight by fleetの積
  ## 結果は四半期齢で収納
  #  cw_fl[,1:nfleet]<-fmort[,2:(nfleet+1)]*fmort[,nfleet+4+1:nfleet]
    cw_fl[,1:nfleet]<-fmort[,paste("F_FL",1:nfleet,sep="")]*fmort[,paste("SelW",1:nfleet,sep="")]
  ### 2011/03/21 R0に関わらずYPRを返すようにした
    cw_fl<-cw_fl *  fmort[,"N"]*OneMinusExpZperZ/fmort[as.numeric(Morph.fn(info$Morph[1])$Bseas),"N"]
  }else{
    OneMinusExpZ<-1-exp(-fmort[,"Z",])
    OneMinusExpZperZ<-OneMinusExpZ/fmort[,"Z",]
 ## partial F と catch weight by fleetの積
  ## 結果は四半期齢で収納
  #  cw_fl[,1:nfleet]<-fmort[,2:(nfleet+1)]*fmort[,nfleet+4+1:nfleet]
#    browser()
    cw_fl[,1:nfleet,]<-fmort[,paste("F_FL",1:nfleet,sep=""),]*fmort[,paste("SelW",1:nfleet,sep=""),]
#    browser()
  ### 2011/03/21 R0に関わらずYPRを返すようにした
    Rdist<-sapply(1:nmorph,FUN=function(i){return(fmort[as.numeric(Morph.fn(info$Morph[i])$Bseas),"N",i])},simplify=TRUE)
#    browser()
    cw_fl<-cw_fl *  aperm((fmort[,"N",]*OneMinusExpZperZ/Rdist)%o%rep(1,nfleet),c(1,3,2))
#    browser()
}
#  cat("In 261\n")
#  browser()

  return(invisible(cw_fl))
}

# 四半期年齢別、漁法別漁獲重量を計算する
# calcCAA.yprを利用することにした
calcCW.ypr.new<-function(fmort.list){

# 四半期年齢別、漁法別漁獲尾数
 caa_fl<-calcCAA.ypr(fmort.list)
## catch weight by age and by fleetをかけて漁獲重量に変換
## 結果は四半期齢で収納
  cw_fl[,1:nfleet]<-cw_fl[,1:nfleet]*fmort[,nfleet+4+1:nfleet]

  return(invisible(cw_fl))
}

# 四半期別年齢別、漁法別漁獲尾数を計算する
calcCAA.ypr<-function(fmort.list){
  fmort<-fmort.list[["fmort.matrix"]]
  nage<-fmort.list[["nage"]]
  nseason<-fmort.list[["nseason"]]
  nfleet<-fmort.list[["nfleet"]]
  nageseason<-fmort.list[["nageseason"]]
  nageseason3<-fmort.list[["nageseason3"]]
  caa_fl<-array(0,dim=c(nageseason3,nfleet))
  colnames(caa_fl)<-paste("FL",1:nfleet,sep="")

  #browser()

  OneMinusExpZ<-1-exp(-fmort[,"Z"])

  OneMinusExpZperZ<-OneMinusExpZ/fmort[,"Z"]

## partial F と catch weight by fleetの積
## 結果は四半期齢で収納
  caa_fl[,1:nfleet]<-fmort[,2:(nfleet+1)]

  caa_fl<-caa_fl *  fmort[,"N"]*OneMinusExpZperZ

  return(invisible(caa_fl))
}

#
# SPR(絶対値）が与えられたときに対応するfmultiplierを計算する
#
## 2010/02/10 高速化するために他の求解法に切り替える
## 2011/03/15 percentがtrueであれば、%SPRに対応するfmultiplierを返すようにした。
##
SPR2fmult<-SPR2fmult.20100210<-function(spr=NA,perc_spr=NA,faa=NULL,year,R0=1,spawnseason=4,fmort.list=NULL,
  fmodifier=ifelse(is.list(faa),rep(1,dim(faa$faa.array)[3]),
  fmort.list[["fmodifier"]]),range=c(0,5),tol=0.01,debug=FALSE,percent=FALSE,geomean=FALSE,debug2=FALSE){
# 下限と上限は初期設定では0と5
  fmultLow=range[1]
  fmultHigh=range[2]
#  browser()
  sprLow<-calcSPR.ypr(faa=faa,year=year,fmult=fmultLow,R0=R0,spawnseason=spawnseason,
            fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean,debug2=debug2)
  sprHigh<-calcSPR.ypr(faa=faa,year=year,fmult=fmultHigh,R0=R0,spawnseason=spawnseason,
            fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean,debug2=debug2)
#  if(debug2){cat("in SPR2fmult\n");browser()}
##########################################################################
  fnc<-function(x){
    if(!percent){
      val<-spr-calcSPR.ypr(faa=faa,year=year,fmult=x,R0=R0,spawnseason=spawnseason,
                    fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean)$spr
    }else{
      val<-perc_spr-calcSPR.ypr(faa=faa,year=year,fmult=x,R0=R0,spawnseason=spawnseason,
                    fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean)$perc_spr
    }
    if(debug2)cat("x=",x,", val=",val," spr=",spr,"\n")
    return(val)
  }
#  browser()
##########################################################################
  temp<-uniroot(f=fnc,interval=c(fmultLow,fmultHigh),tol=min(tol,1.0e-6))
#  browser()
  return(temp$root)
}

## 2010/02/10 高速化するために他の求解法に切り替える
PERC_SPR2fmult<-PERC_SPR2fmult.20100210<-function(perc_spr,faa=NULL,year,R0=1,spawnseason=4,fmort.list=NULL,
  fmodifier=ifelse(is.list(faa),rep(1,dim(faa$faa.array)[3]),fmort.list[["fmodifier"]]),range=c(0,5),
    tol=0.01,debug=FALSE,geomean=TRUE){
# 二分法で計算する初期値は初期設定では0と5
  fmultLow=range[1]
  fmultHigh=range[2]

  sprLow<-calcSPR.ypr(faa=faa,year=year,fmult=fmultLow,R0=R0,
              spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean,debug2=debug2)
  sprHigh<-calcSPR.ypr(faa=faa,year=year,fmult=fmultHigh,R0=R0,
              spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean,debug2=debug2)
##########################################################################
  fnc<-function(x){
    return(perc_spr-calcSPR.ypr(faa=faa,year=year,fmult=x,R0=R0,
              spawnseason=spawnseason,fmort.list=fmort.list,fmodifier=fmodifier,geomean=geomean)$perc_spr)
  }
##########################################################################
  temp<-uniroot(f=fnc,interval=c(fmultLow,fmultHigh),tol=min(tol,1.0e-5))
#  browser()
  return(temp$root)
}


#
# SPRをss2の計算方法に基づいて計算する
# 年単位、または年の範囲で指定する
#
# 実態は次のgetFmort.ypr.0のwrapper
#
calcSPR.ypr<-function(faa=NULL,year,fmult=1,R0=1,spawnseason=4,fmort.list=NULL,
  fmodifier=ifelse(is.list(faa),rep(1,dim(faa$faa.array)[3]),fmort.list[["fmodifier"]]),debug=FALSE,geomean=TRUE,debug2=FALSE){

  tmp<-getFmort.ypr.0(faa=faa,year=year,fmult=fmult,R0=R0,spawnseason=spawnseason,fmort.list=fmort.list,
    fmodifier=fmodifier,debug=debug,geomean=geomean,debug2=debug2)

  spr<-tmp$spr
  spr0<-tmp$spr0
  perc_spr<-tmp$perc_spr
  R0<-tmp$R0
  fmult_return<-tmp$fmult
  if(debug2)cat("in calcSPR.ypr spr=",spr," spr0=",spr0," fmult=",fmult,"\n")
#  browser()
  return(list(spr=spr,spr0=spr0,perc_spr=perc_spr,R0=R0,fmult=fmult_return))
}









calcBRP.20110315<-function(runname,repfile="report.sso",year,spawnseason=4,fmodifier,Fmax=TRUE,FMSY=TRUE,
  FSPR=c(10,20,30,40),F0.1=TRUE,Fmed=TRUE,Floss=TRUE,tol=0.01,range=c(0,5),debug=FALSE,faa=NULL,multiFAA=FALSE){
  if(missing(year))stop("year is missing")
  if(missing(runname))runname<-""
  if(is.null(faa)){
    faa<-calFAA.ss3.x(report=ifelse(length(report)==1,report,report[1]),is.plot=FALSE)
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
    BRPmat<-calcBRP.0(runname=ifelse(length(repfile)==1,repfile,repfile[1]),faa=faa,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
      Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug)
    cat(paste("done", ifelse(length(repfile)==1,repfile,repfile[1]),"\n"))
    if(length(repfile)>1){
      for(i in 2:length(repfile)){
        faa<-calFAA.ss3.x(report=report[i],is.plot=FALSE)
        if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
        BRPmat<-rbind(BRPmat,calcBRP.0(runname=repfile[i],faa=faa,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
          Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug))
        rownames(BRPmat)[i]<-repfile[i]
        cat(paste("done", repfile[i],"\n"))
      }
      rownames(BRPmat)[1]<-repfile[1]
    }
  }else{
    if(!multiFAA){
      if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
      BRPmat<-calcBRP.0(runname=ifelse(length(repfile)==1,repfile,repfile[1]),faa=faa,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
        Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug)
      cat("done for FAA\n")
    }else{
      BRPmat<-NULL
      for(i in 1:length(faa)){
        if(missing(fmodifier))fmodifier<-rep(1,dim(faa[[i]]$faa.array)[3])
        BRPmat<-rbind(BRPmat,calcBRP.0(runname=repfile[i],report=report,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
          Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug,faa=faa[[i]]))
          rownames(BRPmat)[i]<-repfile[i]
          cat(paste("done", i,"th faa\n"))
      }
    }
  }
  class(BRPmat)<-"BRP"
  return(BRPmat)
}


calcBRP.old.20110316<-function(runname,repfile="report.sso",year,spawnseason=4,fmodifier,Fmax=TRUE,FMSY=TRUE,FSPR=c(10,20,30,40),F0.1=TRUE,Fmed=TRUE,Floss=TRUE,tol=0.01,range=c(0,5),debug=FALSE,faa=NULL){
  if(missing(year))stop("year is missing")
  if(missing(runname))runname<-""

  if(is.null(faa) && length(repfile)==1){
     report<-getReport.sso(repfile=repfile)
    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
    return(calcBRP.0(runname=runname,report=report,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
      Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug))
  }else if(is.null(faa) && length(repfile)>1){
    for(filename in repfile){
      if(!file.exists(filename)){stop(paste(filename, "does not exist"))}
    }
    report<-getReport.sso(repfile=repfile[1])
    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
    BRPmat<-calcBRP.0(runname=repfile[1],report=report,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
      Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug)
    cat(paste("done", repfile[1]))
    for(i in 2:length(repfile)){
      report<-getReport.sso(repfile=repfile[i])
      faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
      if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
      BRPmat<-rbind(BRPmat,calcBRP.0(runname=repfile[i],report=report,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
        Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug))
        rownames(BRPmat)[i]<-repfile[i]
        cat(paste("done", repfile[i],"\n"))
    }
    rownames(BRPmat)[1]<-repfile[1]
    class(BRPmat)<-"BRP"
    return(BRPmat)
  }else{
    if(length(faa)==1){
#    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa$faa.array)[3])
    return(calcBRP.0(runname=runname,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
      Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug,faa=faa))
    }else{
    report<-getReport.sso(repfile=repfile[1])
#    faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
    if(missing(fmodifier))fmodifier<-rep(1,dim(faa[[1]]$faa.array)[3])
    BRPmat<-calcBRP.0(runname=repfile[1],year=year,spawnseason=spawnseason,fmodifier=fmodifier,
      Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug,faa=faa[[1]])
    cat(paste("done 1st faa\n"))
    }
    for(i in 2:length(faa)){
#      report<-getReport.sso(repfile=repfile[i])
#      faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
      if(missing(fmodifier))fmodifier<-rep(1,dim(faa[[i]]$faa.array)[3])
      BRPmat<-rbind(BRPmat,calcBRP.0(runname=repfile[i],report=report,year=year,spawnseason=spawnseason,fmodifier=fmodifier,
        Fmax=Fmax,FMSY=FMSY,F0.1=F0.1,FSPR=FSPR,Fmed=Fmed,Floss=Floss,tol=tol,range=range,debug=debug,faa=faa[[i]]))
        rownames(BRPmat)[i]<-repfile[i]
        cat(paste("done", i,"th faa\n"))
    }
  }
}




if(0){

years<-1952:2002

eltime<-system.time(ypr.vect<-lapply(years,function(year){ getFmort.ypr(fmult=1,faa=faa,year=year:(year+2),maximize=TRUE,F0.1=TRUE,spawnseason=4)}))
print(eltime)

years<-1952:2005

eltime<-system.time(ypr.vect2<-lapply(years,function(year){ getFmort.ypr(fmult=1,faa=faa,year=year,maximize=TRUE,F0.1=TRUE,spawnseason=4)}))
print(eltime)


aaa<-list(fmult=unlist(lapply(ypr.vect,"[[","fmult")),ypr=unlist(lapply(ypr.vect,"[[","ypr")),
  spr=unlist(lapply(ypr.vect,"[[","perc_spr")),fmult.opt=unlist(lapply(ypr.vect,"[[","fmult.opt")),
  ypr.opt=unlist(lapply(ypr.vect,"[[","ypr.opt")),rel_ypr=unlist(lapply(ypr.vect,"[[","rel_ypr")),
  perc_spr=unlist(lapply(ypr.vect,"[[","perc_spr")),
  spr.opt=unlist(lapply(ypr.vect,"[[","spr.opt")),perc_spr.opt=unlist(lapply(ypr.vect,"[[","perc_spr.opt")))

aaa2<-list(fmult=unlist(lapply(ypr.vect2,"[[","fmult")),ypr=unlist(lapply(ypr.vect2,"[[","ypr")),
  spr=unlist(lapply(ypr.vect2,"[[","perc_spr")),fmult.opt=unlist(lapply(ypr.vect2,"[[","fmult.opt")),
  ypr.opt=unlist(lapply(ypr.vect2,"[[","ypr.opt")),rel_ypr=unlist(lapply(ypr.vect2,"[[","rel_ypr")),
  perc_spr=unlist(lapply(ypr.vect2,"[[","perc_spr")),
  spr.opt=unlist(lapply(ypr.vect2,"[[","spr.opt")),perc_spr.opt=unlist(lapply(ypr.vect2,"[[","perc_spr.opt")))



}

testAll<-function(faa,range=c(0,5),year=NA){
  multiplier<-seq(range[1],range[2],by=0.01)
  F01<-calcF0.1(faa=faa,spawnseason=4,year=NA)
  eltime<-
    system.time(ypr.vect<-lapply(multiplier,
      function(fmult){cat("fmult=",fmult,"\n");getFmort.ypr(fmult=fmult,faa=faa,year=2004:2006,maximize=TRUE,F0.1=FALSE)}))
    print(eltime)
#
  aaa<-list(fmult=unlist(lapply(ypr.vect,"[[","fmult")),ypr=unlist(lapply(ypr.vect,"[[","ypr")),
    spr=unlist(lapply(ypr.vect,"[[","perc_spr")),fmult.opt=unlist(lapply(lapply(ypr.vect,"[[","ypr.opt"),"[[","fmult")),
    ypr.opt=unlist(lapply(lapply(ypr.vect,"[[","ypr.opt"),"[[","ypr")),rel_ypr=unlist(lapply(ypr.vect,"[[","rel_ypr")),
    spr.opt=unlist(lapply(lapply(ypr.vect,"[[","ypr.opt"),"[[","spr")),perc_spr.opt=unlist(lapply(lapply(ypr.vect,"[[","ypr.opt"),"[[","perc_spr")))

browser()

plot(aaa$fmult,aaa$rel_ypr*100,type="l",xlab="F-multiplier",ylab="%SPR and YPR as YPR at Fmax as 100%")
points(aaa$fmult,aaa$spr*100,type="l")
return(invisible(aaa))
}

#stop("here")

