#  getFmort.ypr.0
#
#  YPRをss2での漁獲量の計算方法に基づいて計算する
#  年単位、あるいは年の範囲で指定する
#  FmaxおよびF0.1の計算を省いた簡略版
#  fmult : Fへのmultiplier すべての漁業に一律にかける
#  fmodifier : 漁法ごとに、Fを何倍するかを指定する
#  例 : getFmort.ypr(year=2004,faa=faa)
#  2008/5/4 コードの整理とfmultをfmultとfmodifierに分けた
#  faa : calcFAA.ss2の返りオブジェクト、
#
#  2009/09/12 defaultの引数を変更した
#  faa とfmort.listの両方が与えられた際には、fmort.listを優先する

#getFmort.ypr<-function(faa=-1,year,fmult=1,R0=1,spawnseason=4,maximize=FALSE,fmult.opt=NA,fmort.list=NA,
# fmodifier=ifelse(is.list(faa),rep(1,dim(faa$faa.array)[3]),fmort.list[["fmodifier"]]),
# stepsize=0.001,interval=c(0,5),F0.1=TRUE,debug=FALSE){

getFmort.ypr.0<-function(faa=NULL,year,fmult=1,R0=1,spawnseason=4,fmort.list=NULL,
  fmodifier=ifelse(!is.null(faa),rep(1,dim(faa$faa.array)[3]),fmort.list[["fmodifier"]]),debug=FALSE,geomean=TRUE,debug2=FALSE){

  if(is.null(faa)&&is.null(fmort.list)){
    cat("ERROR at the begining of getFmort.ypr.0")
    browser()
    stop("Either faa or fmort.list must be given in arguments list")
  }
  if(is.null(fmort.list)&&!is.null(faa)&&is.list(faa)){
    if(length(faa)==0){cat("in getFmort.ypr, length(faa)=",length(faa));browser()}
    fmort<-calcFAA.ypr(faa=faa,fmult=fmult,fmodifier=fmodifier,year=year,
      spawnseason=spawnseason,R0=R0,debug=debug,geomean=geomean)
  }else if(!is.null(fmort.list)  && is.list(fmort.list)){
    fmort<-modifyFAA.ypr(fmort.list=fmort.list,fmult=fmult,R0=R0,debug=debug)
  }
##########################################################################################
## current or modifyed Fでのsprを計算
## 2011/03/21 fmoｒｔを作成した際のR0の値にかかわらず、SPRを正しく計算するようにした
## 2011/04/13 複数のmorphに対応させた
  calc.spr<-function(fmort=NULL,debug=FALSE,debug2=FALSE){
    if(is.null(fmort))stop("fmort is required in calc.spr")

    info<-fmort$info
#    Morph.fn<-getMorphFun(RecrDist=fmort$faa$RecrDist,MorphIndexing=fmort$faa$MorphIndexing)
    Morph.fn<-fmort$Morph.fn
    if(fmort$nmorph==1){
      spr<-sum(fmort[["fmort.matrix"]][,"N"]*fmort[["fmort.matrix"]][,"Mat.Fec"])/fmort[["fmort.matrix"]][as.numeric(Morph.fn(info$Morph[1])$Bseas),"N"]
    }else{
      spr<-0
      for(i in 1:fmort$nmorph){
        spr<-spr+sum(fmort[["fmort.matrix"]][,"N",i]*fmort[["fmort.matrix"]][,"Mat.Fec",i])/fmort[["fmort.matrix"]][ as.numeric(Morph.fn(info$Morph[i])$Bseas),"N",i]
        if(debug){
          cat("i=",i,"\n")
          cat("N:\n",fmort[["fmort.matrix"]][,"N",i],"\n")
          cat("Mat.Fec:\n",fmort[["fmort.matrix"]][,"Mat.Fec",i],"\n")
        }
      }
      if(debug2){cat("spr=",prettyNum(spr),"\n")}
    }
#    cat("here760")
#    browser()
    if(debug){cat("in calc.spr\n");browser()}
    return(spr)
  }


##########################################################################################
## current or modifyed Fでのsprを計算
  spr<-calc.spr(fmort=fmort,debug=debug,debug2=debug2)
##########################################################################################
## spr_F=0を計算
  if(fmult!=0 && sum(fmodifier)!=0){
#    cat("modifyFAA.ypr will be called in getFmort.ypr.0 at line 724\n")
    fmort1<-modifyFmort.ypr(fmort=fmort,fmult=0,R0=R0,spawnseason=spawnseason,debug=debug)
    spr0<-calc.spr(fmort=fmort1,debug=debug,debug2=debug2)
#   spr0<-temp0$spr
  }else{
    spr0<-spr
  }
#########################################################################################
## %spr_current_or_modifiedFを計算
  perc_spr<-spr/spr0
####################################################
## catch in weight by fleet and by ageを計算
 cw_fl<-calcCW.ypr(fmort=fmort)

 if(debug2)write.csv(cw_fl,"cw_fl.csv")
# write.csv(fmort.df,"fmort.csv")
## ypr を計算
  ypr<-sum(cw_fl)

####################################################
## ypr by fleet を計算
  ypr.fl<-apply(cw_fl,2,sum)
  if(debug2)cat(paste(c(ypr,ypr.fl),sep=","),"\n",file="ypr.txt",append=TRUE)
  ypr.0.1.tmp<-NULL

#######################################################
## ypr,ypr by fleet, catch weight by fleet and by age
  calc.ypr<-function(fmort){
    cw_fl<-calcCW.ypr(fmort=fmort)
    ypr.fl<-apply(cw_fl,2,sum)
    ypr<-sum(ypr.fl)
    return(list(ypr=ypr,ypr.fl=ypr.fl,cw_fl=cw_fl))
  }
  rel_ypr<-NULL
#######################################################
 #   browser()
  ypr.tmp<-list(ypr=ypr,ypr.fl=ypr.fl,rel_ypr=rel_ypr,spr=spr,perc_spr=perc_spr,spr0=spr0,
    ypr.detail=cw_fl,fmult=fmult,fmort=fmort,Yield=ypr*R0,Y.fl=ypr.fl*R0,SSB=spr*R0,SSB0=spr0*R0,R0=R0)
  if(debug2)cat(prettyNum(fmult),"\t",prettyNum(ypr),"\t",prettyNum(perc_spr),paste(prettyNum(ypr.fl),"\t"),"\n")
  class(ypr.tmp)<-"ypr.0"
  return(ypr.tmp)
########################################################
  cateq<-function(f,m,n,w=1){
    cat1<-f/(f+m)*(1-exp(-f-m))*n*w
    return(cat1)
  }

##########################################################################################
## current or modified Fでのsprを計算
#  calc.spr<-function(fmort,debug=FALSE){
#    spr<-sum(fmort[["fmort.matrix"]][,"N"]*fmort[["fmort.matrix"]][,"Mat.Fec"])/fmort[["fmort.matrix"]][1,"N"]
#    if(debug)browser()
#    return(spr)
#  }


# 漁獲方程式各種

  cateq_part<-function(f,fpart,m,n,w=1){
    Z<-f+m
    cvect<-fpart/Z
    cvect<-cvect*(1-exp(-Z))
    cvect<-cvect*n
    cvect<-w*cvect
    return(cvect)
  }

  cateq_partZ<-function(Z,fpart,n,w=1){
    cvect<-fpart/Z
    cvect<-cvect*(1-exp(-Z))
    cvect<-cvect*n
    cvect<-w*cvect
    return(cvect)
  }
  cateq_partZnoN<-function(Z,fpart,w=1){
    cvect<-fpart/Z
    cvect<-cvect*(1-exp(-Z))
    cvect<-w*cvect
    return(cvect)
  }

    cateq_partZnoNW<-function(Z,fpart){
    cvect<-fpart/Z
    cvect<-cvect*(1-exp(-Z))
    return(cvect)
  }
  cateq_partZexpZnoNW<-function(Z,expZ,fpart){
    cvect<-fpart/Z
    cvect<-cvect*expZ
    return(cvect)
  }
}

