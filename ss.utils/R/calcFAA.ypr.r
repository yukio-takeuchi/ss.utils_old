# YPRの計算に便利なように季節を展開し、年齢を(nage+1)の3倍までのばした
# F,partialF,Z,M,成熟率x産卵量,漁法別四半期年齢あたり平均漁獲体重の行列を求める。
#
# argument
# faa          : object calculated by calFAA.ss3.x
# year         : vector of integer to calculate average F at age
# fmult        : multiplier of F
# fmodifier    : modifier of F intensity by fleet
# R0           : number of fish at age 0, usually defined as 1
# spawnseason  : Spawning season
# debug        : Logical value if debug is on (default : FALSE)
# SSver        :
# geomean      : Logical value if geometric mean to be applied or not (default=TRUE)
#
# return value
# 以下を要素に持つlist
# fmort.matrix : 2 dimensional array (nageseason x nfleet+3+1+nfleet+1), storing F@A and partial F@A,
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
#
#
calcFAA.ypr<-function(faa,year,fmult=1,fmodifier=1,R0=1,spawnseason=NA,debug=FALSE,SSver=3,geomean=TRUE,debug2=FALSE){
  if(is.na(spawnseason))stop("argument spawnseason is required to set")
  info<-faa$info
  nage<-length(info$Ages)-1
  nfleet<-length(info$Fleet)
  nseason<-length(info$Seas)
  nmorph<-length(info$Morph)
  ngender<-length(info$Gender)
  MorphIndexing<-faa$MorphIndexing
  if(SSver==3){
    sortlist<-order(faa$nma$Age_Beg)
  }else{
    sortlist<-order(faa$nma$age_Beg)
  }
  nma.sorted<-faa$nma[sortlist,]
#  browser()
  nageseason<-(nage+1)*nseason
  nageseason3<-3*nageseason

  ## 幾何平均を計算するために0の代わりに1を代入した
  ## 2時限目の要素数は、漁業数+1+Z+M+成熟率+
  if(nmorph>1){
    fmort<-array(0,dim=c(nageseason3,nfleet+3+1+nfleet+1,nmorph),dimnames=list(seq(from=0,by=1.0/nseason,length.out=nageseason3),
      c("Total",paste("F_FL",1:nfleet,sep=""),"Z","M","Mat.Fec",paste("SelW",1:nfleet,sep=""),"N"),paste("Morph",info$Morph,sep=".")))
  }else{
    fmort<-array(0,dim=c(nageseason3,nfleet+3+1+nfleet+1),dimnames=list(seq(from=0,by=1.0/nseason,length.out=nageseason3),
      c("Total",paste("F_FL",1:nfleet,sep=""),"Z","M","Mat.Fec",paste("SelW",1:nfleet,sep=""),"N")))

  }
#  browser()
  if(length(dim(faa$faa.array))==4 & dim(faa$faa.array)[4]==1){
    nametemp<-dimnames(faa$faa.array)
    dim(faa$faa.array)<-dim(faa$faa.array)[1:3]
    dimnames(faa$faa.array)<-nametemp[1:3]
  }
#  cat(paste(ifelse(geomean,"geomean\n","arithmatic mean\n")))
  # まず年四半期x年齢のF@Aの幾何平均を計算する
  # 幾何平均の場合は、要素がすべて1,算術平均では、要素がすべて0
  if(nmorph==1){
    fmort1<-array(ifelse(geomean,1,0),dim=c(nseason,nage+1))
  }else{
    fmort1<-array(ifelse(geomean,1,0),dim=c(nseason,nage+1,nmorph))
  }
#  browser()
  if(length(year)>1){
    for(y in year){
##################
      if(nmorph==1){
        temp1<-faa$faa[as.numeric(rownames(faa$faa))==y+(1 :nseason-1)*1.0/nseason,]
      }else{
        temp1<-faa$faa[as.numeric(rownames(faa$faa))==y+(1 :nseason-1)*1.0/nseason,,]
      }
      if(length(temp1)==0){cat("length(temp1)=",length(temp1));browser()}
      if(geomean){
      ###  幾何平均
        fmort1<-fmort1*temp1
      }else{
##################
#    算術平均
        fmort1<-fmort1+temp1
      }
#      temp<-faa$faa.array[as.numeric(dimnames(faa$faa.array)[[1]])==y+(1 :nseason-1)*1.0/nseason,,]
#        if(length(temp)==0){cat("length(temp)=",length(temp));browser()}
#        dim(temp)<-c(nageseason,nfleet)
#      fmort[seq(1,nageseason),2:(nfleet+1)]<-fmort[seq(1,nageseason),2:(nfleet+1)]+temp

    }
    if(geomean){
      fmort1<-fmort1^(1/length(year))
    }else{
      fmort1<-fmort1/length(year)
    }
#### partial F@Aに分けるために、対象とする期間の平均のpartial C@Aを計算する
#### この部分は算術平均
    if(nmorph==1){
      caa1<-array(0,dim=c(nseason,nage+1,nfleet))
    }else{
      caa1<-array(0,dim=c(nseason,nage+1,nfleet,nmorph))
    }
#    browser()
    for(y in year){
      if(nmorph==1){
        temp1<-faa$caa.array[as.numeric(dimnames(faa$caa.array)[[1]])==y+(1 :nseason-1)*1.0/nseason,,]
      }else{
        temp1<-faa$caa.array[as.numeric(dimnames(faa$caa.array)[[1]])==y+(1 :nseason-1)*1.0/nseason,,,]
      }

 #     browser()
      caa1<-caa1+temp1
    }
    caa1<-caa1/length(year)
#fmo    browser()
#       dim3<-dim(faa.array)[3]
#    #    faa.array<-(faa/(totcatch+1.e-16))%o%rep(1,dim3)*caa$caa.array
#        faa.array<-(faa/(totcatch*(totcatch>0)))%o%rep(1,dim3)*caa$caa.array
#        faa.array[which(caa$caa.array==0,arr.ind=TRUE)]=0     #### 2011/02/24 ある年四半期年齢で漁獲が全くないときに、F@A にNAが出てしまうので、そこを0に修正、
#    browser()
    if(nmorph==1){
      totcatch<-apply(caa1,c(1,2),sum)
      dim3<-dim(faa$faa.array)[3]
      fmort2<-(fmort1/(totcatch*(totcatch>0)))%o%rep(1,dim3)*caa1
      fmort2[which(caa1==0,arr.ind=TRUE)]=0
      fmort2.org<-fmort2
      dim(fmort2)<-c(nageseason,nfleet)
    }else{
      totcatch<-apply(caa1,c(1,2,4),sum)
      dim3<-dim(faa$faa.array)[3]
#      browser()
      fmort2<-aperm((fmort1/(totcatch*(totcatch>0)))%o%rep(1,dim3),c(1,2,4,3))*caa1
      fmort2[which(caa1==0,arr.ind=TRUE)]=0
      fmort2.org<-fmort2
      dim(fmort2)<-c(nageseason,nfleet,nmorph)
    }
#    cat("here 165\n")
#    browser()
#    fmort2<-cbind(apply(fmort2,1,sum),fmort2)
    if(nmorph==1){
      fmort[1:nageseason,2:(nfleet+1)]<-fmort2
    }else{
      fmort[1:nageseason,2:(nfleet+1),]<-fmort2
    }
#    browser()
#    fmort<-fmort/length(year)
#    fmort<-fmort^(1/length(year))
#    cat("fmort=",fmort[seq(1,nageseason),2:(nfleet+1)],"\n")
# fmort[seq(nageseason+1,nageseason3),2:(nfleet+1)]<-fmort[nageseason-nseason+1:nseason,2:(nfleet+1)]

  }else{
    if(nmorph==1){
      temp<-faa$faa.array[as.numeric(dimnames(faa$faa.array)[[1]])==year+(1:nseason-1)*1.0/nseason,,]
      if(length(temp)==0){cat("length(temp)=",length(temp));browser()}
      dim(temp)<-c(nageseason,nfleet)
      fmort[seq(1,nageseason),2:(nfleet+1)]<-temp
    }else{
      temp<-faa$faa.array[as.numeric(dimnames(faa$faa.array)[[1]])==year+(1:nseason-1)*1.0/nseason,,,]
      if(length(temp)==0){cat("length(temp)=",length(temp));browser()}
      dim(temp)<-c(nageseason,nfleet,nmorph)
      fmort[seq(1,nageseason),2:(nfleet+1),]<-temp
    }
#   fmort[seq(nageseason+1,nageseason3),2:(nfleet+1)]<-fmort[nageseason-nseason+1:nseason,2:(nfleet+1)]%o%rep(1,(nage*2))
  }
#  browser()
  if(length(fmult)!=1){
    stop("fmult must be scalar")
  }else{
#   browser()
    if(length(fmodifier)!=1 && length(fmodifier)!=nfleet)stop("length(fmodifier) must be equal to number of fleet(nfleet) or scalar")

    if(nmorph==1){
      fmort[1:nageseason,1:nfleet+1]<-t(apply(fmort[1:nageseason,1:nfleet+1],1,function(x){tmp<-x*fmodifier*fmult;return(tmp)}))
    }else{
#      browser()
      fmort[1:nageseason,1:nfleet+1,]<-aperm(apply(fmort[1:nageseason,1:nfleet+1,],c(1,3),function(x){tmp<-x*fmodifier*fmult;return(tmp)}),c(2,1,3))
#      browser()
    }
  }
#  cat("HERE188\n")
  if(nmorph==1){
# Calculate total F
    fmort[,1]<-apply(fmort,1,sum)
# Set M
#    cat("setM\n");browser()
    fmort[1:nageseason,"M"]<-as.numeric(nma.sorted$M[1:nageseason])/nseason
# Calculate Z
    fmort[1:nageseason,"Z"]<-fmort[1:nageseason,"Total"]+fmort[1:nageseason,"M"]
# MaturityxFecundity
#  cat("HERE198");browser()
    fmort[seq(spawnseason,nageseason,by=nseason),"Mat.Fec"]<-nma.sorted$"Mat*Fecund"[seq(spawnseason,nageseason,by=nseason)]
#    cat("HERE200\n")
# mean weight by fishery
#  fmort[1:nageseason,nfleet+4+1:nfleet]<-data.matrix(nma.sorted[1:nageseason,20+(1:nfleet-1)*3])
# Biology matrix に列が追加になっても列名が変わらなければ対応出来るように:2010/02/15
#    cat("HERE204");browser()
    fmort[1:nageseason,nfleet+4+1:nfleet]<-data.matrix(nma.sorted[1:nageseason,paste("SelWt:_",1:nfleet,sep="")])
  }else{
# Calculate total F
    fmort[,1,]<-apply(fmort,c(1,3),sum)
# Set M
#    browser()
    for(m in 1:dim(fmort)[3]){
      fmort[1:nageseason,"M",m]<-nma.sorted[nma.sorted$Morph==info$Morph[m],]$M[1:nageseason]/nseason
    }
# Calculate Z
    fmort[1:nageseason,"Z",]<-fmort[1:nageseason,"Total",]+fmort[1:nageseason,"M",]
# MaturityxFecundity
    for(m in 1:dim(fmort)[3]){
#      cat("HERE215")
      fmort[seq(spawnseason,nageseason,by=nseason),"Mat.Fec",]<-
        nma.sorted[nma.sorted$Morph==info$Morph[m],]$"Mat*Fecund"[seq(spawnseason,nageseason,by=nseason)]
#      cat("HERE218\n")
    }
# mean weight by fishery
#  fmort[1:nageseason,nfleet+4+1:nfleet]<-data.matrix(nma.sorted[1:nageseason,20+(1:nfleet-1)*3])
# Biology matrix に列が追加になっても列名が変わらなければ対応出来るように　2010/02/15
    for(m in 1:dim(fmort)[3]){
      fmort[1:nageseason,nfleet+4+1:nfleet,m]<-
        data.matrix(nma.sorted[nma.sorted$Morph==info$Morph[m],paste("SelWt:_",1:nfleet,sep="")])
    }
  }


# Calculate numbers at age in equiribrium with R0 upto nageseason
#################################
#  browser()
  Morph.fn<-getMorphFun(RecrDist=faa$RecrDist,MorphIndexing=faa$MorphIndexing)

  if(nmorph==1){
## To do set appropriate recruitment season
    Bseas<-Morph.fn(info$Morph[1])$Bseas
    Rdist<-Morph.fn(info$Morph[1])$Rvalue
    fmort[faa$info$Seas,"N"]<-0
#    browser()
    fmort[as.numeric(Bseas),"N"]<-R0
  }else{
    Bseas<-sapply(info$Morph,FUN=function(index){return(Morph.fn(index)$Bseas)},simplify=TRUE)
    Rdist<-sapply(info$Morph,FUN=function(index){return(Morph.fn(index)$Rvalue)},simplify=TRUE)
    fmort[faa$info$Seas,"N",]<-0
#    browser()
    for(i in 1:nmorph){
      fmort[Morph.fn(info$Morph[i])$Bseas,"N",i]<-R0*Rdist[i]
    }
  }
#  browser()
# fmort.df$N[1]<-R0
  if(nmorph==1){
    for(age in Morph.fn(info$Morph[1])$Bseas:(nageseason-1)){
  #   fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
      fmort[age+1,"N"]<-fmort[age,"N"]*exp(-fmort[age,"Z"])
    }
  }else{
    for(i in 1:nmorph){
      for(age in Morph.fn(info$Morph[i])$Bseas:(nageseason-1)){
  #     fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
        fmort[age+1,"N",i]<-fmort[age,"N",i]*exp(-fmort[age,"Z",i])
      }
      if(debug2){cat("in calcFAA.ypr\n");browser()}
    }
  }
#  browser()
# Extend matrix to nage*3
  if(nmorph==1){
    temp<-aperm(fmort[nageseason-nseason+1:nseason,]%o%rep(1,((nage+1)*2)),c(1,3,2))
    dim(temp)<-c(nageseason*2,nfleet+3+1+nfleet+1) # totalF, partialF(10), Z,M,matxFec,mean weight by fleet(10),N : 1+10+1+1+1+10+1
    fmort[seq(from=nageseason+1,to=nageseason3),]<-temp
  }else{
    temp<-aperm(fmort[nageseason-nseason+1:nseason,,]%o%rep(1,((nage+1)*2)),c(1,4,2,3))
    dim(temp)<-c(nageseason*2,nfleet+3+1+nfleet+1,nmorph) # totalF, partialF(10), Z,M,matxFec,mean weight by fleet(10),N : 1+10+1+1+1+10+1
#    browser()
    fmort[seq(from=nageseason+1,to=nageseason3),,]<-temp
  }
#  browser()
# Calculate numbers at age in equilibrium from nageseason to nageseason3
  if(nmorph==1){
    for(age in nageseason:nageseason3-1){
  #   fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
      fmort[age+1,"N"]<-fmort[age,"N"]*exp(-fmort[age,"Z"])
    }
  }else{
    for(age in nageseason:nageseason3-1){
  #   fmort.df$N[age+1]<-fmort.df$N[age]*exp(-fmort.df$Z[age])
      fmort[age+1,"N",]<-fmort[age,"N",]*exp(-fmort[age,"Z",])
    }
  }

#  if(debug){edit(fmort);browser()}
#  cat("here\n")
#  browser()
  if(nmorph>1){
    for(i in 1:nmorph){
      maxN<-max(fmort[info$Seas,"N",i])
      if(any(maxN<fmort[(nseason+1):nageseason3,"N",i])){cat("error in calculation of N in calFAA.ypr\n");browser()}
    }
  }

  return(list(fmort.matrix=fmort,nage=nage,nseason=nseason,nfleet=nfleet,
    nageseason=nageseason,nageseason3=nageseason3,spawnseason=spawnseason,
    fmult=fmult,fmodifier=fmodifier,R0=R0,faa=faa,year=year,nmorph=nmorph,info=info,Morph.fn=Morph.fn,MorphIndexing=MorphIndexing))
}

