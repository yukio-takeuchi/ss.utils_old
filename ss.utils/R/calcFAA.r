#######################################################################################
### Fをcaaとnaaから計算してプロットする。
### faa <- calFAA.ss3.x(report,age.limit=c(1,2,3,4,5,rep(6,16)),namae=c(0,1,2,3,4,"5-20"))
### datas <- list(naa=, caa=list(caa=,caa.array), nma=,bim=)
### もし意図的にcaaを減らしたcaaを使ってFを計算する場合、caaは外からdatasの形で与える
### 2011/02/22 高速計算オプション newCalcを常用するようにした。
### 2011/02/23 北太平洋ビンナガのデータの場合（第2四半期に生まれるという設定なのでN@Aの0才の第1四半期が0）、問題が発生したので、newCalcを使わないように戻した
### 2011/02/24 Partial Fを計算する際に、NAが導入されていたので、そこを0で置き換えるようにした
### 2011/02/24 再度newCalcをTRUEに変更した
### 2011/05/02 F@Aの計算のコードを全面的に見直した。
### 2011/06/05 faa.array のdimnamesを設定するようにした
calcFAA<-calFAA.ss3.x <- function(report=getReport.sso(repfile="Report.sso"),datas=NULL,age.limit=NULL,namae=NULL,is.plot=FALSE,
                       nline=-1,Fmulti=1,qt=4,Full=FALSE,debug=FALSE,newCalc=TRUE,
                       do.solv.Feq.new=FALSE,compile=TRUE){
  if(is.null(datas)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)

    naa <- getNAA(report=report,blankLines=blankLines)
#    browser()
    caa <- getCAA(report=report,blankLines=blankLines)

    nma <- getNMA(report=report,blankLines=blankLines)
 # nma内で、"M"がnatural mortality, "Len_Mat"が成熟率, weight at ageは、"Wt_Beg"??
    naa.array<-naa$naa.array
    naa <- naa$naa
    if(is.null(caa$caa.full.array))stop("caa.full.array is null")
    info<-caa$info

    nma <- nma[[1]]

    RecrDist<-getRecruitmentDist(report=report)

    MorphIndexing<-getMorphIndexing(report=report)
  }else{
    naa.array<-datas$naa$naa.array
    naa <- datas$naa[[1]]
    caa <- datas$caa
    info<-caa$info

    nma <- datas$nma[[1]]

    RecrDist<-datas$RecrDist

    MorphIndexing<-datas$MorphIndexing
  }
#  browser()
  caa <- list(caa=caa$caa,caa.array=caa$caa.array,caa.full.array=caa$caa.full.array)
## caa.full.array
## 年四半期、年齢、エリア、漁業、雌雄、モルフ
#  Sex structured な場合の対応
#  2011/04/13 より一般に複数のmorphがある場合に対応
#  if(length(info$Morph)==1){
###### C@A (年四半期、年齢、漁法） #######################
#    caa$caa.array<-apply(caa$caa.full.array,c(1,2,4),sum)
#  }else{
###### C@A (年四半期、年齢、漁法、雌雄） #######################
#    caa$caa.array<-apply(caa$caa.full.array,c(1,2,4,5),sum)
#  }
#  browser()
  qt<-length(info$Seas)
  naa$YQ <- as.numeric(naa$Yr)+(as.numeric(naa$Seas)/qt)-1/qt
###### C@A (年四半期、年齢（、雌雄）） #######################
  if(length(info$Morph)==1){
    totcatch <- apply(caa$caa.full.array,c(1,2),sum)
    dimnames(totcatch)<-dimnames(caa$caa.full.array)[1:2]
  }else{
    totcatch <- apply(caa$caa.full.array,c(1,2,5),sum)
    dimnames(totcatch)<-dimnames(caa$caa.full.array)[c(1:2,5)]
  }

  biom<-if(is.null(datas)){
    getBabs(report=report)[[1]]
  }
  else{
    datas$biom
  }
#  browser()
  tmp<-biom[biom$Era=="TIME",]  ## 2010/12/27
#  browser()
#  漁法別四半期別漁獲量  # total catch
  wtot.org <- if(length(info$Fleet)!=1){
#    apply(apply(tmp[,substr(colnames(tmp),1,11)=="retain(B):_"],c(1,2),as.numeric),1,sum)  # total catch
    apply(apply(tmp[,substr(colnames(tmp),1,6)=="retain" & substr(colnames(tmp),8,8)=="B"],c(1,2),as.numeric),1,sum)  # total catch
  }else{
    as.numeric(tmp[,substr(colnames(tmp),1,6)=="retain" & substr(colnames(tmp),8,8)=="B"])  # total catch
  }
#  browser()
  names(wtot.org) <- unique(naa$YQ)

  faa <- faa.multi <-
    array(0,dim=dim(totcatch),dimnames=dimnames(totcatch))

## faa.full.arrayは今のところ定義のみ
## 2011/02/21 現在　faa.full.arrayも計算する？

  faa.full.array <- faa.full.array.multi <-
    array(0,dim=dim(caa$caa.full.array),dimnames=dimnames(caa$caa.full.array))
#browser()
  faa.array <- faa.array.multi <-if(length(info$Morph)==1){
## 海域と雌雄のみ外す
## 年四半期、年齢、漁業
      array(0,dim=dim(caa$caa.full.array)[c(1:2,4)],dimnames=dimnames(caa$caa.full.array)[c(1:2,4)])
  }else{
## 年四半期、年齢、漁業、Morph
      array(0,dim=dim(caa$caa.full.array)[c(1:2,4,5)],dimnames=dimnames(caa$caa.full.array)[c(1,2,4,5)])
  }
#  browser()
#############################################################################
## Function solve Baranov's catch equation
  solv.Feq <- function(cvec,nvec,mvec,debug=FALSE,tol=0.0001){
#    if(debug)browser()
#    if(is.matrix(cvec)){dim.org<-dim(cvec);dimnames.org<-dimnames(cvec);mat<-TRUE}else{mat<-FALSE}
    if(is.array(cvec)){dim.org<-dim(cvec);dimnames.org<-dimnames(cvec);mat<-TRUE}else{mat<-FALSE}
    Fres <- rep(0,length(cvec))
    tmp.fn<-function(c,n,m){
###   漁獲が0の場合に、
      if(c==0){
        Fout<-0
      }else{
        F0 <- c/n
        F1 <- c*(F0+m)/n/(1-exp(-F0-m))
        if(c<n){
          while(abs(F0-F1)>tol ){
            F0 <- F1
            F1 <- c*(F0+m)/n/(1-exp(-F0-m))
            if(F0-F1==-Inf) cat("\n",c," ",n," \n")
          }
          Fout <- F1
        }else{
          Fout <- 10
          cat("Warning: catch exeeded tot_num ",round(c)," ",round(n),"\n")
        }
      }
      return(Fout)
    }
    Fres<-mapply(tmp.fn,cvec,nvec,mvec,SIMPLIFY=TRUE)
#    browser()
    if(mat){dim(Fres)<-dim.org;dimnames(Fres)<-dimnames.org}
    if(debug)cat("Fres=",Fres,"\n")
    return(Fres)
  }
#  browser()
#########################################################################
#############################################################################
## Function solve Baranov's catch equation
## New version 2011/05/15
  solv.Feq.new <- function(cvec,nvec,mvec,debug=FALSE,tol=0.000001){
#    if(debug)browser()
#    if(is.matrix(cvec)){dim.org<-dim(cvec);dimnames.org<-dimnames(cvec);mat<-TRUE}else{mat<-FALSE}
    if(is.array(cvec)){dim.org<-dim(cvec);dimnames.org<-dimnames(cvec);mat<-TRUE}else{mat<-FALSE}
    select<-as.vector(cvec!=0 | cvec<nvec)
    cvec1<-cvec[select]
    nvec1<-nvec[select]
    mvec1<-mvec[select]
    Fres <- rep(0,length(cvec))
    if(any(cvec>nvec)){
      cat("Warning: catch exeeded tot_num ",round(cvec[cvec/nvec])," ",round(nvec[cvec/nvec]),"\n")
    }
    Fres[cvec>nvec]<-10
    F0<-cvec1/nvec1
    F1 <- cvec1*(F0+mvec1)/nvec1/(1-exp(-F0-mvec1))
    select2<-abs(F0-F1)<=tol
#    browser()
    Fres[select][select2]<-F1[select2]
    cvec1<-cvec1[!select2]
    nvec1<-nvec1[!select2]
    mvec1<-mvec1[!select2]
    F0<-NA
    F0<-F1[!select2]
    F1<-cvec1*(F0+mvec1)/nvec1/(1-exp(-F0-mvec1))

    cat("sum(select2)=",sum(select2),"\n")
#    browser()
#    flush.console()
    tmp.fn<-function(c,n,m,F0,F1){
        while(abs(F0-F1)>tol ){
          F0 <- F1
          F1 <- c*(F0+m)/n/(1-exp(-F0-m))
          if(F0-F1==-Inf) cat("\n",c," ",n," \n")
        }
        Fout <- F0
###   漁獲が0の場合に、
      return(Fout)
     }
    Fres[select][!select2]<-mapply(tmp.fn,cvec1,nvec1,mvec1,F0,F1,SIMPLIFY=TRUE)
#    Fres[select][!select2]<-foreach()
#    browser()
    if(mat){dim(Fres)<-dim.org;dimnames(Fres)<-dimnames.org}
    if(debug)cat("Fres=",Fres,"\n")
    return(Fres)
  }
#  browser()
#########################################################################
  if(compile && "compiler" %in% .packages(all.available=TRUE)){
    require(compiler)
#    browser()
    if(do.solv.Feq.new){
      solv.Feq.new<-compiler::cmpfun(solv.Feq.new)
    }else{
      solv.Feq<-compiler::cmpfun(solv.Feq)
    }
  }




  if(newCalc){
    if(length(info$Morph)==1){
####  2011/05/02 一気に計算できるようにした
      nma.tmp<-nma[,c(1:7,10)]
#      browser()
      nma.tmp$M<-as.numeric(nma.tmp$M)/length(unique(nma$Seas))
      require(plyr)|| stop("package plyr is required")
      Mmat<-plyr::daply(.data=nma.tmp,.variables=c("Seas","Age","Morph"),.fun=function(x){return(x$M)})
#      cat("L773\n");browser()
#      ageNames<-dimnames(Mmat)[[2]]
      Mmat<-rep(1,length(sort(unique(info$Yr))))%o%Mmat
      dim(Mmat)<-dim(totcatch)
      dimnames(Mmat)<-dimnames(totcatch)
#      Mmat<-t(Mmat)
#      dimnames(Mmat)[[2]]<-ageNames
#      dimnames(Mmat)[[1]]<-dimnames(faa)[[1]]
      faa<-if(!do.solv.Feq.new){
        solv.Feq(cvec=totcatch,nvec=naa.array,mvec=Mmat,debug=debug)
      }else{
        solv.Feq.new(cvec=totcatch,nvec=naa.array,mvec=Mmat,debug=debug)
      }
#################
#      browser()
#      for(j in 1:ncol(faa)){
#        nage <- colnames(faa)[j]
#        faa[,j] <-solv.Feq(cvec=totcatch[,j],
#                  nvec=naa[naa$YQ %in% as.numeric(rownames(faa)),colnames(naa)==nage],
#                  mvec=sapply(naa[naa$YQ %in% as.numeric(rownames(faa)),]$Seas,function(s)nma$M[nma$Age==nage & nma$Seas==s])/qt)
#      }
#      browser()
  ########### Partial F@A ##########################################
      dim3<-dim(faa.array)[3]
      dimnames3<-dimnames(faa.array)[[3]]
#       browser()
  #    faa.array<-(faa/(totcatch+1.e-16))%o%rep(1,dim3)*caa$caa.array
      faa.array<-(faa/(totcatch*(totcatch>0)))%o%rep(1,dim3)*caa$caa.array
      dimnames(faa.array)[[3]]<-dimnames3
      names(dimnames(faa.array))[3]<-"Fleet"
      faa.array[which(caa$caa.array==0,arr.ind=TRUE)]=0     #### 2011/02/24 ある年四半期年齢で漁獲が全くないときに、F@A にNAが出てしまうので、そこを0に修正、
  ####################################################### 対応済 2010/05/22
      dimnames(faa.array)<- dimnames(caa$caa.full.array)[c(1:2,4)]
    ## 海域と雌雄のみ外す
    ## 年四半期、年齢、漁業
#      cat("here\n")
#      browser()

      if(Fmulti!=1){
#        for(j in 1:ncol(faa)){
#          nage <- colnames(faa)[j]
#          faa.multi[,j] <-solv.Feq(cvec=totcatch[,j]*Fmulti,
#                  nvec=naa[naa$YQ %in% as.numeric(rownames(faa)),colnames(naa)==nage],
#                  mvec=sapply(naa[naa$YQ %in% as.numeric(rownames(faa)),]$Seas,function(s)nma$M[nma$Age==nage & nma$Seas==s])/qt)
#        }
        faa.multi<-solv.Feq(cvec=totcatch*Fmulti,nvec=naa.array,mvec=Mmat,debug=debug)
    ######################################################
        dim3<-dim(faa.array)[3]
        dimnames3<-dimnames(faa.array)[[3]]
        faa.array.multi<-(faa.multi/(totcatch*(totcatch>0)))%o%rep(1,dim3)*caa$caa.array
        dimnames(faa.array)[[3]]<-dimnames3
        names(dimnames(faa.array.multi))[3]<-"Fleet"
        faa.array.multi[which(caa$caa.array==0,arr.ind=TRUE)]=0     #### 2011/02/24 ある年四半期年齢で漁獲が全くないときに、F@A にNAが出てしまうので、そこを0に修正、
        dimnames(faa.array.multi)<- dimnames(caa$caa.full.array)[c(1:2,4)]
      }
    }else{
####### if(length(info$Morph)>1)
####  2011/05/02 一気に計算できるようにした
      nma.tmp<-nma[,c(1:7,10)]
      nma.tmp$M<-nma.tmp$M/length(unique(nma$Seas))
      require(plyr)|| stop("package plyr is required")
      Mmat<-daply(.data=nma.tmp,.variables=c("Seas","Age","Morph"),.fun=function(x){return(x$M)})
#      ageNames<-dimnames(Mmat)[[2]]
#      browser()
      Mmat<-rep(1,length(sort(unique(info$Yr)))) %o% Mmat
#      browser()
      dim(Mmat)<-c(dim(Mmat)[1]*dim(Mmat)[2],dim(Mmat)[3],dim(Mmat)[4])
#      Mmat<-t(Mmat)
      dimnames(Mmat)<-dimnames(faa)
#      dimnames(Mmat)[[1]]<-dimnames(faa)[[1]]
#      faa<-solv.Feq(cvec=totcatch,nvec=naa.array,mvec=Mmat,debug=debug)
     faa<-if(!do.solv.Feq.new){
        solv.Feq(cvec=totcatch,nvec=naa.array,mvec=Mmat,debug=debug)
      }else{
        solv.Feq.new(cvec=totcatch,nvec=naa.array,mvec=Mmat,debug=debug)
      }


  ########### Partial F@A ##########################################
#      browser()
      dim3<-dim(faa.array)[3]
      dimnames3<-dimnames(faa.array)[[3]]
#       browser()
  #    faa.array<-(faa/(totcatch+1.e-16))%o%rep(1,dim3)*caa$caa.array
      faa.array<-aperm((faa/(totcatch*(totcatch>0)))%o%rep(1,dim3),c(1,2,4,3))*caa$caa.array
      dimnames(faa.array)[[3]]<-dimnames3
      names(dimnames(faa.array))[3]<-"Fleet"
      faa.array[which(caa$caa.array==0,arr.ind=TRUE)]=0     #### 2011/02/24 ある年四半期年齢で漁獲が全くないときに、F@A にNAが出てしまうので、そこを0に修正、

      dimnames(faa.array)<-dimnames(caa$caa.full.array)[c(1,2,4,5)]
    ## 海域と雌雄のみ外す
    ## 年四半期、年齢、漁業、Morph

######################################################################################
     if(Fmulti!=1){
       faa.multi<-solv.Feq(cvec=totcatch*Fmulti,nvec=naa.array,mvec=Mmat,debug=debug)
###################################################################################
#        browser()
        dim3<-dim(faa.array)[3]
        dimnames3<-dimnames(faa.array)[[3]]
        faa.array.multi<-aperm((faa.multi/(totcatch*(totcatch>0)))%o%rep(1,dim3),c(1,2,4,3))*caa$caa.array
        dimnames(faa.array)[[3]]<-dimnames3
        names(dimnames(faa.array.multi))[3]<-"Fleet"
        faa.array.multi[which(caa$caa.array==0,arr.ind=TRUE)]=0     #### 2011/02/24 ある年四半期年齢で漁獲が全くないときに、F@A にNAが出てしまうので、そこを0に修正、
        dimnames(faa.array.multi)<-dimnames(caa$caa.full.array)[c(1,2,4,5)]
      }

    }
  }else{
#   browser()
    for(j in 1:dim(faa)[2]){
      for(i in 1:dim(faa)[1]){
        ## faa[i,j] はnyear年のnage才に対応する
        nage <- dimnames(faa)[[2]][j]
        nyear <- dimnames(faa)[[1]][i]
        if(length(info$Morph)==1){
          faa[i,j] <-ifelse(totcatch[i,j]==0,0, solv.Feq(cvec=totcatch[i,j],
                               nvec=naa[naa$YQ==as.numeric(nyear),dimnames(naa)[[2]]==nage],
                               mvec=nma$M[nma$Age==nage & nma$Seas==naa[naa$YQ==as.numeric(nyear),]$Seas]/qt))
          if(debug && totcatch[i,j]!=0.0){
            mvec=nma$M[nma$Age==nage & nma$Seas==naa[naa$YQ==as.numeric(nyear),]$Seas]/qt
            nvec=naa[naa$YQ==as.numeric(nyear),colnames(naa)==nage]
            if(abs(totcatch[i,j]-faa[i,j]/(faa[i,j]+mvec)*(1-exp(-faa[i,j]-mvec))*nvec)>1){
              cat("warning y,a,C,N,M,F=",nyear,nage,totcatch[i,j],nvec,mvec,faa[i,j],"\n")
            }
          }
    #     browser()
    #     2009/06/04 ifelse was corrected to if
          faa.array[i,j,] <- if(totcatch[i,j]!=0){
            faa[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
          }else{
            rep(0,length(faa.array[i,j,]))
          }

          if(Fmulti!=1){
            faa.multi[i,j] <- ifelse(tocatch[i,j]==0, 0, solv.Feq(cvec=totcatch[i,j]*Fmulti,
                               nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                               mvec=nma$M[nma$age==nage]/qt))
            faa.array.multi[i,j,] <- if(totcatch[i,j]!=0){
              faa[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
                }else{
                  rep(0,length(faa.array[i,j,]))
                }
          }

        }else{
#          browser()
          for(g in 1:dim(faa)[3]){
            morph<-dimnames(faa)[[3]][g]
#            browser()
            faa[i,j,g] <-ifelse(totcatch[i,j,g]==0,0, solv.Feq(cvec=totcatch[i,j,g],
                                 nvec=naa[naa$YQ==as.numeric(nyear) & naa$Morph==morph, dimnames(naa)[[2]]==nage],
                                 mvec=nma$M[nma$Age==nage & nma$Seas==naa[naa$YQ==as.numeric(nyear),]$Seas]/qt))
       #      browser()
            if(debug && totcatch[i,j,g]!=0.0){
              mvec=nma$M[nma$Age==nage & nma$Seas==naa[naa$YQ==as.numeric(nyear),]$Seas & nma$Morph==as.numeric(morph)]/qt
              nvec=naa[naa$YQ==as.numeric(nyear) & naa$Gender==gender, dimnames(naa)[[2]]==nage]
              if(abs(totcatch[i,j,g]-faa[i,j,g]/(faa[i,j,g]+mvec)*(1-exp(-faa[i,j,g]-mvec))*nvec)>1){
                cat("warning y,a,C,N,M,F=",nyear,nage,totcatch[i,j,g],nvec,mvec,faa[i,j,g],"\n")
              }
            }

            faa.array[i,j,,g] <- if(totcatch[i,j,g]!=0){
              faa[i,j,g]*caa$caa.array[i,j,,g]/sum(caa$caa.array[i,j,,g])
            }else{
              rep(0,length(faa.array[i,j,,g]))
            }

            if(Fmulti!=1){
              faa.multi[i,j,g] <- ifelse(tocatch[i,j,g]==0, 0, solv.Feq(cvec=totcatch[i,j,g]*Fmulti,
                                 nvec=naa[naa$YQ==nyear & dimnames(naa)[[2]]==nage &naa$Morph==morph],
                                 mvec=nma$M[nma$age==nage & nma$Morph==morph]/qt))
              faa.array.multi[i,j,,g] <- if(totcatch[i,j,g]!=0){
                faa[i,j,g]*caa$caa.array[i,j,,g]/sum(caa$caa.array[i,j,,g])
                  }else{
                    rep(0,length(faa.array[i,j,,g]))
                  }
            }
          }
        }
      }
    }
  }
#  browser()
  require(plyr)||stop("package plyr is required")

#  options(stringsAsFactors = FALSE)
  if(length(info$Morph)==1){
    faa.data.frame<-plyr::adply(.data=if(Fmulti==1){faa}else{faa.multi},.margins=c(1),.fun=function(x){return(x)})
    faa.array.data.frame<-plyr::adply(.data=if(Fmulti==1){faa.array}else{faa.array.multi},.margins=c(1,3),.fun=function(x){return(x)})
  }else{
    faa.data.frame<-plyr::adply(.data=if(Fmulti==1){faa}else{faa.multi},.margins=c(1,3),.fun=function(x){return(x)})
    faa.array.data.frame<-plyr::adply(.data=if(Fmulti==1){faa.array}else{faa.array.multi},.margins=c(1,3,4),.fun=function(x){return(x)})
  }
  faa.array.data.frame$YQ<-as.numeric(levels(faa.array.data.frame$YQ))[faa.array.data.frame$YQ]
  faa.data.frame$YQ<-as.numeric(levels(faa.data.frame$YQ))[faa.data.frame$YQ]

  faa.array.data.frame$Fleet<-as.numeric(levels(faa.array.data.frame$Fleet))[faa.array.data.frame$Fleet]
#  faa.data.frame$YQ<-as.numeric(levels(faa.data.frame$YQ))[faa.data.frame$YQ]
#  browser()
#  options(stringsAsFactors = TRUE)
  if(Fmulti==1){

    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,
                faa=faa,faa.array=faa.array,nma=nma,
                wtot=wtot.org,totcatch=totcatch,
                faa.full.array=faa.full.array,
                repfile=ifelse(is.null(names(report)),"",names(report)[1]),info=info,RecrDist=RecrDist,
                MorphIndexing=MorphIndexing,naa.array=naa.array,
                faa.data.frame=faa.data.frame,faa.array.data.frame=faa.array.data.frame)
  }else{

    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,
                faa=faa.multi,faa.array=faa.array.multi,nma=nma,
                faa.org=faa,faa.array.org=faa.array,wtot=wtot.org,totcatch=totcatch,
                faa.full.array=faa.full.array,
                repfile=ifelse(is.null(names(report)),"",names(report)[1]),info=info,RecrDist=RecrDist,
                MorphIndexing=MorphIndexing,naa.array=naa.array,
                faa.data.frame=faa.data.frame,faa.array.data.frame=faa.array.data.frame)
  }
  if(is.plot==TRUE){
    set.mypar()
    par(mfrow=c(2,1),mar=c(3,3,1,1))
    if(is.null(age.limit)) age.limit <- 1:ncol(faa)
    if(is.null(namae)) namae <- 1:ncol(faa)
#    plotFvalue(list("F at age table"=faa),age.limit=age.limit,namae=namae,cex=0.7,VPA=FALSE,locate="n")
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(1952, 1959, 1960, 1969,
                  1970, 1979, 1980, 1989, 1990, 1999,2000,2004), 2, 6),VPA=FALSE)
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(2000,2000.9,2001,2001.9,2002,2002.9,2003,2003.9,2004,2004.9,2005,2005.9), 2, 6),
                VPA=FALSE)
#    setncol(dim(faa.array)[[3]])
#    for(i in 1:dim(faa.array)[[3]]){
#      plotFvalue(list("F at age table"=faa.array[,,i]),
#                 age.limit=age.limit,namae=namae,cex=0.7,VPA=FALSE)
#      title(paste("Fleet",i),line=nline)
#    }
  }
#  browser()
  return(dat)
}

####################################################################################
