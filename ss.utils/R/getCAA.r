
getCAA<-getCAA.ss3.x <- function(report=getReport.sso(repfile="Report.sso"),cl=NULL,tb=NULL,target.line=NULL,browse=FALSE,blankLines=NULL,caa.full=TRUE){
  owarn<-options()$warn
  options(warn=-1)
  on.exit(options(warn=owarn))
  if(browse)browser()
#  unfactor <- function (x)
#  {
#    ch <- as.character(x)
#    opt <- options(warn = -1)
#    num <- as.numeric(ch)
#    options(opt)
#    if (any(is.na(num)))
#        ch
#    else num
#  }

  header.char<- "^CATCH_AT_AGE$"
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  caa<-readDat(report=report,header.char=header.char,blankLines=blankLines,header=TRUE)
 # 2009/06/04
 # 2011/05/04
 # 先に処理するように変更
 # browser()
  caa<-caa[caa$Era=="TIME",]

  caa$Area<-as.numeric(caa$Area)
  caa$Fleet<-as.numeric(caa$Fleet)
  caa$Gender<-as.numeric(caa$Gender)
  caa$Morph<-as.numeric(caa$Morph)
  caa$Yr<-as.numeric(caa$Yr)
  caa$Seas<-as.numeric(caa$Seas)
  name.label<-names(caa)

  # 2010/10/08 maxを使用していたのをlengthに変更
  Fleet<- unique(caa$Fleet)
  Area<-unique(caa$Area)
  Morph<-unique(caa$Morph)
  Gender<-unique(caa$Gender)
  Yr<-unique(caa$Yr)
  Seas<-unique(caa$Seas)
  Ages<-as.numeric(name.label[(!is.na(as.numeric(name.label)))])
  nage<-length(Ages)


# browser()
  del.col<-grep(value=FALSE,pattern="XX",x=name.label)
  caa<-caa[,-1*del.col]
  caa[,8:dim(caa)[2]]<-apply(caa[,8:dim(caa)[2]],c(1,2),as.numeric)
 # EraCol<-grep(value=FALSE,pattern="Era",x=names(caa))

  YQ <- as.numeric(caa$Yr)+(as.numeric(caa$Seas)/length(Seas))-1.0/length(Seas)

  caa<-cbind(YQ,caa)
  YQ.vect<-sort(unique(caa$YQ[!is.na(caa$YQ)]))
  col.Era<-grep(value=FALSE,pattern="Era",x=names(caa))+1
  info<-list(Fleet=Fleet,Area=Area,Morph=Morph,Gender=Gender,Seas=Seas,Ages=Ages,Yr=Yr,Era=caa$Era)
#  Area Fleet Gender  XX XX Morph Yr Seas XX Era
# 2011/02/06
  colnames(caa)[col.Era:ncol(caa)]<-paste("Age",Ages,sep=".")
# browser()
  if(caa.full){
#     browser()
    tmp<-reshape(caa,direction="long",idvar=c("Area","Fleet","Gender","Morph","Yr","Seas","Era"),
        varying=paste("Age",Ages,sep="."))
    colnames(tmp)[9:10]<-c("Age","Catch")
    caa.full.array<-as.array(xtabs(data=tmp,formula=Catch~YQ+Age+Area+Fleet+Morph))
    ## 年四半期、年齢、エリア、漁業、モルフ
#    browser()
     caa.full.array[which(is.na(caa.full.array),arr.ind=TRUE)]=0
     dimnames(caa.full.array)[[2]]<-paste(Ages)
#     browser()
  }else{
    caa.full.array<-NULL
  }

### 2011/04/12 Morph の個数で分岐するようにした
  if(!caa.full){
    if(length(Morph)==1){
  #  #  年四半期、年齢、漁業
      tmp.long<-reshape(caa,direction="long",idvar=c("Area","Fleet","Morph","Yr","Seas","Era"),
      varying=paste("Age",Ages,sep="."))
  #    browser()
  #    names(tmp.long)[10]<-"YQ"
      names(tmp.long)[9:10]<-c("Age","Number")
      caa.array<-as.array(xtabs(data=tmp.long,formula=Number~YQ+Age+Fleet))

    }else{
  #  # 年四半期、年齢、漁業、Morph
      tmp.long<-reshape(caa,direction="long",idvar=c("Area","Gender","Fleet","Morph","Yr","Seas","Era"),
      varying=paste("Age",Ages,sep="."))
  #    names(tmp.long)[10]<-"YQ"
      names(tmp.long)[9:10]<-c("Age","Number")
      caa.array<-as.array(xtabs(data=tmp.long,formula=Number~YQ+Morph+Age+Fleet))

    }
  }else{
    if(length(Morph)==1){
      caa.array<-apply(caa.full.array,c(1,2,4),sum)
    }else{
      caa.array<-apply(caa.full.array,c(1,2,4,5),sum)
    }
  }

#  browser()
  caa<-list(caa=caa,target.line=0,caa.array=caa.array,caa.full.array=caa.full.array,info=info)
  return(caa)
}
