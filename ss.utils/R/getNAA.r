
getNAA<-getNAA.ss3.x<-function(report=getReport.sso(repfile="Report.sso"),blankLines=NULL,header.char="^NUMBERS_AT_AGE$",newCode=FALSE,naaFull=FALSE){
# header.char<-"^NUMBERS_AT_AGE$" # 第１四半期の、漁獲＆自然死亡前、加入後の個体数
  owarn<-options()$warn
  options(warn=-1)
  on.exit(options(warn=owarn))
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
#  browser()
  naa<-readDat(report=report,header.char=header.char,blankLines=blankLines,header=TRUE)
#  browser()
## 2012/03/08 とりあえずの対応
#  browser()
  if(class(report)!="report.trad")naa<-naa[[1]]
#  browser()
  Yr<-sort(as.numeric(unique(naa$Yr[naa$Era=="TIME"])))
# To-do to make maintenac xe free for version change
  if(class(report)!="report.trad"){report1<-report[[1]][[1]]}else{report1<-report}
  if(checkSSversion(report=report1,verbose=FALSE) %in% c("11B","11C","11D","20-","20B","21A","21D","23B","24F")){naa<-naa[naa$"Beg/Mid"=="B",];naa<-naa[,-10]}
#  browser()
  naa[,11:dim(naa)[2]]<-apply(naa[,11:dim(naa)[2]],c(1,2),as.numeric)

  naa<-naa[naa$Era=="TIME",]
  naa$Area<-as.numeric(naa$Area)
  naa$Bio_Pattern<-as.numeric(naa$Bio_Pattern)
  naa$Gender<-as.numeric(naa$Gender)
  naa$BirthSeas<-as.numeric(naa$BirthSeas)
  naa$SubMorph<-as.numeric(naa$SubMorph)
  naa$Morph<-as.numeric(naa$Morph)
  naa$Yr<-as.numeric(naa$Yr)
  naa$Seas<-as.numeric(naa$Seas)
  naa$Time<-as.numeric(naa$Time)


  # 2010/10/08 maxを使用していたのをlengthに変更
  Bio_Pattern<- unique(naa$Bio_Pattern)
  BirthSeas<-unique(naa$BirthSeas)
  SubMorph<-unique(naa$SubMorph)
  Morph<-unique(naa$Morph)
  Area<-unique(naa$Area)
  Morph<-unique(naa$Morph)
  Gender<-unique(naa$Gender)
  Ages<-as.numeric(names(naa)[!is.na(as.numeric(names(naa)))])
  Seas<-unique(naa$Seas)

### 2011/05/01 getCAA から、コピーしてきた
### 2011/04/12 Morph の個数で分岐するようにした
  naa.tmp<-naa[naa$Era=="TIME",]
#  if(0){
    YQ <- as.numeric(naa$Yr)+(as.numeric(naa$Seas)/length(unique(naa$Seas)))-1.0/length(unique(naa$Seas))
    naa.tmp<-cbind(YQ,naa.tmp)
#  }
  if(!newCode)colnames(naa.tmp)[which(!is.na(as.numeric(colnames(naa.tmp))))]<-paste("Age",0:(length(which(!is.na(as.numeric(colnames(naa.tmp)))))-1),sep=".")
  if(newCode){require(reshape2)||stop("package reshape2 is required")}
  if(newCode){require(plyr)||stop("package plyr is required")}
  if(length(Morph)==1){
#  #  年四半期、年齢、
#    browser()
    tmp.long<-if(!newCode){
      reshape(naa.tmp,direction="long",idvar=c("Area","Morph","Yr","Seas","Era"),
      varying=paste("Age",Ages,sep="."))
    }else{
      reshape2::melt(naa.tmp,id.vars=c("Area","Morph","YQ","Seas","Era"),na.rm=TRUE,measure.vars=paste(Ages),value.name="Number")
    }

#    browser()
#    names(tmp.long)[10]<-"YQ"
#    cat("L481\n");browser()
    if(!newCode){
      names(tmp.long)[12:13]<-c("Age","Number")
      naa.array<-as.array(xtabs(data=tmp.long,formula=Number~YQ+Age))
    }else{
      names(tmp.long)[6:7]<-c("Age","Number")
      naa.array<-daply(.data=tmp.long,.variables=~YQ+Age,.fun=function(x){return(x$Number)})
    }
  }else{
#  # 年四半期、年齢、雌雄
#    browser()
    tmp.long<-if(!newCode){
      browser()
      reshape(naa.tmp,direction="long",idvar=c("Area","Morph","Yr","Seas","Era"),
      varying=paste("Age",Ages,sep="."))
    }else{
      reshape2::melt(naa.tmp,id.vars=c("Area","Morph","YQ","Seas","Era"),na.rm=TRUE,measure.vars=paste(Ages),value.name="Number")
    }
 #   browser()
#    names(tmp.long)[10]<-"YQ"
#    cat("L496\n");browser()
     if(!newCode){
      names(tmp.long)[12:13]<-c("Age","Number")
      naa.array<-as.array(xtabs(data=tmp.long,formula=Number~YQ+Age+Morph))
    }else{
      names(tmp.long)[6:7]<-c("Age","Number")
 #     browser()
      naa.array<-daply(.data=tmp.long,.variables=~YQ+Age+Morph,.fun=function(x){return(x$Number)})
    }
  }
  if(naaFull){
  require(reshape2)||stop("package reshape2 is required")
#  naa.full.tmp<-reshape2::melt(naa,id.vars=c("Area","Bio_Pattern","Gender","BirthSeas","SubMorph","Morph","Yr","Seas","Time","Era" ),measure.vars=paste(Ages))
  naa.full.tmp<-reshape2::melt(naa,id.vars=c("Area","Bio_Pattern","Gender","BirthSeas","SubMorph","Morph","Time","Era" ),measure.vars=paste(Ages))
  colnames(naa.full.tmp)[(length(colnames(naa.full.tmp))-2)+1:2]<-c("Age","Number")
  require(plyr)||stop("package plyr is required")
#  browser()
#  naa.full.array<-daply(.data=naa.full.tmp,.variables=c("Area","Bio_Pattern","Gender","BirthSeas","SubMorph","Morph","Yr","Seas","Time","Era","Age" ),.fun=function(x){return(as.numeric(x$Number))},.drop=FALSE)
  naa.full.array<-daply(.data=naa.full.tmp,.variables=c("Area","Bio_Pattern","Gender","BirthSeas","SubMorph","Morph","Time","Era","Age" ),.fun=function(x){return(as.numeric(x$Number))},.drop=FALSE)
  dinamesOld<-dimnames(naa.full.array)[1:(length(dim(naa.full.array))-1)]
  dim(naa.full.array)<-dim(naa.full.array)[1:(length(dim(naa.full.array))-1)]
  dimnames(naa.full.array)<-dinamesOld
  }else{
    naa.full.array<-NULL
  }
#  browser()
  if(0){
    naa.array.new<-aaply(.data=naa.full.array,.margin=c(6,7,9),.fun=function(x){sum(x,na.rm=TRUE)},.drop=FALSE)
    dinamesOld<-dimnames(naa.array.new)[1:(length(dim(naa.array.new))-1)]
    dim(naa.array.new)<-dim(naa.array.new)[1:(length(dim(naa.array.new))-1)]
    dimnames(naa.array.new)<-dinamesOld
    if(names(dimnames(naa.array.new))[1]=="Morph"){naa.array.new<-aperm(naa.array.new,c(2,1,3))}
  }

  info<-list(Bio_Pattern=Bio_Pattern,BirthSeas=BirthSeas,SubMorph=SubMorph,Morph=Morph,Area=Area,Gender=Gender,Ages=Ages,Seas=Seas,Yr=Yr)

  return(list(naa=naa,nrows=length(naa),info=info,naa.array=naa.array,naa.full.array=naa.full.array))
}