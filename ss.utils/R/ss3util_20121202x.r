######################
###   Read report.sso

getReport<-getReport.sso<-function(repfile="Report.sso",oldStyle=TRUE,interactive=FALSE,case.sensitive=TRUE){
#  require(R.utils) || stop("package R.utils is required")
# case.sensitive is not yet implemented 2011/11/23
  if(!case.sensitive){case.sensitive<-TRUE;warning("case.sensive is not yet implemeted ")}
#  report<-lapply(repfile,readLines)
  if(interactive){
    report<-lapply(repfile,readLinesInteract,interactive=interactive)
  }else{
    # check if file(s) exist
    #
    lapply(repfile,FUN=function(filename){if(!file.exists(filename))stop(paste(filename, "is not exist"))}) # 2013/01/28
    report<-lapply(repfile,readLines)  # 2011/02/11
  }
  names(report)<-repfile

  if(!oldStyle){
    report<-lapply(report,
          FUN=function(z){
            blankLines<-grep(value=FALSE,pattern="^$",x=z)
            return(list(report=z,blankLines=blankLines))
            })
    out<-list(report=report,n=length(report))
    class(out)<-"report.list"
    return(out)
  }else{
    if(length(report)==1){
      out<-report[[1]]
      class(out)<-"report.trad"
      return(out)
    }else{
      out<-list(report=report,n=length(report))
      class(out)<-"report.list.trad"
      return(out)
    }
  }
}

is.report.trad<-function(report){
  if(!is.oldStyle(report))stop("class(report) is report.list")
  if(class(report)=="report.trad"){
    return(TRUE)
  }else if(class(report)=="report.list.trad"){
    return(FALSE)
  }else{
    stop("report is something wrong in is.report.trad")
  }
}

getBlankLines<-function(report=NULL){
  if(is.null(report)){stop("report is missing")}
  blankLines<-if(is.oldStyle(report)){
    if(is.report.trad(report)){
      grep(value=FALSE,pattern="^$",x=report)
    }else{
      lapply(report[[1]],FUN=function(z){grep(value=FALSE,pattern="^$",x=z)})
    }
  }else{
    # lapply(report[[1]],FUN=function(z){grep(value=FALSE,pattern="^$",x=z))
    # lapply(report[[1]],FUN=function(z){z$blankLines})
    lapply(report[[1]],"[[","blankLines")
  }
}



is.oldStyle<-function(report=NULL){
  if(is.null(report))stop("report is missing")
  if(class(report) %in% c("report.trad","report.list.trad.")){
    return(TRUE)
  }else if(class(report)=="report.list"){
    return(FALSE)
  }else{
    cat("class:",class(report),"\n")
    stop("report is something wrong in is .oldStyle")
  }
}





componentEndL<-function(headerL,blankLines){
  if(missing(blankLines))stop("blankLines is missing in componentEndL")
  return(min(blankLines[blankLines>headerL])-1)
#  return(min(which(blankLines>headerL))-1)
}


#テキストのvectorをdata.frameに読み込む
#
  read.table.texts<-function(texts,header=FALSE,skip=0,colClasses=NA,col.names=NULL){
    nrow<-length(texts)
    start<-1+(header)+skip
    if(start>nrow)start<-1
    texts.lists<-strsplit(texts[start:nrow],split="[[:blank:]]+")
    row.length<-sapply(texts.lists,length)
    if(length(texts)==1){
      temp<-texts.lists[[1]]
    }else if(!all(row.length==row.length[1])){
      cat("HERE100 length of elements is different in some line(s)\n")
    #  cat("HERE98\n")
      browser()
    }else{
      temp<-lapply(1:row.length[1],FUN=function(i){sapply(texts.lists,FUN=function(x){x[i]})})
      temp<-as.data.frame(temp,stringsAsFactors =FALSE)
      names(temp)<-paste("V",1:row.length[1],sep="")
      if(header)names(temp)<-paste(unlist(strsplit(texts.lists[[(header)+skip]],split="[[:blank:]]+")))
      if(!is.null(col.names) && length(col.names)>=row.length[1])names(temp)<-col.names[1:row.length[1]]
      cat("HERE102\n")
      print(names(temp))
      cat("\n")
    }
    return(temp)
  }


  read.table.texts.old.20130127<-function(texts,header=FALSE,skip,colClasses=NA,col.names=NULL){
    cat("HERE93\nlength(texts)=")
    print(length(texts))
    cat("\n")

    Tfile=file()
    on.exit(if(isOpen(Tfile)){close(Tfile)})
    zz<-cat(texts,file=Tfile,sep="\n")
    print(Tfile)
    cat("\nzz=")
    print(zz)
    cat("\n")
    if(length(texts)>30000){
      cat("HERE100\n")
      browser()
    }
    if(is.null(col.names)){
      res<-read.table(file=Tfile,as.is=TRUE,fill=TRUE,header=header,skip=skip,colClasses=colClasses)
    }else{
      res<-read.table(file=Tfile,as.is=TRUE,fill=TRUE,header=header,skip=skip,colClasses=colClasses,col.names=col.names)
    }
    cat("HERE113\n")
# close(Tfile)
    return(res)
  }

readLinesNoCase<-function(con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown"){
  if (is.character(con)){

    con <- file(con, "r")
    on.exit(close(con))
  }
  return(readLines(con=con,n=n,ok=ok,warn=warn,encoding=encoding))
}

read.compReport <-function(repfile="CompReport.sso",oldStyle=TRUE){
  report<-lapply(repfile,readLines)
# if(length(repfile)==1){
#   return(report[[1]])
# }else{
#   return(report)
# }
  if(!oldStyle){
    return(list(report=report,n=length(report)))
  }else{
    if(length(report)==1){
      return(report[[1]])
    }else{
      return(list(report=report,n=length(report)))
    }
  }
}

# getKeywords<-function()

get.info<-function(report=getReport()){
  Bio_Pattern<- unique(naa$Bio_Pattern)
  BirthSeas<-unique(naa$BirthSeas)
  SubMorph<-unique(naa$SubMorph)
  Morph<-unique(naa$Morph)
  Area<-unique(naa$Area)
  Morph<-unique(naa$Morph)
  Gender<-unique(naa$Gender)
  Ages<-as.numeric(names(naa)[!is.na(as.numeric(names(naa)))])
  Seas<-unique(naa$Seas)
}

getReport.info<-function(report=getReport()){
#Data_File: ISC11Alb_a_20110326_2008-09_JPN_Cat_update.dat
#Control_File: ISC11Alb_1_20110326_run01.ctl
#StartTime: Mon Mar 28 09:52:26 2011
#EndTime: Mon Mar 28 10:01:09 2011
#This run took: 0 hours, 8 minutes, 43 seconds.
#
#Data_File: ISC11Alb_a_20110326_2008-09_JPN_Cat_update.dat
#Control_File: ISC11Alb_1_20110326_run01.ctl
#
#Convergence_Level: 4.11321e-005 is_final_gradient
#Hessian:
  Mon2Dec<-function(month){
    strings<-c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec")
    Dec<-1:12
    Month<-as.data.frame(list(str=strings,Dec=Dec),stringAsFactors=FALSE)
#    browser()
    return(Month[Month$str==month,]$Dec)
  }
  info<-list()
  info$Data_File<-strsplit(grep(x=report,pattern="^Data_File",value=TRUE),split="[[:blank:]]+")[[1]][2]
  info$Control_File<-strsplit(grep(x=report,pattern="^Control_File",value=TRUE),split="[[:blank:]]+")[[1]][2]
  tmp<-strsplit(grep(x=report,pattern="^StartTime",value=TRUE),split="[[:blank:]]+")[[1]]
  info$StartTime<-strptime(paste(Mon2Dec(tmp[3]),paste(tmp[-(1:3)],collapse=" "),collapse=" "), "%m %d %H:%M:%S %Y")
  tmp<-strsplit(grep(x=report,pattern="^EndTime",valu=TRUE),split="[[:blank:]]+")[[1]]
  info$EndTime<-strptime(paste(Mon2Dec(tmp[3]),paste(tmp[-(1:3)],collapse=" "),collapse=" "), "%m %d %H:%M:%S %Y")
  info$Convergence_Level<-as.numeric(strsplit(grep(x=report,pattern="^Convergence_Level",value=TRUE),split="[[:blank:]]+")[[1]][2])
#  FLHeaderL<-getHeaderL("^fleet_names",report)
  blankLines<-grep(x=report,pattern="^$",value=FALSE)
#  FLEL<-componentEndL(headerL=FLHeaderL,blankLines=blankLines)
#  browser()
#  info$FleetNames<-getKWS(FLHeaderL,EL=FLEL,report=report)
  info$FleetID<-as.numeric(strsplit(grep(x=report,pattern="^fleet_ID",value=TRUE),split="[[:blank:]]+")[[1]][-1])
  info$FleetNames<-strsplit(grep(x=report,pattern="^fleet_names",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  info$Catch_units<-as.numeric(strsplit(grep(x=report,pattern="^Catch_units",value=TRUE),split="[[:blank:]]+")[[1]][-1])
  info$Catch_error<-as.numeric(strsplit(grep(x=report,pattern="^Catch_error",value=TRUE),split="[[:blank:]]+")[[1]][-1])
  info$Survey_units<-as.numeric(strsplit(grep(x=report,pattern="^Survey_units",value=TRUE),split="[[:blank:]]+")[[1]][-1])
  info$Survey_error<-as.numeric(strsplit(grep(x=report,pattern="^Survey_error",value=TRUE),split="[[:blank:]]+")[[1]][-1])

  Likelihoods<-list()
  Likelihoods$LIKELIHOOD<-as.numeric(strsplit(grep(x=report,pattern="^LIKELIHOOD",value=TRUE),split="[[:blank:]]+")[[1]][2])
  Likelihoods$TOTAL<-as.numeric(strsplit(grep(x=report,pattern="^TOTAL",value=TRUE),split="[[:blank:]]+")[[1]][2])
  Likelihoods$Catch<-as.numeric(strsplit(grep(x=report,pattern="^Catch",value=TRUE),split="[[:blank:]]+")[[1]][2])
  Likelihoods$Equil_catch<-as.numeric(strsplit(grep(x=report,pattern="^Equil_catch",value=TRUE),split="[[:blank:]]+")[[1]][2:3])
  Likelihoods$Survey<-as.numeric(strsplit(grep(x=report,pattern="^Survey",value=TRUE),split="[[:blank:]]+")[[3]][2])
  tmp<-strsplit(grep(x=report,pattern="^Length_comp",value=TRUE),split="[[:blank:]]+")
#  browser()
  Likelihoods$Age_comp<-ifelse(length(tmp)>0,tmp[[1]][2],NA)
    tmp<-strsplit(grep(x=report,pattern="^Age_comp",value=TRUE),split="[[:blank:]]+")
#  browser()
  Likelihoods$Length_comp<-ifelse(length(tmp)>0,tmp[[1]][2],NA)
  tmp<-strsplit(grep(x=report,pattern="^SizeFreq",value=TRUE),split="[[:blank:]]+")
  Likelihoods$SizeFreq<-ifelse(length(tmp)>0,tmp[[1]][2],NA)
  tmp<-strsplit(grep(x=report,pattern="^Size_at_age",value=TRUE),split="[[:blank:]]+")
#  browser()
  Likelihoods$Size_at_age<-ifelse(length(tmp)>0,tmp[[1]][2],NA)
  Likelihoods$Recruitment<-as.numeric(strsplit(grep(x=report,pattern="^Recruitment",value=TRUE),split="[[:blank:]]+")[[1]][2:3])
  Likelihoods$Forecast_Recruitment<-as.numeric(strsplit(grep(x=report,pattern="^Forecast_Recruitment",value=TRUE),split="[[:blank:]]+")[[1]][2:3])
  Likelihoods$Parm_priors<-as.numeric(strsplit(grep(x=report,pattern="^Parm_priors",value=TRUE),split="[[:blank:]]+")[[1]][2:3])
  Likelihoods$Parm_softbounds<-as.numeric(strsplit(grep(x=report,pattern="^Parm_softbounds",value=TRUE),split="[[:blank:]]+")[[1]][2])
  Likelihoods$Parm_devs<-as.numeric(strsplit(grep(x=report,pattern="^Parm_devs",value=TRUE),split="[[:blank:]]+")[[1]][2:3])
  Likelihoods$Crash_Pen<-as.numeric(strsplit(grep(x=report,pattern="^Crash_Pen",value=TRUE),split="[[:blank:]]+")[[1]][2:3])
#  browser()
#
#Fleet:  ALL 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#Catch_lambda: _  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#Catch_like: 0.0910289  6.28483e-006 8.98721e-005 8.87295e-005 1.96174e-006 2.62767e-007 0.00337151 4.69236e-006 0.0873001 8.85873e-005 5.98704e-005 4.50822e-007 1.35448e-005 2.92902e-006 1.03889e-008 1.3571e-008 8.12189e-012 0 0 0 0 0 0 0 0
#Surv_lambda: _  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0
#Surv_like: -68.9074  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -8.8704 1808.29 -23.6161 -7.03191 3.98687 -44.6981 -21.1643 1496.7
#Length_lambda: _  1 1 0 1 1 1 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0
#Length_like: 1581.33  274.772 222.302 0 272.991 274.131 140.2 0 313.572 0 0 0 48.8771 0 0 34.4829 0 0 0 0 0 0 0 0 0
#
  Likelihoods_Components<-list()
  Likelihoods_Components$Fleet<-strsplit(grep(x=report,pattern="^Fleet:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  Likelihoods_Components$Catch_lambda<-strsplit(grep(x=report,pattern="^Catch_lambda:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  Likelihoods_Components$Catch_like<-strsplit(grep(x=report,pattern="^Catch_like:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  Likelihoods_Components$Surv_lambda<-strsplit(grep(x=report,pattern="^Surv_lambda:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  Likelihoods_Components$Surv_like<-strsplit(grep(x=report,pattern="^Surv_like:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  if(!is.na(Likelihoods$Length_comp)){
    Likelihoods_Components$Length_lambda<-strsplit(grep(x=report,pattern="^Length_lambda:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
    Likelihoods_Components$Length_like<-strsplit(grep(x=report,pattern="^Length_like:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  }

  if(!is.na(Likelihoods$Age_comp)){
    Likelihoods_Components$Age_lambda<-strsplit(grep(x=report,pattern="^Age_lambda:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
    Likelihoods_Components$Age_like<-strsplit(grep(x=report,pattern="^Age_like:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  }

  if(!is.na(Likelihoods$Size_at_age)){
    Likelihoods_Components$Sizeatage_lambda<-strsplit(grep(x=report,pattern="^Sizeatage_lambda:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
    Likelihoods_Components$Sizeatage_like<-strsplit(grep(x=report,pattern="^Sizeatage_like:",value=TRUE),split="[[:blank:]]+")[[1]][-1]
  }

  if(!is.na(Likelihoods$SizeFreq)){
    Likelihoods_Components$SizeFreq_lambda<-lapply(strsplit(grep(x=report,pattern="^SizeFreq_lambda:",value=TRUE),split="[[:blank:]]+"),FUN=function(x){as.numeric(x[c(-1,-2)])})
    Likelihoods_Components$SizeFreq_like<-lapply(strsplit(grep(x=report,pattern="^SizeFreq_like:",value=TRUE),split="[[:blank:]]+"),FUN=function(x){as.numeric(x[c(-1,-2)])})
#    browser()
  }

#Input_Variance_Adjustment
#Fleet  1 2 3
#Index_extra_CV  0 0 0
#Discard_extra_CV  0 0 0
#MeanBodyWt_extra_CV  0 0 0
#effN_mult_Lencomp  0.01 0.01 0.01
#effN_mult_Agecomp  1 1 1
#effN_mult_Len_at_age  1 1 1
#MG_parmsUsing_offset_approach_#:_1  (1=none, 2= M, G, CV_G as offset from female_GP1, 3=like SS2 V1.x)
#

  Input_Variance_Adjustment<-list()
  HeaderL<-grep(x=report,pattern="^Input_Variance_Adjustment",value=FALSE)
  comp<-report[HeaderL+1:8]
  Input_Variance_Adjustment$Fleet<-as.numeric(strsplit(comp[1],split="[[:blank:]]+")[[1]][-1])
  Input_Variance_Adjustment$Index_extra_CV<-as.numeric(strsplit(comp[2],split="[[:blank:]]+")[[1]][-1])
  Input_Variance_Adjustment$Discard_extra_CV<-as.numeric(strsplit(comp[3],split="[[:blank:]]+")[[1]][-1])
  Input_Variance_Adjustment$MeanBodyWt_extra_CV<-as.numeric(strsplit(comp[4],split="[[:blank:]]+")[[1]][-1])
  Input_Variance_Adjustment$effN_mult_Lencomp<-as.numeric(strsplit(comp[5],split="[[:blank:]]+")[[1]][-1])
  Input_Variance_Adjustment$effN_mult_Agecomp<-as.numeric(strsplit(comp[6],split="[[:blank:]]+")[[1]][-1])
  Input_Variance_Adjustment$effN_mult_Len_at_age<-as.numeric(strsplit(comp[7],split="[[:blank:]]+")[[1]][-1])
#  Input_Variance_Adjustment$effN_mult_Len_at_age<-as.numeric(strsplit(comp[8],split="[[:blank:]]+")[[1]][-1])
  tmp<-strsplit(comp[8],split="[[:blank:]]+")[[1]][1]
  Input_Variance_Adjustment$MG_parmsUsing_offset_approach_<-as.numeric(substr(tmp,nchar(tmp),nchar(tmp)))

  return(list(info=info,Likelihoods=Likelihoods,Input_Variance_Adjustment=Input_Variance_Adjustment))
}

read.parameters<-function(report=NULL,repfile="Report.sso",versionNo="3.21",
  new.minor.version=c("04-","04a","04b","10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d"),blankLines=NULL){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  if(is.null(blankLines))blankLines<-grep(value=FALSE,pattern="^$",x=report)
  header.char<-"^PARAMETERS$"
  headerL<-grep(value=FALSE,pattern=header.char,x=report)
  EL<-componentEndL(headerL=headerL,blankLines=blankLines)
  dat.fields<-report[(headerL+1):EL]
  minor.version<-checkSSversion(report=report,verbose=FALSE)

  col.names<-paste(unlist(strsplit(dat.fields[1],split="[[:blank:]]+")))
  parameters<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=col.names,as.numeric.col=NULL,
    as.numeric.as.possible=FALSE,skip=0)
  return(parameters)
}

read.MGParm<-function(report=NULL,repfile="Report.sso",versionNo="3.21",
  new.minor.version=c("04-","04a","04b","10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d","23b"),blankLines=NULL){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  if(is.null(blankLines))blankLines<-grep(value=FALSE,pattern="^$",x=report)
  header.char<-"^MGparm_By_Year_after_adjustments$"
  headerL<-grep(value=FALSE,pattern=header.char,x=report)
  EL<-componentEndL(headerL=headerL,blankLines=blankLines)
  dat.fields<-report[(headerL+1):EL]
  minor.version<-checkSSversion(report=report,verbose=FALSE)
#  browser()
  col.names<-paste(unlist(strsplit(dat.fields[1],split="[[:blank:]]+")))
  temp<-paste(unlist(strsplit(dat.fields[2],split="[[:blank:]]+")))
  if(length(temp)>length(col.names)){
    col.names<-c(col.names,paste("V",1:(length(temp)-length(col.names)),sep=""))
  }
#  browser()
  parameters<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=col.names,as.numeric.col=NULL,
    as.numeric.as.possible=FALSE,skip=0)
  return(parameters)
}

read.Selex.ts<-function(report=NULL,repfile="Report.sso",versionNo="3.21",
  new.minor.version=c("04-","04a","04b","10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d"),blankLines=NULL){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  if(is.null(blankLines))blankLines<-grep(value=FALSE,pattern="^$",x=report)
  header.char<-"^selparm\\(Size\\)_By_Year_after_adjustments$"
  minor.version<-checkSSversion(report=report,verbose=FALSE)
  Selex.Length<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=NULL,as.numeric.col=NULL,
    as.numeric.as.possible=FALSE,skip=1,debug=FALSE,skip.col.names=-1)
  colnames(Selex.Length)[1:2]<-c("Fleet/Svy", "Year")
  header.char<-"^selparm\\(Age\\)_By_Year_after_adjustments$"
  Selex.Age<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=NULL,as.numeric.col=NULL,
    as.numeric.as.possible=FALSE,skip=1,debug=FALSE,skip.col.names=-1)
  colnames(Selex.Age)[1:2]<-c("Fleet/Svy", "Year")

  return(list(Selex.Length=Selex.Length,Selex.Age=Selex.Age))
}

read.Exploitation<-function(report=NULL,repfile="Report.sso",versionNo="3.21",
 new.minor.version=c("04-","04a","04b","10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d"),blankLines=NULL,debug=FALSE){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  if(is.null(blankLines))blankLines<-grep(value=FALSE,pattern="^$",x=report)
  header.char<-"^EXPLOITATION$"
#  minor.version<-checkSSversion(report=report,verbose=FALSE)
  EXPLOITATION<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=NULL,as.numeric.col=NULL,
    as.numeric.as.possible=FALSE,skip=4,debug=debug,skip.col.names=4)
#  colnames(Selex.Length)[1:2]<-c("Fleet/Svy", "Year")
  headerL<-grep(x=report,value=FALSE,pattern=header)
  F_Method_no<-strsplit(report[headerL+1],split="[[:blank:]]+")[[1]][2]
  F_Method<-strsplit(report[headerL+1],split="[[:blank:]]+")[[1]][3]
  F_std_units<-strsplit(report[headerL+2],split="[[:blank:]]+")[[1]][2]
  YUnit<-strsplit(report[headerL+3],split="[[:blank:]]+")[[1]][-(1:3)]
  FLno<-strsplit(report[headerL+4],split="[[:blank:]]+")[[1]][-(1:3)]
  return(list(EXPLOITATION=EXPLOITATION,F_Method_no=F_Method_no,F_Method=F_Method,F_std_units=F_std_units,YUnit=YUnit,FLno=FLno))
}

read.SPR<-function(report=NULL,repfile="Report.sso",header.char="^SPR_series_uses_R0=",versionNo="3.21",
 new.minor.version=c("04-","04a","04b","10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d"),blankLines=NULL,debug=FALSE){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  if(is.null(blankLines))blankLines<-grep(value=FALSE,pattern="^$",x=report)
#  minor.version<-checkSSversion(report=report,verbose=FALSE)
  SPR<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=NULL,as.numeric.col=NULL,
    as.numeric.as.possible=FALSE,skip=5,debug=debug,skip.col.names=4)
#  colnames(Selex.Length)[1:2]<-c("Fleet/Svy", "Year")
  headerL<-grep(x=report,value=FALSE,pattern=header.char)
  R0<-strsplit(report[headerL],split="[[:blank:]]+")[[1]][2]
  Depletion_method<-strsplit(report[headerL+2],split="[[:blank:]]+")[[1]][2]
  F_std_method<-strsplit(report[headerL+3],split="[[:blank:]]+")[[1]][2]
  SPR_std_method<-strsplit(report[headerL+4],split="[[:blank:]]+")[[1]][2]
#  YUnit<-strsplit(report[headerL+3],split="[[:blank:]]+")[[1]][-(1:3)]
#  FLno<-strsplit(report[headerL+4],split="[[:blank:]]+")[[1]][-(1:3)]
  return(list(SPR=SPR,R0=R0,Depletion_method=Depletion_method,F_std_method=F_std_method,SPR_std_method=SPR_std_method))
}





getHeaderL.old<-function(header.char,report,startL=NULL,endL=NULL,first=TRUE){
  if(is.null(startL))startL=1
  if(is.null(endL))endL=length(report)
  if(first){
    header<-grep(value=FALSE,pattern=header.char,x=report[startL:endL])[1]
  }else{
    header<-grep(value=FALSE,pattern=header.char,x=report[startL:endL])
  }
  return(header)
}



getKWS<-function(headerL,EL,report,oldStyle=TRUE){
#  KWS<-lapply(report,function(report1){browser();unlist(lapply(strsplit(report1[(headerL+1):EL],split="[[:blank:]]+"),function(x)(x[2])))})
  if(!oldStyle){
    KWS<-lapply(report,function(report1){browser();unlist(lapply(strsplit(report1[(headerL+1):EL],split="[[:blank:]]+"),function(x)(x[2])))})
    return(list(KWS=KWS,nKWS=length(KWS)))
  }else{
    KWS<-unlist(lapply(strsplit(report[(headerL+1):EL],split="[[:blank:]]+"),function(x)(x[2])))
    return(list(KWS=KWS,nKWS=length(KWS)))
  }
}

getKWS.old<-function(headerL,EL,report){
  KWS<-unlist(lapply(strsplit(report[(headerL+1):EL],split="[[:blank:]]+"),function(x)(x[2])))
  return(list(KWS=KWS,nKWS=length(KWS)))
}

getLenSelex<-function(report=report,blankLines,oldStyle=FALSE){
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  LenSelex<-getComponent(header.char="LEN_SELEX",report=report,blankLines=blankLines,skip=5,header=TRUE)
}


getLenSelex.old<-function(report=report,blankLines){
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  LenSelex<-getComponent(header.char="LEN_SELEX",report=report,blankLines=blankLines,skip=5,header=TRUE)
}

###############################################################################
##
##
#readDat.new<-function(report,header.char=NULL,footer.char=NULL,blankLines=NULL,colClasses=NULL,col.names=NULL,
#    skip.col.names=0,as.numeric.col=NULL,as.numeric.as.possible=FALSE,
#    skip=0,checkEndRec=FALSE,debug=FALSE){
### getHeaderL<-function(header,report,startL=NULL,endL=NULL,first=TRUE,oldStyle=TRUE){

#    headerL<-getHeaderL(header.char=header.char, report=report)

### getComponent<-function(header.char,report=NULL,blankLines=NULL,skip=0,header=FALSE,footer.char=NULL){

#    res<-getComponent(header.char=header.char,report=report,blankLines=blankLines)

#}

###############################################################################
##
##
##
##
readDat.old<-readDat.oldStyle<-
  function(report,header.char,footer.char=NULL,blankLines=NULL,colClasses=NULL,col.names=NULL,
    skip.col.names=0,as.numeric.col=NULL,as.numeric.as.possible=FALSE,
    skip=0,checkEndRec=FALSE,debug=FALSE){
  if(is.list(report) && length(report)>1)stop("error report should not be list of text")
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  headerL<-grep(value=FALSE,pattern=header.char,x=report)
  if(is.null(footer.char)){
    EL<-componentEndL(headerL=headerL,blankLines=blankLines)
  }else{
    EL<-grep(value=FALSE,pattern=footer.char,x=report)-1
  }
  if(debug){cat("in readDat\n");browser()}
  dat.fields<-report[(headerL+1):EL]
  if(is.null(col.names)){
    if(skip.col.names>=0){
      col.names<-paste(unlist(strsplit(dat.fields[1+skip.col.names],split="[[:blank:]]+")))
    }else{ ###
      ncol<-max(sapply(report[(headerL+1):EL],FUN=function(line){
                                                    length(strsplit(line,split="[[:blank:]]+")[[1]])
                                                    },
                simplify=TRUE))
      col.names<-paste("V",1:ncol,sep="")
    }
  }
  ## header 行無しのデータ部分のみとして読み込む
  dat<-read.table.texts(texts=dat.fields,header=FALSE,skip=skip+1,colClasses=colClasses,col.names=col.names)
  colnames(dat)<-col.names
  if(checkEndRec){
    tmp<-(dat==-1)
    tmp<-apply(tmp,1,FUN=function(x){sum(x,na.rm=TRUE)})
    tmp<-(tmp!=(dim(dat)[2]-2))
    dat<-dat[tmp,]
  }
  if(as.numeric.as.possible){
    dat<-
    apply(dat,c(1,2),FUN=function(x){
      if(length(grep(x=x,pattern="[[:digit:]]"))>0){return(as.numeric(x))}else{return(x)}})
    dat<-as.data.frame(dat)
 #  browser()
  }
  if(!is.null(as.numeric.col)){
    dat[,as.numeric.col]<-apply(dat[,as.numeric.col],c(1,2),as.numeric)
  }
  return(dat)
}


#########################################################################

getRecruitmentDist<-function(report){
  return(getComponent(header.char="^RECRUITMENT_DIST",report=report,header=TRUE))
}

calcGenders<-function(report){
  tmp<-getRecruitmentDist(report=report)
  return(unique(unique(tmp[tmp$"Used?"==1,])$Gender))
}

getMorphIndexing<-function(report){
  ss.version<-checkSSversion(report=report,verbose=TRUE)
#  browser()
  if(as.numeric(substring(ss.version,1,2))<20 || as.numeric(substring(ss.version,1,2))>22){
    return(getComponent(header.char="^MORPH_INDEXING",report=report,header=TRUE))
  }else{
    return(getComponent(header.char="^MORPH_INDEXING",report=report,header=TRUE,footer.char="^#$"))
    }
}




#########################################################################
###### allplot.ss2を流用するためにいくつかの函数を改造

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

#NUMBERS_AT_AGE_Annual_2

getAnnualNAA.ss3.x<-function(report,blankLines=NULL){
  header.char<-"^NUMBERS_AT_AGE_Annual_2" # 年当初の個体数
  getNAA.ss3.x(report=report,blankLines=blankLines,header.char=header.char)
}

# getNAA.ss2.x(report)


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

#browser()
#cat("here\n")
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
    Fres[select][!select2]<-foreach()
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




# aa<-getAgecomp.ss2.1.x(report)

getAgecomp<-getAgecomp.ss3.x <- function(report,blankLines=NULL,size.kind=c("LEN","AGE","SIZE"),compReportFile="compReport.sso"){
  if(is.null(compReportFile))compReportFile<-"compReport.sso"
  composition.database <- getAgecomp.ss3.2.x(compReportFile=compReportFile)
#  cat("finished getAgecomp.ss3.2.x\n")
  fit.len.comps <- lapply(size.kind,FUN=function(x){
    getAgecomp.ss3.1.x(report=report,blankLines=blankLines,size.kind=x)
  })
#  cat("finished getAgecomp.ss3.1.x\n")
# browser()
  composition.database[[1]]$FYS<-
    paste(ifelse(composition.database[[1]]$Fleet<10,paste(0,composition.database[[1]]$Fleet,sep=""),
      composition.database[[1]]$Fleet),floor(composition.database[[1]]$Yr),composition.database[[1]]$Seas,sep="-")
# browser()
  return(list(composition.database[[1]],fit.len.comps[[1]],
       c(composition.database[[2]],fit.len.comps[[2]]),size.kind=size.kind))
}

plotEffNts<-function(report,fleets=NULL,size.kind=c("LEN","AGE","SIZE")){
  lapply(size.kind,FUN=function(x){
      ac1<-getAgecomp.ss3.1.x(report=report,size.kind=x)
#  browser()
      if(is.null(fleets)){
        fleets<-unique(ac1[[1]]$fleet)
        ac<-ac1[[1]]
      }else{
        ac<-ac1[[1]][ac1[[1]] %in% fleets]
      }
      ac$fleetC<-paste("FL",ifelse(ac$fleet<10,paste(0,ac$fleet,sep=""),
        ac$fleet),sep="")
      ac<-ac[ac$Nsamp>0,]
      xy1<-xyplot(data=ac,Nsamp+effN~Year|fleetC,type="l",as.table=TRUE)
      print(xy1)
    }
  )
}


getAgecomp.ss3.1.x <- function(report,blankLines=NULL,size.kind=c("LEN","AGE","SIZE")){

#  desc <- ifelse(len==TRUE,"^FIT_LEN_COMPS","^FIT_AGE_COMPS")
  desc1<-if("LEN" %in% size.kind){"^FIT_LEN_COMPS"}
  desc2<-if("AGE" %in% size.kind){"^FIT_AGE_COMPS"}
  desc3<-if("SIZE" %in% size.kind){"^FIT_SIZE_COMPS"}
  desc<-cbind(desc1,desc2,desc3)
  cat("HERE1312\nheader.char=")
  print(desc)
  cat("\nsize.kind=")
  print(size.kind)
  cat("\n")

  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
# comps<-readDat(report=report,header.char=desc,blankLines=blankLines)
  comps<-readDat(report=report,header.char=desc,blankLines=blankLines,as.numeric.as.possible=TRUE)
  res<-list(comps=comps,nrows=length(comps))


  if(size.kind=="LEN"){
    colnames(res[[1]]) <- c("Fleet","Year","Seas","Gender","Mkt","Nsamp","effN","Like")
  }
  else if(size.kind=="AGE"){
    colnames(res[[1]]) <-
      c("Fleet","Year","Seas","Gender","Mkt","Ageerr","Lbin_lo","Lbin_hi","Nsamp","effN","Like")
  }
  else if(size.kind=="SIZE"){
    colnames(res[[1]]) <-
      c("Fleet", "Yr", "Seas", "Method", "Gender", "Mkt", "Nsamp", "effN", "Like")
  }
  return(res)
}



getAgecomp.2<-getAgecomp.ss3.2.x<-getAgecomp.ss3.2.x.20090618 <- function(compReportFile="compReport.sso",blankLines=NULL,type=c("AGE","LEN","SIZE")){

#    name.tmp<-c("Yr", "Seas", "Fleet", "Rep", "Pick_gender", "Kind", "Part", "Ageerr", "Gender", "Lbin_lo",
#     "Lbin_hi", "Bin", "Obs", "Exp", "Pearson", "N",
#      "effN", "Like", "Cum_obs", "Cum_exp" ,"Used")
#    name.tmp<-c("Yr", "Seas", "Fleet", "Rep", "Pick_gender", "Kind", "Part", "Ageerr", "Gender", "Lbin_lo",
#     "Lbin_hi", "Bin", "Obs", "Exp", "Pearson", "N",
#      "effN", "Like", "Cum_obs", "Cum_exp" ,"Used")
#    name.tmp <- c("year","season","fleet","rep","pick_gender",
#                  "kind","part","ageerr","gender","Lbin_lo","Lbin_hi",
#                  "bin","obs","exp","Pearson","N","effN","like", "Cum_obs", "Cum_exp","Used")


#    name.tmp <- c("year","season","fleet","rep","pick_gender",
#                  "kind","part","ageerr","gender","Lbin_lo","Lbin_hi",
#                  "bin","obs","exp","Pearson","N","effN","like","Used")

  report<-getReport.sso(repfile=compReportFile)
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  header.char<-"^Composition_Database"
  headerL<-grep(x=report,pattern=header.char,value=FALSE)
  name.tmp<-unlist(strsplit(report[headerL+1],split="[[:blank:]]+"))
  type.tmp <- c(rep("numeric",5),"character",rep("numeric",length(name.tmp)-8),"character","character")
#  browser()
  footer.char<-"[[:blank:]]end[[:blank:]]"
#  footer<-"End_comp_data"
  comps<-readDat(report=report,header.char=header.char,footer.char=footer.char,blankLines=blankLines,colClasses=type.tmp,col.names=name.tmp,checkEndRec=TRUE)
  comps<-comps[comps$Kind %in% c("AGE","LEN","SIZE"),]  ## 2012/04/15 added "SIZE"

  nline<-length(comps)
#  close(Comps.fields)
  res<-list(comps=comps,nrows=length(comps))
}




# bb<-getAgecomp.ss2.2.x(report)

getSPR.ss3.x<-function(report){



}

if(0){
getSPR.ss2.x <- function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "SPR_series"
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=TRUE,col.names=paste("V",1:max(cl),sep=""),as.is=TRUE,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=0,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")
    res <- find.and.read.table2(read.char,skipline=1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=0,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")
    res <- find.and.read.table(read.char,skipline=1,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  res[[1]] <- lapply(res[[1]],as.character)
  res[[1]] <- lapply(res[[1]],as.numeric)
  res[[1]] <- as.data.frame(res[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
  res
}
}
##################################

getBabs<-getBabs.ss3.x<-function(report=NULL,repfile="Report.sso",blankLines=NULL,oldStyle=TRUE){
  header.char<-"^TIME_SERIES"
  if(is.null(report))report<-getReport.sso(repfile=repfile,oldStyle=oldStyle)
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  Babs<-readDat(report=report,header.char=header.char,blankLines=blankLines,as.numeric.col=NULL)
#  browser()
  spawnseason<-as.numeric(unique(Babs[!is.na(as.numeric(Babs$SpawnBio)),"Seas"]))
  return(list(biom=Babs,nrow=nrow(Babs),spawnseason=spawnseason))
}

setncol <- function(n){
  if(n<7){
    par(mfrow=c(3,2))
  }
  else{
    if(n<11){
      par(mfrow=c(ceiling(n/2),2))
    }
    else{
      par(mfrow=c(5,2))
    }
  }
  par(mar=c(3,3,1.5,0),oma=c(0,0,3,3),ps=14,mgp=c(1.8,0.3,0))
}
###############################################

plotComps<-function(report,length.on=TRUE,agecomp.on=FALSE,sizecomp.on=FALSE,blankLines=NULL,multiplot=FALSE,
  len.residual.plot=FALSE,lwd.var=NULL,fleet.name=NULL,nline=0.5,
  compReportFile="compReport.sso"){
  ## To show observed and expected size frequency
  if(is.null(lwd.var)){
    if(length(report)>1) lwd.var <- rep(1,length(report))
    else lwd.var <- 1
  }
  set.mypar()
  par(font.main=1,cex.main=1)
  if(length.on==TRUE | agecomp.on==TRUE|sizecomp.on==TRUE){
#    makedevice(filename="length_cor",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=TRUE,append=TRUE)
  nfile <- 1

  if(is.null(lwd.var)){
    if(length(repfile)>1) lwd.var <- rep(1,length(repfile))
    else lwd.var <- 1
  }
  size.kind<-c("LEN","AGE","SIZE")[c(length.on,agecomp.on,sizecomp.on)]
  tmp <- getAgecomp.ss3.x(report=report,blankLines=blankLines,size.kind=size.kind,
            compReportFile=compReportFile)
    comps <- list(list(tmp[[1]],tmp[[2]]))
    comps.target <- tmp[[3]]
    if(multiplot){
#      b.list <- as.list(rep(0,len.rep))
      for(i in 2:len.rep){
        comps[[i]] <-
          getAgecomp.ss3.x(report[i],
                         size.kind=size.kind)
      }
    }

    len.data <- comps[[1]][[1]]
    fleet.row <- sort(unique(comps[[1]][[1]]$fleet))
    #  setncol(nfleet)
    setncol(length(fleet.row))

    # Pearson residual of length data:
    # !!!!!!!! Caution: when size data is too huge, this plot make the result file too heavy!!!!!!!!
    if(len.residual.plot==TRUE){
      #browser()
      s <- 1
      for(i in fleet.row){
        tmp <- len.data[len.data$fleet==i,]
        plot(tmp$bin,y <- tmp$Pearson,ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")
        title(main=paste("Fleet",i,":",fleet.name[i]),line=-1)
      #    sd.tmp <- (tmp$obs-tmp$exp)/sqrt(tmp$exp*(1-tmp$exp)*tmp$N)
      #    plot(tmp$bin,sd.tmp,
        if(!multiplot){
          x1 <- tapply(y,tmp$bin,median)
          x2 <- tapply(y,tmp$bin,mean)
          abline(h=0,col="yellow")
          points(names(x1),x1,type="l",col="red")
          points(names(x2),x2,type="l",col="blue")
        }
        else{
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]][comps[[j]][[1]]$fleet==i,]
            points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }
        }
      }
    }

    ### とりあえず保留する obs vs expの散布図
    if(0){
      for(i in fleet.row){
        plot(x <- len.data$exp[len.data$fleet==i & !is.na(len.data$obs)],
        y <- len.data$obs[len.data$fleet==i & !is.na(len.data$obs)],lwd=lwd.var[1],
             ylim=c(0,max(x,y)),xlim=c(0,max(x,y)),xlab="Predictet size freq",ylab="Observed size freq")
        title(main=paste("fleet ",i),line=nline)

        if(multiplot){
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]]
            points(x <- tmp$exp[tmp$fleet==i & !is.na(tmp$obs)],
                   y <- tmp$obs[tmp$fleet==i & !is.na(tmp$obs)]
                   ,type="p",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }}
        if(length(fleet.row)>10 && s%%10==0){
          mtext(side=3,line=0.5,adj=0.3,"Predicted vs observed size composition",outer=TRUE)
#          nfile <- nfile+1
#          makedevice(filename="length_cor",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                     new=TRUE,append=TRUE)
#          setncol(length(fleet.row))
        }
        s <- s+1
      }
      mtext(side=3,line=0.5,adj=0.3,"Predicted vs observed size composition",outer=TRUE)
      cat("Scatter plot of length frequency (obs vs est) was done.  \n")
    }


  ## Size frequency 2
  ##  (lengthdist関数でもっと短く書くこともできるが、横幅の設定などについて
  ## このほうが都合がよいので、このままにしておく。
    length.bin <- sort(unique(len.data$bin))
    sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
    dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))

    if(multiplot){
#      sum.length <- as.list(rep(0,len.rep))
      for(i in 2:len.rep){
        sum.length[[i]] <- sum.length[[1]]
      }}

#    nfile <- 1
#    makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=TRUE,append=TRUE,width=700)
#    par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)

#   browser()

    s <- 1
    row.name <- paste(floor(as.numeric(len.data$year)),len.data$season,len.data$fleet,sep="-")
    tmp2 <- unique(row.name)
    tmp.ref <- paste(floor(as.numeric(comps[[1]][[2]]$Year)),comps[[1]][[2]]$Seas,comps[[1]][[2]]$fleet,sep="-")
    for(j in 1:length(fleet.row)){
      for(i in 1:length(tmp2)){
        if(sum(len.data$fleet[row.name==tmp2[i]]==fleet.row[j])){
          matplot(x <- len.data$bin[row.name==tmp2[i]],
                  y <- cbind(len.data$obs[row.name==tmp2[i]],
                             len.data$exp[row.name==tmp2[i]])*comps[[1]][[2]]$Nsamp[tmp.ref==tmp2[i]],
                  lwd=lwd.var[1],col=c("royalblue",1),type=c("b","l"),
                  pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")
        ## Sum up size data by fisheries
          y <- y[!is.na(y[,1]),]
          x <- x[!is.na(x)]
          sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
          ##
          title(main=paste(tmp2[i]),line=0.5)

          ## For multiple plots ##
          if(multiplot){
            for(k in 2:len.rep){
              b.tmp <- comps[[k]]
              row.name2 <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
              tmp.ref2 <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Fleet,sep="-")

              x <- b.tmp[[1]]$bin[row.name2==tmp2[i]]
              y <- cbind(b.tmp[[1]]$obs[row.name2==tmp2[i]],
                         b.tmp[[1]]$exp[row.name2==tmp2[i]]) * b.tmp[[2]]$Nsamp[tmp.ref2==tmp2[i]]
              points(x,y[,2],col=col.var[k],lty=lty.var[k],lwd=lwd.var[k],type="l")
              y <- y[!is.na(y[,1]),]
              x <- x[!is.na(x)]
              sum.length[[k]][match(x,length.bin),j,] <- sum.length[[k]][match(x,length.bin),j,]+y
            }}
        ##

          if(s%%25==0){
            mtext(side=3,line=0.5,adj=0.3,
                  "Length fit (by sample, line: expected, line+circle: observed)",outer=TRUE)
#            nfile <- nfile+1
#            makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                       new=TRUE,append=TRUE,width=700)
#            par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)#mgp=c(2,1,0),ps=14)
          }
          s <- s+1
        }}}
  cat("Plot of expected and observed length frequency by each observation was done.  \n")

#  par(mfrow=c(length(fleet.row),1))
    nfile <- 1
#    makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=TRUE,append=TRUE)
    n <- dim(sum.length[[1]])[[2]]
    setncol(n)
    for(i in 1:n){
      matplot(x <- length.bin,y <- sum.length[[1]][,i,],type=c("b","l"),lty=1,
              pch=1,ylab="nsmple",col=c("black","royalblue"),lwd=lwd.var[1],xlab="Length (cm)")
      title(paste("Fleet",fleet.row[i],":",fleet.name[fleet.row[i]]),line=nline)

      if(multiplot){
        for(k in 2:len.rep){
          points(length.bin,sum.length[[k]][,i,2],
                 type="l",col=col.var[k+1],lty=lty.var[k],lwd=lwd.var[k])
          }}

      if(n>10 && i%%10==0){
        mtext(side=3,line=0.5,adj=0.3,
              "Length fit (by fleet, line: expected, line+circle: observed)",outer=TRUE)
#        nfile <- nfile+1
#        makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                   new=TRUE,append=TRUE)
#        setncol(n)
      }}
    mtext(side=3,line=0.5,adj=0.3,"Length fit (by fleet, line:predicted, line+circle: observed))",
          outer=TRUE)
    cat("Plot of expected and observed length frequency by fleets was done.  \n")

    # Bubble plot not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=TRUE)
    max.res.fleet <- tapply(abs(len.data$Pearson),len.data$fleet,max,na.rm=TRUE)
    par(mfrow=c(3,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"pink")
    for(j in 1:length(fleet.row)){
      with(len.data[len.data$fleet==j,],symbols(year,bin,circles=sqrt(abs(x <- Pearson))/8,
                          fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                          inches=FALSE,ylim=yrange,xlim=xrange))
#                          inches=max.res.fleet[names(max.res.fleet)==fleet.row[j]]/100))
      title(paste("Fleet",j,":",fleet.name[j]),line=nline)
      if(j%%3==0) mtext("Bubble plot of Pearson residuals, pink(obs<exp), black (obs>exp)",outer=TRUE)
    }
  }
#  if(names(dev.cur())!="null device") dev.off()
}

set.mypar <- function(){
  par(mgp=c(1.8,0.3,0),tck=0.03,ps=14,cex.main=1.5,font.main=1,adj=0.5)
}

## Initialization of graphics device
##
## 2011/04/12
##

#############################family##########################################################
  init.graph<-function(type=ifelse(.Platform$GUI=="RStudio","RStudio",ifelse(.Platform$GUI=="AQUA","quartz",
    ifelse(.Platform$OS.type=="windows","windows","x11"))),
    ggplotlike=FALSE,filename=NULL,Cairo=FALSE,height=11,width=9,onefile=TRUE,
    units="in",lattice=TRUE,title="",family="Helvetica",gtype=NULL,ggplot2=FALSE,mfrow=c(3,2),
    debug=FALSE,GUI=ifelse(.Platform$OS.type=="windows",TRUE,FALSE),japanese=FALSE){

    if(is.null(gtype))gtype="base"
    if(lattice)gtype="lattice"
    if(ggplot2)gtype="ggplot2"
    if(is.null(type)){
      type<-ifelse(.Platform$GUI=="RStudio","RStudio",ifelse(.Platform$GUI=="AQUA","quartz",ifelse(.Platform$OS.type=="windows","windows","pdf")))
    }
#    cat("type=",type,"\n")
    if(gtype=="base"){
      plot.type<-gtype
      lattice<-FALSE
      ggplot2<-FALSE

    }else if(gtype=="lattice"){
      plot.type<-gtype
      lattice<-TRUE
      ggplot2<-FALSE
      require(lattice)||stop("lattice is required")
      require(latticeExtra)||stop("latticeExtra is required")
    }else if(gtype=="ggplot2"){
      plot.type<-gtype
      lattice<-FALSE
      ggplot2<-TRUE
    }else{
      stop("no appropriate gtype")
    }
    if(Cairo && sessionInfo()$R.version$arch=="x86_64" && as.numeric(sessionInfo()$R.version$minor)<12.2 && .Platform$OS.type=="windows"){
      cat("Currently neither Cairo nor cairoDevice work properly in 64bit version so that ordinary pdf device will be used\n")
      Cairo<-FALSE
    }

    if(Cairo)require(Cairo) || stop("package Cairo is required")
    if(is.null(type))type="windows"
    if(is.null(filename)){
      filename="plot"
      if(type=="pdf")filename<-"plot"
      if(type=="win.metafile")filename<-"plot"
    }
    if(type=="RStudio")GUI<-TRUE
    if(type=="AQUA")GUI<-TRUE
    cat("type=",type,"\n")
    dev.used<-NULL
    if(debug)browser()
    if(type=="pdf"){
      if(!Cairo){
        pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=11,width=9,family=ifelse(japanese,"Japan1",family))

      }else{
        CairoPDF(file=paste(filename,".pdf",sep=""),paper="a4",height=11,width=9,family=ifelse(japanese,"Japan1",family))
      }
    }else if(type=="windows"){
      if(!Cairo){
        windows(record=TRUE,,height=11,width=9)
      }else{
        CairoWin(record=TRUE,,height=11,width=9)
      }
    }else if(type=="win.metafile"){
      win.metafile(paste(filename,"%03d.emf",sep=""), pointsize = 10,height=height,width=width)
    }else if(type=="png"){
      png(filename = paste(filename,"%03d.png",sep=""), width = 800, height = 800,units = "px", pointsize = 12, bg = "white", res = NA,
        restoreConsole = TRUE)
    }else if(type=="SVG" && Cairo){
      CairoSVG(file = ifelse(onefile, paste(filename,".svg",sep=""), paste(filename,"%03d.svg",sep="")),
         width = width, height = height, onefile = onefile, bg = "transparent",pointsize = 12)
    }else if(type=="postscript"){
      if(!Cairo){
          postscript(file = ifelse(onefile, paste(filename,".ps",sep=""), paste(filename,"%03d.ps",sep="")),
           onefile=onefile, family=family, title=title, fonts=NULL, encoding="default", bg="transparent", fg="black",
           width=width, height=height, horizontal=FALSE,
           paper="a4", pagecentre=TRUE,
           useKerning=TRUE)
      }else{
        CairoPS(file = ifelse(onefile, paste(filename,".ps",sep=""), paste(filename,"%03d.ps",sep="")),
          onefile = onefile, family=family, title =title, fonts = NULL,
          bg="transparent",  width=width, height=height,horizontal=FALSE)
      }
    }else if(type=="quartz"){
      quartz()
    }else if(type=="x11"){
      x11()
    }else if(type=="RStudio"){
      # do nothing
      if(!names(dev.cur())=="null device")dev.off()
    }else{
      stop("no appropriate graphics device type")
    }
#    if(debug)browser()

    dev.used<-dev.cur()
    opar<-NULL
    if(lattice||ggplot2){
      opar <- trellis.par.get()
    }else{
      opar<-par(no.readonly = TRUE)
    }
    oopt<-NULL
    if(ggplotlike && lattice){
      trellis.par.set(ggplot2like(n = 4, h.start = 180))
      oopt <- lattice.options()
      lattice.options(ggplot2like.opts())
    }
    exit.fn<-function(lattice=FALSE,ggplot2=FALSE,ggplotlike=NULL,opar=NULL,oopt=NULL,dev.used=NULL,gtype=NULL){
      if(is.null(gtype))gtype<-"base"
      if(lattice)gtype<-"lattice"
      if(ggplot2)gtype<-"ggplot2"
      if(gtype=="base"){
        par(opar)
      }else if(gtype=="lattice"||gtype=="ggplot2"){
        trellis.par.set(opar)
        if(gtype=="lattice"){
          lattice<-TRUE
        }else if(gtype=="ggplot2"){
          ggplot2<-TRUE
        }
      }
#      cat("exit.fn is called")
#      if(names(dev.cur())!="null device" && type!="windows")dev.off()
      cat("dev.used=\n")
      print(dev.used)
      if(names(dev.used)!="windows")dev.off(which=dev.used[[1]])
      cat(paste("ggplotlike=",ggplotlike,"\n",sep=""))
      cat(paste("lattice=",lattice,"\n",sep=""))
      if(ggplotlike && lattice)lattice.options(oopt)
    }
#    on.exit(exit.fn)
    return(list(exit.fn=exit.fn,opar=opar,oopt=oopt,lattice=lattice,
      ggplotlike=ggplotlike,dev.used=dev.used,gtype=gtype))
#    return(list(opar=opar,oopt=oopt))
  }



#######################################################################################

plotComps.lattice<-function(repfile="Report.sso",report=NULL,filename=NULL,type=NULL,layout=c(2,6),fit=TRUE,aggregated=TRUE,
  fleets=NULL,blankLines=NULL,Season=FALSE,Prop=FALSE,xlim,age=FALSE,summaryObs=FALSE,ggplotlike=FALSE,plot.new=TRUE,Cairo=FALSE,
  compReportFile="compReport.sso"){

  if(is.null(report)){report<-getReport(repfile=repfile,interactive=TRUE)}
#############################################################################################3

  if(plot.new){
    inits<-init.graph(type=type,ggplotlike=ggplotlike,filename=filename,Cairo=Cairo,lattice=TRUE)
    on.exit(inits$exit.fn(dev.used=inits$dev.used,lattice=inits$lattice,ggplotlike=inits$ggplotlike,oopt=inits$oopt,opar=inits$opar))
#    cat("called\n")
  }
########################################################################################################
#  browser()
#  on.exit(if(names(dev.cur())!="null device" && type!="windows") dev.off())
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  a2<-getAgecomp.ss3.x(report=report,blankLines=blankLines,compReportFile=compReportFile)
  # age=Tなら年齢組成のみ取り出す
  a2[[1]]<-a2[[1]][a2[[1]]$Kind==ifelse(age,"AGE","LEN"),]
  # a2[[1]]$FYS<-paste(ifelse(a2[[1]]$fleet<10,paste(0,a2[[1]]$fleet,sep=""),a2[[1]]$fleet),floor(a2[[1]]$year),a2[[1]]$season,sep="-")
  if(!is.null(fleets)){
    tmp<-a2[[1]][a2[[1]]$Fleet %in% fleets,]
  }else{
    tmp<-a2[[1]]
  }
  if(fit){
    tmp1<-subset(tmp,!is.na(Bin))
#    browser()
    if(missing(xlim))xlim<-c(min(tmp1$Bin)-0.1,max(tmp1$Bin)+0.1)
#    browser()
    xx<-xyplot(data=tmp1,Obs*N+Exp*N~Bin|FYS,layout=layout,drop.unused.level=TRUE,
      scales=list(y="free",x=list(tick.number=(xlim[2]-xlim[1])/5)),
      auto.key=list(points=FALSE,lines=TRUE,columns=2),type=c("l","h"),lty=c(1,2),xlab="Length",ylab="pred vs obs",as.table=TRUE,xlim=xlim)
      print(xx)
  }

  if(Season){
    tmp1<-subset(tmp,!is.na(Bin))
    if(missing(xlim))xlim<-c(min(tmp1$Bin)-0.1,max(tmp1$Bin)+0.1)
 #   browser()
    if(!Prop){
      t5<-aggregate(tmp1$Exp*tmp1$N,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),sum )
    }else{
      t5<-aggregate(tmp1$Exp,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),mean )
      }
#    browser()
    names(t5)<-c("Bin","Fleet","Seas","Exp")
    if(!Prop){
      t6<-aggregate(tmp1$Obs*tmp1$N,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),sum )
    }else{
      t6<-aggregate(tmp1$Obs,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),mean )
    }
    names(t6)<-c("Bin","Fleet","Seas","Obs")
    t5<-t5[-c(1,2,3)]
    t7<-cbind(t6,t5)
    t7$FS<-paste(ifelse(t7$Fleet<10,paste(0,t7$Fleet,sep=""),t7$Fleet),t7$Seas,sep="-")
   # browser()
    xy<-xyplot(data=t7,Exp+Obs~as.numeric(Bin)|FS,auto.key=list(points=FALSE,lines=TRUE,columns=2),
      lty=c(1,2),type=c("l","g"),scale=list(y="free",x=list(tick.number=(xlim[2]-xlim[1])/10)),columns=2,
      as.table=TRUE,layout=layout,xlab="Length",ylab="pred vs obs",xlim=xlim)
    print(xy)
  }

  if(aggregated){
    tmp1<-subset(tmp,!is.na(Bin))
    if(missing(xlim))xlim<-c(min(tmp1$Bin)-0.1,max(tmp1$Bin)+0.1)
    if(!Prop){
      t5<-aggregate(tmp1$Exp*tmp1$N,list(tmp1$Bin,tmp1$Fleet),sum )
    }else{
      t5<-aggregate(tmp1$Exp,list(tmp1$Bin,tmp1$Fleet),mean )
    }
    names(t5)<-c("Bin","Fleet","Exp")
    if(!Prop){
      t6<-aggregate(tmp1$Obs*tmp1$N,list(tmp1$Bin,tmp1$Fleet),sum )
    }else{
      t6<-aggregate(tmp1$Obs,list(tmp1$Bin,tmp1$Fleet),mean )
    }
    names(t6)<-c("Bin","Fleet","Obs")
    t5<-t5[-c(1,2)]
    t7<-cbind(t6,t5)

    xy<-xyplot(data=t7,Exp+Obs~as.numeric(Bin)|factor(Fleet),auto.key=list(points=FALSE,lines=TRUE,columns=2),
    lty=c(1,2),type=c("l","g"),scale=list(y="free",x=list(tick.number=(xlim[2]-xlim[1])/10)),columns=2,
    as.table=TRUE,layout=layout,xlab="Length",ylab="pred vs obs",xlim=xlim)
    print(xy)
  }
  if(summaryObs){
    tmp1<-subset(tmp,!is.na(Bin))
    if(missing(xlim))xlim<-c(min(tmp1$Bin)-0.1,max(tmp1$Bin)+0.1)
#    t5<-aggregate(tmp1$Exp*tmp1$N,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),sum )

#    browser()
#    names(t5)<-c("Bin","Fleet","Seas","Exp")
    if(!Prop){
      t6<-aggregate(tmp1$Obs*tmp1$N,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),sum )
    }else{
      t6<-aggregate(tmp1$Obs,list(tmp1$Bin,tmp1$Fleet,tmp1$Seas),mean )
    }
    names(t6)<-c("Bin","Fleet","Seas","Obs")
#    t5<-t5[-c(1,2,3)]
    t7<-t6
    t7$FS<-paste("FL",ifelse(t7$Fleet<10,paste(0,t7$Fleet,sep=""),t7$Fleet),sep="")
#   browser()
#   key.Seas <- list(space = "top", text = lapply(paste("S",unique(t7$Seas),sep=""),FUN=function(x){return(x)}), points = list(pch = 1:4, col = "black"))
#,type=c("o","g")
    cols<-c("black","pink","purple","blue")
    ltys<-c(1,2,3,4)
 #   browser()
    texts<-levels(as.factor(t7$Seas))
    xy<-xyplot(data=t7,Obs~as.numeric(Bin)|FS,auto.key=list(points=FALSE,lines=TRUE,lty=ltys,columns=4,col=cols,text=texts,type=c("l","l","l","l")),
      lty=ltys,lwd=1,type=c("l","l","l","l","g"),cex=2,col=cols,scale=list(y="free",x=list(tick.number=(xlim[2]-xlim[1])/10)),
      columns=2,par.settings = simpleTheme(pch = 16),
      as.table=TRUE,layout=layout,xlab="Length",ylab="obs",xlim=xlim,groups=t7$Seas,distribute.type=TRUE)
    print(xy)
#    browser()
#    xy<-xyplot(data=t7,Obs~as.numeric(Bin)|FS,auto.key=list(columns=2,cex=1:4,lines=TRUE),
#      lty=c(1,2,3,4),type=c("l","g"),scale=list(y="free",x=list(tick.number=(xlim[2]-xlim[1])/10)),
#      columns=2,
#      as.table=TRUE,layout=layout,xlab="Length",ylab="obs",xlim=xlim,groups=Seas)
#
##    browser()
#    print(xy)
#    browser()
  }
}
# xyplot(data=az,Value+ValueMinus2sigma+ValuePlus2sigma~as.numeric(Index)|Label,auto.key=list(points=TRUE,lines=TRUE,columns=2),
#  as.table=TRUE,scales=list(y="free"),type=c("o","l","l","g"),ylab="SSB(ton)",xlab="year",distribute.type=TRUE,col="Black")

getDerivedQuant<-function(repfile="Report.sso", report=NULL,blankLines=NULL,Quant=NULL){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }

  header.char<-"^DERIVED_QUANTITIES$"
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  hl<-grep(x=report,pattern=header.char,value=FALSE)
  EL<-blankLines[grep(x=blankLines,pattern=hl-1,value=FALSE)+1][1]
  ## derived quantityが含まれる行を切り出す。
  DQ<-report[(hl+5):(EL-1)]
  ## 空白文字で切り分ける、出力は、一行毎のリスト
  tmp1<-strsplit(DQ,split="[[:blank:]]+")

  Quant.vector<-sapply(sapply(tmp1,FUN=function(x){x[-1]},simplify=TRUE),FUN=function(x){strsplit(x[1],split="_")[[1]][1]},simplify=TRUE)
#  browser()  要チェック
  if(is.null(Quant)){
    Quant<-unique(Quant.vector)
  }
  # 先頭の空白のみの要素を削除
  tmp<-t(sapply(tmp1,FUN=function(x){x[-1]},simplify=TRUE))
  # ラベル部分を編集
#  if(is.array(tmp)){
#   tt<-strsplit(tmp[,1],split="_")
#   x1<-sapply(tt,FUN=function(x){x[[1]]})
#   x2<-sapply(tt,FUN=function(x){x[[2]]})
#   browser()
#   tmp2<-cbind(cbind(x1,x2),tmp[,2:3])
#   browser()
#  }else{
  tmp2<-sapply(tmp,FUN=function(x){tt<-c(strsplit(x[1],split="_")[[1]],x[-1]);return(tt)},simplify=TRUE)
#   }
# browser()
#  zz<-data.frame()
#  browser()
# if(is.array(tmp2)){
#   zz<-tmp2
#     colnames(zz)<-c("Label", "Index", "Value",  "StdDev","(Val-1.0)/Stddev", "CumNorm")[1:length(colnames(zz))]
#     browser()
#     zz<-as.data.frame(zz)
#   }else{
    zz<-
      as.data.frame(sapply(1:6,FUN=function(i){sapply(tmp2,FUN=function(x){if(i>=3){as.numeric(x[i])}else{x[i]}})}),stringsAsFactors=FALSE)
      colnames(zz)<-c("Label", "Index", "Value",  "StdDev","(Val-1.0)/Stddev", "CumNorm")
# }
#  browser()
  for(i in 3:length(colnames(zz))){
    zz[,i]<-as.numeric(zz[,i])
  }
#  zz[,3]<-as.numeric(zz[,3])
#  zz[,4]<-as.numeric(zz[,4])
#  zz[,5]<-as.numeric(zz[,5])
#  zz[,6]<-as.numeric(zz[,6])

#  zz$minus2sigma<--2*zz$StdDev
#  zz$plus2sigma<-2*zz$StdDev
#  zz$ValuePlus2sigma<-zz$Value+2*zz$StdDev
#  zz$ValueMinus2sigma<-zz$Value-2*zz$StdDev
#  browser()
  DerivedQuant.list<-lapply(Quant,FUN=function(x){
                                    #browser()
                                    #if(is.array(tmp)){
                                      zz<-tmp[,which(Quant.vector %in% x)]
                                    #}else{
                                      zz<-tmp[which(Quant.vector %in% x)]
                                      xx<-t(sapply(zz,FUN=function(y){suppressWarnings(as.numeric(y[2:length(y)]))}))
                                      if(dim(xx)[2]==2){
                                        dimnames(xx)[[2]]<-c("Value",  "StdDev")
                                      }else if(dim(xx)[2]==4){
                                        dimnames(xx)[[2]]<-c("Value",  "StdDev","(Val-1.0)/Stddev", "CumNorm")
                                      }
                                    #}
                                    yy<-sapply(zz,FUN=function(y){strsplit(y[1],split="_")[[1]][2]})

                                    DQ.DF<-data.frame(Label=yy,stringsAsFactors=FALSE)
                                  # DQ.DF<-as.data.frame(xx)
                                    DQ.DF<-cbind(DQ.DF,as.data.frame(xx))
                                  # (DQ.DF)
                                  # DQ.DF$Label<-yy

                                    return(DQ.DF)
                                  })



#  DQ<-getComponent(header.char=header,report=report,blankLines=blankLines,header=FALSE,skip=3)
#  browser()
  names(DerivedQuant.list)<-Quant
#  hl<-grep(x=report,pattern=header,value=FALSE)
#  name.label<-unlist(strsplit(report[hl+3],split="[[:blank:]]+"))
#  names(DQ)<-name.label[1:ncol(DQ)]

  return(list(DerivedQuant.list=DerivedQuant.list,DerivedQuant.D.F=zz))
}

if(0){
  az<-DQ[[2]][!is.na(as.numeric(DQ[[2]][,2])),]
  xyplot(data=az,Value+ValueMinus2sigma+ValuePlus2sigma~as.numeric(Index)|Label,auto.key=list(points=TRUE,lines=TRUE,columns=2),
  as.table=TRUE,scales=list(y="free"),type=c("o","l","l","g"),ylab="SSB(ton)",xlab="year",distribute.type=TRUE,col="Black")

  DQSPB<-DQ$SPB
  DQSPB$minus2sigma<--2*DQSPB$StdDev
  DQSPB$plus2sigma<-2*DQSPB$StdDev
  DQSPB$ValuePlus2sigma<-DQSPB$Value+2*DQSPB$StdDev
  DQSPB$ValueMinus2sigma<-DQSPB$Value-2*DQSPB$StdDev

  DQSPB.tmp<-DQSPB[!is.na(as.numeric(DQSPB$Label)),]
  DQSPB.tmp$Year<-as.numeric(DQSPB.tmp$Label)
#  ab<-reshape(data=DQSPB.tmp,timevar="Label",varying=c("Value","StdDev","minus2sigma","plus2sigma","ValueMinus2sigma","ValuePlus2sigma"),v.names="val",direction="long",idvar="DerivedQ")


  xyplot(DQSPB.tmp$Value+DQSPB.tmp$ValueMinus2sigma+DQSPB.tmp$ValuePlus2sigma~DQSPB.tmp$Year,aspect="xy",type=c("o","l","l","g"),ylab="SSB(ton)",xlab="year",distribute.type=TRUE,col="Black")


}

getCPUE<-getCPUE.ss3.x<-function(report,blankLines=NULL,FleetNames=NULL){
  header.char<-"^INDEX_1$"
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  if(is.null(FleetNames)){
    FLHeaderL<-getHeaderL("^FleetNames$",report)
    FLEL<-componentEndL(FLHeaderL,blankLines)
    FleetNames<-getKWS(FLHeaderL,EL=FLEL,report=report)
  }
  headerL<-grep(value=FALSE,pattern=header.char,x=report)
 #EL<-componentEndL(headerL=headerL,blankLines=blankLines)
  CPUE.fields<-report[(headerL+1):(headerL+FleetNames[[2]]+1)]
  name.label<-c(unlist(strsplit(CPUE.fields[1],split="[[:blank:]]+")),"FleetNames")
  con.CPUE<-textConnection(CPUE.fields)
  CPUE<-read.table(con.CPUE,fill=TRUE,as.is=TRUE,header=FALSE,skip=1)
  close(con.CPUE)
  names(CPUE)<-name.label
  return(CPUE)

}

  layout.fill.page<-function(Fleet,wide=FALSE){
    fleets<-unique(Fleet)
    if(length(fleets)<=8){
      if(length(fleets)%%2 ==0){
        layout<-c(2,floor(length(fleets)/2))
      }else{
        layout<-c(2,floor(length(fleets)/2)+1)
      }
    }else{
      layout<-c(2,4)
    }
    if(wide)layout<-c(1,8)
    return(layout)
  }

makeCPUEfit<-getCPUEfit<-getCPUEfit.ss3.x<-function(repfile="Report.sso",report=NULL,blankLines=NULL,
  layout=c(2,6),plot=FALSE,type=NULL,Cairo=FALSE,filename=NULL,ggplotlike=FALSE,xscale="free",yscale="free",
  adjust=1.0,kernel="gaussian",kern.window="gaussian",ylab=NULL,kernel.bw="nrd0",plot.new=TRUE,fill.page=TRUE,
  plotType=c("CPUE","logCPUE","Vuln_bio","LikeSigma","Like","residualsTS","residualsHist","Q","logQ"),base0=TRUE){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }

  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  fit<-getComponent(header.char="^INDEX_2$",report=report,blankLines=blankLines,header=FALSE,as.numeric.as.possible=TRUE)
  cat("HERE2052\n")
#  browser()
  hl<-grep(x=report,pattern="^INDEX_2$",value=FALSE)
  name.label<-unlist(strsplit(report[hl+1],split="[[:blank:]]+"))
  names(fit)<-name.label[1:ncol(fit)]

  fit$logObs<-log(as.numeric(fit$Obs))
  fit$logExp<-log(as.numeric(fit$Exp))
#  fit$Residuals<-fit$logObs-fit$logExp
  cat("HERE2069\n")
#  browser() ## HERE
  fit$Residuals<-as.numeric(fit$Dev)



  if(plot){
    if(fill.page)layout<-layout.fill.page(fit$Fleet)
#    require(lattice) || stop("package lattice is required")
#    require(latticeExtra) || stop("package latticeExtra is required")
    if(plot.new){
      inits<-init.graph(type=type,ggplotlike=ggplotlike,filename=filename,Cairo=Cairo,lattice=TRUE)
      on.exit(inits$exit.fn(dev.used=inits$dev.used,lattice=inits$lattice,ggplotlike=inits$ggplotlike,oopt=inits$oopt,opar=inits$opar))

    }
    tmp.fnc<-function(plotType){
      cat("plotType:",plotType,"\n")
      if(plotType=="CPUE"){
  #      cat("HERE2077\n")
  #      browser()
        if(is.null(ylab))
        xy<-xyplot(data=fit,Obs+Exp~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("l","l","g"),
          lty=c(1,2),col=c("blue","red"),layout=layout,distribute.type=TRUE,auto.key=list(columns=2),
          ylab="Obs vs Exp CPUE",xlab="Year",main="Expected vs Observed CPUEs")
      }else if(plotType=="logCPUE"){
        xy<-xyplot(data=fit,logObs+logExp~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("l","l","g"),
          lty=c(1,2),col=c("blue","red"),layout=layout,distribute.type=TRUE,auto.key=list(columns=2),
          ylab="Obs vs Exp logCPUE",xlab="Year",main="Obs vs Exp logCPUE")
      }else if(plotType=="Vuln_bio"){
        xy<-xyplot(data=fit,Vuln_bio~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("l","g"),
          lty=c(1,2),col=c("blue"),layout=layout,distribute.type=TRUE,auto.key=list(columns=2),
          ylab="Vulnerable Biomass",xlab="Year",main="Vulnerable biomass")
      }else if(plotType=="Like"){
        xy<-xyplot(data=fit,Like~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("h","g"),
          lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
          ylab="-logLikelihood w/o logS",xlab="Year",main="-logLikelihood without logS")
      }else if(plotType=="LikeSigma"){
  #      browser()
        xy<-xyplot(data=fit,fit$'Like+log(s)'~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("h","g"),
          lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
          ylab="-logLikelihood with logS",xlab="Year",main="-logLikelihood with logS")
      }else if(plotType=="residualsTS"){
        xy<-xyplot(data=fit,Residuals~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("h","g"),
          lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
          ylab="residuals",xlab="Year",main="residuals by year")
      }else if(plotType=="residualsHist"){
        xy<-histogram(data=fit,~Residuals|as.factor(Fleet),as.table=TRUE,scales=list(y=yscale,x=xscale),
          lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
          ylab="Frequency",xlab="residual",type="density",main="residuals density",
          panel = function(x, ...) {
                      panel.histogram(x, ...)
                      panel.densityplot(x,plot.points=FALSE,
                      darg = list(adjust=adjust,kernel=kernel,window=kern.window,bw=kernel.bw), col = "black",
                      )
        })
      }else if(plotType=="Q"){
        xy<-xyplot(data=fit,Calc_Q+Eff_Q~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("l","l","g"),
          lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
          ylab="Q",xlab="Year",main="Calc_Q and Eff_Q by year")
      }else if(plotType=="logQ"){
        fit$logCalc_Q<-log(fit$Calc_Q)
        fit$logEff_Q<-log(fit$Eff_Q)
        xy<-xyplot(data=fit,logCalc_Q+logEff_Q~Yr|Fleet,as.table=TRUE,scales=list(y=yscale),type=c("l","l","g"),
          lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
          ylab="logQ",xlab="Year",main="logCalc_Q and logEff_Q by year")
      }else{
        stop("no appropriate plotType")
      }
      if(plot)print(xy)
      return(xy)
    }
    xy<-lapply(plotType,FUN=tmp.fnc)
    ## reset
  }else{
    xy<-NULL
  }
  return(invisible(list(xy=xy,fit=fit,nrow=nrow(fit))))
}

getNMA<-getNMA.ss3.x<-function(report=NULL,repfile="Report.sso",blankLines=NULL,oldStyle=FALSE){
  if(is.null(report))report<-getReport.sso(repfile=repfile,oldStyle=oldStyle)
  if(is.null(blankLines) && oldStyle){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }else{
      lapply(report[[1]],FUN=function(z){grep(value=FALSE,pattern="^$",x=z)})
    }

  nma<-getComponent(header.char="^Biology_at_age",report=report,blankLines=blankLines,header=TRUE,as.numeric.as.possible=TRUE)
  return(list(nma=nma,nrow=nrow(nma)))
}

getALK<-getALK.ss3.x<-function(report,blankLines=NULL,all=TRUE,qt=4){
  header.char="^AGE_LENGTH_KEY$"
  if(is.null(blankLines)){
    blankLines<-grep(value=FALSE,pattern="^$",x=report)
  }
  ALK<-getComponent(header.char=header.char,report=report,blankLines=blankLines,header=TRUE,skip=5)

  if(all){
    ALK.list<-list()
    ALK.list[[1]]<-ALK
    headerLALK<-getHeaderL("^AGE_LENGTH_KEY$",report)
    headerL<-getHeaderL("^[ \t]+Seas:",report[headerLALK:length(report)])
    EL<-componentEndL(headerL=headerL,blankLines=blankLines)
    for(s in 2:qt){
      blankLines<-grep(value=FALSE,pattern="^$",x=report[headerL:length(report)])
      ALK
    }
  }

  return(list(ALK=ALK,nrow=nrow(ALK)))
}

getKeywords<-function(report){
  blankLines<-grep(value=FALSE,pattern="^$",x=report)
  KeyWordsHeaderL<-grep(value=FALSE,pattern="KeyWords",x=report)
  KeyWordsL<-(KeyWordsHeaderL+1):(min(blankLines[blankLines>KeyWordsHeaderL ])-1)
  KeyWords<-strsplit(report[KeyWordsL],split="[[:blank:]]+")
  KeyWords<-unlist(lapply(KeyWords,FUN<-function(x)(x[2])))
  return(KeyWords)
}

if(0){

report<-readLines("report.sso")

blankLines<-grep(value=FALSE,pattern="^$",x=report)

KeyWordsHeaderL<-grep(value=FALSE,pattern="KeyWords",x=report)

KeyWordsL<-(KeyWordsHeaderL+1):(min(blankLines[blankLines>KeyWordsHeaderL ])-1)

KeyWords<-strsplit(report[KeyWordsL],split="[[:blank:]]+")
KeyWords<-unlist(lapply(KeyWords,FUN<-function(x)(x[2])))


FLHeaderL<-getHeaderL("^FleetNames$",report)
FLEL<-componentEndL(FLHeaderL,blankLines)

FleetNames<-getKWS(FLHeaderL,EL=FLEL,report=report)

Composition_Database<-getComponent(header.char="Composition_Database",report=report,blankLines=blankLines)


FLHeaderL<-getHeaderL("^FleetNames$",report)
FLEL<-componentEndL(FLHeaderL,blankLines)

FleetNames<-getKWS(FLHeaderL,EL=FLEL,report=report)
}


if(0){
LFMaster<-read.table("LFMaster.txt",header=TRUE)
LF1<-LFMaster[LFMaster$TOTAL>100,]
LF2<-reshape(data=LF1,varying=4:118,sep="",direction="long",idvar=c("Yr","Qt","Fl"))
modifyBin<-function(data,newbin){
  binwidth<-c(diff(newbin),newbin[length(newbin)]-newbin[length(newbin)-1])
  time1.Index<-sapply(data$time,FUN=function(x){findInterval(x,newbin)})
  time1<-newbin[time1.Index]
  data$time<-time1
  return(data)
}
LF3<-modifyBin(data=LF2,newbin=seq(26,140,2))
LF3.1<-aggregate(x=LF3$L,list(LF3$Yr,LF3$Qt,LF3$Fl,LF3$time),sum)
 colnames(LF3.1)<-c("Yr","Qt","Fl","time","L")

LF3.2<-reshape(data=LF3.1,direction="wide",idvar=c("Yr","Qt","Fl"))
}

getNAA.y<-getNAA.y.ss3<-function(repfile="Report.sso",report=NULL,Seas=1){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  naa<-getNAA.ss3.x(report=report)

  naa.tmp<-naa$naa[naa$naa$Seas==Seas,]
  naa.tmp<-naa.tmp[,-c(1:5,8:10)]
  require(reshape)||stop("library reshape is required")
  naa.melted<-melt(naa.tmp,id.vars=c("Yr","Morph"))
  colnames(naa.melted)[3]<-"Age"
#  browser()
  naa_y<-as.array(xtabs(data=naa.melted,value~Yr+Age+Morph))
#  browser()
#  naa_y<-subset(naa[[1]][(naa[[1]]$Seas==1 & naa[[1]]$Era=="TIME"),],select=-c(Era,Seas))
  dimnames(naa_y)[[2]]<-paste("Age",dimnames(naa_y)[[2]],sep=".")
  if(length(naa$info$Morph)==1){
#    browser()
#    row.names(naa_y)<-naa_y$Yr
#    naa_y<-naa_y[,-(1:8)]
    dimnames1<-dimnames(naa_y)[[1]]
    dimnames2<-dimnames(naa_y)[[2]]
    dim(naa_y)<-dim(naa_y)[1:2]
    dimnames(naa_y)[[1]]<-dimnames1
#    browser()
    dimnames(naa_y)[[2]]<-dimnames2

    naa_y2<-naa_y
  }else{
      naa_y2<-apply(naa_y,c(1,2),sum)
      dimnames(naa_y2)[[2]]<-dimnames(naa_y2)[[2]]
  }
  return(list(naa_y=naa_y,naa_y2=naa_y2,nmorph=length(naa$info$Morph)))
}

getCAA.y<-getCAA.y.ss3<-function(repfile="Report.sso",report=NULL){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  caa<-getCAA.ss3.x(report=report)
#  browser()
  if(length(caa$info$Morph)==1){
    caa_q<-apply(caa$caa.array,c(1,2),sum)
    row.names(caa_q)<-floor(as.numeric(row.names(caa_q)))

    caa_y<-aggregate(caa_q,by=list(row.names(caa_q)),sum)
    row.names(caa_y)<-caa_y[["Group.1"]]
    caa_y<-data.matrix(caa_y[-1])
    caa_y2<-caa_y
  }else{
    caa_q<-apply(caa$caa.array,c(1,2,3),sum)
    caa_y.tmp<-caa_q
    nseason<-length(caa$info$Seas)
    dim(caa_y.tmp)<-c(nseason,dim(caa_q)[1]/nseason,dim(caa_q)[2:3])
    caa_y<-aperm(apply(caa_y.tmp,c(2,3,4),sum),c(1,3,2))
#    browser()
    dimnames1<-sort(unique(floor(as.numeric(dimnames(caa_q)[[1]]))))
    dimnames(caa_y)[[1]]<-dimnames1
#    browser()
    dimnames(caa_y)[[2]]<-dimnames(caa_q)[[3]]
    dimnames(caa_y)[[3]]<-dimnames(caa_q)[[2]]
#    require(reshape)||stop("library reshape is required")
#    browser()
#    caa_q.df<-melt.array(caa_q)
#    colnames(caa_q.df)[1]<-"Yr"

    caa_y2<-apply(caa_y,c(1,2),sum)
#    browser()
  }
  return(list(caa_y=caa_y,caa_y2=caa_y2,nmorph=length(caa$info$Morph)))
}

############################################
#####  2011/05/04 1) Refactored code to calculate annual F
#####             2) Put code to output annual F by Fleet
#####
getFAA.y<-getFAA.y.ss3<-function(repfile="Report.sso",report=NULL,fleets=NULL,debug=FALSE){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  faa<-calcFAA(report=report)

#  faa.array.data.frame=faa$faa.array.data.frame
  faa.array<-faa$faa.array
  info<-faa$info
  dimnames.org<-dimnames(faa.array)
  dim.org<-dim(faa.array)
  dim(faa.array)<-c(length(info$Seas),length(info$Yr),dim.org[2:length(dim.org)])
#  browser()
  dimnames(faa.array)<-c(list(Seas=info$Seas,Yr=info$Yr),dimnames.org[2:length(dim.org)])

#  browser()c

  if(debug){cat("L2094\n");browser()}
  if(length(faa$info$Morph)==1){
#    faa_q<-apply(faa$faa.array,c(1,2),sum)
#    row.names(faa_q)<-floor(as.numeric(row.names(faa_q)))
#    faa_y<-aggregate(faa_q,by=list(row.names(faa_q)),sum)
#    row.names(faa_y)<-faa_y[["Group.1"]]
#    faa_y<-data.matrix(faa_y[-1])
#    browser()
    faa_y<-apply(faa.array,c(2,3),sum)
#    cat("here\n")
#    browser()
    dimnames(faa_y)<-dimnames(faa.array)[c(2,3)]
    faa_y2<-faa_y
    if(!is.null(fleets)){
      faa.tmp<-apply(faa.array,c(2,3,4),sum)
      require(plyr)
      faa_fl<-plyr::alply(.data=faa.tmp,.margins=3,.fun=function(x){dimnames(x)<-dimnames(faa_y);return(x)})
#      browser()

      if(fleets!="ALL" && length(fleets)<length(info$Fleet)){
        names_faa_fl<-names(faa_fl)
        faa_fl<-lapply(fleets,FUN=function(fleet){faa_fl[[fleet]]})
        names(faa_fl)<-names_faa_fl[fleets]
      }
      browser()
      names(faa_fl)<-paste("FL",names(faa_fl),sep="")

      faa_fl2<-NULL
    }else{
      faa_fl<-NULL
      faa_fl2<-NULL
    }
  }else{
#    faa_q<-apply(faa$faa.array,c(1,2,4),sum)
#    faa_y.tmp<-faa_q
#    nseason<-length(faa$info$Seas)
##    browser()
#    dim(faa_y.tmp)<-c(nseason,dim(faa_q)[1]/nseason,dim(faa_q)[2:3])
#    faa_y<-apply(faa_y.tmp,c(2,3,4),sum)
##    browser()
#    dimnames1<-sort(unique(floor(as.numeric(dimnames(faa_q)[[1]]))))
#    dimnames(faa_y)[[1]]<-dimnames1
##    browser()
#    dimnames(faa_y)[[2]]<-dimnames(faa_q)[[2]]
#    dimnames(faa_y)[[3]]<-dimnames(faa_q)[[3]]
##    require(reshape)||stop("library reshape is required")
##    browser()
##    caa_q.df<-melt.array(caa_q)dim()
##    colnames(caa_q.df)[1]<-"Yr"
#
#    faa_y2<-apply(faa_y,c(1,2),sum)
    faa_y<-apply(faa.array,c(2,3,5),sum)
    dimnames(faa_y)<-dimnames(faa.array)[c(2,3,5)]
    faa_y2<-apply(faa.array,c(2,3),sum)
    dimnames(faa_y2)<-dimnames(faa.array)[c(2,3)]
#    browser()
    if(!is.null(fleets)){
      faa.tmp<-apply(faa.array,c(2,3,4,5),sum)
      faa_fl<-alply(.data=faa.tmp,.margins=3,.fun=function(x){dimnames(x)<-dimnames(faa_y);return(x)})
      names(faa_fl)<-paste("FL",names(faa_fl),sep="")
      if(fleets!="ALL" && length(fleets)<length(info$Fleets))faa_fl<-faa_fl[fleets]
#      browser()
      faa_fl2<-lapply(faa_fl,FUN=function(x){apply(x,c(1,2),sum)})
#      browser()
    }else{
      faa_fl<-NULL
      faa_fl2<-NULL
    }

  }
  return(list(faa_y=faa_y,faa_y2=faa_y2,nmorph=length(faa$info$Morph),fleets=fleets,
                faa_fl=faa_fl,faa_fl2=faa_fl2))
}


getFAA.y.old<-getFAA.y.ss3.old<-function(repfile="Report.sso",report=NULL,fleets=NULL){
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }
  faa<-calFAA.ss3.x(report=report)
#  browser()
  if(length(faa$info$Morph)==1){
    faa_q<-apply(faa$faa.array,c(1,2),sum)
    row.names(faa_q)<-floor(as.numeric(row.names(faa_q)))
    faa_y<-aggregate(faa_q,by=list(row.names(faa_q)),sum)
    row.names(faa_y)<-faa_y[["Group.1"]]
    faa_y<-data.matrix(faa_y[-1])
    faa_y2<-faa_y
  }else{
    faa_q<-apply(faa$faa.array,c(1,2,4),sum)
    faa_y.tmp<-faa_q
    nseason<-length(faa$info$Seas)
#    browser()
    dim(faa_y.tmp)<-c(nseason,dim(faa_q)[1]/nseason,dim(faa_q)[2:3])
    faa_y<-apply(faa_y.tmp,c(2,3,4),sum)
#    browser()
    dimnames1<-sort(unique(floor(as.numeric(dimnames(faa_q)[[1]]))))
    dimnames(faa_y)[[1]]<-dimnames1
#    browser()
    dimnames(faa_y)[[2]]<-dimnames(faa_q)[[2]]
    dimnames(faa_y)[[3]]<-dimnames(faa_q)[[3]]
#    require(reshape)||stop("library reshape is required")
#    browser()
#    caa_q.df<-melt.array(caa_q)dim()
#    colnames(caa_q.df)[1]<-"Yr"

    faa_y2<-apply(faa_y,c(1,2),sum)

  }
  return(list(faa_y=faa_y,faa_y2=faa_y2,nmorph=length(faa$info$Morph),fleets=fleets))
}

#### Calculate aggragate F

calc.aggregateF.old<-function(year,repfile="Report.sso"){
  report<-getReport.sso(repfile=repfile)
  faa<-calFAA.ss3.x(report=report,is.plot=FALSE)
  nage<-dim(faa$faa.array)[2]-1
  nfleet<-dim(faa$faa.array)[3]
  nseason<-nlevels(factor(faa$naa$Seas))
  ##  sortlist<-order(faa$nma$age_Beg)
  ##  nma.sorted<-faa$nma[sortlist,]
  nageseason<-(nage+1)*nseason
  ##  nageseason3<-3*nageseason
  ##  fmort<-array(0,dim=c(nseason,nage+1,nfleet+1),dimnames=list(paste("seas",1:4,sep=""),c(0:19,"20+"),
  ##    c("Total",paste("F_FL",1:nfleet,sep=""))))

  fmort<-array(0,dim=c(nseason,nage+1,nfleet+1),dimnames=list(paste("seas",1:nseason,sep=""),c(0:(nage-1),paste(nage,"+",sep="")),
                                                  c("Total",paste("F_FL",1:nfleet,sep=""))))

  if(length(year)>1){
    for(y in year){
      temp<-faa$faa.array[as.numeric(dimnames(faa$faa.array)[[1]])==y+(1:nseason-1)*1.0/nseason,,]
      if(length(temp)==0){cat("length(temp)=",length(temp));browser()}
      ##    dim(temp)<-c(nageseason,nfleet)
      fmort[,,2:(nfleet+1)]<-fmort[,,2:(nfleet+1)]+temp
    }
    fmort<-fmort/length(year)
    ##  fmort[seq(nageseason+1,nageseason3),2:(nfleet+1)]<-fmort[nageseason-nseason+1:nseason,2:(nfleet+1)]

  }else{
    temp<-faa$faa.array[as.numeric(dimnames(faa$faa.array)[[1]])==year+(1:nseason-1)*1.0/nseason,,]
    if(length(temp)==0){cat("length(temp)=",length(temp));browser()}
    ##  dim(temp)<-c(nageseason,nfleet)
        fmort[,,2:(nfleet+1)]<-temp
    ##    fmort[seq(nageseason+1,nageseason3),2:(nfleet+1)]<-fmort[nageseason-nseason+1:nseason,2:(nfleet+1)]%o%rep(1,(nage*2))
  }
  fmort[,,1]<-apply(fmort,1:2,sum)
  return(invisible(fmort))
}

#################################
#### Calculate aggragate F
####
####
####
calc.aggregateF<-function(year=NA,repfile="Report.sso",report=NULL,geomean=TRUE){

  if(is.null(report))report<-getReport.sso(repfile=repfile)
  faa<-calcFAA(report=report,is.plot=FALSE)
  nage<-length(faa$info$Ages)-1
  nfleet<-length(faa$info$Fleet)
  nseason<-length(faa$info$Seas)
  if(is.na(any(year)) | is.null(year)){
    cat("year is missing\n3 year geometric mean from terminal year\n")
    year<-as.numeric(rev(rev(faa$info$Yr)[1:3]))
  }
#  warnings()
#  browser()
  ##  sortlist<-order(faa$nma$age_Beg)
  ##  nma.sorted<-faa$nma[sortlist,]
  nageseason<-(nage+1)*nseason
  ##  nageseason3<-3*nageseason
  ##  fmort<-array(0,dim=c(nseason,nage+1,nfleet+1),dimnames=list(paste("seas",1:4,sep=""),c(0:19,"20+"),
  ##    c("Total",paste("F_FL",1:nfleet,sep=""))))

  # まず年四半期x年齢のF@Aの幾何平均を計算する
  # 幾何平均の場合は、要素がすべて1,算術平均では、要素がすべて0
#  fmort1<-array(ifelse(geomean,1,0),dim=c(nseason,nage+1))
  dimYr<-dimnames(faa$faa)[[1]]

  if(length(dim(faa$faa))==2){
    faa.temp<-faa$faa[dimYr<max(year)+1 & dimYr>=min(year),]
  }else{
    faa.temp<-faa$faa[dimYr<max(year)+1 & dimYr>=min(year),,]
  }
  if(length(year)>1){
    dim.old<-dim(faa.temp)
    dimnames.old<-dimnames(faa.temp)
#    browser()
    dim(faa.temp)<-c(nseason,length(year),dim.old[-1])
    dims<-1:length(dim(faa.temp))
    faa.temp.mean<-apply(faa.temp,dims[-2],FUN=function(x){if(geomean){ifelse(all(x > 0), exp(mean(log(x))), 0)}else{mean(x)}})
#    browser()
    dimnames.old[[1]]<-faa$info$Seas
    dimnames(faa.temp.mean)<-dimnames.old
    faa.mean<-faa.temp.mean
  }else{
    faa.mean<-faa.temp
    faa.array.mean<-if(length(dim(faa$faa.array))==3){
      faa$faa.array[dimYr<max(year)+1 & dimYr>=min(year),,]
    }else{
      faa$faa.array[dimYr<max(year)+1 & dimYr>=min(year),,,]
    }
  }
  names(dimnames(faa.mean))[1]<-"Seas"

#### partial F@Aに分けるために、対象とする期間の平均のpartial C@Aを計算する
#### この部分は算術平均
#  caa1<-array(0,dim=c(nseason,nage+1,nfleet))
  if(length(year)>1){
    caa.temp<-if(length(dim(faa$faa.array))==3){
              faa$caa.array[dimYr<max(year)+1 & dimYr>=min(year),,]
            }else{
              faa$faa.array[dimYr<max(year)+1 & dimYr>=min(year),,,]
            }

    dim.old<-dim(caa.temp)
    dimnames.old<-dimnames(caa.temp)
    dim(caa.temp)<-c(nseason,length(year),dim.old[-1])
    dims<-1:length(dim(caa.temp))
    caa.temp.mean<-apply(caa.temp,dims[-2],sum) # 年について和をとる
    dimnames.old[[1]]<-faa$info$Seas
    dimnames(caa.temp.mean)<-dimnames.old
    totcatch<-apply(caa.temp,dims[-c(2,4)],sum) # 年、漁業について和をとる
#    browser()
    if(length(dim(faa.mean))==2){
      faa.array.mean<-(ifelse(totcatch>0,faa.mean/totcatch,0)%o%rep(1,dim.old[3]))*caa.temp.mean
    }else{
      faa.array.mean<-aperm(ifelse(totcatch>0,faa.mean/totcatch,0)%o%rep(1,dim.old[3]),c(1,2,4,3))*caa.temp.mean
    }
#    browser()
  }
#  browser()
#  flush.console()
  names(dimnames(faa.array.mean))[1]<-"Seas"
  return(invisible(list(faa=faa.mean,faa.array=faa.array.mean)))
}


checkSSversion<-function(report,verbose=TRUE){
  ##rephead <- readLines(con=repfile,n=3)

  a<-1:10
  noCommentLines<-a[!(1:10 %in%  as.numeric(grep(x=report,pattern="^#",value=FALSE)))]
  rephead<-report[noCommentLines[1:3]]

  ## warn if SS version used to create rep file is too old or too new for this code
  SS_version <- rephead[1]
  SS_versionshort <- toupper(substr(SS_version,1,9))
  if(!(SS_versionshort %in% paste("SS-V3.2",c("4F"),sep=""))){
    print(paste("! Warning, this function tested on SS-V3.24f. You are using",substr(SS_version,1,9)),quote=FALSE)
    minor.version<-substr(SS_versionshort,7,9)
    cat("checkSSversion\n")
    # browser()
  }else{
    if(verbose) print(paste("You're using",SS_versionshort,"which should work with this R code."),quote=FALSE)
    minor.version<-substr(SS_versionshort,7,9)
  }
  return(minor.version)
}

read.AGE_SELEX<-function(report,blankLines){
  header.char<-"^AGE_SELEX"

  ## readDat<-function(report,header,footer.char=NULL, blankLines=NULL,colClasses=NULL,col.names=NULL,as.numeric.col=NULL,as.numeric.as.possible=FALSE,skip=1,checkEndRec=FALSE){

  AGE_SELEX_DB<-readDat(report=report,header.char=header.char,skip=3,skip.col.names=3)

  return(AGE_SELEX_DB)

}

## Function to do future projection by annual time step
prj.annual<-function(repfile="Report.sso",nyr.prj=10,startYr,currentYy){
  report<-getReport.sso(repfile=repfile)
  NAAy<-getNAA.y.ss3.x(report=report)
  NAAy.array<-array(data=NAAy,dimnames=dimnames(NAAy))

  FAAy<-getFAA.y(report=report)
  FAAy.array<-array(data=FAAy,dimnames=dimnames(FAAy))

  CAAy<-getCAA.y.ss3.x(report=report)
  CAAy.array<-array(data=CAAy,dimnames=dimnames(CAAy))

  if(missing(startYr))startYr<-as.numeric(dimnames(NAAy.array)[[1]][dim(NAAy.array)[1]])+1

## set up future F

## 1st step current F
  if(missing(currentYr))currentYr<-as.numeric(dimnames(NAAy.array)[[1]][dim(NAAy.array)[1]])
}


#   if(.Platform$OS.type=="unix"){
#      system("cp ss3.par SS3.PAR")
#      system(paste("ss3 ",ss3.arg))
#    }
#    else{
#      shell(paste("ss3.exe ",ss3.arg))
#    }



do.boot<-function(starterfile="starter.ss",ss3.exe=ifelse(.Platform$OS.type=="unix","ss3","ss3.exe"),forecast.file="forecast.ss",
  dat.file_new=ifelse(.Platform$OS.type=="unix","data.ss_new","Data.SS_New"),parallel=TRUE,cpus=4,mail=FALSE,type="SOCK",runname="",
  doHess=FALSE,withPE=TRUE,to=NULL,from=NULL,bootRange=NA,
  nonParamBoot=FALSE,report,verbose=TRUE,verboseDisplay=FALSE,saveBoot=TRUE,derived.quant=TRUE,doForeach=TRUE,
  ssArgs=NULL,showScreen=ifelse(.Platform$OS.type=="unix",FALSE,FALSE),doFAA=TRUE,
  smtpServer="mail.affrc.go.jp",do.debug=FALSE,chunkSize=30,withWine=FALSE,rename=TRUE){

  if(mail && (is.null(to) || is.null(from) )){
    stop("In order to use mail option, you need to set to and from")
  }

  if(doForeach){
    if(parallel){
      if(.Platform$OS.type=="unix"){
        fun.list<-list(readDat=readDat,getReport=getReport,calcFAA=calcFAA,getNAA=getNAA,
             getCAA=getCAA,readLinesInteract=readLinesInteract,componentEndL=componentEndL,getHeaderL=getHeaderL)

        if(0){
          require(doSMP)||stop("package doSMP is required")
          w<-startWorkers(cpus)
          registerDoSMP(w)
        }
        if(1){
          require(doMC)||stop("package doMC is required")
          MC<-TRUE
          registerDoMC(cores=cpus)
        }
#        cat("doSMP is loaded\n")
      }else if(.Platform$OS.type=="windows" && !is.element("ss.utils", installed.packages()[,1])){
        save(readDat,getReport,calcFAA,getNAA,getCAA,getNMA,readLinesInteract,componentEndL,getHeaderL,checkSSversion,
          getComponent,getRecruitmentDist,getMorphIndexing,getBabs,file="funList.r")
        require(doSMP)||stop("package doSMP is required")
        w<-startWorkers(cpus)
        registerDoSMP(w)
      }
    }else{
      require(foreach)||stop("package foreach is required")
    }

  }else{
    require(snowfall) || stop("package snowfall is required")
  }
  require(R.utils) || stop("package R.utils is required")
#  temp.dir<-tempdir()
  now<-Sys.time()  # 現在時刻を得る
  temp.dir<-paste("backup",floor(as.numeric(now)+0.5),sep="") # 一意なディレクトリー名を生成するために
                                                        # 現在時刻を1960年１月１日午前０時からの経過秒数に変換
                                                        # それを"backup"の後につなげる
#  dir.create(backupDir)                                 # back up ディレクトリーを作成
#  temp.dir<-"./temp"
  cur.dir<-getwd()

  dir.create(temp.dir)
  if(!file.exists(ss3.exe)){
    stop(paste(ss3.exe," did not exist."))
  }else{
    ss3.exe<-getAbsolutePath(pathname=ss3.exe,workDirectory=getwd())
  }
  exit.fun<-function(){
    setwd(cur.dir)
    file.copy(from=paste(temp.dir,starterfile,sep="/"),to=".",overwrite=TRUE)
    ab<-unlink(paste(temp.dir,"/*",sep=""),recursive=TRUE)
    cat("return value of unlink is",ab,"\n")
    if(.Platform$OS.type=="unix"){
      system(paste("cp ","boots/*", runname,collapse=" "))
    }else{
      shell(paste("copy ", "boots\\*.*", runname,collapse=" "))
    }
#    detach(fun.list)
#    file.copy(from="boots/*",to=runname,overwrite=TRUE)
    unlink("boots",recursive=TRUE)
    if(parallel){
      if(doForeach){
        if(.Platform$OS.type=="windows")stopWorkers(w)
#        stopWorkers(w)
#      stopCluster(w)
      }else{
        sfStop()
      }
    }
    end.all<-Sys.time()
    eltime<-as.numeric(difftime(end.all,start.all,units="secs"))
    if(mail){
      require(sendmailR) || stop("package sendmailR is required")
      from<-from
      to<-to
      subject <- paste("Finished in ",Sys.info()[4])
      msg <- paste(length(bootLists1), " calculation(s) on ",Sys.info()[4], " finished at ",format(Sys.time(), "%Y-%m-%d  %H:%M:%OS3"),
        "\nusing",formatC(eltime/60.0),"minutes with",cpus,"core(s)")

      sapply(to,function(x){sendmail(from=from, to=x, subject=subject, msg=msg,control=list(smtpServer=smtpServer))})
    }
  }
  on.exit(exit.fun())
  file.copy(from=starterfile,to=temp.dir)
  print(temp.dir)
#  if(.Platform$OS.type=="unix"){
#    ss3.exe<-system(paste("which",ss3.exe),intern=TRUE)
#  }

  getCumReport.boot<-function(cumReport="CumReport.sso",i){
    dat<-readLines(cumReport)
    tmp<-strsplit(dat,split="[[:blank:]]+")
    tmp1<-sapply(tmp,FUN=function(x){x[1]<-paste(i);return(paste(x,collapse=" "))},simplify=TRUE)
    return(invisible(tmp1))
  }
#### dat.file      : オリジナルのdat fileのファイル名
#### forecast.file : forecast.ss のファイル名
###
  separate.dat.file<-function(dat.file,forecast.file,runname,runtime,ss3.exe,doHess,withPE=TRUE,
    bootRange=NA,nonParamBoot=FALSE,report,verbose,verboseDisplay,saveBoot=TRUE,do.debug=FALSE){
#    browser()
    bootLists<-list()
    a<-readLines(dat.file)
#    browser()
    if(nonParamBoot){
#     Nonparametric bootstrapのときはデータを入れ替える
#     現状では、CPUEのみ
## 計算するしないにかかわらず(bootRangeに入る入らないにかかわらず)すべてのデータについて実行
      a<-createNonParamBoot.dat.file(dat.file.data=a,report=report,saveBoot=saveBoot,runname=runname)
    }
####################################
    cut.point<-grep(value=FALSE,pattern="^#_bootstrap",x=a)
    cut.point2<-grep(value=FALSE,pattern="^999$",x=a)
    if(withPE){
      cut.point<-c(grep(value=FALSE,pattern="^#_observed data:",x=a),cut.point)
      cut.point2<-cut.point2[-2]
 #     browser()
    }else{
      cut.point2<-cut.point2[-(1:2)]
    }
#    browser()
#  To-do 再検討が必要
    if(length(bootRange)==1 && is.na(bootRange)){
      nboot<-length(cut.point)
      cutList<-1:nboot
    }else if(max(bootRange)>length(cut.point)){
      nboot<-length(cut.point)
      cutList<-1:nboot
    }else{
      cutList<-bootRange
    }
#
############## SS3 の引数の設定
      if(is.null(ssArgs)){
        if(.Platform$OS.type=="unix"){
          ssArgs<-" -nox -cbs 1000000000 -gbs 1000000000 -ams 100000000 "
        }else if(.Platform$OS.type=="windows"){
          ssArgs<-" -nox -cbs 600000000 -gbs 600000000 -ams 60000000 "
        }
      }

#########################################
    for(i in cutList){
      if((i %% 10)==0){
        cat("Number:",i,"\n")
      }else{
        cat("Number:",i,", ")
      }
#      flush()
#      bootDat<-a[cut.point[i]:grep(value=FALSE,pattern="^999",x=a[cut.point[i]:length(a)])[1]]
###################################################
##  Prepare dat.file
      bootDat<-a[cut.point[i]:cut.point2[i]]
#      browser()
      bootdat.file<-paste("boots/data-boot",formatC(i,width=log10(max(cutList))+1,flag=0),".dat",sep="")
#      browser()
      zz<-file(bootdat.file,"w")
      writeLines(bootDat,con=zz)
      close(zz)
#      write(bootDat,file=bootdat.file) #
########################################################
#     prepare starter.ss
      st<-readLines(starterfile)
      stlines<-sort(c(grep(value=FALSE,pattern="^#",x=st),grep(value=FALSE,pattern="^$",x=st))) # Find line number of lines with only comment or blank
      stlines<-setdiff(1:length(st),stlines)
      st[stlines[1]]<-paste("data-boot",formatC(i,width=log10(max(cutList))+1,flag=0),".dat",sep="")
      st[stlines[2]]<-"control.ss"
      if(as.numeric(strsplit(st[stlines[3]],split="#")[[1]][1])!=1){
        st[stlines[3]]<-paste(1,strsplit(st[stlines[3]],split="#")[[1]][2],sep=" #")
      }

      if(as.numeric(strsplit(st[stlines[11]],split="#")[[1]][1])>0){
        st[stlines[11]]<-paste(0,strsplit(st[stlines[11]],split="#")[[1]][2],sep=" #")
      }
      if(!verboseDisplay){
        if(as.numeric(strsplit(st[stlines[4]],split="#")[[1]][1])!=0){
          st[stlines[4]]<-paste(0,strsplit(st[stlines[4]],split="#")[[1]][2],sep=" #")
        }
      }
##########################################
###    Prepare control file         ######
###
      ctl<-readLines("control.ss_new")
      ctl_boot<-ctl
      if(!withPE || i!=1){
        line_mult_lencomp<-grep(value=FALSE,pattern="#_mult_by_lencomp_N$",x=ctl)
        mult_lencomp<-strsplit(ctl[line_mult_lencomp],split="#")
        mult_lencomp[[1]][1]<-sub(pattern="^[[:blank:]]+",replacement="",x<-mult_lencomp[[1]][1])
        nfleet<-length(strsplit(mult_lencomp[[1]][1],split="[[:blank:]]+")[[1]])
  #      browser()
        mult_lencomp[[1]][1]<-paste(rep(1,nfleet),collapse=" ")
        ctl_boot[line_mult_lencomp]<-paste(mult_lencomp[[1]][1],mult_lencomp[[1]][2],sep=" #")
      }
#############################################
###    weight at age file
###
      if(file.exists("wtatage.ss")){
        waa<-readLines("wtatage.ss")
      }else{
        waa<-NULL
      }
  #
#############################################
##  par file
      if(file.exists("ss3.par")){
        ss3.par<-readLines("ss3.par")
      }else{
        ss3.par<-NULL
      }
#############################################
##  forecast file
      if(file.exists(forecast.file)){
        forecast.dat<-readLines(forecast.file)
      }else{
        forecast.dat<-NULL
      }
#######################################################
#      browser()
      bootLists[[i]]<-list()
      bootLists[[i]]$i<-i
      bootLists[[i]]$starter<-st
      bootLists[[i]]$ss3.exe<-ss3.exe
      bootLists[[i]]$ctl<-ctl_boot
      bootLists[[i]]$dat.file<-bootdat.file  # こちらはファイル名
      bootLists[[i]]$bootDat<-bootDat
      bootLists[[i]]$forecast.file<-forecast.file
      bootLists[[i]]$forecast.dat<-forecast.dat
      bootLists[[i]]$runname<-runname
      bootLists[[i]]$doHess<-doHess
      bootLists[[i]]$verbose<-verbose
      bootLists[[i]]$waa<-waa
      bootLists[[i]]$ssArgs<-ssArgs
      bootLists[[i]]$showScreen<-showScreen
      bootLists[[i]]$doFAA<-doFAA
      bootLists[[i]]$withWine<-withWine
#      bootLists[[i]]$calcFAA<-calcFAA
#      bootLists[[i]]$getReport<-getReport
#      bootLists[[i]]$getNAA<-getNAA
#      bootLists[[i]]$getCAA<-getCAA
#      bootLists[[i]]$readLinesInteract<-readLinesInteract
#      bootLists[[i]]$readDat<-readDat
      bootLists[[i]]$do.debug<-do.debug
      bootLists[[i]]$ss3.par<-ss3.par
#      browser()
    }
    return(bootLists)
  }

###############################################################
## １回の計算用
###############################################################
  do1bootsim<-function(bootList){
    cur.dir<-getwd()
    do.debug<-bootList$do.debug
    nboot<-length(bootLists1)
    wk.dir<-paste("boot_",formatC(bootList$i,width=log10(nboot)+1,flag=0),sep="")
    dir.create(wk.dir)
    wk.dir.abs<-getAbsolutePath(wk.dir)
    exit.fun<-function(){
      if(do.debug){
        cat("called in do1bootsim\n")
        browser()
      }
      setwd(cur.dir)
    #  setwd("..")
      unlink(wk.dir,recursive=TRUE)
      if(.Platform$OS.type=="windows" && !is.element("ss.utils", .packages())){
        detach("file:funList.r")
      }
    }
    on.exit(exit.fun())
    setwd(wk.dir) # この時点で、作業ディレクトリー("./boot_0**")に移動
    if(.Platform$OS.type=="windows" && !is.element("ss.utils", .packages())){
      shell("copy ..\\funList.r .")
      attach("funList.r")
    }
#    browser()
    if(.Platform$OS.type=="unix"){
      if(!bootList$withWine){
        ss3.exe<-system(paste("which",bootList$ss3.exe),intern=TRUE)
        system(paste("cp ",ss3.exe, ".",collapse=" "))
      }else{
        system(paste("cp ",bootList$ss3.exe, ".",collpase=" "))
        parent<-getParent(bootList$ss3.exe)
        ss3.exe<-getRelativePath(bootList$ss3.exe,relativeTo=parent)
      }
    }else{
#      browser()
      shell(paste("copy ", bootList$ss3.exe, ".",collapse=" "),mustWork=TRUE,translate=TRUE)
      parent<-getParent(bootList$ss3.exe)
      ss3.exe<-getRelativePath(bootList$ss3.exe,relativeTo=parent)
    }
#    browser()
    write(bootList$starter,file="starter.ss")
#    browser()
    write(bootList$ctl,file="control.ss")
    if(!is.null(bootList$waa)){
      write(bootList$waa,file="wtatage.ss")
    }
#    file.copy(from=bootList$dat.file,to=wk.dir.abs)
    write(bootList$bootDat,file=strsplit(bootList$dat.file,"/")[[1]][2])

#    file.copy(from=bootList$forecast.file,to=wk.dir.abs)
    if(!is.null(bootList$forecast.dat)){
      write(bootList$forecast.dat,file="forecast.ss")
    }
#    cat("Here\n")
#    browser()
#    file.copy(from="ss3.par",to=wk.dir.abs)
    write(bootList$ss3.par,file="ss3.par")
#    setwd(wk.dir)
#    browser()
    hess<-ifelse(bootList$doHess,""," -nohess")
#    browser()

    if(.Platform$OS.type=="unix"){
      if(!bootList$withWine){
        if(!do.debug)system(paste(bootList$ss3.exe,bootList$ssArgs,hess,sep=""))
      }else{
        if(!do.debug)system(paste("wine ", ss3.exe,bootList$ssArgs,hess,sep=""))
      }
    }else{
      cat(file.exists(ss3.exe),"\n")
#      browser()
#      if(!do.debug)shell(paste(ss3.exe,bootList$ssArgs,hess,sep=""),translate=TRUE,invisible=bootList$showScreen)
      if(!do.debug)shell(paste(ss3.exe,bootList$ssArgs,hess,sep=""),translate=TRUE,invisible=TRUE)
    }
#    browser()
    fileMove<-function(file,check=TRUE){
      if(check)exist<-file.exists(file)
      if(!check||exist){
        file.copy(from=file,to=paste("../",bootList$runname,
          "/",strsplit(file,"\\.")[[1]][1],"_b",formatC(bootList$i,width=log10(nboot)+1,flag=0),".",
            strsplit(file,"\\.")[[1]][2],sep=""))
      }
    }
    fileMove(file=strsplit(bootList$dat.file,"/")[[1]][2])
    fileMove(file="control.ss")
    fileMove(file="Report.sso")
    fileMove(file="Forecast-report.sso")
    fileMove(file="ss3.par")
    fileMove(file="covar.sso")
    fileMove(file="wtatage.ss")
    if(bootList$verbose){
      fileMove("warning.sso")
      fileMove("echoinput.sso")
    }

    if(bootList$doFAA){
      getNAA<-bootList$getNAA
      getCAA<-bootList$getCAA
      getReport<-bootList$getReport
      calcFAA<-bootList$calcFAA
      readDat<-bootList$readDat
      readLinesInteract<-bootList$readLinesInteract

      fileList<-dir()
      aa<-grep(x=toupper(fileList),pattern=toupper("Report.sso"),value=FALSE)
      if(is.null(aa)){
        stop("can not find repfile")
      }else{
        FAA<-calcFAA(report=getReport(repfile=fileList[aa]))
        }

      save(FAA,file=paste("../",bootList$runname,"/faa",formatC(bootList$i,width=log10(nboot)+1,flag=0),".r",sep=""))
    }
    cumReport<-getCumReport.boot(i=bootList$i)

    return(invisible(cumReport))
  }

##########################################################################################################################
#####  計算本体
  runtime<-format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  runname<-paste("boot-results",runtime,sep="")

  start.all<-Sys.time()
  dir.create(runname)
  dir.create("boots")

  bootLists<-separate.dat.file(dat.file=dat.file_new,forecast.file=forecast.file,
    runname=runname,runtime=runtime,ss3.exe=ss3.exe,doHess=doHess,withPE=withPE,bootRange=bootRange,
    nonParamBoot=nonParamBoot,report=report,verbose=verbose,verboseDisplay=verboseDisplay,saveBoot=saveBoot,do.debug=do.debug)
#  browser()
  if(.Platform$OS.type=="unix"){
    tmp.dir<-"/var/tmp/"
  }else{
#    tmp.dir<-paste(shell("echo %tmp% ",intern=TRUE))
    tmp.dir<-NULL
  }
#  dir.create(paste("boot-results",runtime,sep=""))
#  browser()
  if(length(bootRange)==1 && is.na(bootRange)){
    bootLists1<-bootLists
  }else if(max(bootRange)>length(bootLists)){
    bootLists1<-bootLists
  }else{
    bootLists1<-bootLists[bootRange]
  }
################################################

  if(doForeach){
    if(parallel){
#      dir.create("./temp1")
#      if(.Platform$OS.type=="windows"){
#        require(doSMP)
#        w<-startWorkers(workerCount=cpus)
#      w<-makeSOCKcluster(rep("localhost",cpus))
#      cat("here")
#      registerDoSNOW(w)
#        registerDoSMP(w)
#      }
#      browser()
      cat("start parallel computaion\n")
      packages<-c("R.oo","R.methodsS3","R.utils")
      if(.Platform$OS.type=="windows" && !is.element("ss.utils", .packages())){
        packages<-c(packages,"ss.utils")
      }

#      browser()
      ListSize=length(bootLists1)
      cat("ListSize:",ListSize,"\n")
      if(ListSize> cpus*chunkSize && .Platform$OS.type=="windows"){
         residue<-ListSize %% (cpus*chunkSize)
        replicates<-ListSize %/% (cpus*chunkSize) +ifelse(residue>0,1,0)
#        cumReports<-list()
        for(i in 1:replicates){
          startNo<-(i-1)*cpus*chunkSize+1
          if(i!=replicates){
            endNo<-i*cpus*chunkSize
          }else{
            endNo<-(i-1)*cpus*chunkSize+residue
          }

          cat(i,"th loop ",startNo,":",endNo," of ",ListSize,"\n")
#          browser()
          temp.list<-bootLists1[startNo:endNo]
          cumReport.temp<-foreach(x=temp.list,.inorder=TRUE,
            .packages=packages,.verbose=FALSE,.errorhandling="pass")%dopar%
            do1bootsim(bootList=x)
          if(i==1){
            cumReports<-cumReport.temp
          }else{
            cumReports<-c(cumReports,cumReport.temp)
          }
        }
      }else{
        cumReports<-foreach(x=bootLists1,.inorder=TRUE,
            .packages=packages,.verbose=FALSE,.errorhandling="pass")%dopar%
            do1bootsim(bootList=x)
      }
    }else{
     cumReports<-foreach(x=bootLists1,.inorder=TRUE)%do%
        do1bootsim(bootList=x)
    }

  }else{
    sfInit(parallel=parallel,cpus=cpus,type=type)
    sfLibrary(R.utils)
    sfLibrary(plyr)
    sfLibrary(reshape2)
    sfExport(list("calcFAA","getReport","getNAA","getCAA"))
    #    registerDoSNOW

    cumReports<-sfLapply(x=bootLists1,fun=do1bootsim)
  }



#  nboot<-lapply(bootLists[1:6],do1bootsim)
#  dim(cumReports)<-dim(cumReports)[1]*dim(cumReports)[2]
  return(invisible(cumReports))
}

##########################################################

###################################################################################


getDatafromDat<-
function(report=NA,datFile="data.ss_new",header.char="#_year seas index obs err",skip=0){
  if(is.na(report))report<-getReport(datFile)
  return(readDat(report=report,header.char=header.char,skip=skip))
  grep(x=dat.new,pattern="#_N_LengthBins",value=T)[4]
}

getCPUEfromDat<-function(datFile="data.ss_new",header.char="#_year seas index obs err",skip=0){
  tmp<-getDatafromDat(datFile=datFile,header.char=header.char)
  tmp<-tmp[,1:5]
  names(tmp)[1:5]<-c("Year","Season","index","obs","err")
  return(tmp)
}


getSizeFreqFromDat<-function(datFile="data.ss_new",header.char="#_Year season Fleet Gender Partition SampleSize <data>"){
  tmp<-getDatafromDat(datFile=datFile,header.char=header.char)
  names(tmp)[1:7]<-c("SizeMethod","Year","Season","Fleet","Gender","Part","SampleSize")
  return(tmp)
}

getLenCompsFromDat<-function(datFile="data.ss_new",header.char="#Yr Seas Flt/Svy Gender Part Nsamp datavector(female-male)"){
  tmp<-getDatafromDat(datFile=datFile,header.char=header.char)
  names(tmp)[1:6]<-c("Year","Season","Fleet","Gender","Part","SampleSize")
  return(tmp)
}

getAgeCompsFromDat<-function(datFile="data.ss_new",header.char="#Yr Seas Flt/Svy Gender Part Ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)"){
  tmp<-getDatafromDat(datFile=datFile,header.char=header.char)
  names(tmp)[1:9]<-c("Year","Season","Fleet","Gender","Part","Ageerr","Lbin_lo","Lbin_hi","SampleSize")
  return(tmp)
}

getSizeAtAgeFromDat<-function(datFile="data.ss_new",header.char="#Yr Seas Flt/Svy Gender Part Ageerr Ignore datavector(female-male)"){
  tmp<-getDatafromDat(datFile=datFile,header.char=header.char,skip=1)
  names(tmp)[1:7]<-c("Year","Season","Fleet","Gender","Part","Ageerr","Ignore")
  return(tmp)
}



#Yr Seas Flt/Svy Gender Part Ageerr Ignore datavector(female-male)

###################################################################################
createResampledCPUE<-function(report,nboot,saveBoot=FALSE,runname){
  index2<-readDat(report=report,header.char="^INDEX_2")
  index2$Res<-ifelse(as.numeric(index2$Obs)>0,log(as.numeric(index2$Obs))-log(as.numeric(index2$Exp)),NA)
  index2$stdRes<-ifelse(!is.na(index2$Res),index2$Res/as.numeric(index2$SE),NA)
  index2$FleetNo<-sapply(strsplit(index2$Fleet,split="_"),FUN=function(x){as.numeric(x[1])})
  FleetList<-unique(index2$Fleet)
  index2List<-lapply(FleetList,FUN=function(fl){return(index2[index2$Fleet==fl,])})

  #newObs<-sample(index2List[[1]]$stdRes,nrow(index2List[[1]]),replace=TRUE)*as.numeric(index2List[[1]]$SE)
  #index2List[[i]]<-cbind(index2List[[i]],newObs)

  index2ListBoot<-lapply(index2List,FUN=function(ll){
    sapply(1:nboot,FUN=function(i){
      tmp<-as.numeric(ll$Obs[ll$Obs>0])*
      exp(as.numeric(sample(ll$stdRes[!is.na(ll$stdRes)],sum(!is.na(ll$stdRes)),replace=TRUE))*as.numeric(ll$SE[!is.na(ll$stdRes)]))
      tmp1<-as.numeric(ll$Obs);tmp1[tmp1>0]<-tmp;return(tmp1)},simplify=TRUE)
  })
  index2boot<-index2ListBoot[[1]]
  if((ncpues<-length(index2ListBoot))>1)
  for(i in 2:ncpues){
    index2boot<-rbind(index2boot,index2ListBoot[[i]])
  }
  if(saveBoot)save(index2ListBoot,file=paste(".",runname,"index2ListBoot.Rimage",sep="/"))
  return(index2boot=index2boot)
}
##############################################################################################################

createNonParamBoot.dat.file<-function(dat.file.data,report,saveBoot,runname){
#    browser()
#  bootLists<-list()
#    a<-readLines(dat.file)
#    browser()
  cut.point<-grep(value=FALSE,pattern="^#_bootstrap",x=dat.file.data)
  cut.point2<-grep(value=FALSE,pattern="^999",x=dat.file.data)
  cut.point2<-cut.point2[-(1:2)]
  nboot<-length(cut.point)
  index2boot<-createResampledCPUE(report=report,nboot=nboot,saveBoot=saveBoot,runname=runname)
  cutList<-1:nboot
#    nboot<-length(cut.point)
  for(i in 1:nboot){
#    cat(i,"\n")
#    if(i>1)browser()
#     process dat file, take out i\'th dat file\'s data only
    bootDat<-dat.file.data[cut.point[i]:cut.point2[i]]
    startLine<-grep(value=FALSE,pattern="#_N_cpue_and_surveyabundance_observations",x=bootDat)
#    cat(startLine,"\n")
#    browser()
    nobs<-as.numeric(strsplit(bootDat[[startLine]],split="[[:blank:]]+")[[1]][1]) # split by any number of space charactors
    cpueData<-bootDat[(startLine+2):(startLine+2+nobs-1)]
#      FleetNos<-sapply(strsplit(cpueDat,split="[[:blank:]]+"),FUN=function(x){as.numeric(x[3])})
#    cpueArray<-sapply(strsplit(cpueData,split="[[:blank:]]+"),FUN=function(x){return(as.numeric(x[1:5]))})
    cpueArray<-sapply(strsplit(cpueData,split="[[:blank:]]+"),FUN=function(x){return(x)})
#    browser()
    cpueArray[4,]<-index2boot[,i]
    bootDat[(startLine+2):(startLine+2+nobs-1)]<-cpueData<-sapply(1:nobs,FUN=function(j){paste(cpueArray[,j],sep="",collapse=" ")})
    dat.file.data[cut.point[i]:cut.point2[i]]<-bootDat
#    browser()
  }
#  browser()
  if(saveBoot){
    zz<-file(paste(".",runname,"data.ss_new_nonparametric",sep="/"),"w")
    writeLines(dat.file.data,con=zz)
    close(zz)
  }
#  browser()
  return(dat.file.data)
}

##############################################################################################

# sub(pattern="^[[:blank:]]+",replacement="",strsplit(ctl[436],split="#")[[1]][1])
# length(as.numeric(strsplit(a1,split="[[:blank:]]+")[[1]]))


## Utility rourines
coh2arr <- function(object){
  # dimensions and co
  dobj <- dim(object)
  dflq <- dobj[1:2]
  dflq[2] <- dobj[2]-dobj[1]+1

  flq <- array(NA, dim=dflq)
  for(i in 1:dflq[1]) flq[i,] <- object[i,(dobj[1]-i+1):(dobj[2]-i+1)]
  return(flq)
}

arr2coh <- function(object){

  # dimensions and co
  dobj <- dim(object)
  dflq <- dobj
  dflq[2] <- dobj[2] + dobj[1] - 1

  flq <- array(NA, dim = dflq)
  for(i in 1:dflq[1]) flq[i,(dobj[1] - i + 1):(dflq[2] + 1 - i)] <- object[i,]
  return(flq)
}

arr2data.frame<-function(arr){
  dim.array<-dim(arr)


}


if(0){
caa_LPS<-CAA$caa.array[,,3]
row.names(caa_LPS)<-floor(as.numeric(row.names(caa_LPS)))
caa_LPS_y<-aggregate(caa_LPS,by=list(row.names(caa_LPS)),sum)
row.names(caa_LPS_y)<-caa_LPS_y[["Group.1"]]
caa_LPS_y<-data.matrix(caa_LPS_y[-1])
write.csv(caa_LPS_y,"caa_LPS_y.csv")


}



read.spawner_recruit<-function(report=NULL,repfile="Report.sso",plot=FALSE,versionNo="3.21",
  new.minor.version=c("04-","04a","04b","10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d","23b"),B0=TRUE,lattice=FALSE,layout=c(1,1)){
#  cat("enter read.spawner_recruit\n")
  if(lattice && plot)require(lattice)||stop("Package lattice is required")
  if(is.null(report)){
    report<-getReport.sso(repfile=repfile)
  }

  blankLines<-grep(value=FALSE,pattern="^$",x=report)
  header.char<-"^SPAWN_RECRUIT"
  headerL<-grep(value=FALSE,pattern=header.char,x=report)
  EL<-componentEndL(headerL=headerL,blankLines=blankLines)
  dat.fields<-report[(headerL+1):EL]
  minor.version<-checkSSversion(report=report,verbose=FALSE)
#  browser()

  if(toupper(minor.version) %in% toupper(c("04-","04a","04b"))){
    col.line<-11
  }else if(toupper(minor.version) %in% toupper(c("10a","10b","10c","10-","11b","11c","11d","20-","20b","21a","21d","23b","24f"))){
    col.line<-11
  }else{
    col.line<-7
  }

  SRform<-as.numeric(strsplit(report[headerL],split="[[:blank:]]+")[[1]][3])

#  browser()
  col.names<-paste(unlist(strsplit(dat.fields[col.line],split="[[:blank:]]+")))
  sr<-readDat(report=report,header.char=header.char,footer.char=NULL, blankLines=blankLines,colClasses=NULL,col.names=col.names,as.numeric.col=NULL,
  as.numeric.as.possible=FALSE,skip=col.line)
#  browser()
  lnR0<-as.numeric(unlist(strsplit(dat.fields[1],split="[[:blank:]]+"))[1])
  steep<-as.numeric(unlist(strsplit(dat.fields[2],split="[[:blank:]]+"))[1])
  if(SRform==5){
    minRecr<-as.numeric(strsplit(grep(value=FALSE,pattern="^SR_minRecr$",x=report),split="[[:blank:]]+")[3])
  }
  sigmaR.input<-as.numeric(unlist(strsplit(dat.fields[3],split="[[:blank:]]+"))[1])
  if(minor.version %in% new.minor.version){
    sigmaR.output.main<-as.numeric(unlist(strsplit(dat.fields[9],split="[[:blank:]]+"))[4])
  }else{
    sigmaR.output.main<-as.numeric(unlist(strsplit(dat.fields[3],split="[[:blank:]]+"))[4])
  }
  sr$is.forecast<-sr[,9]=="Forecast"
 #browser()
  sr$spawn_bio<-as.numeric(sr$spawn_bio)
  sr$pred_recr<-as.numeric(sr$pred_recr)
  sr$exp_recr<-as.numeric(sr$exp_recr)
  sr$RPS<-sr$pred_recr/sr$spawn_bio
  sr.params<-as.numeric(sr[1,2:3])
  if(SRform==5)srparams[3]<-minRecr
  SSB0<-sr.params[1]
#  browser()
  R0<-as.numeric(unlist(strsplit(dat.fields[1],split="[[:blank:]]+"))[3])
#  browser()
  if(SRform==1 || SRform==6 && toupper(minor.version) %in% toupper(c("21d","23b")) ){  #  Beverton-Holt flat top for SSB>SSB0
    SRfn<-function(SSB){
      return(ifelse(SSB==0,0,ifelse(SSB>SSB0,SSB0,4*steep*R0*SSB/(SSB0*(1-steep)+SSB*(5*steep-1)))))
    }
  }
  if(SRform==2){
## NewRecruits = R_base*SpawnBio1/S_base * mfexp(steepness*(1.-SpawnBio1/S_base));
    SRfn<-function(SSB){
      return(ifelse(SSB==0,steep*R0*SSB/SSB0*exp(steep*(1-SSB/SSB0))))
    }
  }
  if(SRform==3){ #  Original Beverton-Holt
    SRfn<-function(SSB){
      return(ifelse(SSB==0,0,4*steep*R0*SSB/(SSB0*(1-steep)+SSB*(5*steep-1))))
    }
  }
  if(SRform==4){ # SCAA
    SRfn<-function(SSB)return(R0)
  }
  if(SRform==5){ # Hockey Stick
    SRfn<-function(SSB){
      Join_Fxn<-function(MinPoss,MaxPoss,Inflec,Xvar,Y1,Y2){
          #        Original definition
          #        join<-1/(1.0+exp(1000*(Xvar-Inflec)/(MaxPoss-MinPoss)))
          #        To prevent overflow
        join<-exp(-1000*(Xvar-Inflec)/(MaxPoss-MinPoss))/(1.0+exp(-1000*(Xvar-Inflec)/(MaxPoss-MinPoss)))
        return(join)
      }
          #      temp=SR_parm(3)*R_base + SpawnBio1/(steepness*S_base)*(R_base-SR_parm(3)*R_base);  //  linear decrease below steepness*S_base
          #       NewRecruits=Join_Fxn(0.0*S_base,S_base,steepness*S_base, SpawnBio1, temp, R_base);
      temp<-minRecr*R0+SSB/(steepness*SSB0)*(R0-minRecr*R0)
      return(join_Fxn(0.0*SSB0,SSB0,steepness*SSB0,SSB,temp,R0))
    }
  }
#  browser()
  if(SRform>5 && !is.element(toupper(minor.version), toupper(c("21d","23b"))) || (SRform>6 && toupper(minor.version) %in% toupper(c("21d","23b"))) || SRform<1)stop(paste(SRform,"is not defined"))

  if(plot)
  {

#   plot(sr[3:nrow(sr),"spawn_bio"],sr[3:nrow(sr),"pred_recr"],xlab="SSB in MT",ylab="Recruit in 1000fish")
    xlimMax<-max(sr[sr[,9]=="Main","spawn_bio"])
    ylimMax<-max(sr[sr[,9]=="Main","pred_recr"])
    if(B0){SSBmax<-max(xlimMax,SSB0)*1.05}else{SSBmax<-xlimMax*1.05}

    main=paste("S-R relationship; SSB_B0:",floor(SSB0),"ton")
    if(!lattice){
      SSBseq<-seq(from=0,to=SSBmax,by=SSBmax/1000)
      plot(sr[sr[,9]=="Main","spawn_bio"],sr[sr[,9]=="Main","pred_recr"],xlab="",ylab="",ylim=c(0,ylimMax),xlim=c(0,SSBmax))
      par(new=TRUE)
      plot(SSBseq,SRfn(SSBseq),type="l",xlab="SSB in MT",ylab="Recruit in 1000fish",ylim=c(0,ylimMax),xlim=c(0,SSBmax),main=main)
      grid()
      plot.obj<-NULL
    }else{
#      SRlist<-list(SSBseq=SSBseq,Rseq=SRfn(SSBseq))
      plot.obj<-xyplot(data=subset(sr,sr[,9]=="Main"),pred_recr~spawn_bio,xlab="SSB in MT",ylab="Recruit in 1000fish",ylim=c(0,ylimMax),xlim=c(0,SSBmax)
        ,panel=function(x,y,...){panel.curve(expr=SRfn,from=0,to=SSBmax,n=1000,curve.type="l")
                                 panel.xyplot(x,y,...)
                                 panel.grid(h=-1,v=-1,...)
      },main=main,layout=layout)
    }
  }else{
    plot.obj<-NULL
  }
  SR<-
    list(plot.obj=plot.obj,sr=sr,R0=R0,SSB0=SSB0,steep=steep,sigmaR.input=sigmaR.input,
      sigmaR.output.main=sigmaR.output.main,
      lnR0=lnR0,sr.params=sr.params,SRform,SRfn=SRfn,lattice=lattice)
  return(invisible(SR))
}

make.SRfn<-function(SRform=1,SSB0=NA,R0=NA,steep=NA,minRecr=NA){
  if(any(c(is.na(SSB0),is.na(R0),is.na(steep))))stop("SSB0, R0 and steep is required")
  if(SRform==5 && is.na(minRecr))stop("minRecr is required for hockey-stick")
  if(SRform==1){  #  Beverton-Holt flat top for SSB>SSB0
    SRfn<-function(SSB){
      return(ifelse(SSB==0,0,ifelse(SSB>SSB0,SSB0,4*steep*R0*SSB/(SSB0*(1-steep)+SSB*(5*steep-1)))))
    }
  }
  if(SRform==2){
## NewRecruits = R_base*SpawnBio1/S_base * mfexp(steepness*(1.-SpawnBio1/S_base));
    SRfn<-function(SSB){
      return(ifelse(SSB==0,steep*R0*SSB/SSB0*exp(steep*(1-SSB/SSB0))))
    }
  }
  if(SRform==3){ #  Original Beverton-Holt
    SRfn<-function(SSB){
      return(ifelse(SSB==0,0,4*steep*R0*SSB/(SSB0*(1-steep)+SSB*(5*steep-1))))
    }
  }
  if(SRform==4){ # SCAA
    SRfn<-function(SSB)return(R0)
  }
  if(SRform==5){ # Hockey Stick
    SRfn<-function(SSB){
      Join_Fxn<-function(MinPoss,MaxPoss,Inflec,Xvar,Y1,Y2){
          #        Original definition
          #        join<-1/(1.0+exp(1000*(Xvar-Inflec)/(MaxPoss-MinPoss)))
          #        To prevent overflow
        join<-exp(-1000*(Xvar-Inflec)/(MaxPoss-MinPoss))/(1.0+exp(-1000*(Xvar-Inflec)/(MaxPoss-MinPoss)))
        return(join)
      }
          #      temp=SR_parm(3)*R_base + SpawnBio1/(steepness*S_base)*(R_base-SR_parm(3)*R_base);  //  linear decrease below steepness*S_base
          #       NewRecruits=Join_Fxn(0.0*S_base,S_base,steepness*S_base, SpawnBio1, temp, R_base);
      temp<-minRecr*R0+SSB/(steepness*SSB0)*(R0-minRecr*R0)
      return(join_Fxn(0.0*SSB0,SSB0,steepness*SSB0,SSB,temp,R0))
    }
  }
  if(SRform >5 || SRform<1)stop(paste(SRform,"is not defined"))
}




plotSSB.boots<-function(report.pe,reports,sr.list=NULL,alpha=128/(ifelse(is.null(sr.list),reports[[2]],length(sr.list))/100),Cairo=FALSE){
  if(is.null(sr.list)){
    sr.list<-lapply(reports[[1]],FUN=read.spawner_recruit)
  }
  sr.list.main<-lapply(sr.list,FUN=function(sr){sr$sr[sr$sr$era=="Main",]})
  SSB<-lapply(sr.list.main,"[[","spawn_bio")
  year<-sr.list.main[[1]]$year
  SR.pe<-read.spawner_recruit(report=report.pe)
  SSB.pe<-SR.pe$sr[SR.pe$sr$era=="Main",]$spawn_bio
#  browser()
  plotSim(trueData=SSB.pe,qyear=year,sims=SSB,alpha=alpha,Cairo=Cairo)
}

plotSim<-function(trueData,sims,overlayData=NULL,ylab="",col=NULL,qyear,alpha=128/(length(sims)/100),main="",selex=FALSE,ylim0fix=FALSE,Cairo=FALSE){
    on.exit(dev.off())
     if(!selex){
      maxValue<-max(sapply(sims,max))
      minValue<-min(sapply(sims,min))
      maxValue<-max(maxValue,max(trueData))
      minValue<-min(minValue,min(trueData))
      if(!is.null(overlayData)){
        maxValue<-max(maxValue,max(overlayData))
        minValue<-min(minValue,min(overlayData))
      }
    }else{
      maxValue<-max(trueData)
      minValue<-min(trueData)
      trueData<-(trueData-minValue)/(maxValue-minValue)
    }
    ylim<-if(!selex){
      c(if(!ylim0fix){minValue}else{0},maxValue)
    }else{
      c(0,1)
    }

    if(is.null(col)){
      if(alpha>256){
        alpha<-256
      }
  #   cat("alpha=",alpha,"\n")
      col<-rgb(0, 0, 255, alpha=alpha, max = 256)#
    }

    if(selex){
      sims<-lapply(sims,function(sim){maxV<-max(sim);minV<-min(sim);return((sim-minV)/(maxV-minV))})
    }
    if(Cairo){
      require(Cairo) || stop("package Cairo is required")
      CairoPDF(file="plot.pdf",paper="a4",width=8,height=11,version="1.6")
    }else{
      pdf(file="plot.pdf",paper="a4",width=8,height=11,version="1.6")
    }
    plot(qyear,sims[[1]],ylim=ylim,ylab="",lty=3,type="l",xlab="",col=col)
    if(length(sims)>1){
      for(i in 2:(length(sims))){
        par(new=TRUE)
        plot(qyear,sims[[i]],ylim=ylim,ylab="",lty=3,type="l",xlab="",col=col)
      }
    }
#   par(new=TRUE)
#   plot(qyear,sims[[length(sims)]],ylim=ylim,lty=3,type="l",ylab="",xlab="",col=col)
    par(new=TRUE)
    plot(x=qyear,y=trueData,ylim=ylim,type="o",ylab=ylab,xlab="",main=main)
    if(!is.null(overlayData)){
      par(new=TRUE)
      plot(x=qyear,y=overlayData,ylim=ylim,type="l",ylab=ylab,xlab="",main="",col="red",lwd=2,lty=2,axes=FALSE)
#     cat("overlay data was plotted\n")
#     browser()
    }else{
#     cat("overlayData is null\n")
    }
}

summaryProfile<-function(cumReport="ComReport.sso",Cairo=FALSE,plot=TRUE,mfrow=c(4,2)){

  if(plot){
    if(dev.cur()!=1){
      dev.off()
    }
    if(Cairo){
      on.exit(dev.off())
      require(Cairo) || stop("package Cairo is required")
      CairoPDF(file="plot.pdf",paper="a4",width=8,height=11,version="1.6")
    }
    par(mfrow=mfrow)
  }
  # rm(list=ls())



  ll<-readLines(cumReport)
  ########################
  tmp<-strsplit(ll,split="[[:blank:]]+")

  tmp1<-unique(sapply(tmp,FUN=function(x){return(as.numeric(x[1]))},simplify=TRUE))

  lines<-tmp1[!is.na(tmp1)]

  ##############################################
  SR_parm<-grep(x=ll,pattern="SR_parm",value=TRUE)
  SR_parm.header<-strsplit(SR_parm[1],split="[[:blank:]]+")[[1]][-c(1,2)]
  if(identical(SR_parm.header[3],"SR_sigmaR")){
    hockey.stick<-FALSE
  }else{hockey.stick<-TRUE}

  SR_parm<-SR_parm[(1:length(lines))*2]

  # SR_parm[(1:15)*2]

  SR_parm.list<-strsplit(SR_parm[1:length(lines)],split="[[:blank:]]+")

  SR_parm.list.tmp<-sapply(SR_parm.list,FUN=function(x){return(as.numeric(x[-(1:2)]))},simplify=TRUE)

  if(plot){
    plot(SR_parm.list.tmp[2,][order(SR_parm.list.tmp[2,])],
      if(hockey.stick){
        SR_parm.list.tmp[3,][order(SR_parm.list.tmp[2,])]
      }else{
        SR_parm.list.tmp[1,][order(SR_parm.list.tmp[2,])]
      },type="o",
      xlab=ifelse(hockey.stick,"B_hinge/B_0","Steepness"),ylab=ifelse(hockey.stick,"R_B=0/R_0","R_0"),if(hockey.stick){ylim=c(0,1)})
  }
  ################################################################################

  plotCI<-function(x,z=x,y=NULL,uiw=NULL,liw=NULL,ui=NULL,li=NULL,ylo=NULL,yhi=NULL,...,
                   sfrac = 0.01,ymax=NULL,add=FALSE,ylim=NULL)
  {
    # Written by Venables; modified for access to ylim and contents
    if(is.list(x))
    { y <- x$y
      x <- x$x}
    if(is.null(y))
    { if(is.null(x)){stop("both x and y NULL")}
      y <- as.numeric(x)
      x <- seq(along = x)}
    if(is.null(ui) && !is.null(uiw)){
      ui <- y + uiw}
    if(is.null(li) && !is.null(liw)){
      li <- y - liw
    }
    if(is.null(ylim)){
      ylim <- range(c(y,ui,li,ylo,yhi,ymax))
    }
#   browser()
    if(!add) plot(x,y,ylim=ylim,...) else points(x,y,...)
    smidge <- diff(par("usr")[1:2]) * sfrac
    segments(x,li,x,ui)
    x2 <- c(x, x)
    ul <- c(li, ui)
    segments(x2 - smidge, ul, x2 + smidge, ul)
    invisible(list(x = x, y = y))
  }



  #Like_Value Total
  llv<-grep(x=ll,pattern="Like_Value Total",value=TRUE)

  llv.list<-strsplit(llv,split="[[:blank:]]+")

  llv.vector<-sapply(llv.list,FUN=function(x){return(as.numeric(x[4]))},simplify=TRUE)
  if(plot){
    plot(SR_parm.list.tmp[2,],llv.vector,type="o",xlab=ifelse(hockey.stick,"B_hinge/B_0","Steepness"),ylab="-logL")
  }

  #Like_Value Total
#  llv<-grep(x=ll,pattern="Like_Value Total",value=TRUE)
#
#  llv.list<-strsplit(llv,split="[[:blank:]]+")
#
#  llv.vtr<-sapply(llv.list,FUN=function(x){return(as.numeric(x[4]))},simplify=TRUE)
#

  Spbio<-grep(x=ll,pattern="Spbio ",value=TRUE)

  Spbio.list<-strsplit(Spbio,split="[[:blank:]]+")

  Spbio.arry<-sapply(Spbio.list,FUN=function(x){return(as.numeric(x[-(1:5)]))},simplify=TRUE)

  Spbio.min<-apply(Spbio.arry,2,min)

  Year<-grep(x=ll,pattern="TimeSeries",value=TRUE)[[1]]
  Year<-strsplit(x=Year,split="[[:blank:]]+")[[1]][-c(1:5)]
  Year<-sapply(Year,FUN=function(y){if(nchar(y)>nchar(Year[1])){strtrim(y,nchar(Year[1]))}else{y}},simplify=TRUE)

  B0<-grep(x=ll,pattern="Mgmt_Quant ",value=TRUE)
  B0.list<-strsplit(B0,split="[[:blank:]]+")
  B0.vector<-sapply(B0.list,FUN=function(x){return(as.numeric(x[3]))},simplify=TRUE)

  # plot(ab[2,]*B0.vtr/Spbio.min,llv.vtr,type="l",xlab="B_hinge/B_min",ylab="-logL",xlim=c(0,3))

  Bcur<-grep(x=ll,pattern="Vir_Start_End",value=TRUE)
  Bcur.list<-strsplit(Bcur,split="[[:blank:]]+")
  Bcur.vector<-sapply(Bcur.list,FUN=function(x){return(as.numeric(x[7]))},simplify=TRUE)


  plot(SR_parm.list.tmp[2,]*B0.vector/Bcur.vector,llv.vector,type="l",xlab="B_hinge/B_cur",ylab="-logL",xlim=c(0,3))

#  Spbio<-grep(x=ll,pattern="Spbio ",value=TRUE)
#
#  Spbio.list<-strsplit(Spbio,split="[[:blank:]]+")
#
#  Spbio.arry<-sapply(Spbio.list,FUN=function(x){return(as.numeric(x[-(1:5)]))},simplify=TRUE)

#  matplot(x=as.numeric(Year),y=Spbio.arry,type="l",ylab="SSB(ton)")
  require(gplots) || stop("package gplots is required")
#  plotmeans(Spbio.arry~Year)
#  browser()
  ui<-apply(Spbio.arry[,2:1001],1,FUN=function(x){quantile(x,0.90)})
  li<-apply(Spbio.arry[,2:1001],1,FUN=function(x){quantile(x,0.10)})
# browser()
  gplots::plotCI(x=as.numeric(Year),y=Spbio.arry[,1],ui=ui,li=li,type="l",ylim=c(0,max(ui)*1.1),ylab="SSB(ton)",main="SSB with 80%CIs")

  plotCI(x=as.numeric(Year),y=Spbio.arry[,1],ui=ui,li=li,type="l",ylab="SSB(ton)",main="SSB with 80%CIs")


 # plotCI(x=as.numeric(Year),y=Spbio.arry[,1],uiw=apply(Spbio.arry[,2:1001],1,sd),liw=apply(Spbio.arry[,2:1001],1,sd))


  #######################

  Recruit<-grep(x=ll,pattern="Recruit ",value=TRUE)

  Recruit.list<-strsplit(Recruit,split="[[:blank:]]+")

  Recruit.arry<-sapply(Recruit.list,FUN=function(x){return(as.numeric(x[-(1:5)]))},simplify=TRUE)

#  matplot(x=Year,y=Recruit.arry,type="l",ylab="Recruit(1000fish)")

  ui<-apply(Recruit.arry[,2:1001],1,FUN=function(x){quantile(x,0.90)})
  li<-apply(Recruit.arry[,2:1001],1,FUN=function(x){quantile(x,0.10)})

  gplots::plotCI(x=as.numeric(Year),y=Recruit.arry[,1],ui=ui,li=li,type="l",,ylim=c(0,max(ui)*1.1),ylab="Recruit(1000fish)",main="Recruits with 80%CIs")

  plotCI(x=as.numeric(Year),y=Recruit.arry[,1],ui=ui,li=li,type="l",ylab="Recruit(1000fish)",main="Recruits with 80%CIs")

# matplot()
  browser()
#  plotCI(x=as.numeric(Year),y=Recruit.arry[,1],uiw=apply(Spbio.arry[,2:1001],1,sd),liw=apply(Spbio.arry[,2:1001],1,sd))


  ####################
}

plotCI<-function (x, y = NULL, uiw, liw = uiw, ui, li, err = "y", ylim = NULL,
    xlim = NULL, type = "p", col = par("col"), barcol = col,
    pt.bg = par("bg"), sfrac = 0.01, gap = 1, lwd = par("lwd"),
    lty = par("lty"), labels = FALSE, add = FALSE, xlab, ylab,
    minbar, maxbar, ...)
{
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (invalid(xlab))
        xlab <- deparse(substitute(x))
    if (invalid(ylab)) {
        if (is.null(y)) {
            xlab <- ""
            ylab <- deparse(substitute(x))
        }
        else ylab <- deparse(substitute(y))
    }
    if (is.null(y)) {
        if (is.null(x))
            stop("both x and y NULL")
        y <- as.numeric(x)
        x <- seq(along = x)
    }
    if (err == "y")
        z <- y
    else z <- x
    if (invalid(uiw))
        uiw <- NA
    if (invalid(liw))
        liw <- NA
    if (invalid(ui))
        ui <- z + uiw
    if (invalid(li))
        li <- z - liw
    if (!invalid(minbar))
        li <- ifelse(li < minbar, minbar, li)
    if (!invalid(maxbar))
        ui <- ifelse(ui > maxbar, maxbar, ui)
    if (err == "y") {
        if (is.null(ylim))
            ylim <- range(c(y, ui, li), na.rm = TRUE)
        if (is.null(xlim) && !is.R())
            xlim <- range(x, na.rm = TRUE)
    }
    else if (err == "x") {
        if (is.null(xlim))
            xlim <- range(c(x, ui, li), na.rm = TRUE)
        if (is.null(ylim) && !is.R())
            ylim <- range(x, na.rm = TRUE)
    }
    if (!add) {
        if (invalid(labels) || labels == FALSE)
            plot(x, y, ylim = ylim, xlim = xlim, col = col, xlab = xlab,
                ylab = ylab, ...)
        else {
            plot(x, y, ylim = ylim, xlim = xlim, col = col, type = "n",
                xlab = xlab, ylab = ylab, ...)
            text(x, y, label = labels, col = col, ...)
        }
    }
    if (is.R())
        myarrows <- function(...) arrows(...)
    else myarrows <- function(x1, y1, x2, y2, angle, code, length,
        ...) {
        segments(x1, y1, x2, y2, open = TRUE, ...)
        if (code == 1)
            segments(x1 - length/2, y1, x1 + length/2, y1, ...)
        else segments(x2 - length/2, y2, x2 + length/2, y2, ...)
    }
    if (err == "y") {
        if (gap != FALSE)
            gap <- strheight("O") * gap
        smidge <- par("fin")[1] * sfrac
        if (!is.null(li))
            myarrows(x, li, x, pmax(y - gap, li), col = barcol,
                lwd = lwd, lty = lty, angle = 90, length = smidge,
                code = 1)
        if (!is.null(ui))
            myarrows(x, ui, x, pmin(y + gap, ui), col = barcol,
                lwd = lwd, lty = lty, angle = 90, length = smidge,
                code = 1)
    }
    else {
        if (gap != FALSE)
            gap <- strwidth("O") * gap
        smidge <- par("fin")[2] * sfrac
        if (!is.null(li))
            myarrows(li, y, pmax(x - gap, li), y, col = barcol,
                lwd = lwd, lty = lty, angle = 90, length = smidge,
                code = 1)
        if (!is.null(ui))
            myarrows(ui, y, pmin(x + gap, ui), y, col = barcol,
                lwd = lwd, lty = lty, angle = 90, length = smidge,
                code = 1)
    }
    points(x, y, col = col, lwd = lwd, bg = pt.bg, type = type,
        ...)
    invisible(list(x = x, y = y))
}

############################################################
### 2011/04/08 グラフィックスデバイスの初期化を独立した関数にした
###
###
#init.Graphics<-function(type="base",device=NULL,Cairo=TRUE,plot=TRUE){
#  if(type=="base"){
#    plot.type=type
#  }else if(type=="lattice"){
#    plot.type=type
#    require(lattice)||stop("lattice is required")
#    require(lattice)||stop("latticeExtra is required")
#  }else if(type=="ggplot2"){
#    plot.type=type
#  }else{
#    stop("no appropriate gtype")
#  }
#  if(plot){
#    if(dev.cur()!=1){
#      dev.off()
#    }
#    if(!is.null(file))file<-"plot"
#    if(device=="pdf"){
#      exit.fn<-function(){dev.off()}
#      if(Cairo&& as.numeric(sessionInfo()$R.version$minor)>=12.2 && .Platform$OS.type=="windows"){
#        ap<-.packages(all.available=TRUE)
#        if(sum(ap %in% "Cairo")>0){
#          require(Cairo) || stop("package Cairo is required")
#          CairoPDF(file=file,width=8,height=11,version="1.6")
#        }else if(sum(ap %in% "cairoDevice")>0){
#          require(cairoDevice) || stop("package ciroDevice is required")
#          height<-297/25.4
#          width<-210/25.4
#          Cairo_pdf(file=file,width=width,height=height)
#        }else{
#          stop("Please install either Cairo or cairoDevice through install.packages")
#        }
#      }else{
#        pdf(file=file,paper="a4",width=width,height=height,version="1.6")
#      }
#    }else if(is.null(type)|type=="windows" ){
#      if(Cairo && as.numeric(sessionInfo()$R.version$minor)>=12.2 && .Platform$OS.type=="windows"){
#        ap<-.packages(all.available=TRUE)
#        if(sum(ap %in% "Cairo")>0){
#          require(Cairo) || stop("package Cairo is required")
#          CairoWin(record=TRUE,paper="a4",width=8,height=11)
#        }
#      } else if(Cairo&& sessionInfo()$R.version$arch=="x86_64" && as.numeric(sessionInfo()$R.version$minor)<12.2 && .Platform$OS.type=="windows" ){
#          cat("Currently neither Cairo nor cairoDevice work properly in 64bit version so that ordinary pdf device will be used\n")
#            windows.options(record=TRUE)
#      }
#    }
#    if(gtype=="base"){
#      opar<-par()
#      par(mfrow=mfrow,mar=c(3,4,2,1)+0.1)
#    }
#  }
#  return(list(exit.fn=exit.fn))
#}


### R-2.12.1 windows x64では、現状で、Cairoが提供されていないので、cairoはディフォールトでは使わない。
### R-2.12.2 windows x64では、Cairoが使えるようになったのでディフォールトで使用する事にした。
### To-do 1.  consider to use lattice
###       2.  consider to use xYplot in Hmisc
###       3.  consider to use getMarginWidth? in plotrix to calculate appropriate margin of plots
###       4.  consider to use c.trellis and xyplot.list {latticeExtra}
###       5.  consider to use arrangeGrob {gridExtra}
###
###
###
summarySS<-function(reportFile="Report.sso",filename=NULL,plot.new=TRUE,Cairo=FALSE,type=NULL,mfrow=c(3,2),
  age=TRUE,len=TRUE,size=TRUE,scale=1,gtype="lattice",interactive=TRUE,plot.grid=TRUE,width=8,height=11,
  compReportFile="CompReport.sso",japanese=FALSE,fill.page=TRUE){
  inits<-NULL
  if(plot.new){
    ggplotlike<-FALSE
    inits<-init.graph(type=type,ggplotlike=ggplotlike,filename=filename,Cairo=Cairo,gtype=gtype,japanese=japanese)
    on.exit(inits$exit.fn(dev.used=inits$dev.used,lattice=inits$lattice,ggplotlike=inits$ggplotlike,oopt=inits$oopt,opar=inits$opar))
#    cat("called\n")
  }


  report<-getReport.sso(repfile=reportFile,interactive=interactive)
### Calculate F@A
  cat("calculating F@A\n")
  faa<-getFAA.y.ss3(report=report)
  cat("Done\n")
### Calculate N@A
  cat("Reading N@A\n")
  naa<-getNAA.y.ss3(report=report)
  cat("Done\n")
### get estimated C@A
  cat("Reading C@A\n")
  caa<-getCAA.y.ss3(report=report)
  cat("Done\n")
### get derived quantity
  DQ<-getDerivedQuant(report=report)
  cat("Finished reading derived quantities\n")
#browser()
### グラフの作成
### 親魚量
#browser()
  SPB<-DQ$DerivedQuant.D.F[DQ$DerivedQuant.D.F$Label=="SPB",]
  x<-as.numeric(SPB$Index[-c(1,2)])
  y<-as.numeric(SPB$Value[-c(1,2)])
  uiw<-liw<-as.numeric(SPB[,4][-c(1,2)])[1:length(x)]
#plotCI(x=x,y=y,uiw=uiw,liw=liw,type="l")
# browser()
  if(gtype=="base"){
    plotCI(x=x,y=y,uiw=uiw,liw=liw,type="l",xlab="Year",ylab="SSB(ton)",
      main="SSB with SD",xlim=range(x),plot.grid=TRUE,plot.type=plot.type)
  }else if(gtype=="lattice"){
    lower<-y-liw
    upper<-y+uiw
#    browser()
    require(Hmisc)||stop("package Hmisc is required")
    panel<-function(x,y,...){
      panel.xYplot(x,y,...)
      panel.grid(col.line="lightgrey",h=-1,v=-1,...)
    }
    plot.SPB<- xYplot(Cbind(y,lower,upper)~x,type="l",col="blue",xlab=""
     ,ylab="",layout=c(1,3),as.table=TRUE,panel=panel,main="SSB with 1SD")
  }
#grid()
#browser()
### 加入量
  Recr<-DQ$DerivedQuant.D.F[DQ$DerivedQuant.D.F$Label=="Recr",]
  x<-as.numeric(Recr$Index[-c(1,2)])
  x<-x[!is.na(x)]
  y<-as.numeric(Recr$Value[-c(1,2)])
  y<-y[1:length(x)]
  uiw<-liw<-as.numeric(Recr[,4][-c(1,2)])[1:length(x)]
  #browser()
  if(gtype=="lattice")brewer.div<-colorRampPalette(c("pink","green","blue","yellow","red"))
  if(gtype=="base"){
    plotCI(x=x,y=y,uiw=uiw,liw=liw,type="l",xlab="Year",ylab="Recruit(1000fish)",
    main="Recruits with SD",xlim=range(x),plot.grid=TRUE,plot.type=plot.type)
    ## Spawner recruit
    sr<-read.spawner_recruit(report=report,plot=TRUE,B0=FALSE)
  }else if(gtype=="lattice"){
    lower<-y-liw
    upper<-y+uiw
#    browser()
    panel<-function(x,y,...){
      panel.xYplot(x,y,...)
      panel.grid(col.line="lightgrey",h=-1,v=-1,...)
    }
    plot.R<- xYplot(Cbind(y,lower,upper)~x,type="l",col="blue",xlab="Year",
      ylab="Recruit(1000fish)",layout=c(1,3),as.table=TRUE,panel=panel,main="Recruit")
    ## Spawner recruit
    sr<-read.spawner_recruit(report=report,plot=TRUE,B0=FALSE,lattice=TRUE,layout=c(1,3))
    print(c(SSB=plot.SPB,Recruit=plot.R,StockRecruit=sr$plot.obj,x.same = FALSE,layout=c(1,3)))
#    browser()
  }

### F at age
  if(gtype=="base"){
    if(faa$nmorph==1){
      matplot(y=faa$faa_y,x=dimnames(faa$faa_y)[[1]],ylab="F",xlab="Year",main="F at age",type="l",col="Black",lty=1:dim(faa$faa_y)[2])
        legend("topleft",paste("age_",dimnames(faa$faa_y)[[2]],sep=""),col="Black",lty=1:dim(faa$faa_y)[2])
      grid()
    }else{
      for(i in 1:faa$nmorph){
        faa1<-faa$faa_y[,,i]
      matplot(y=faa1,x=dimnames(faa1)[[1]],ylab="F",xlab="Year",main="F at age",type="l",col="Black",lty=1:dim(faa1)[2])
        legend("topleft",paste("age_",dimnames(faa1)[[2]],sep=""),col="Black",lty=1:dim(faa1)[2])
        grid()
      }
    }
  }else if(gtype=="lattice"){
#    browser()
    require(RColorBrewer)||stop("Package RColorBrewer required")
    xlab<-"Year"
    ylab<-"Age"
    main<-"annual F@A"

    if(faa$nmorph==1){
      faa<-faa$faa_y
      ncuts<-floor(100*max(faa))
      xy.faa<-levelplot(faa,xlab=xlab,ylab=ylab,prety=TRUE,main=main,
        scales=list(y=list(rot=0),x=list(rot=90)),cuts=ncuts,col.regions=brewer.div(ncuts),
        drop.unused.levels=TRUE,aspect="fill")
    }else{
      faa<-faa$faa_y
      ncuts<-floor(100*max(faa))
      require(reshape)||stop("package reshape is required\n")
      faa.df<-melt.array(faa)
      colnames(faa.df)<-c("Yr","Age","Morph","value")
      main<-"annual F@A by Morph"
      xy.faa<-levelplot(data=faa.df,value~Yr+Age|factor(Morph),xlab=xlab,ylab=ylab,prety=TRUE,main=main,
        scales=list(y=list(rot=0),x=list(rot=90)),cuts=ncuts,col.regions=brewer.div(ncuts),as.table=TRUE,
        drop.unused.levels=TRUE,aspect="xy")
#      faa.mat<-as.matrix(faa)
#      colnames(faa.mat)<-0:(length(colnames(faa.mat))-1)
#      xy.faa<-levelplot(data=faa.mat,xlab=xlab,ylab=ylab,prety=TRUE,main=main,
#        scales=list(y=list(rot=0),x=list(rot=90)),cuts=ncuts,col.regions=brewer.div(ncuts),as.table=TRUE,
#        drop.unused.levels=TRUE,aspect="xy")
    }
  }

### N at age
#browser()
  if(gtype=="base"){
  matplot(naa$naa_y,x=rownames(naa$naa_y),ylab="N(1000 fish)",xlab="Year",main="N at age",type="l",col="Black",lty=1:dim(naa$naa_y)[2])
  legend("topleft",paste("age_",colnames(naa),sep=""),col="Black",lty=1:dim(naa$naa_y)[2])
  grid()
  }else if(gtype=="lattice"){
    xlab<-"Year"
    ylab<-"Age"
    main<-"Numbers at age in log10 at the begining of year"
    naa.mat<-as.matrix(naa$naa_y2)
    colnames(naa.mat)<-0:(length(colnames(naa.mat))-1)
    naa.mat<-log10(naa.mat)
    naa.mat[naa.mat<0]<-0
    ncuts<-floor(10*max(naa.mat))
    xy.naa<-levelplot(naa.mat,xlab=xlab,ylab=ylab,prety=TRUE,main=main,
      scales=list(y=list(rot=0),x=list(rot=90)),cuts=ncuts,col.regions=brewer.div(ncuts),
      drop.unused.levels=TRUE,aspect="fill")

    print(xy.faa,position=c(0,0.5,1,1),more=TRUE)
#    browser()
    print(xy.naa,position=c(0,0,1,0.5))
  }

### CPUE fits
#  browser()
  cat("CPUE fit\n")
  fit<-getCPUEfit.ss3.x(report=report)
  fleets<-unique(fit$fit$Fleet)
  if(gtype=="base"){
    fit.fn<-function(x){
      xlab<-"Year"
      tt<-fit$fit[fit$fit$Fleet==x,]
      ylim<-range(tt$Obs,tt$Exp)
      ylab<-"CPUE"

      main=paste("CPUE:",strsplit(x,split="_")[[1]][2])
      plot(data=tt,Exp~Yr,type="l",main=main,ylim=ylim,ylab=ylab,xlab=xlab)
      points(data=tt,Obs~Yr)
      grid()
      legend("topleft",c("Expected","Observed"),lty=c(1,0),pch=c(NA,1))
    }
    lapply(fleets,FUN=fit.fn)
  }else if(gtype=="lattice"){
    cat("HERE4032\n")
    makeCPUEfit(report=report,plot=TRUE,plot.new=FALSE)
  }

  cat("Age and/or length fits\n")
  if(gtype=="base"){
    plotPearson.trad(report=report,filename=NULL,type=type,scale=scale,age.fit=age,length.fit=len,size.fit=size,
      plot.new=FALSE,japanese=FALSE,compReportFile=compReportFile,japanese=japanese)
  }else if(gtype=="lattice"){
    plotPearson(report=report,filename=NULL,type=type,scale=scale,age.fit=age,length.fit=len,size.fit=size,
    plot.new=FALSE,compReportFile=compReportFile,fill.page=fill.page,japanese=japanese)
  }
  cat("Finished summarySS\n")
  return(invisible(list(faa=faa,caa=caa,naa=naa,fit=fit$fit,sr=sr$sr,derived.quant=DQ)))
}

#######################################################################################
########### r4ss から拝借

plotCI<-function (x, y = NULL, uiw, liw = uiw, ylo = NULL, yhi = NULL,
    ..., sfrac = 0.01, ymax = NULL, plot.type="base",plot.grid=FALSE)
{
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (is.null(y)) {
        if (is.null(x)) {
            stop("both x and y NULL")
        }
        y <- as.numeric(x)
        x <- seq(along = x)
    }
    ui <- y + uiw
    li <- y - liw
    ylim <- range(c(y, ui, li, ylo, yhi, ymax))
#    browser()
    smidge <- diff(par("usr")[1:2]) * sfrac
    x2 <- c(x, x)
    ul <- c(li, ui)
    if(plot.type=="base"){
      a<-plot(x, y, ylim = ylim, ...)
      segments(x, li, x, ui)
      segments(x2 - smidge, ul, x2 + smidge, ul)
      if(plot.grid)grid()
#      browser()
    }else{
      require(ggplot2) || stop("package ggplot2 is required")
    # Define the top and bottom of the errorbars
      limits <- aes(ymax = y + uiw, ymin=y - uiw)
#      browser()
      gr<-qplot(x,y, ylim = ylim, ...)+geom_line()+geom_point()+geom_errorbar(limits)
      print(gr)
#      browser()
    }
    invisible(list(x = x, y = y))
}
## cm.colors(n=100,alpha=1)
## grey(1:100/100)
## heat.colors(n=100,alpha=1)

plotPearson<-function(repfile="Report.sso",report=NULL,filename=NULL,type=NULL,layout=c(2,4),scale=1,
  length.fit=TRUE,age.fit=FALSE,size.fit=FALSE,plot.new=TRUE,sqrt=TRUE,fill.page=TRUE,brewer.div=NULL,compress=NA,
  adjust=1.0,kernel="gaussian",kern.window="gaussian",kernel.bw="nrd0",compReportFile="compReport.sso",family="gaussian",
  transparent=FALSE,japanese=FALSE){
  require(lattice) || stop("package lattice is required")
  require(RColorBrewer)|| stop("package RColorBrewer is required")
  require(locfit)|| stop("package locfit is required")


  if(is.null(report))report<-getReport(repfile=repfile)

  if(plot.new){
    if(is.null(type))type="windows"
    if(is.null(filename)){
      if(type=="pdf")filename="plot.pdf"
    }
    if(is.null(type)||type=="windows" ){
      windows(record=TRUE)
    }else if(type=="pdf"){
      pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=11,width=9)
    }
    on.exit(if(names(dev.cur())!="null device" && type!="windows") dev.off())
  }
#  brewer.div<-colorRampPalette(c("pink","green","blue","yellow","red"))
  if(is.null(brewer.div)){
    brewer.div <-colorRampPalette(brewer.pal(11, "Spectral"),interpolate = "spline")
  }else{
    brewer.div<-colorRampPalette(brewer.div)
  }

  tmp.fnc<-function(report,Kind,compReportFile){
    a2<-getAgecomp.ss3.x(report=report,size.kind="LEN",
        compReportFile=compReportFile)
    a2[[1]]<-a2[[1]][a2[[1]]$Kind==Kind,]
    if(nrow(a2[[1]])>0){
      main<-ifelse(Kind=="LEN","Length fit","Age fit")
      cat(paste(main,"\n"))
      fleets<-unique(subset(a2[[1]],!is.na(Bin))[,"Fleet"])
      if(fill.page){
        if(length(fleets)<=8 && length(fleets)>1){
          if(length(fleets)%%2 ==0){
            layout<-c(2,floor(length(fleets)/2))
              layout0<-c(1,2)
          }else{
            layout<-c(2,floor(length(fleets)/2)+1)
              layout0<-c(1,2)
          }
        }else if(length(fleets)==1){
          layout<-c(1,2)
        }else{
          layout<-c(2,4)
        }
      }
#      browser()
      plot.data<-subset(a2[[1]],!is.na(Bin))
      yr.range<-range(plot.data$Yr)
      plot.data$period<-ifelse(plot.data$Yr>=mean(yr.range),"late","early")
      plot.data$cex<-scale*if(sqrt){sqrt(abs(plot.data$Pearson))}else{abs(plot.data$Pearson)}
      plot.data$col<-ifelse(plot.data$Pearson>=0,"blue","red")
      ylab<-ifelse(Kind=="LEN","Length","Age")
      if(japanese)ylab<-ifelse(Kind=="LEN","体長","年齢")
      xx<-xyplot(data=plot.data,Bin~Yr|period*factor(Fleet),cex=plot.data$cex,
        layout=layout,drop.unused.level=TRUE,xlab="Year",ylab=ylab,main=paste(main,"(Blue:Obs>Pred, Red:Obs<Pred)"),col=plot.data$col,
        as.table=TRUE,type=c("p","g"),scale=list(x="free"))

      print(xx)
#      browser()
      if(is.na(compress)){
        plot.data$cex<-scale*if(sqrt){sqrt(abs(plot.data$Pearson))*sign(plot.data$Pearson)}else{abs(plot.data$Pearson)*sign(plot.data$Pearson)}
      }else{
        tmp<-ifelse(abs(plot.data$Pearson)>=compress,compress,abs(plot.data$Pearson))
        plot.data$cex<-scale*if(sqrt){sqrt(tmp)*sign(plot.data$Pearson)}else{tmp*sign(plot.data$Pearson)}
      }
#      browser()
      dum.df<-expand.grid(Yr=sort(unique(plot.data$Yr)),Bin=sort(unique(plot.data$Bin)),Fleet=sort(unique(plot.data$Fleet)))
#      dum.df$cex<-rep(NA,nrow(dum.df))
      dum.df1<-merge(dum.df,plot.data,all.x=TRUE)
      xy<-levelplot(data=dum.df1,cex~Yr*Bin|factor(Fleet),col.regions=brewer.div(100),
      layout=layout,as.table=TRUE,xlab="Year",ylab=ylab,main=main,
      drop.unused.levels=TRUE)
      print(xy)
      cols<-with(plot.data,level.colors(cex,at=do.breaks(range(cex),100)),col.regions=brewer.div(100))
      if(0){
      xz<-xyplot(data=plot.data,Bin~Yr|factor(Fleet),
        layout=layout,drop.unused.level=TRUE,xlab="Year",ylab=ylab,main=main,
        as.table=TRUE,
        colours=cols,
        panel=function(x,y,colours,subscripts,...){
          panel.xyplot(x,y,pch=21,col="transparent",
          fill=colours[subscripts],...)
        }
        )
      print(xz)
      }
      xx2<-histogram(data=plot.data,~Pearson|factor(Fleet),as.table=TRUE,xlab="Pearson residuals",
        layout=layout,type="density",scales=list(x="free"),equal.width=TRUE)
      print(xx2)

      xy2<-histogram(data=plot.data,~Pearson|as.factor(Fleet),as.table=TRUE,scales=list(y="free",x="free"),
        lty=1,col="blue",layout=layout,distribute.type=TRUE,auto.key=TRUE,
        ylab="Frequency",xlab="Pearson residual",type="density",
        panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.densityplot(x,plot.points=FALSE,
                    darg = list(adjust=adjust,kernel=kernel,window=kern.window,bw=kernel.bw), col = "black",
                    )
      })
      print(xy2)
      xz2<-xyplot(data=plot.data,Pearson~Bin|factor(Fleet),
        layout=layout,drop.unused.level=TRUE,xlab="Year",ylab="Pearson residual",main="Pearson residuals by Bin",family=family,
        as.table=TRUE,type=c("p","g"),scales=list(y="free"),groups=Seas,auto.key=list(columns=4),jitter.x=TRUE,pch=21,cex=0.2,
#        panel = function(x,y,family, ...) {
#                  panel.xyplot(x,y, ...)
#                  panel.loess(x,y,span = 2/3, degree = 1,
#                      family = family,evaluation = 30,horizontal = FALSE,...)
#                  panel.locfit(x,y,...)}
        )
      print(xz2)

      xz3<-xyplot(data=plot.data,Pearson~Yr|factor(Fleet),
        layout=layout,drop.unused.level=TRUE,xlab="Year",ylab="Pearson residual",main="Pearson residuals by Year",family=family,
        as.table=TRUE,type=c("p","g"),scales=list(y="free"),groups=Seas,auto.key=list(columns=4),jitter.x=TRUE,pch=21,cex=0.2,
#        panel = function(x,y,family, ...) {
#                  panel.xyplot(x,y, ...)
#                  panel.loess(x,y,span = 2/3, degree = 1,
#                      family = family,evaluation = 30,horizontal = FALSE,...)
#                  panel.locfit(x,y,...)}
        )
      print(xz3)
    }
  }
  if(age.fit){
    tmp.fnc(report=report,Kind="AGE",compReportFile=compReportFile)
  }

  if(length.fit){
    tmp.fnc(report=report,Kind="LEN",compReportFile=compReportFile)
  }

  if(size.fit){
    tmp.fnc(report=report,Kind="SIZE",compReportFile=compReportFile)
  }

}


plotPearson.trad<-function(repfile="Report.sso",report=NULL,filename=NULL,type=NULL,scale=1,age.fit=FALSE,size.fit=FALSE,
  length.fit=FALSE,plot.new=TRUE,sqrt=TRUE,japanese=FALSE,main="",compReportFile="compReport.sso"){
#  require(lattice)

  if(is.null(report))report<-getReport(repfile=repfile)

  if(plot.new){
    if(is.null(type))type="windows"
    if(is.null(filename)){
      if(type=="pdf")filename="plot.pdf"
    }
    if(is.null(type)|type=="windows" ){
      windows(record=TRUE)
    }else if(type=="pdf"){
      pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=9,width=9)
    }

  on.exit(if(names(dev.cur())!="null device" && type!="windows") dev.off())
  }
  if(age.fit){
    cat("age fit\n")
    a2<-getAgecomp.ss3.x(report=report,size.kind="AGE",compReportFile=compReportFile)
    a2[[1]]<-a2[[1]][a2[[1]]$Kind=="AGE",]
    if(nrow(a2[[1]])>0){
      main<-"Age Fit,"
      fleets<-unique(subset(a2[[1]],!is.na(Bin))[,"Fleet"])
      if(length(fleets)==1){single=TRUE}else{single=FALSE}
      bins<-unique(subset(a2[[1]],!is.na(Bin))[,"Bin"])
      if(japanese){Year="年";Age="年齢";Length="体長"}else{Year="Year";Age="Age";Length="Length"}
      tmp.fn<-function(x){
           a2.1<-a2[[1]][!is.na(a2[[1]]$Bin) & a2[[1]]$Fleet==x,]
          cols<-ifelse(a2.1$Pearson>=0,"Black","Red")
          pchs<-ifelse(a2.1$Pearson>=0,1,19)
          cex<-scale*if(sqrt){sqrt(abs(a2.1$Pearson))}else{abs(a2.1$Pearson)}
          ymax<-max(a2.1$Bin)+0.25
          if(!single)main<-paste(main,"Fleet",x,":")
          plot(a2.1$Bin~a2.1$Yr,cex=cex,col=cols,xlab=Year,ylab=Age,
            main=paste(main,"Black:Positive,Red:negative"),axes=FALSE,pch=pchs,ylim=c(-0.25,ymax))
          box()
          grid()
    #      browser()
          axis(side=2,at=seq(from=0,to=max(a2.1$Bin),by=1))
          axis(side=1)
      }

      if(length(fleets)>1){
        lapply(sort(fleets),FUN=tmp.fn)
      }else{
        tmp.fn(fleets)
      }
    }else{
      cat("age fit data is missing!\n")
    }

  }

  if(length.fit){
    cat("length fit\n")
    a2<-getAgecomp.ss3.x(report=report,size.kind="LENGTH",compReportFile=compReportFile)
    a2[[1]]<-a2[[1]][a2[[1]]$Kind=="LEN",]
    if(nrow(a2[[1]])>0){
      main<-"Length Fit,"
      fleets<-unique(subset(a2[[1]],!is.na(Bin))[,"Fleet"])
      if(length(fleets)==1){single=TRUE}else{single=FALSE}
      bins<-unique(subset(a2[[1]],!is.na(Bin))[,"Bin"])
      if(japanese){Year="年";Age="年齢";Length="体長"}else{Year="Year";Age="Age";Length="Length"}

      tmp.fn2<-function(x){
           a2.1<-a2[[1]][!is.na(a2[[1]]$Bin) & a2[[1]]$Fleet==x,]
          cols<-ifelse(a2.1$Pearson>=0,"Black","Red")
          pchs<-ifelse(a2.1$Pearson>=0,1,19)
          cex<-scale*if(sqrt){sqrt(abs(a2.1$Pearson))}else{abs(a2.1$Pearson)}
          if(!single)main<-paste(main,"Fleet",x,":")
          plot(a2.1$Bin~a2.1$Yr,cex=cex,col=cols,xlab=Year,ylab=Length,
            main=paste(main,"Black:Positive,Red:negative"),axes=FALSE,pch=pchs)

          box()
          axis(side=2)
          axis(side=1)
      }
      if(length(fleets)>1){
        lapply(sort(fleets),FUN=tmp.fn2)
      }else{
        tmp.fn(fleets)
      }
    }else{
      cat("Length fit data is missing!\n")
    }
  }
}
###########
###########  Function taken from package gstat (sp)
bubble<-function (obj, zcol = 1, ..., fill = TRUE, maxsize = 3, do.sqrt = TRUE,
    pch, col = c(2, 3), key.entries = quantile(data[, zcol]),
    main = ifelse(is.numeric(zcol), names(data)[zcol], zcol),
    identify = FALSE, labels = row.names(data.frame(obj)), key.space = "right",
    scales = list(draw = FALSE), xlab = NULL, ylab = NULL, panel = panel.bubble,
    sp.layout = NULL)
{
    obj = as(obj, "SpatialPointsDataFrame")
    data = as.data.frame(obj)
    cc = coordinates(obj)
    x = cc[, 1]
    y = cc[, 2]
    if (NCOL(data) == 1)
        z = data
    else if (NCOL(data) == 0)
        z = rep(1, NROW(cc))
    else z = data[, zcol]
    if (missing(pch))
        pch = ifelse(fill, 16, 1)
    if (length(col) == 1)
        col = rep(col, 2)
    z.col = as.vector(ifelse(z < 0, col[1], col[2]))
    q = key.entries
    q.pch = rep(pch, length(q))
    q.text = as.character(round(q, 3))
    q.col = as.vector(ifelse(q < 0, col[1], col[2]))
    az = abs(z)
    q = abs(q)
    if (do.sqrt) {
        az = sqrt(az)
        q = sqrt(q)
    }
    cex = as.vector(maxsize * az/max(az, q))
    q.cex = as.vector(maxsize * q/max(az, q))
    if (identify) {
        plot(x, y, asp = 1, cex = cex, main = main, ...)
        return(identify(x, y, labels))
    }
    else {
        key = list(space = key.space, points = list(pch = q.pch,
            col = q.col, cex = q.cex), text = list(q.text))
        xyplot(y ~ x, col = z.col, cex = cex, pch = pch, asp = mapasp(obj),
            key = key, main = main, scales = scales, xlab = xlab,
            ylab = ylab, panel = panel, sp.layout = sp.layout,
            ...)
    }
}



plotBubble<-function(df=NULL,x,y,z,negative,sqrt=TRUE,multiplier=1,xlab="Year",ylab="Age",main="",ymin=NA,by=1,log=FALSE,scaled=TRUE){
  if(!is.null(df)){
    x<-df$x
    y<-df$y
    z<-df$z
  }
  cols<-ifelse(z>=0,"Black","Red")
  pchs<-ifelse(z>=0,1,19)
  if(scaled)absz<-abs(z)/max(abs(z)+1.0e-10)*4
  if(log)absz<-log10(abs(z))
  if(sqrt){
    cex<-multiplier*sqrt(absz)
  }else{
    cex<-multiplier*absz
  }
  ymax<-max(y)+0.25
  if(is.na(ymin))ymin<-min(y)-0.25
  plot(y~x,cex=cex,col=cols,xlab=xlab,ylab=ylab,main=main,axes=FALSE,pch=pchs,ylim=c(ymin,ymax))
  box()
  grid()
      browser()
  axis(side=2,at=seq(from=0,to=max(y),by=1))
  axis(side=1)
}

## Lattice で複数のプロットを1ページに収める例
if(0){
  data(AirPassengers)
# a dataset supplied with base R
AP = AirPassengers
# re-bind to save some typing
# split the AP data set into two pieces so we have unique data for each of the two plots
w1 = window(AP, start=c(1949, 1), end=c(1952, 1))
w2 = window(AP, start=c(1952, 1), end=c(1960, 12))
px1 = xyplot(w1)
px2 = xyplot(w2)
# arrange the two plots vertically
print(px1, position=c(0, 0.6, 1, 1), more=TRUE)
 print(px2, position=c(0, 0, 1, 0.4))
}



############################################################
####  Taken and modified from SS_readstarter.R in the package r4ss
SS_readstarter <-  function(file='starter.ss', verbose=TRUE,interactive=FALSE){
  if(verbose) cat("running SS_readstarter\n")
#  starter <- readLines(file,warn=F)
  starter<-readLinesInteract(file,warn=FALSE,interactive=interactive)
  onlyComments<-grep(value=FALSE,pattern="^#",x=st)
  empty<-grep(value=FALSE,pattern="^$",x=st)
  blank<-grep(value=FALSE,pattern="^[[:blank:]]+$",x=st)

  stlines<-sort(c(onlyComments,empty,blank)) # Find line number of lines with only comment or blank
  stlines<-setdiff(1:length(st),stlines)

  mylist <- list()

  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_starter_file"
  mylist$SSversion <- "SSv3.10b_or_later"

  # get strings for control and data file names
  starter2 <- NULL
  for(i in 1:length(starter)){
      # get only stuff before # marks
      mysplit <- strsplit(starter[i],split="#")[[1]][1]
      if(!is.na(mysplit) && length(mysplit) > 0) starter2 <- c(starter2,mysplit)
  }
  strings <- NULL
  for(i in 1:length(starter2)){
      mysplit <- strsplit(starter2[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      strings <- c(strings,mysplit)
  }
  strings <- strings[is.na(suppressWarnings(as.numeric(strings)))]
  if(length(strings)>2){
      cat("too many strings in starter file?\n")
      print(strings)
      return()
  }else{
      mylist$datfile <- strings[1]
      mylist$ctlfile <- strings[2]
  }

  # get numbers (could be better integrated with function above)
  allnums <- NULL
  for(i in 1:length(starter)){
      # split apart numbers from text in file
      mysplit <- strsplit(starter[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      nums <- suppressWarnings(as.numeric(mysplit))
      if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
      else maxcol <- length(nums)
      if(maxcol > 0){
          nums <- nums[1:maxcol]
          allnums <- c(allnums, nums)
      }
  }

  # go through numerical values and save as elements of a big list
  i <- 1

  mylist$init_values_src <- allnums[i]; i <- i+1
  mylist$run_display_detail <- allnums[i]; i <- i+1
  mylist$detailed_age_structure <- allnums[i]; i <- i+1
  mylist$checkup <- allnums[i]; i <- i+1
  mylist$parmtrace <- allnums[i]; i <- i+1
  mylist$cumreport <- allnums[i]; i <- i+1
  mylist$prior_like <- allnums[i]; i <- i+1
  mylist$soft_bounds <- allnums[i]; i <- i+1
  mylist$N_bootstraps <- allnums[i]; i <- i+1
  mylist$last_estimation_phase <- allnums[i]; i <- i+1
  mylist$MCMCburn <- allnums[i]; i <- i+1
  mylist$MCMCthin <- allnums[i]; i <- i+1
  mylist$jitter_fraction <- allnums[i]; i <- i+1
  mylist$minyr_sdreport <- allnums[i]; i <- i+1
  mylist$maxyr_sdreport <- allnums[i]; i <- i+1
  mylist$N_STD_yrs <- N_STD_yrs <- allnums[i]; i <- i+1
  if(N_STD_yrs>0){
      mylist$STD_yr_vec <- allnums[i:(i+N_STD_yrs-1)]; i <- i+N_STD_yrs
  }
  mylist$converge_criterion <- allnums[i]; i <- i+1
  mylist$retro_yr <- allnums[i]; i <- i+1
  mylist$min_age_summary_bio <- allnums[i]; i <- i+1
  mylist$depl_basis <- allnums[i]; i <- i+1
  mylist$depl_denom_frac <- allnums[i]; i <- i+1
  mylist$SPR_basis <- allnums[i]; i <- i+1
  mylist$F_report_units <- allnums[i]; i <- i+1
  mylist$F_report_basis <- allnums[i]; i <- i+1

  # check final value
  if(allnums[i]==999){
    if(verbose) cat("read of starter file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # all done
  return(mylist)
}



read.starter<-function(starterfile="starter.ss",interactive=FALSE){
  #     prepare starter.ss
      st<-readLinesInteract(starterfile)
      onlyComments<-grep(value=FALSE,pattern="^#",x=st)
      empty<-grep(value=FALSE,pattern="^$",x=st)
      blank<-grep(value=FALSE,pattern="^[[:blank:]]+$",x=st)

      stlines<-sort(c(onlyComments,empty,blank)) # Find line number of lines with only comment or blank
      stlines<-setdiff(1:length(st),stlines)

      st[stlines[1]]<-paste("data-boot",formatC(i,width=log10(max(cutList))+1,flag=0),".dat",sep="")
      st[stlines[2]]<-"control.ss"
      if(as.numeric(strsplit(st[stlines[3]],split="#")[[1]][1])!=1){
        st[stlines[3]]<-paste(1,strsplit(st[stlines[3]],split="#")[[1]][2],sep=" #")
      }

      if(as.numeric(strsplit(st[stlines[11]],split="#")[[1]][1])>0){
        st[stlines[11]]<-paste(0,strsplit(st[stlines[11]],split="#")[[1]][2],sep=" #")
      }
      if(!verboseDisplay){
        if(as.numeric(strsplit(st[stlines[4]],split="#")[[1]][1])!=0){
          st[stlines[4]]<-paste(0,strsplit(st[stlines[4]],split="#")[[1]][2],sep=" #")
        }
      }


}

readLinesInteract<-function(filename,interactive=FALSE){
  require(R.utils) || stop("package R.utils is required")
  if(file.exists(path=filename)){
    report<-readLines(filename)
  }else{
    if(!interactive){
      stop(paste(getAbsolutePath(filename)," does not exist."))
    }else{
      filename<-file.choose()
      report<-readLines(filename)
    }
  }
  return(report)
}

readWithComments<-function(fileName,interactive=FALSE){
  contents<-readLinesInteract(filename=filenme,interactive=interactive)

}

###########################
### Modified from plotSim

plotMult<-function(ts.lists,ylab="",xlab="",col=NULL,
  alpha=128/(length(sims)/100),main="",ylim0fix=FALSE,plot.new=FALSE,xlim=NULL,type=NULL,filename=NULL){
#    on.exit(dev.off())

#### ylim の設定
    maxValue<-max(sapply(ts.lists,FUN=function(z){max(as.numeric(z[,2]))}))
    minValue<-min(sapply(ts.lists,FUN=function(z){min(as.numeric(z[,2]))}))

    ylim<-c(if(!ylim0fix){minValue}else{0},maxValue)

#### xlim の設定
    maxValue<-max(sapply(ts.lists,FUN=function(z){max(as.numeric(z[,1]))}))
    minValue<-min(sapply(ts.lists,FUN=function(z){min(as.numeric(z[,1]))}))


    xlim<-if(is.null(xlim)){c(minValue,maxValue)}else{xlim}

    if(is.null(col)){
      if(alpha>256){
        alpha<-256
      }
  #   cat("alpha=",alpha,"\n")
      col<-rgb(0, 0, 255, alpha=alpha, max = 256)#
    }

  if(plot.new){
    if(is.null(type))type="windows"
    if(is.null(filename)){
      if(type=="pdf")filename="plot.pdf"
    }
    if(is.null(type)|type=="windows" ){
      windows(record=TRUE)
    }else if(type=="pdf"){
      pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=9,width=9)
    }

  on.exit(if(names(dev.cur())!="null device" && type!="windows") dev.off())
  }

#    if(cairo){
#      require(Cairo)
#      CairoPDF(file="plot.pdf",paper="a4",width=8,height=11,version="1.6")
#    }else{
#      pdf(file="plot.pdf",paper="a4",width=8,height=11,version="1.6")
#    }
    plot(as.numeric(ts.lists[[1]][,1]),as.numeric(ts.lists[[1]][,2]),xlim=xlim,ylim=ylim,ylab=ylab,lty="solid",type="l",xlab=xlab,col=col)
    if(length(ts.lists)>1){
      for(i in 2:(length(ts.lists))){
        par(new=TRUE)
#        browser()
        plot(as.numeric(ts.lists[[i]][,1]),as.numeric(ts.lists[[i]][,2]),ylim=ylim,xlim=xlim,ylab="",lty="dashed",type="l",xlab="",col=col,main="",axes=FALSE)
      }
    }

}

plotSSB.retro<-function(reports,sr.lists=NULL,alpha=128/(ifelse(is.null(sr.lists),reports[[2]],length(sr.lists))/100),plot.new=FALSE){
  if(is.null(sr.lists)){
    sr.lists<-lapply(reports[[1]],FUN=read.spawner_recruit)
  }
  sr.lists<-lapply(sr.lists,FUN=function(sr){sr$sr[sr$sr$era=="Main",c("year","spawn_bio")]})
#  SSB<-lapply(sr.list.main,"[[","spawn_bio")
#  year<-sr.list.main[[1]]$year
#  SR.pe<-read.spawner_recruit(report=report.pe)
#  SSB.pe<-SR.pe$sr[SR.pe$sr$era=="Main",]$spawn_bio
#  browser()
  plotMult(ts.lists=sr.lists,alpha=alpha,plot.new=plot.new)
  return(invisible(sr.lists))
}

plotRecruit.retro<-function(reports,sr.lists=NULL,alpha=128/(ifelse(is.null(sr.lists),reports[[2]],length(sr.lists))/100),plot.new=FALSE){
  if(is.null(sr.lists)){
    sr.lists<-lapply(reports[[1]],FUN=read.spawner_recruit)
  }
  sr.lists<-lapply(sr.lists,FUN=function(sr){sr$sr[sr$sr$era=="Main",c("year","pred_recr")]})
#  SSB<-lapply(sr.list.main,"[[","spawn_bio")
#  year<-sr.list.main[[1]]$year
#  SR.pe<-read.spawner_recruit(report=report.pe)
#  SSB.pe<-SR.pe$sr[SR.pe$sr$era=="Main",]$spawn_bio
#  browser()
  plotMult(ts.lists=sr.lists,alpha=alpha,plot.new=plot.new)
  return(invisible(sr.lists))
}

readSSmodel<-function(dir=NULL,datfile=NULL,ctlfile=NULL,starterfile="starter.ss",forecastfile="forecast.ss",verbose=TRUE){
# require(r4ss)
#  if(!updated.r4ss){update_r4ss_files();updated.r4ss<<-TRUE}
#  update_r4ss_files()
  starter<-SS_readstarter(file=starterfile, verbose=verbose)
  if(is.null(datfile)){datfile<-starter$datfile}else{starter$datfile<-datfile}
  if(is.null(ctlfile)){ctlfile<-starter$ctlfile}else{starter$ctlfile<-ctlfile}
  dat<-SS_readdat(file=datfile,verbose=verbose,echoall=FALSE,section=NULL)
#  ctl<-SS_readctl(file=ctlfile)
  ctl<-readLines(ctlfile)
  lastMainRdevYrLn<-grep(x=ctl,pattern="last year of main recr_devs; forecast devs start in following year",value=F)
  lastMainRdevYr<-as.numeric(strsplit(x=ctl[lastMainRdevYrLn],split="[[:blank:]]+")[[1]][1])
#  starter<-SS_readstarter(file=starterfile, verbose=verbose)
#  forecast<-SS_readforecast(file=forecastfile, Nfleets=dat$Nfleet, verbose=verbose)
  forecast<-readLines(forecastfile)
  cat("read of forecast file was done\n")
  return(list(dat=dat,ctl=ctl,starter=starter,forecast=forecast))

}

writeSSmodel<-function(mylist,dir=NULL,datfile=NULL,ctlfile=NULL,
  starterfile="starter.ss",forecastfile="forecast.ss",overwrite=TRUE,verbose=TRUE,retro=FALSE,retro.yr=-1,warn=TRUE){

  if(is.null(datfile)){datfile<-mylist$starter$datfile}else{mylist$starter$datfile<-datfile}
  if(is.null(ctlfile)){ctlfile<-mylist$starter$ctlfile}else{mylist$starter$ctlfile<-ctlfile}

  join<-function(vect){
    return(paste(vect,sep="",collpase=""))
  }
#  browser()
  if(!is.null(dir))dir.create(dir,showWarnings=warn)
#  browser()
  if(retro){
    lastMainRdevYrLn<-grep(x=mylist$ctl,pattern="last year of main recr_devs; forecast devs start in following year",value=FALSE)
    lastMainRdevYr<-as.integer(strsplit(x=mylist$ctl[lastMainRdevYrLn],split="[[:blank:]]+")[[1]][1])+retro.yr
    mylist$ctl[lastMainRdevYrLn]<-paste(lastMainRdevYr,"last year of main recr_devs; forecast devs start in following year",sep=" # ")


    endyr.org<-as.integer(mylist$dat$endyr)
    if(retro.yr<=0){
      endyr.new<-endyr.org+retro.yr
    }else{
      endyr.new<-retro.yr
    }
    mylist$dat$endyr<-endyr.new
  }
  writeLines(mylist$ctl,ifelse(!is.null(dir),paste(dir,ctlfile,sep="/"),ctlfile))
  SS_writedat(datlist=mylist$dat,outfile=ifelse(!is.null(dir),paste(dir,datfile,sep="/"),datfile),overwrite=overwrite,verbose=verbose)

#  SS_writectl(ctllist=mylist$ctl,outfile=ifelse(!is.null(dir),paste(dir,ctlfile,sep=""),ctlfile),overwrite=overwrite,verbose=verbose)
  SS_writestarter(mylist=mylist$starter, dir=dir, file=starterfile,overwrite=overwrite,verbose=verbose)
#  SS_write_forecast(mylist=mylist$forecast,dir=dir,file=forecastfile,nareas=nareas, nfleets=nfleets,overwrite=overwrite,verbose=verbose)
  writeLines(mylist$forecast,ifelse(!is.null(dir),paste(dir,forecastfile,sep="/"),forecastfile))


}

do.ss<-function(sspath,wk.dir=NULL,intern=FALSE,
  cmd.args=" -cbs 100000000 -gbs 100000000 -ams 100000000 ",
  copy.ss=FALSE,mylist=NULL,doHess=TRUE,retro=FALSE,retro.yr=-1,warn=FALSE,nox=FALSE,overwrite=TRUE){

  wd.org<-getwd()
  on.exit(setwd(wd.org))
  if(doHess){
    hess<-""
  }else{
    hess<-" -nohess"
  }
  if(nox){
    nox<-"-nox"
  }else{
    nox<-""
  }
  if(file.exists(sspath)){
    ss.exe<-sspath
  }else{
    stop(paste(sspath,"was not found"))
  }
  if(is.null(wk.dir))wk.dir<-"."

  if(!is.null(mylist)){
#    browser()
    writeSSmodel(mylist=mylist,dir=wk.dir,datfile=NULL,ctlfile=NULL,
    starterfile=mylist$starter$sourcefile,forecastfile="forecast.ss",overwrite=overwrite,
    verbose=TRUE,retro=retro,retro.yr=retro.yr,warn=warn)
  }
#  browser()
  if(copy.ss){
    if(.Platform$OS.type=="unix"){
#      ss.exe<-system(paste("which",ss3.exe),intern=TRUE)
      system(paste("cp ",sspath, wk.dir,collapse=" "))
    }else{
      cmd<-paste("copy ", sspath, wk.dir,collapse=" ")
#      browser()
      shell(cmd,intern=intern,translate=TRUE)
    }
  }
  setwd(wk.dir)
  cmd<-paste(ifelse(copy.ss,paste(".",basename(ss.exe),sep="/"),ss.exe),nox,cmd.args,hess,sep=" ",collapse=" ")
#  browser()
  out<-system(cmd,intern=intern)
  if(intern){
    return(invisible(out))
  }else{

  }
}

do.retro<-function(sspath,copy.ss=TRUE,retro.yrs=0:5,mymodel,intern=FALSE,cmd.args=NULL,nox=FALSE,doHess=TRUE,warn=TRUE,
  parallel=TRUE,cpus=4,mail=TRUE,type="SOCK",runname="",
  to=c("yukiot@fra.affrc.go.jp"),from="yukiot@fra.affrc.go.jp",smtp="mail.affrc.go.jp"){
  require(snowfall) || stop("package snowfall is required")
   start.all<-Sys.time()
#  temp.dir<-tempdir()
  exit.fun<-function(){

    sfStop()
    end.all<-Sys.time()
    eltime<-as.numeric(difftime(end.all,start.all,units="secs"))
    if(mail){
      require(sendmailR) || stop("package sendmailR is required")
      from<-from
      to<-to
      subject <- paste("Finished in ",Sys.info()[4])
      msg <- paste(length(retro.yrs), " calculation(s) on ",Sys.info()[4], " finished at ",format(Sys.time(), "%Y-%m-%d  %H:%M:%OS3"),
        "\nusing",formatC(eltime/60.0),"minutes with",cpus,"core(s)")

      sapply(to,function(x){sendmail(from=from, to=x, subject=subject, msg=msg,control=list(smtpServer=smtp))})
    }
  }
  on.exit(exit.fun())
  sfInit(parallel=parallel,cpus=cpus,type=type,slaveOutfile="./log.txt")
  if(parallel)sfExport(list=c("do.ss","sspath","nox","cmd.args","doHess","intern","mymodel","copy.ss","warn","writeSSmodel","SS_writedat","SS_writestarter"))
#  browser()
  sfLapply(retro.yrs,fun=function(y){do.ss(sspath=sspath,wk.dir=paste("retro-",y,sep=""),nox=nox,
    cmd.args=cmd.args,doHess=doHess,intern=intern,retro=TRUE,retro.yr=-1*as.integer(y),mylist=mymodel,copy.ss=copy.ss,warn=warn)})
  retro.reports<-getReport.sso(repfile=paste("retro-",retro.yrs,"/report.sso",sep=""),oldStyle=FALSE)
  return(retro.reports=retro.reports)
}

######################################################################################
##  Copied from r4ss

SS_readstarter <-  function(file='starter.ss', verbose=TRUE){
  if(verbose) cat("running SS_readstarter\n")
  starter <- readLines(file,warn=FALSE)
  mylist <- list()

  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_starter_file"
  mylist$SSversion <- "SSv3.10b_or_later"

  # get strings for control and data file names
  starter2 <- NULL
  for(i in 1:length(starter)){
      # get only stuff before # marks
      mysplit <- strsplit(starter[i],split="#")[[1]][1]
      if(!is.na(mysplit) && length(mysplit) > 0) starter2 <- c(starter2,mysplit)
  }
  strings <- NULL
  for(i in 1:length(starter2)){
      mysplit <- strsplit(starter2[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      strings <- c(strings,mysplit)
  }
  strings <- strings[is.na(suppressWarnings(as.numeric(strings)))]
  if(length(strings)>2){
      cat("too many strings in starter file?\n")
      print(strings)
      return()
  }else{
      mylist$datfile <- strings[1]
      mylist$ctlfile <- strings[2]
  }

  # get numbers (could be better integrated with function above)
  allnums <- NULL
  for(i in 1:length(starter)){
      # split apart numbers from text in file
      mysplit <- strsplit(starter[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      nums <- suppressWarnings(as.numeric(mysplit))
      if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
      else maxcol <- length(nums)
      if(maxcol > 0){
          nums <- nums[1:maxcol]
          allnums <- c(allnums, nums)
      }
  }

  # go through numerical values and save as elements of a big list
  i <- 1

  mylist$init_values_src <- allnums[i]; i <- i+1
  mylist$run_display_detail <- allnums[i]; i <- i+1
  mylist$detailed_age_structure <- allnums[i]; i <- i+1
  mylist$checkup <- allnums[i]; i <- i+1
  mylist$parmtrace <- allnums[i]; i <- i+1
  mylist$cumreport <- allnums[i]; i <- i+1
  mylist$prior_like <- allnums[i]; i <- i+1
  mylist$soft_bounds <- allnums[i]; i <- i+1
  mylist$N_bootstraps <- allnums[i]; i <- i+1
  mylist$last_estimation_phase <- allnums[i]; i <- i+1
  mylist$MCMCburn <- allnums[i]; i <- i+1
  mylist$MCMCthin <- allnums[i]; i <- i+1
  mylist$jitter_fraction <- allnums[i]; i <- i+1
  mylist$minyr_sdreport <- allnums[i]; i <- i+1
  mylist$maxyr_sdreport <- allnums[i]; i <- i+1
  mylist$N_STD_yrs <- N_STD_yrs <- allnums[i]; i <- i+1
  if(N_STD_yrs>0){
      mylist$STD_yr_vec <- allnums[i:(i+N_STD_yrs-1)]; i <- i+N_STD_yrs
  }
  mylist$converge_criterion <- allnums[i]; i <- i+1
  mylist$retro_yr <- allnums[i]; i <- i+1
  mylist$min_age_summary_bio <- allnums[i]; i <- i+1
  mylist$depl_basis <- allnums[i]; i <- i+1
  mylist$depl_denom_frac <- allnums[i]; i <- i+1
  mylist$SPR_basis <- allnums[i]; i <- i+1
  mylist$F_report_units <- allnums[i]; i <- i+1
  mylist$F_report_basis <- allnums[i]; i <- i+1

  # check final value
  if(allnums[i]==999){
    if(verbose) cat("read of starter file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # all done
  return(mylist)
}

SS_splitdat <-
function(
                        inpath     = 'working_directory' ,
                        outpath    = 'working_directory' ,
                        inname     = 'data.ss_new'       ,
                        outpattern = 'BootData'          ,
                        number     = FALSE                   ,
                        verbose    = TRUE                   ,
                        fillblank  = TRUE                   ,
                        MLE        = TRUE                   ,
                        notes      = ""
                        )
{
  # this is a function to split bootstrap aggregated in the data.ss_new file
  # which is output from Stock Synthesis into individual data files.
  if(inpath=="working_directory") inpath=getwd()
  if(outpath=="working_directory") outpath=getwd()

  infile    <- paste(inpath,inname,sep='/')
  filelines <- readLines(infile)
  if(fillblank)  filelines[filelines==""] <- "#"

  string    <- '#_bootstrap file'
  starts    <- grep(string, filelines)
  ends      <- c(starts[-1]-1,length(filelines)-1)
  MLEstring <- '#_expected values with no error added'
  MLEstart  <- grep(MLEstring, filelines)
  MLEend    <- starts[1]-1

  if(!MLE){
    for(i in 1:length(starts)) {
      outfile <- paste(outpath,'/',outpattern,ifelse(number,i,''),'.ss',sep='')
      outline <- paste('# Data file created from',infile,'to',outfile)
      if(verbose) print(outline,quote=F)
      writeLines(c(outline,filelines[starts[i]:ends[i]]),outfile)
    }
  }else{
    outfile <- paste(outpath,'/',outpattern,'.ss',sep='')
    if(notes!="") notes <- paste("#C",notes) else notes <- NULL
    notes <- c(notes,paste('#C MLE data file created from',infile,'to',outfile))
    if(verbose) print(paste('MLE data file created from',infile,'to',outfile),quote=F)
    writeLines(c(notes,filelines[MLEstart:MLEend]),outfile)
  }
}

SS_writedat <- function(datlist,outfile,overwrite=FALSE,verbose=TRUE){
  # function to write Stock Synthesis data files

  if(verbose) cat("running SS_writedat\n")

  if(datlist$type!="Stock_Synthesis_data_file"){
    stop("input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'")
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
    # simple function to clean up many repeated commands
    value = datlist[names(datlist)==name]
    writeLines(paste(value," #_",name,sep=""),con=zz,)
  }

  # write a header
  writeLines("#C data file created using the SS_writedat function")
  writeLines("#C function is available at http://code.google.com/p/r4ss/source/browse/#svn/branches/input_file_objects")
  writeLines(paste("#C should work with SS version:",datlist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # write the contents
  wl("styr")
  wl("endyr")
  wl("nseas")
  writeLines(paste(paste(datlist$months_per_seas,collapse=" "),"#_months_per_seas"))
  wl("spawn_seas")
  wl("Nfleet")
  wl("Nsurveys")
  wl("N_areas")
  writeLines(paste(paste(datlist$fleetnames,collapse="%"),"#_fleetnames"))
  writeLines(paste(paste(datlist$surveytiming,collapse=" "),"#_surveytiming"))
  writeLines(paste(paste(datlist$areas,collapse=" "),"#_areas"))
  writeLines(paste(paste(datlist$units_of_catch,collapse=" "),"#_units_of_catch"))
  writeLines(paste(paste(datlist$se_log_catch,collapse=" "),"#_se_log_catch"))
  wl("Ngenders")
  wl("Nages")
  writeLines(paste(paste(datlist$init_equil,collapse=" "),"#_init_equil_catch"))
  wl("N_catch")
  if(!is.null(datlist$catch)) printdf(datlist$catch)
  wl("N_cpue")
  if(datlist$N_cpue>0){
    printdf(datlist$CPUEinfo)
    printdf(datlist$CPUE)
  }
  # wl("discard_units")
  wl("N_discard_fleets")
  wl("N_discard")
  if(!is.null(datlist$discard_data)) printdf(datlist$discard_data)
  wl("N_meanbodywt")
  if(!is.null(datlist$meanbodywt)) printdf(datlist$meanbodywt)

  wl("DF_for_meanbodywt")

  # length data
  wl("lbin_method")
  if(datlist$lbin_method==2){
    wl("binwidth")
    wl("minimum_size")
    wl("maximum_size")
  }
  if(datlist$lbin_method==3){
    wl("N_lbinspop")
    writeLines("#_lbin_vector_pop")
    writeLines(paste(datlist$lbin_vector_pop,collapse=" "))
  }
  wl("comp_tail_compression")
  wl("add_to_comp")
  wl("max_combined_lbin")
  wl("N_lbins")
  writeLines("#_lbin_vector")
  writeLines(paste(datlist$lbin_vector,collapse=" "))
  wl("N_lencomp")
  if(!is.null(datlist$lencomp)) printdf(datlist$lencomp)
  wl("N_agebins")
  writeLines("#_agebin_vector")
  writeLines(paste(datlist$agebin_vector,collapse=" "))
  wl("N_ageerror_definitions")
  if(!is.null(datlist$ageerror)) printdf(datlist$ageerror)
  wl("N_agecomp")
  wl("Lbin_method")
  wl("max_combined_age")
  if(!is.null(datlist$agecomp)) printdf(datlist$agecomp)
  wl("N_MeanSize_at_Age_obs")
  #    datlist$MeanSize_at_Age_obs2 <- matrix(datlist$N_MeanSize_at_Age_obs)
  if(!is.null(datlist$MeanSize_at_Age)) printdf(datlist$MeanSize_at_Age_obs)
  wl("N_environ_variables")
  wl("N_environ_obs")
  wl("N_sizefreq_methods")
  wl("do_tags")
  if(datlist$do_tags != 0){
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    if(!is.null(datlist$tag_releases)) printdf(datlist$tag_releases)
    if(!is.null(datlist$tag_recaps)) printdf(datlist$tag_recaps)
  }
  wl("morphcomp_data")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth,max.print=oldmax.print)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}

SS_writestarter <- function(mylist, dir=NULL, file="starter.ss",
                            overwrite=FRUE, verbose=TRUE){
  if(verbose) cat("running SS_writestarter",quote=FRUE)
  if(mylist$type!="Stock_Synthesis_starter_file"){
    cat("input 'mylist' should be a list with $type=='Stock_Synthesis_starter_file'",quote=F)
    return()
  }
  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  outfile <- paste(dir,file,sep="/")
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=T to replace\n"))
    }else{
      cat("overwriting file:",outfile,"\n")
      file.remove(outfile)
    }
  }else{
    cat("writing new file:",outfile,"\n")
  }

  # preliminary setup
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
    # simple function to clean up many repeated commands
    value = mylist[names(mylist)==name]
    writeLines(paste(value," #_",name,sep=""),con=zz)
  }

  writeLines("#C starter file written by R function SS_writestarter")
  writeLines("#C rerun model to get more complete formatting in starter.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # strings for control and data file names
  wl("datfile")
  wl("ctlfile")

  wl("init_values_src")
  wl("run_display_detail")
  wl("detailed_age_structure")
  wl("checkup")
  wl("parmtrace")
  wl("cumreport")
  wl("prior_like")
  wl("soft_bounds")
  wl("N_bootstraps")
  wl("last_estimation_phase")
  wl("MCMCburn")
  wl("MCMCthin")
  wl("jitter_fraction")
  wl("minyr_sdreport")
  wl("maxyr_sdreport")
  wl("N_STD_yrs")
  if(mylist$N_STD_yrs>0){
    wl("STD_yr_vec")
  }
  wl("converge_criterion")
  wl("retro_yr")
  wl("min_age_summary_bio")
  wl("depl_basis")
  wl("depl_denom_frac")
  wl("SPR_basis")
  wl("F_report_units")
  wl("F_report_basis")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}

SS_readdat <- function(file,verbose=TRUE,echoall=FALSE,section=NULL){
  # function to read Stock Synthesis data files

  if(verbose) cat("running SS_readdat\n")
  dat <- readLines(file,warn=FALSE)

  if(!is.null(section)){
    Nsections <- as.numeric(substring(dat[grep("Number_of_datafiles",dat)],24))
    if(!section %in% 1:Nsections) stop("The 'section' input should be within the 'Number_of_datafiles' in a data.ss_new file.\n")
    if(section==1) dat <- dat[grep("#_observed data:",dat):grep("#_expected values with no error added",dat)]
    if(section==2) dat <- dat[grep("#_expected values with no error added",dat):grep("#_bootstrap file: 1",dat)]
    if(section>=3){
      start <- grep(paste("#_bootstrap file:",section-2),dat)
      end <- grep(paste("#_bootstrap file:",section-1),dat)
      if(length(end)==0) end <- length(dat)
      dat <- dat[start:end]
    }
  }

  temp <- strsplit(dat[2]," ")[[1]][1]
  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in 1:length(dat)){
    mysplit <- strsplit(dat[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
    else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  i <- 1
  datlist <- list()

  datlist$sourcefile <- file
  datlist$type <- "Stock_Synthesis_data_file"
  datlist$SSversion <- "SSv3.20"

  if(verbose) cat("SSversion =",datlist$SSversion,"\n")
  # model dimensions
  datlist$styr  <- allnums[i]; i <- i+1
  datlist$endyr <- allnums[i]; i <- i+1
  datlist$nseas <- nseas <- allnums[i]; i <- i+1
  datlist$months_per_seas <- allnums[i:(i+nseas-1)]; i <- i+nseas
  datlist$spawn_seas <- allnums[i]; i <- i+1
  datlist$Nfleet <- Nfleet <- allnums[i]; i <- i+1
  datlist$Nsurveys <- Nsurveys <- allnums[i]; i <- i+1
  Ntypes <- Nfleet+Nsurveys
  datlist$N_areas <- allnums[i]; i <- i+1

  # an attempt at getting the fleet names based on occurance of %-sign
  if(Ntypes>1){
    fleetnames <- dat[grep('%',dat)[1]]
    fleetnames <- strsplit(fleetnames,'%')[[1]]
    if(length(fleetnames)!=Ntypes)
      fleetnames <- c(paste("fishery",1:Nfleet),paste("survey",1:Nsurveys))
  }else{
    fleetnames <- "fleet1"
  }
  datlist$fleetnames <- fleetnames
  datlist$surveytiming <- surveytiming <- allnums[i:(i+Ntypes-1)]; i <- i+Ntypes
  datlist$areas <- areas <- allnums[i:(i+Ntypes-1)]; i <- i+Ntypes

  # fleet info
  fleetinfo1 <- data.frame(rbind(surveytiming,areas))
  names(fleetinfo1) <- fleetnames
  fleetinfo1$input <- c("#_surveytiming","#_areas")
  datlist$fleetinfo1 <- fleetinfo1

  datlist$units_of_catch <- units_of_catch <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet
  datlist$se_log_catch <- se_log_catch <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet
  fleetinfo2 <- data.frame(rbind(units_of_catch,se_log_catch))
  names(fleetinfo2) <- fleetnames[1:Nfleet]
  fleetinfo2$input <- c("#_units_of_catch","#_se_log_catch")
  datlist$fleetinfo2 <- fleetinfo2

  # more dimensions
  datlist$Ngenders <- Ngenders <- allnums[i]; i <- i+1
  datlist$Nages <- Nages <- allnums[i]; i <- i+1
  datlist$init_equil <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet

  # catch
  datlist$N_catch <- N_catch <- allnums[i]; i <- i+1
  if(verbose) cat("N_catch =",N_catch,"\n")
  Nvals <- N_catch*(Nfleet+2)
  catch <- data.frame(matrix(
    allnums[i:(i+Nvals-1)],nrow=N_catch,ncol=(Nfleet+2),byrow=TRUE))
  names(catch) <- c(fleetnames[1:Nfleet],"year","seas")
  datlist$catch <- catch
  i <- i+Nvals
  if(echoall) print(catch)

  # CPUE
  datlist$N_cpue <- N_cpue <- allnums[i]; i <- i+1
  if(verbose) cat("N_cpue =",N_cpue,"\n")
  if(N_cpue > 0){
    CPUEinfo <- data.frame(matrix(allnums[i:(i+Ntypes*3-1)],
                                  nrow=Ntypes,ncol=3,byrow=TRUE))
    i <- i+Ntypes*3
    names(CPUEinfo) <- c("Fleet","Units","Errtype")
    CPUE <- data.frame(matrix(
      allnums[i:(i+N_cpue*5-1)],nrow=N_cpue,ncol=5,byrow=TRUE))
    i <- i+N_cpue*5
    names(CPUE) <- c('year','seas','index','obs','se_log')
  }else{
    CPUEinfo <- NULL
    CPUE <- NULL
  }
  datlist$CPUEinfo <- CPUEinfo
  datlist$CPUE <- CPUE
  if(echoall){
    print(CPUEinfo)
    print(CPUE)
  }

  # discards
  # datlist$discard_units <- discard_units <- allnums[i]; i <- i+1
  datlist$N_discard_fleets <- N_discard_fleets <- allnums[i]; i <- i+1
  if(verbose) cat("N_discard_fleets =",N_discard_fleets,"\n")
  datlist$N_discard <- N_discard <- allnums[i]; i <- i+1
  if(verbose) cat("N_discard =",N_discard,"\n")
  if(N_discard > 0){
    Ncols <- 5
    discard_data <- data.frame(matrix(
      allnums[i:(i+N_discard*Ncols-1)],nrow=N_discard,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard*Ncols
    names(discard_data) <- c('Yr','Seas','Flt','Discard','Std_in')
  }else{
    discard_data <- NULL
  }
  datlist$discard_data <- discard_data

  # meanbodywt
  datlist$N_meanbodywt <- N_meanbodywt <- allnums[i]; i <- i+1
  if(verbose) cat("N_meanbodywt =",N_meanbodywt,"\n")
  if(N_meanbodywt > 0){
    Ncols <- 6
    meanbodywt <- data.frame(matrix(
      allnums[i:(i+N_meanbodywt*Ncols-1)],nrow=N_meanbodywt,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard*Ncols
    names(meanbodywt) <- c('Year','Seas','Type','Partition','Value','CV')
  }else{
    meanbodywt <- NULL
  }
  datlist$meanbodywt <- meanbodywt

  datlist$DF_for_meanbodywt <- allnums[i]
  i <- i+1
  if(echoall) print(datlist$DF_for_meanbodywt)

  # length data
  datlist$lbin_method <- lbin_method <- allnums[i]; i <- i+1
  if(lbin_method==2){
    datlist$binwidth <- allnums[i]; i <- i+1
    datlist$minimum_size <- allnums[i]; i <- i+1
    datlist$maximum_size <- allnums[i]; i <- i+1
  }else{
    datlist$binwidth <- NA
    datlist$minimum_size <- NA
    datlist$maximum_size <- NA
  }
  if(lbin_method==3){
    datlist$N_lbinspop <- N_lbinspop <- allnums[i]; i <- i+1
    datlist$lbin_vector_pop <- allnums[i:(i+N_lbinspop-1)]; i <- i+N_lbinspop
  }else{
    datlist$N_lbinspop <- NA
    datlist$lbin_vector_pop <- NA
  }
  if(echoall){
    cat("N_lbinspop =",N_lbinspop,"\nlbin_vector_pop:\n")
    print(datlist$lbin_vector_pop)
  }
  datlist$comp_tail_compression <- allnums[i]; i <- i+1
  datlist$add_to_comp <- allnums[i]; i <- i+1
  datlist$max_combined_age <- allnums[i]; i <- i+1
  datlist$N_lbins <- N_lbins <- allnums[i]; i <- i+1
  datlist$lbin_vector <- lbin_vector <- allnums[i:(i+N_lbins-1)]; i <- i+N_lbins
  if(echoall) print(lbin_vector)

  datlist$N_lencomp <- N_lencomp <- allnums[i]; i <- i+1

  # if(verbose) cat(datlist[-15:0 + length(datlist)])
  if(verbose) cat("N_lencomp =",N_lencomp,"\n")

  if(N_lencomp > 0){
    Ncols <- N_lbins*Ngenders+6
    lencomp <- data.frame(matrix(
                                 allnums[i:(i+N_lencomp*Ncols-1)],nrow=N_lencomp,ncol=Ncols,byrow=TRUE))
    i <- i+N_lencomp*Ncols
    names(lencomp) <- c("Yr","Seas","FltSvy","Gender","Part","Nsamp",
                        if(Ngenders==1){paste("l",lbin_vector,sep="")}else{NULL},
                        if(Ngenders>1){ c(paste("f",lbin_vector,sep=""),paste("m",lbin_vector,sep="")) }else{ NULL } )
  }else{
    lencomp <- NULL
  }
  datlist$lencomp <- lencomp

  # age data
  datlist$N_agebins <- N_agebins <- allnums[i]; i <- i+1
  if(verbose) cat("N_agebins =",N_agebins,"\n")
  if(N_agebins > 0){
    agebin_vector <- allnums[i:(i+N_agebins-1)]; i <- i+N_agebins
  }else{
    agebin_vector <- NULL
  }
  datlist$agebin_vector <- agebin_vector

  datlist$N_ageerror_definitions <- N_ageerror_definitions <- allnums[i]; i <- i+1
  if(N_ageerror_definitions > 0){
    Ncols <- Nages+1
    ageerror <- data.frame(matrix(
      allnums[i:(i+2*N_ageerror_definitions*Ncols-1)],
      nrow=2*N_ageerror_definitions,ncol=Ncols,byrow=TRUE))
    i <- i+2*N_ageerror_definitions*Ncols
    names(ageerror) <- paste("age",0:Nages,sep="")
  }else{
    ageerror <- NULL
  }
  datlist$ageerror <- ageerror

  datlist$N_agecomp <- N_agecomp <- allnums[i]; i <- i+1
  if(verbose) cat("N_agecomp =",N_agecomp,"\n")

  datlist$Lbin_method <- allnums[i]; i <- i+1
  datlist$max_combined_lbin <- allnums[i]; i <- i+1

  if(N_agecomp > 0){
    if(N_agebins==0) stop("N_agecomp =",N_agecomp," but N_agebins = 0")
    Ncols <- N_agebins*Ngenders+9
    agecomp <- data.frame(matrix(
      allnums[i:(i+N_agecomp*Ncols-1)],nrow=N_agecomp,ncol=Ncols,byrow=TRUE))
    i <- i+N_agecomp*Ncols
    names(agecomp) <- c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi","Nsamp",
                        if(Ngenders==1){paste("a",agebin_vector,sep="")}else{NULL},
                        if(Ngenders>1){ c(paste("f",agebin_vector,sep=""),paste("m",agebin_vector,sep="")) }else{ NULL } )
  }else{
    agecomp <- NULL
  }
  datlist$agecomp <- agecomp

  # MeanSize_at_Age
  datlist$N_MeanSize_at_Age_obs <- N_MeanSize_at_Age_obs <- allnums[i]; i <- i+1
  if(verbose) cat("N_MeanSize_at_Age_obs =",N_MeanSize_at_Age_obs,"\n")
  if(N_MeanSize_at_Age_obs > 0){
    Ncols <- 2*N_agebins*Ngenders + 7
    MeanSize_at_Age_obs <- data.frame(matrix(
      allnums[i:(i+N_MeanSize_at_Age_obs*Ncols-1)],nrow=N_MeanSize_at_Age_obs,ncol=Ncols,byrow=TRUE))
    i <- i+N_MeanSize_at_Age_obs*Ncols
    names(MeanSize_at_Age_obs) <- c('Yr','Seas','Fleet','Gender','Part','AgeErr','Ignore',
                                    if(Ngenders==1){paste("a",agebin_vector,sep="")}else{NULL},
                                    if(Ngenders>1){ c(paste("f",agebin_vector,sep=""),paste("m",agebin_vector,sep="")) }else{ NULL },
                                    if(Ngenders==1){paste("N_a",agebin_vector,sep="")}else{NULL},
                                    if(Ngenders>1){ c(paste("N_f",agebin_vector,sep=""),paste("N_m",agebin_vector,sep="")) }else{ NULL } )
  }else{
    MeanSize_at_Age_obs <- NULL
  }
  datlist$MeanSize_at_Age_obs <- MeanSize_at_Age_obs

  # other stuff
  datlist$N_environ_variables <- N_environ_variables <- allnums[i]; i <- i+1
  datlist$N_environ_obs <- N_environ_obs <- allnums[i]; i <- i+1
  datlist$N_sizefreq_methods <- N_sizefreq_methods <- allnums[i]; i <- i+1
  datlist$do_tags <- do_tags <- allnums[i]; i <- i+1
  if(do_tags != 0){
    datlist$N_tag_groups <- N_tag_groups <- allnums[i]; i <- i+1
    datlist$N_recap_events <- N_recap_events <- allnums[i]; i <- i+1
    datlist$mixing_latency_period <- mixing_latency_period <- allnums[i]; i <- i+1
    datlist$max_periods <- max_periods <- allnums[i]; i <- i+1

    # read tag release data
    if(N_tag_groups > 0){
      Ncols <- 8
      tag_releases <- data.frame(matrix(allnums[i:(i+N_tag_groups*Ncols-1)],nrow=N_tag_groups,ncol=Ncols,byrow=TRUE))
      i <- i+N_tag_groups*Ncols
      names(tag_releases) <- c('TG', 'Area', 'Yr', 'Season', 'tfill', 'Gender', 'Age', 'Nrelease')
    }else{
      tag_releases <- NULL
    }
    datlist$tag_releases <- tag_releases

    # read tag recapture data
    if(N_recap_events > 0){
      Ncols <- 5
      tag_recaps <- data.frame(matrix(allnums[i:(i+N_recap_events*Ncols-1)],nrow=N_recap_events,ncol=Ncols,byrow=TRUE))
      i <- i+N_recap_events*Ncols
      names(tag_recaps) <- c('TG', 'Yr', 'Season', 'Fleet', 'Nrecap')
    }else{
      tag_recaps <- NULL
    }
    datlist$tag_recaps <- tag_recaps
  }

  datlist$morphcomp_data <- morphcomp_data <- allnums[i]; i <- i+1

  if(allnums[i]==999){
    if(verbose) cat("read of data file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # return the result
  return(datlist)
}

read.covar<-function(file="covar.sso"){
  cov<-read.table(header=TRUE,skip=3,file=file)
  hea
}

plotChiSq<-function(repfile="Report.sso",report=NULL,compReportFile="compReport.sso",plotType="ChiSq",
  fleets=NULL,type="h",age=FALSE,Seasons=NULL,layout=c(2,3),plotUnused=FALSE,graphDev="windows",ggplotlike=FALSE,
  filename=NULL,plot.new=TRUE){
  require(lattice)||stop("package lattice is required")
  if(is.null(report))report<-getReport(repfile=repfile)
  comps<-getAgecomp(report=report,compReportFile=compReportFile)
  comps1<-comps[[1]]
  if(!plotUnused)comps1<-comps1[comps1$N>0,]
  comps1<-comps1[comps1$Kind==ifelse(age,"AGE","LEN"),]
  if(nrow(comps1)==0)stop(paste(ifelse(age,"AGE","LEN")," data is not contained in compReport"))
  if(plotType=="ChiSq"){
    comps1$quant<-comps1$Pearson^2
    FUN<-"sum"
    ylab<-"Chi-Squared"
  }else if(plotType=="Like"){
    comps1$quant<-comps1$Like
    FUN<-"sum"
    ylab<-"-logLikelihood"
  }else if(plotType=="effN"){
    comps1$quant<-comps1$effN
    FUN<-"mean"
    ylab<-"Effective sample size"
  }else{stop("no appropriate plotType")}
  if(!is.null(fleets)){
    comps1<-comps1[comps1$Fleet %in% fleets,]
  }
 # browser()
  if(!is.null(Seasons)){
    comps1<-comps1[comps1$Seas %in% Seasons,]
  }
  types<-c(type,"g")
  require(lattice) || stop("package lattice is required")
  require(latticeExtra) || stop("package latticeExtra is required")
  if(is.null(graphDev))graphDev="windows"
  if(is.null(filename)){
    if(graphDev=="pdf")filename="plot.pdf"
  }

#    if(type=="pdf")pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=9,width=9)
  if(graphDev=="pdf")pdf(file=filename,paper="a4",height=9,width=9)
  if(graphDev=="windows")windows(record=TRUE)

  opar <- trellis.par.get()
  if(ggplotlike){

    trellis.par.set(ggplot2like(n = 4, h.start = 180))
    oopt <- lattice.options(ggplot2like.opts())
  }
  exit.fn<-function(){
    if(names(dev.cur())!="null device" && type!="windows")dev.off()
    trellis.par.set(opar)
    if(ggplotlike)lattice.options(oopt)
  }
  on.exit(exit.fn)

  ChiSq<-aggregate(quant~Yr+Fleet,data=comps1,FUN=FUN)
  ChiSq$Season<-((ChiSq$Yr-floor(ChiSq$Yr))+0.25)*4
  ChiSq$FL<-ifelse(ChiSq$Fleet<10,paste("FL0",ChiSq$Fleet,sep=""),paste("FL",ChiSq$Fleet,sep=""))
  xy<-xyplot(data=ChiSq,quant~Yr|as.factor(FL),scale=list(y="free"),
    as.table=TRUE,type=types,group=Season,auto.key=list(columns=4),lty=1:4,
    xlab="Year",ylab=ylab,layout=layout)
  if(plot)print(xy)

  xy2<-densityplot(data=aa$ChiSq,~quant|factor(FL),as.table=TRUE,scales=list(x="free"),type=c("count"),auto.key=TRUE) #,xlim=c(0,210))

  if(plot)print(xy2)

  return(invisible(list(xy=xy,ChiSq=ChiSq)))
}


plotFAA.y3d<-function(repfile="Report.sso",report=NULL,faa=NULL,plot=TRUE,ylab="Age",
  graphDev="windows",filename=NULL,xlab="Year",main="Yearly Fishing mortality",fleets=NULL){
  require(rgl)||stop("package rgl is required")
  if(is.null(faa)){
    if(is.null(report))report<-getReport(repfile=repfile,interactive=TRUE)
  }
  faa<-getFAA.y(report=report,fleets=fleets)
#  browser()
  persp3d(y=as.numeric(colnames(faa$faa_y2)),x=as.numeric(rownames(faa$faa_y2)),faa$faa_y2,xlab=xlab,ylab=ylab,zlab="Annual F",col="lightblue")


}

plotFAA.y<-function(repfile="Report.sso",report=NULL,faa=NULL,plot=TRUE,ylab="Age",
  graphDev="windows",filename=NULL,xlab="Year",main="Yearly Fishing mortality",fleets=NULL,
  ggplotlike=TRUE){
  require(lattice)||stop("lattice is required")
  if(is.null(faa)){
    if(is.null(report))report<-getReport(repfile=repfile,interactive=TRUE)
  }
  faa<-getFAA.y(report=report,fleets=fleets)

  if(is.null(graphDev))graphDev="windows"
  if(is.null(filename)){
    if(graphDev=="pdf")filename="plot.pdf"
  }

#    if(type=="pdf")pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=9,width=9)
  if(graphDev=="pdf")pdf(file=filename,paper="a4",height=9,width=9)
  if(graphDev=="windows")windows(record=TRUE)
  opar <- trellis.par.get()
  if(ggplotlike){

    trellis.par.set(ggplot2like(n = 4, h.start = 180))
    oopt <- lattice.options()
    lattice.options(ggplot2like.opts())
  }
  exit.fn<-function(){
    if(names(dev.cur())!="null device" && type!="windows")dev.off()
    trellis.par.set(opar)
    if(ggplotlike)lattice.options(oopt)
  }
  on.exit(exit.fn)


  xy<-levelplot(faa$faa_y2,xlab=xlab,ylab=ylab,prety=TRUE,main=main,
    scales=list(y=list(rot=0),x=list(rot=90)),
    drop.unused.levels=TRUE)
  if(plot)print(xy)
  return(xy)
}

plotNAA.y<-function(naa=NULL,repfile="Report.sso",report=NULL,plot=TRUE,
  graphDev="windows",filename=NULL,main="N(1000fish) at the beggining of the year",
  log=TRUE,zlab=ifelse(log,"logN","N"),lwd=0.5){
  require(lattice)||stop("lattice is required")
  if(is.null(naa)){
    if(is.null(report))report<-getReport(repfile=repfile,interactive=TRUE)
  }
  naa<-getNAA.y(report=report)
#  browser()

  if(is.null(graphDev))graphDev="windows"
  if(is.null(filename)){
    if(graphDev=="pdf")filename="plot.pdf"
  }

#    if(type=="pdf")pdf(file=paste(filename,".pdf",sep=""),paper="a4",height=9,width=9)
  if(graphDev=="pdf")pdf(file=filename,paper="a4",height=9,width=9)
  if(graphDev=="windows")windows(record=TRUE)
  opar <- trellis.par.get()
  if(ggplotlike){

    trellis.par.set(ggplot2like(n = 4, h.start = 180))
     oopt <- lattice.options()
    lattice.options(ggplot2like.opts())
  }
  exit.fn<-function(){
    if(names(dev.cur())!="null device" && type!="windows")dev.off()
    trellis.par.set(opar)
    if(ggplotlike)lattice.options(oopt)
  }
  on.exit(exit.fn)


  xy<-wireframe(as.matrix(naa$naa_y),xlab="Year",ylab="Age",main=main,
    scales=list(y=list(rot=0),x=list(rot=90),z=list(log=log)),lwd=lwd,zlab=zlab)
  if(plot)print(xy)
  return(xy)
}

rich.colors.short <- function(n,alpha=1){
  # a subset of rich.colors by Arni Magnusson from the gregmisc package
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
}


make.projection<-function(fore.years=5,start.year=NA,nrandsim=5,aggregateF=NA,repfile="Report.sso",report=NULL,seed=6997,year=NULL,geomean=TRUE){
  if(is.null(report))report<-getReport.sso(repfile=repfile)
  if(is.na(aggregateF))aggregateF<-calc.aggregateF(repfile=repfile,year=year,geomean=geomean)
  faa.org<-calcFAA(report=report,is.plot=FALSE)
  info<-faa.org$info
  if(is.na(start.year))start.year<-as.numeric(rev(info$Yr)[1])+1
  require(plyr)||stop("package plyr is required")
  dim.fore.array<-c(Seas=length(info$Seas),Yr=length(info$Yr)+fore.years,sapply(dimnames(faa$naa.array)[-1],length),nrandsim=nrandsim)
  dimnames.fore.array<-list(Seas=info$Seas,Yr=c(info$Yr,paste(start.year-1+1:fore.years)))
  dimnames.fore.array[2+1:length(sapply(dimnames(faa.org$naa.array)[-1],length))]<-dimnames(faa.org$naa.array)[-1]
  names(dimnames.fore.array)[2+1:length(sapply(dimnames(faa.org$naa.array)[-1],length))]<-names(dimnames(faa.org$naa.array))[-1]
######################################
  dimnames.fore.array[length(dim.fore.array)]<-list(randsim=paste(1:nrandsim))
  names(dimnames.fore.array)[length(dim.fore.array)]<-"randsim"
######################################
## N@A
  naa.array<-faa.org$naa.array
  dim(naa.array)<-c(Seas=length(info$Seas),Yr=length(info$Yr),dim(naa.array)[-1])
  naa.fore.array<-array(0,dim=c(Seas=length(info$Seas),Yr=fore.years,sapply(dimnames(faa.org$naa.array)[-1],length)))
  require(abind)||stop("package abind is required")
#  browser()
# 季節、（将来予測の分も含めた）年、年齢(,morph)の配列を作成
  naa.array<-abind(naa.array,naa.fore.array,along=2)
  dim.fore.array<-sapply(dimnames.fore.array,length,simplify=TRUE)
#  naa.forecast<-aaply(.data=array(naa.array,dim=dim.fore.array,dimnames=dimnames.fore.array),.margins=c(length(dim.fore.array)),.fun=function(x){return(x)},.drop=FALSE)
#  browser()
# 季節、（将来予測の分も含めた）年、年齢(,morph),randsimの配列に拡大して、randsimをキーにlist-arrayに変換
  naa.forecast<-alply(.data=array(naa.array,dim=dim.fore.array,dimnames=dimnames.fore.array),.margins=c(length(dim.fore.array)),.fun=function(x){return(x)})
#  browser()
#######################################
## F@A
##
  faa<-faa.org$faa
  dim(faa)<-c(Seas=length(info$Seas),Yr=length(info$Yr),dim(faa.array)[-1])
  faa.fore.array<-array(0,dim=c(Seas=length(info$Seas),Yr=fore.years,sapply(dimnames(faa.org$faa)[-1],length)))
# 季節、（将来予測の分も含めた）年、年齢(,morph)の配列を作成
  faa<-abind(faa,faa.fore.array,along=2)
  dim.fore.array<-sapply(dimnames.fore.array,length,simplify=TRUE)
#  naa.forecast<-aaply(.data=array(naa.array,dim=dim.fore.array,dimnames=dimnames.fore.array),.margins=c(length(dim.fore.array)),.fun=function(x){return(x)},.drop=FALSE)
#  browser()
# 季節、（将来予測の分も含めた）年、年齢(,morph),randsimの配列に拡大して、randsimをキーにlist-arrayに変換
  faa.forecast<-alply(.data=array(naa.array,dim=dim.fore.array,dimnames=dimnames.fore.array),.margins=c(length(dim.fore.array)),.fun=function(x){return(x)})
#######################################
## C@A
##



  nma.tmp<-faa.org$nma[,c(1:7,10)]
  nma.tmp$M<-nma.tmp$M/length(unique(faa.org$nma$Seas))
  require(plyr)|| stop("package plyr is required")
  Mmat<-daply(.data=nma.tmp,.variables=c("Seas","Age","Morph"),.fun=function(x){return(x$M)})
#      cat("L773\n");browser()
#      ageNames<-dimnames(Mmat)[[2]]
  Mmat<-rep(1,length(sort(unique(info$Yr))))%o%Mmat  ## To-do need to check
  Mmat<-if(length(dim(Mmat))==3){aperm(Mmat,c(2,1,3))}else{aperm(Mmat,c(2,1,3,4))}

  browser()
#  dim(Mmat)<-dim(faa.org$faa)
#  dimnames(Mmat)<-dimnames(faa.org$faa)

#  dim(Mmat)<-c(dim(Mmat)[1]*dim(Mmat)[2],dim(Mmat)[3],dim(Mmat)[4])

  browser()

  require(rlecuyer)||stop("package rlecuyer is required")
  require(foreach) ||stop("package is required")
#  names<-paste("rngstream",1:nrandsim,sep="")
#  .lec.CreateStream(names)
#  .lec.WriteStateFull(names)
  library(foreach)
  registerDoSEQ()
  aa<-foreach(x=naa.forecast,.inorder,.export=c("Mmat","info")) %do%
    x
  browser()


  project1<-function(naa1,faa.org,start.year,fore.years,Seas,info){
    if(start.year>as.numeric(rev(info$Yr)[1])){
    # 少なくとも1 season分、計算して、計算最終年の１ season当初の年齢別尾数を計算する
      faa<-faa.org$faa
      dim(faa)<-c(Seas=length(info$Seas),Yr=length(info$Yr),sapply(dimnames(faa$naa.array)[-1],length))
      ndim<-length(dim(naa1))
      if(ndim==3){
        temp<-naa1[length(Seas),length(info$Yr),]*exp(-faa[length(Seas),length(info$Yr),]-Mmat[length(Seas),length(info$Yr),])
        naa1[1,length(info$Yr)+1,]<-c(0,rev(rev(temp)[-1]))
        naa1[1,length(info$Yr)+1,length(Ages)]<-naa1[1,length(info$Yr)+1,length(Ages)]+rev(temp[1])
      }else if( ndim==4){
        temp<-naa1[length(Seas),length(info$Yr),,]*exp(-faa[length(Seas),length(info$Yr),,]-Mmat[length(Seas),length(info$Yr),,])
        naa1[1,length(info$Yr)+1,,]<-aaply(.data=temp,.margin=2,.fun=function(x){
          temp.next<-c(0,rev(rev(x)[-1]))
          temp.next[length(temp.next)]<-temp.next[length(temp.next)]+rev(x)[1]
          return(temp.next)
        })
      }
    }

    for(y in 1:fore.years){
      # To-Do calculate SSB
       SSB<-calcSSB()
      # To-Do calculate Recruitment
      # To-Do Project
    }

  }
  foreach(i=1:nrandsim)%do%
    project1(i)

  browser()

}
