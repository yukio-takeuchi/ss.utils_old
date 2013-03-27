getHeaderL<-function(header.char,report=NULL,startL=NULL,endL=NULL,first=TRUE,oldStyle=TRUE){
### header.char : header of a block of report.sso (string)
### report : contents of a report.sso file, can be a list of contents of multiple report.sso files
### first  : default=TRUE, if TRUE, line number of the first line containing header(kew word) will be reported
###                        if FALSE, all line numbers of the lines containing header(key word) will be reported
### oldStyle : kept for compatibility
  if(is.null(report))stop("report missing")
  if(is.oldStyle(report)){
    if(is.report.trad(report)){
      if(is.null(startL))startL=1
      if(is.null(endL))endL=length(report)
    }else{
      if(is.null(startL))startL=rep(1,length(report[[1]]))
      if(is.null(endL))endL=sapply(report[[1]],FUN=function(z){length(z$report)})
    }
  }

  tmp.fn2<-function(lno){
    if(first){
      headerL<-grep(value=FALSE,pattern=header.char,x=report[[1]][[lno]][startL[lno]:endL[lno]])[1]
    }else{
      headerL<-grep(value=FALSE,pattern=header.char,x=report[[1]][[lno]][startL[lno]:endL[lno]])
    }
    return(headerL)
  }

  if(is.oldStyle(report)){
    if(is.report.trad(report)){
      if(first){
        headerL<-grep(value=FALSE,pattern=header.char,x=report[startL:endL])[1]
      }else{
        headerL<-grep(value=FALSE,pattern=header.char,x=report[startL:endL])
      }
    }else{
#      if(first){
#        header<-mapply(FUN=function(x,y,z){
#                          grep(value=FALSE,pattern=header,x=x[y:z])[1]
#        },report[[1]],startL,endL)
#      }else{
#        header<-mapply(FUN=function(x,y,z){
#                          grep(value=FALSE,pattern=header,x=x[y:z])
#        },report[[1]],startL,endL)
#      }
      headerL<-mapply(FUN=function(x,y,z){
                      grep(value=FALSE,pattern=header.char,x=x[y:z])
      },report[[1]],startL,endL)
      if(first)headerL<-sapply(headerL,"[",1)
    }
#    return(list(header=header,n=length(header)))
    return(headerL)
  }else{
###############################################
      tmp.fn<-function(chunk){
        if(first){
#          browser()
          headerL<-grep(value=FALSE,pattern=header.char,x=chunk$report)[1]
        }else{
           headerL<-grep(value=FALSE,pattern=header.char,x=chunk$report)
        }
        return(headerL)
      }
###################################################

#   header<-lapply(report[[1]],tmp.fn)
#    browser()
#    headerL<-lapply(1:length(report[[1]]),tmp.fn)
    headerL<-lapply(report[[1]],tmp.fn)
    if(first)headerL<-sapply(headerL,"[",1)
#    if(length(header)>1){
#      browser()
      return(list(headerL=headerL,n=length(headerL)))
#    }else{
#      return(header[[1]])
#    }
  }
}
