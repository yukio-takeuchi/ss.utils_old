##########################################################################
##
##  SSの出力を項目毎に読み込むための函数
##
##  2011/11/26 同様の機能を持つreadDatとgetComponentをまとめた
##
##  2012/11/21 getComponentの標準のオプションを変更した
##
##  2013/01/27
##

readDat<-function(header.char,report=NULL,blankLines=NULL,skip=0,header=FALSE,footer.char=NULL,col.names=NULL,
                        skip.col.names=0,checkEndRec=FALSE,colClasses=NULL,as.numeric.as.possible=FALSE,as.numeric.col=NULL){
  if(is.null(report))stop("report is missing in readDat")
#  cat("HERE13 in readDat\n")
#  cat("as.numeric.as.possible=",as.numeric.as.possible,"\n")
#  cat("is.oldStyle(report)=",is.oldStyle(report),"\n")
  headerL<-getHeaderL(header.char=header.char,report=report)
#  browser()
  if(is.oldStyle(report)){
    res<-readDat.trad(header.char=header.char,report=report,blankLines=blankLines,skip=skip,header=header,footer.char=footer.char,
    col.names=col.names,skip.col.names=skip.col.names,checkEndRec=checkEndRec,colClasses=colClasses,as.numeric.as.possible=as.numeric.as.possible,
    as.numeric.col=as.numeric.col,headerL=headerL)
    return(res)
  }else{
#    browser()
    if(is.null(footer.char)){
      EL<-mapply(FUN=function(x,y){
        #cat("HERE\n");browser();
        res<-componentEndL(x,y$blankLines)
        #;cat("res=",res,"\n");
        return(res)},headerL[[1]],report[[1]])
    }else{
      EL<-mapply(FUN=function(x,y){
            x+grep(x=y[x:length(y)],value=FALSE,pattern=footer.char)[1]-1},headerL,lapply(report[[1]],"[[",1)
            )
    }
#    cat("EL=",EL,"\n")
#    browser()
#    require(plyr)
#    cat("HERE0\n");browser()
    if(0){
    component<-lapply(1:length(EL),FUN=function(i){


            comp.tmp<-report[[i]][[1]][[1]][(headerL[[1]][i]+1):EL[i]]
#            cat("HERE1\n");browser()
            if(header||is.null(col.names))col.names<-name.label<-unlist(strsplit(comp.tmp[skip.col.names+1],split="[[:blank:]]+"))
            comp.tmp<-read.table.texts(texts=comp.tmp,skip=skip+1,header=FALSE)

            #if(header)names(comp.tmp)<-name.label
            if(header)names(comp.tmp)<-col.names
            if(checkEndRec){
              tmp<-(comp.tmp==-1)
              tmp<-apply(tmp,1,FUN=function(xy){sum(xy,na.rm=TRUE)})
              tmp<-(tmp!=(dim(comp.tmp)[2]-2))
              comp.tmp<-comp.tmp[tmp,]
            }
              if(as.numeric.as.possible){
                comp.tmp<-
                  apply(comp.tmp,c(1,2),FUN=function(xz){
              if(length(grep(x=x,pattern="[[:digit:]]"))>0){return(as.numeric(xz))}else{return(xz)}})
                comp.tmp<-as.data.frame(compoent)
              }
              if(!is.null(as.numeric.col)){
                comp.tmp[,as.numeric.col]<-apply(comp.tmp[,as.numeric.col],c(1,2),as.numeric)
              }
#              cat("HERE3\n");browser()
              return(comp.tmp)
            })
    }
#
#
#

#    })
#    if(0){
    component<-mapply(FUN=function(x,y,z){
                        comp.tmp<-x[(y+1):z]
                        cat("HERE\n");browser()
                        if(header||is.null(col.names))col.names<-name.label<-unlist(strsplit(comp.tmp[skip.col.names+1],split="[[:blank:]]+"))
                        comp.tmp<-read.table.texts(texts=comp.tmp,skip=skip+1,header=FALSE)

                        #if(header)names(comp.tmp)<-name.label
                        if(header)names(comp.tmp)<-col.names
                        if(checkEndRec){
                          tmp<-(comp.tmp==-1)
                          tmp<-apply(tmp,1,FUN=function(x){sum(x,na.rm=TRUE)})
                          tmp<-(tmp!=(dim(comp.tmp)[2]-2))
                          comp.tmp<-comp.tmp[tmp,]
                        }
                          if(as.numeric.as.possible){
#                              cat("HERE90 in readDat.r\n")
#                              browser()
                            comp.tmp<-
                              apply(comp.tmp,c(1,2),FUN=function(x){
                          if(length(grep(x=x,pattern="[[:digit:]]"))>0){return(as.numeric(x))}else{return(x)}})
                            comp.tmp<-as.data.frame(compoent)
                          }
                          if(!is.null(as.numeric.col)){
                            comp.tmp[,as.numeric.col]<-apply(comp.tmp[,as.numeric.col],c(1,2),as.numeric)
                          }
                          cat("HERE3\n");browser()
                          return(comp.tmp)
                        },lapply(report[[1]],"[[",1),headerL[[1]],EL,SIMPLIFY=FALSE)
#    }
    cat("HERE4\n");browser()
    return(component)
  }

}

getComponent<-function(header.char,report=NULL,blankLines=NULL,skip=0,header=FALSE,footer.char=NULL,col.names=NULL,
                        skip.col.names=0,checkEndRec=FALSE,colClasses=NULL,as.numeric.as.possible=TRUE,as.numeric.col=NULL){
  if(is.null(report))stop("report is NULL at getCompoent")
  results<-readDat(header.char=header.char,
           report=report,
           blankLines=blankLines,
           skip=skip,
           header=header,
           footer.char=footer.char,
           col.names=col.names,
                     skip.col.names=skip.col.names,
                     checkEndRec=checkEndRec,
                     colClasses=colClasses,
                     as.numeric.as.possible=as.numeric.as.possible,
                     as.numeric.col=as.numeric.col)
  return(results)
}

readDat.trad<-function(header.char,report=NULL,blankLines=NULL,skip=0,header=FALSE,footer.char=NULL,
    col.names=NULL,skip.col.names=0,checkEndRec=FALSE,colClasses=NULL,as.numeric.as.possible=FALSE,
    as.numeric.col=NULL,headerL=NULL){
    cat("readDat.trad is called in line 132 of readDat.r\n")
      if(is.null(headerL))headerL<-getHeaderL(header.char=header.char,report=report)
    if(is.null(blankLines)){
      #blankLines<-grep(value=FALSE,pattern="^$",x=report)
      blankLines<-getBlankLines(report)
    }
    cat("HERE140 in readDat.r\n")
    print(is.report.trad(report))
    if(is.report.trad(report)){
      cat("143footer.char\n")
      print(is.null(footer.char))
      if(is.null(footer.char)){
        EL<-componentEndL(headerL=headerL,blankLines=blankLines)
      }else{
        EL<-headerL+grep(x=report[headerL:length(report)],value=FALSE,pattern=footer.char)[1]-2
        cat("headerL=")
        print(headerL)
        cat("\nHERE149\nEL=")
        print(EL)
        cat("\nreport[EL]=")
        print(report[EL])
    #    browser()
      }
#  browser()
      component<-report[(headerL+1):EL]
      if(header){
        col.names<-name.label<-unlist(strsplit(component[skip.col.names+1],split="[[:blank:]]+"))
      }
      if(is.null(col.names)){
        if(skip.col.names>=0){
#          browser()
          col.names<-paste(unlist(strsplit(component[1+skip.col.names],split="[[:blank:]]+")))
        }else{ ###
          ncol<-max(sapply(report[(headerL+1):EL],FUN=function(line){
                                                    length(strsplit(line,split="[[:blank:]]+")[[1]])
                                                    },
                simplify=TRUE))
          col.names<-paste("V",1:ncol,sep="")
        }
      }
     cat("HERE164 in readDat.r\n") 


  # con.component<-textConnection(component)
# component<-read.table(con.component,fill=TRUE,as.is=TRUE,header=FALSE,skip=1)
# close(con.component)
      component<-read.table.texts(texts=component,skip=skip+1,header=FALSE,colClasses=colClasses,col.names=col.names)
      if(header){
#        names(component)<-name.label
        names(component)<-col.names
      }


   #   if(as.numeric.as.possible)browser()
      ### it is not certain this block is working or not
      if(checkEndRec){
        tmp<-(component==-1)
        tmp<-apply(tmp,1,FUN=function(x){sum(x,na.rm=TRUE)})
        tmp<-(tmp!=(dim(component)[2]-2))
        component<-component[tmp,]
      }
      if(as.numeric.as.possible){
        cat("HERE184 in readDat.r")
        print(str(component))
        cat("\n")
        pattern="^[-+]?([0-9]+(.[0-9]*)?|.[0-9]+)([eE][-+]?[0-9]+)?$" ## 
        # pattern="^[-+]?([0-9]+(.[0-9]*)?|.[0-9]+)?$"
        # pattern="^[-+]?[0-9]+$"
        print(sapply(component,FUN=function(y){length(grep(x=y,pattern=pattern))}))
        cat("\n")
#        component<-
#          apply(component,c(1,2),FUN=function(x){

#            if(sum(grep(x=x,pattern=pattern))>0){return(as.numeric(x))}else{return(x)}})
        cat("HERE197")
#        browser()      
        component<-lapply(component,FUN=function(y){if(length(grep(x=y,pattern))==length(y)){as.numeric(y)}else{y}})

        component<-as.data.frame(component)
        print(str(component))
 #  browser()
      }
      if(!is.null(as.numeric.col)){
        component[,as.numeric.col]<-apply(component[,as.numeric.col],c(1,2),as.numeric)
      }
      return(component)
    }else{
      if(is.null(footer.char)){ #{temp<-"^$"}else{temp<-footer.char}
        EL<-mapply(FUN=function(x,y){componentEndL(x,y)},headerL,blankLines)
      }else{
        EL<-mapply(FUN=function(x,y){
              x+grep(x=y[x:length(y)],value=FALSE,pattern=footer.char)[1]-1
        },headerL,report[[1]])
      }
      component<-mapply(FUN=function(x,y,z){
                          comp.tmp<-x[(y+1):z]
                          if(header)col.names<-name.label<-unlist(strsplit(comp.tmp[skip.col.names+1],split="[[:blank:]]+"))
                          comp.tmp<-read.table.texts(texts=comp.tmp,skip=skip+1,header=FALSE,
                            colClasses=colClasses,col.names=col.names)
                          #if(header)names(comp.tmp)<-name.label
                          if(header)names(comp.tmp)<-col.names
                          if(checkEndRec){
                            tmp<-(comp.tmp==-1)
                            tmp<-apply(tmp,1,FUN=function(x){sum(x,na.rm=TRUE)})
                            tmp<-(tmp!=(dim(comp.tmp)[2]-2))
                            comp.tmp<-comp.tmp[tmp,]
                          }
                          if(as.numeric.as.possible){
                            cat("HERE212 in readDat.r\n")
                            browser()
                            comp.tmp<-
                              apply(comp.tmp,c(1,2),FUN=function(x){
                          if(length(grep(x=x,pattern="[[:digit:]]"))>0){return(as.numeric(x))}else{return(x)}})
                            comp.tmp<-as.data.frame(compoent)
                          }
                          if(!is.null(as.numeric.col)){
                            comp.tmp[,as.numeric.col]<-apply(comp.tmp[,as.numeric.col],c(1,2),as.numeric)
                          }
                          return(comp.tmp)
                        },report[[1]],headerL,EL)
      return(component)
    }
}
