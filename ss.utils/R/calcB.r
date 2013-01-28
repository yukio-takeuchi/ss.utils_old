calcB<-function(ypr,SR,debug2=FALSE,tol=0.01){
    cnt<-0
#    browser()
    R<-SR$SRfn(ypr$spr*ypr$R0)
    diff<-abs(ypr$R0-R)
    if(debug2)cat(paste("R0,R,SSB,diff:",prettyNum(ypr$R0),prettyNum(R),prettyNum(ypr$spr*R),prettyNum(diff),"\n"))
    while(cnt<100 && diff>tol){
      Rnew<-SR$SRfn(ypr$spr*R)
      if(debug2)cat(paste("R,Rnew,SSB:",prettyNum(R),prettyNum(Rnew),prettyNum(ypr$spr*R)),"\n")
      diff<-abs(R-Rnew)
      R<-Rnew
      cnt<-cnt+1
    }
    if(cnt>99){cat(paste("cnt=",cnt," in calcFmax"));browser()}
    ypr$SSB<-ypr$spr*R
    ypr$Yield<-ypr$ypr*R
    ypr$Req<-R
  return(ypr)
}