fl<-dir("../ss.utils/R")
cat(fl)
cat("\n")
cat("start\n")
for(i in 1:length(fl)){
  cat(i,"\n")
  fn<-paste("../ss.utils/R/",fl[i],sep="")
  cat("filename=",fn,"\n")
  source(fn)
}
#sapply(fl[1],FUN=function(x){source(paste("../ss.utils/R/",x,sep=""));cat(x,"\n");return("OK")})
cat("finished\n")
