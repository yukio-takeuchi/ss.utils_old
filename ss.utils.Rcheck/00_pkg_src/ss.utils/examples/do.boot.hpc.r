source("/home/Rscripts/ss_bootstrap.r")
res<-parSsboot(mcore=11,nboot=300,,namesfile="Starter.SS",orgdir="./orgrun",
  control.boot="control_boot_scale10.ss"ss3.arg=" -nox -nohess")