pkgname <- "ss.utils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ss.utils')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("arr2coh")
### * arr2coh

flush(stderr()); flush(stdout())

### Name: arr2coh
### Title: Function x
### Aliases: arr2data.frame bubble calc.aggregateF calcGenders calFAA.ss3.x
###   checkSSversion coh2arr componentEndL compReport.sso
###   createNonParamBoot.dat.file createResampledCPUE do.boot do.retro
###   do.ss getAgecomp.ss3.1.x getAgecomp.ss3.2.x
###   getAgecomp.ss3.2.x.20090618 getAgecomp.ss3.x getALK.ss3.x
###   getAnnualNAA.ss3.x getBabs.ss3.x getCAA.ss3.x getCAA.y.ss3.x
###   getComponent getCPUE.ss3.x getCPUEfit.ss3.x getCPUEfromDat
###   getDerivedQuant getFAA.y.ss3 getHeaderL getHeaderL.old getKeywords
###   getKWS.old getKWS getLenSelex.old getLenSelex getMorphIndexing
###   getNAA.ss3.x getNAA.y.ss3 getNMA.ss3.x getRecruitmentDist
###   getReport.information getReport.sso getReport.sso.old getSPR.ss3.x
###   plotBubble plotCI plotComps.lattice plotComps plotEffNts plotMult
###   plotRecruit.retro plotSim plotSSB.boots plotSSB.retro prj.annual
###   read.AGE_SELEX read.spawner_recruit read.starter read.table.texts
###   readDat readLinesInteract readSSmodel readWithComments set.mypar
###   setncol SS_readdat SS_readstarter SS_splitdat SS_writedat
###   SS_writestarter summarySS summaryProfile writeSSmodel
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object) 
{
    dobj <- dim(object)
    dflq <- dobj
    dflq[2] <- dobj[2] + dobj[1] - 1
    flq <- array(NA, dim = dflq)
    for (i in 1:dflq[1]) flq[i, (dobj[1] - i + 1):(dflq[2] + 
        1 - i)] <- object[i, ]
    return(flq)
  }



cleanEx()
nameEx("ss.utils-package")
### * ss.utils-package

flush(stderr()); flush(stdout())

### Name: ss.utils-package
### Title: package of set of small functions for SS
### Aliases: ss.utils-package ss.utils
### Keywords: package

### ** Examples

  getwd()
  calcBRP(year=2007:2009,repfile="../ss.utils/data/Report.sso")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
