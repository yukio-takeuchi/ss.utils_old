\name{arr2coh}
\alias{arr2data.frame}
\alias{bubble}
\alias{calc.aggregateF}
\alias{calcGenders}
\alias{calFAA.ss3.x}
\alias{checkSSversion}
\alias{coh2arr}
\alias{componentEndL}
\alias{compReport.sso}
\alias{createNonParamBoot.dat.file}
\alias{createResampledCPUE}
\alias{do.boot}
\alias{do.retro}
\alias{do.ss}
\alias{getAgecomp.ss3.1.x}
\alias{getAgecomp.ss3.2.x}
\alias{getAgecomp.ss3.2.x.20090618}
\alias{getAgecomp.ss3.x}
\alias{getALK.ss3.x}
\alias{getAnnualNAA.ss3.x}
\alias{getBabs.ss3.x}
\alias{getCAA.ss3.x}
\alias{getCAA.y.ss3.x}
\alias{getComponent}
\alias{getCPUE.ss3.x}
\alias{getCPUEfit.ss3.x}
\alias{getCPUEfromDat}
\alias{getDerivedQuant}
\alias{getFAA.y.ss3}
\alias{getHeaderL}
\alias{getHeaderL.old}
\alias{getKeywords}
\alias{getKWS.old}
\alias{getKWS}
\alias{getLenSelex.old}
\alias{getLenSelex}
\alias{getMorphIndexing}
\alias{getNAA.ss3.x}
\alias{getNAA.y.ss3}
\alias{getNMA.ss3.x}
\alias{getRecruitmentDist}
\alias{getReport.information}
\alias{getReport.sso}
\alias{getReport.sso.old}
\alias{getSPR.ss3.x}
\alias{plotBubble}
\alias{plotCI}
\alias{plotComps.lattice}
\alias{plotComps}
\alias{plotEffNts}
\alias{plotMult}
\alias{plotRecruit.retro}
\alias{plotSim}
\alias{plotSSB.boots}
\alias{plotSSB.retro}
\alias{prj.annual}
\alias{read.AGE_SELEX}
\alias{read.spawner_recruit}
\alias{read.starter}
\alias{read.table.texts}
\alias{readDat}
\alias{readLinesInteract}
\alias{readSSmodel}
\alias{readWithComments}
\alias{set.mypar}
\alias{setncol}
\alias{SS_readdat}
\alias{SS_readstarter}
\alias{SS_splitdat}
\alias{SS_writedat}
\alias{SS_writestarter}
\alias{summarySS}
\alias{summaryProfile}
\alias{writeSSmodel}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function x
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arr2coh(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
